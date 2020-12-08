{-# LANGUAGE DataKinds, GADTs, PolyKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module RetroClash.Memory2 where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Port

import Data.Kind
import Data.Singletons.Prelude.List (type (++))
import Control.Monad.RWS hiding (Product)

import Data.Maybe
import Data.Kind
import Data.Dependent.Map as DMap
import Data.Dependent.Sum as DSum
import Data.GADT.Compare
import Type.Reflection

data Component {- s -} (addr :: Type) = Component (TypeRep addr) Int

instance GEq (Component {- s -}) where
    geq (Component a _) (Component b _) = geq a b

instance GCompare (Component {- s -}) where
    gcompare (Component a _) (Component b _) = gcompare a b

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap {- s -} dom = AddrMap{ addrMap :: DMap (Component {-s-}) (FanIn dom) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap {- s -} dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

newtype Susp0 dom dat addr = Susp0{ unSusp0 :: Signal dom (Maybe addr) -> Signal dom (Maybe dat) }
newtype Susp1 w dom dat addr = Susp1{ unSusp1 :: Signal dom (Maybe addr) -> (Signal dom (Maybe dat), w) }

type Addressing0 dom addr dat = RWS
    (Signal dom (Maybe dat))
    (AddrMap dom)
    Int

data Addressing dom addr dat (ts :: [Type]) a where
    Return :: a -> Addressing dom addr dat '[] a
    Bind :: Addressing dom addr dat ts a -> (a -> Addressing dom addr dat us b) -> Addressing dom addr dat (ts ++ us) b

    Fresh :: (Typeable addr') => Addressing dom addr dat '[] (Component addr')
    WR :: Addressing dom addr dat '[] (Signal dom (Maybe dat))
    Match :: (addr -> Maybe addr') -> Addressing dom addr' dat ts a -> Addressing dom addr dat ts a
    Connect :: Component addr -> Addressing dom addr dat '[] ()
    Tell0 :: DSum Component (Susp0 dom dat) -> Addressing dom addr dat '[] ()
    Tell1 :: DSum Component (Susp1 w dom dat) -> Addressing dom addr dat '[w] ()

data Susps dom dat (ts :: [Type]) where
    NilS :: Susps dom dat '[]
    Cons0 :: DSum Component (Susp0 dom dat) -> Susps dom dat ts -> Susps dom dat ts
    Cons1 :: DSum Component (Susp1 t dom dat) -> Susps dom dat ts -> Susps dom dat (t:ts)

data Results (ts :: [Type]) where
    NilR :: Results '[]
    ConsR :: a -> Results ts -> Results (a : ts)

concatS :: Susps dom dat ts -> Susps dom dat us -> Susps dom dat (ts ++ us)
concatS = \case
    NilS -> id
    Cons0 x xs -> Cons0 x . concatS xs
    Cons1 x xs -> Cons1 x . concatS xs

runAddressing1
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe addr)
    -> Addressing dom addr dat ts a
    -> Addressing0 dom addr dat
          ( Susps dom dat ts
          , a
          )
runAddressing1 addr = \case
    Return x -> nilS $ return x
    Bind m n -> do
        (rd1, x) <- runAddressing1 addr m
        (rd2, y) <- runAddressing1 addr $ n x
        return (concatS rd1 rd2, y)
    Fresh -> nilS $ Component typeRep <$> get <* modify succ
    WR -> nilS ask
    Match f body -> runAddressing1 (fmap (f =<<) addr) body
    Connect handle -> nilS $ do
        tell $ AddrMap $ DMap.singleton handle $ fanInMaybe addr -- TODO: block later connections
    Tell0 comp -> do
        return (Cons0 comp NilS, ())
    Tell1 comp -> do
        return (Cons1 comp NilS, ())
  where
    nilS act = do
        x <- act
        return (NilS, x)

runAddressing
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat ts a
    -> ( Signal dom (Maybe dat)
       , a
       , Results ts
       )
runAddressing addr wr body = (join <$> firstIn rd, x, xs)
  where
    ((susps, x), conns) = evalRWS (runAddressing1 addr body) wr 0
    (rd, xs) = toRead susps (addrMap conns)

toRead
    :: forall dom dat ts. (HiddenClockResetEnable dom)
    => Susps dom dat ts
    -> DMap Component (FanIn dom)
    -> (FanIn dom (Maybe dat), Results ts)
toRead susps conns = go susps
  where
    go :: Susps dom dat us -> (FanIn dom (Maybe dat), Results us)
    go = \case
        NilS -> (mempty, NilR)
        Cons0 (h :=> (Susp0 mk)) ss ->
            let (rd', xs) = go ss
            in (mappend rd rd', xs)
          where
            addr = fromMaybe mempty $ DMap.lookup h conns
            rd0 = mk (firstIn addr)
            rd = gated (delay False $ isJust <$> firstIn addr) $ fanIn rd0
        Cons1 (h :=> (Susp1 mk)) ss ->
            let (rd', xs) = go ss
            in (mappend rd rd', ConsR x xs)
          where
            addr = fromMaybe mempty $ DMap.lookup h conns
            (rd0, x) = mk (firstIn addr)
            rd = gated (delay False $ isJust <$> firstIn addr) $ fanIn rd0

readWrite
    :: (Typeable addr')
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing {- s -} dom addr dat '[a] (Component {- s -} addr')
readWrite mkComponent = do
    handle <- Fresh
    wr <- WR
    Tell1 $ handle :=> Susp1 (\addr -> mkComponent addr wr)
    return handle
  where
    return = Return
    (>>=) = Bind
    (=<<) = flip (>>=)
    m >> n = Bind m (const n)

readWrite_
    :: (Typeable addr')
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat)))
    -> Addressing {- s -} dom addr dat '[] (Component {- s -} addr')
readWrite_ mkComponent = do
    handle <- Fresh
    wr <- WR
    Tell0 $ handle :=> Susp0(\addr -> mkComponent addr wr)
    return handle
  where
    return = Return
    (>>=) = Bind
    (=<<) = flip (>>=)
    m >> n = Bind m (const n)

type Port dom addr dat a = Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a)
type Port_ dom addr dat = Signal dom (Maybe (PortCommand addr dat)) -> Signal dom (Maybe dat)

port
    :: (HiddenClockResetEnable dom, Typeable addr', NFDataX dat)
    => Port dom addr' dat a
    -> Addressing {- s -} dom addr dat '[a] (Component {- s -} addr')
port mkPort = readWrite $ \addr wr ->
    let (read, x) = mkPort $ portFromAddr addr wr
    in (delay Nothing read, x)

romFromFile
    :: (HiddenClockResetEnable dom, 1 <= n, BitPack dat)
    => SNat n
    -> FilePath
    -> Addressing {- s -} dom addr dat '[] (Component {- s -} (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr wr ->
    fmap (Just . unpack) $ romFilePow2 fileName (maybe 0 bitCoerce <$> addr)

ram0
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, Num dat)
    => SNat n
    -> Addressing {- s -} dom addr dat '[] (Component {- s -} (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
    fmap Just $ blockRam1 ClearOnReset size 0 (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> wr)


matchAddr
    :: (addr -> Maybe addr')
    -> Addressing {- s -} dom addr' dat ts a
    -> Addressing {- s -} dom addr dat ts a
matchAddr = Match

matchLeft
    :: Addressing {- s -} dom addr1 dat ts a
    -> Addressing {- s -} dom (Either addr1 addr2) dat ts a
matchLeft = matchAddr $ either Just (const Nothing)

matchRight
    :: Addressing {- s -} dom addr2 dat ts a
    -> Addressing {- s -} dom (Either addr1 addr2) dat ts a
matchRight = matchAddr $ either (const Nothing) Just

from
    :: forall addr' s dom addr dat ts a. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> Addressing {- s -} dom addr' dat ts a
    -> Addressing {- s -} dom addr dat ts a
from base = matchAddr $ \addr -> do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= lim
    return $ fromIntegral offset
  where
    lim = fromIntegral (maxBound :: addr')

connect
    :: Component {- s -} addr
    -> Addressing {- s -} dom addr dat '[] ()
connect = Connect

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

gated :: Signal dom Bool -> FanIn dom a -> FanIn dom a
gated p sig = fanInMaybe $ mux p (firstIn sig) (pure Nothing)
