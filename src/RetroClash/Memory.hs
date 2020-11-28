{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, RecursiveDo #-}
module RetroClash.Memory where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Port
import Control.Arrow (second)
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import Data.Kind
import Data.Dependent.Map as DMap
import Data.GADT.Compare
import Type.Reflection

type RAM dom addr dat = Signal dom addr -> Signal dom (Maybe (addr, dat)) -> Signal dom dat
type ROM dom addr dat = Signal dom addr ->                                   Signal dom dat
type Port dom addr dat a = Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a)

packRam :: (BitPack dat) => RAM dom addr (BitVector (BitSize dat)) -> RAM dom addr dat
packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

data Component s (addr :: Type) = Component (TypeRep addr) Int

instance GEq (Component s) where
    geq (Component a _) (Component b _) = geq a b

instance GCompare (Component s) where
    gcompare (Component a _) (Component b _) = gcompare a b

newtype FanIn dom addr = FanIn{ fanIn :: Signal dom `Ap` First addr }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: DMap (Component s) (FanIn dom) }

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

instance Monoid (AddrMap s dom) where
    mempty = AddrMap mempty

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (Signal dom (Maybe addr), Signal dom (Maybe dat), AddrMap s dom)
          (Signal dom `Ap` First (Maybe dat), AddrMap s dom)
          Int
          a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr a)
    -> (Signal dom (Maybe dat), a)
memoryMap addr wr body = (join . getFirst <$> read, x)
  where
    (x, (Ap read, conns)) = evalRWS (unAddressing body) (addr, wr, conns) 0

readWrite_
    :: (HiddenClockResetEnable dom, Typeable addr')
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> Signal dom (Maybe dat))
    -> Addressing s dom dat addr (Component s addr')
readWrite_ mkComponent = fmap fst $ readWrite $ \addr wr ->
    let read = mkComponent addr wr
    in (read, ())

theType :: forall (a :: Type). (Typeable a) => TypeRep a
theType = typeOf undefined

conduit
    :: (Typeable addr', HiddenClockResetEnable dom)
    => Signal dom (Maybe dat)
    -> Addressing s dom dat addr (Component s addr', Signal dom (Maybe addr'), Signal dom (Maybe dat))
conduit read = Addressing $ do
    component@(Component _ i) <- Component theType <$> get <* modify succ
    (_, wr, addrs) <- ask
    let addr = fmap getFirst . getAp $ maybe mempty fanIn $ DMap.lookup component (addrMap addrs)
        selected = isJust <$> addr
    tell (Ap $ mux (register False selected) (pure <$> read) (pure mempty), mempty)
    return (component, addr, wr)

readWrite
    :: (HiddenClockResetEnable dom, Typeable addr')
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing s dom dat addr (Component s addr', a)
readWrite mkComponent = Addressing $ do
    rec (component, addr, wr) <- unAddressing $ conduit read
        let (read, x) = mkComponent addr wr
    return (component, x)

romFromFile
    :: (HiddenClockResetEnable dom, 1 <= n, BitPack dat)
    => SNat n
    -> FilePath
    -> Addressing s dom dat addr (Component s (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr _wr ->
    fmap (Just . unpack) $ romFilePow2 fileName (maybe 0 bitCoerce <$> addr)

ram0
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, Num dat)
    => SNat n
    -> Addressing s dom dat addr (Component s (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
      fmap Just $ blockRam1 ClearOnReset size 0 (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> wr)

port
    :: (HiddenClockResetEnable dom, Typeable addr', NFDataX dat)
    => Port dom addr' dat a
    -> Addressing s dom dat addr (Component s addr', a)
port mkPort = readWrite $ \addr wr ->
    let (read, x) = mkPort $ portFromAddr addr wr
    in (delay Nothing read, x)

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ do
    (addr, wr, addrs) <- ask
    let addr' = (match =<<) <$> addr
        wr' = mux (isJust <$> addr') wr (pure Nothing)
    s <- get
    let (x, s', (read, components)) = runRWS (unAddressing body) (addr', wr', addrs) s
    put s'
    let read' = Ap . fmap First $ mux (isJust <$> addr') (fmap getFirst . getAp $ read) (pure Nothing)
    tell (read', components)
    return x

tag
    :: addr'
    -> Addressing s dom dat (addr', addr) a
    -> Addressing s dom dat addr a
tag t = matchAddr $ \addr -> Just (t, addr)

matchLeft
    :: (HiddenClockResetEnable dom)
    => Addressing s dom dat addr1 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchLeft = matchAddr $ either Just (const Nothing)

matchRight
    :: (HiddenClockResetEnable dom)
    => Addressing s dom dat addr2 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchRight = matchAddr $ either (const Nothing) Just

override
    :: forall dom dat addr a s. (HiddenClockResetEnable dom)
    => Signal dom (Maybe dat)
    -> Addressing s dom dat addr a
    -> Addressing s dom dat addr a
override sig = Addressing . censor (\(read, conns) -> (overrule read, conns)) . unAddressing
  where
    overrule :: Signal dom `Ap` First (Maybe dat) -> Signal dom `Ap` First (Maybe dat)
    overrule read = Ap $ mux (isJust <$> sig) (pure <$> sig) (getAp read)

from
    :: forall addr' s dom dat addr a. (Integral addr, Ord addr, Integral addr', Bounded addr', Show addr, Show addr')
    => addr
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
from base = matchAddr $ \addr -> do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= lim
    return $ fromIntegral offset
  where
    lim = fromIntegral @addr' @addr maxBound

connect
    :: Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component _ i) = Addressing $ do
    (addr, _, _) <- ask
    tell (mempty, AddrMap $ DMap.singleton component (FanIn . Ap . fmap First $ addr))
