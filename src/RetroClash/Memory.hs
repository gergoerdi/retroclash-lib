{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory
    ( RAM, ROM, Port, Port_
    , packRam

    , memoryMap, memoryMap_

    , conduit, readWrite, readWrite_
    , romFromVec, romFromFile
    , ram0, ramFromFile
    , port, port_
    , connect

    , override

    , from
    , matchLeft, matchRight
    , tag
    ) where

import Clash.Prelude hiding (Exp, lift)
import RetroClash.Utils
import RetroClash.Port
import Data.Maybe
import Control.Arrow (second)
import Control.Monad
import Control.Monad.RWS
import Data.Kind (Type)
import Control.Arrow (first)

import Data.List as L
import Data.Map as Map

import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH.Syntax as TH

type RAM dom addr dat = Signal dom addr -> Signal dom (Maybe (addr, dat)) -> Signal dom dat
type ROM dom addr dat = Signal dom addr ->                                   Signal dom dat
type Port dom addr dat a = Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a)
type Port_ dom addr dat = Signal dom (Maybe (PortCommand addr dat)) -> Signal dom (Maybe dat)

packRam :: (BitPack dat) => RAM dom addr (BitVector (BitSize dat)) -> RAM dom addr dat
packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

data Handle s (addr :: Type) = Handle Name

type FanIn dom a = Signal dom `Ap` First (Maybe a)

-- | type Addr dom addr = TExpQ (Signal dom (Maybe addr))
type Addr = ExpQ
type Addrs = [Addr]

-- | type Dat dom dat = TExpQ (Signal dom (Maybe dat))
type Dat = ExpQ

-- | type Component dom dat a = TExpQ (Signal dom (Maybe dat), a)
type Component = ExpQ

-- | type MkComponent dom dat addr a = TExpQ (Signal dom (Maybe addr)) -> TExpQ (Signal dom (Maybe dat), a)
type MkComponent = Addr -> Component

newtype ComponentMap = ComponentMap
    { components :: Map Name (PatQ, MkComponent) }
    deriving newtype (Semigroup, Monoid)

newtype ConnectionMap = ConnectionMap
    { connections :: Map Name Addrs }
    deriving newtype (Monoid)

instance Semigroup ConnectionMap where
    ConnectionMap m1 <> ConnectionMap m2 = ConnectionMap $ Map.unionWith (<>) m1 m2

newtype Addressing (s :: Type) (addr :: Type) (a :: Type) = Addressing
    { runAddressing :: RWST
          (Addr, Dat)
          (DecsQ, ComponentMap, ConnectionMap)
          ()
          Q
          a
    }
    deriving newtype (Functor, Applicative, Monad)

class Backpane a where
    backpane :: a -> ExpQ

instance Backpane () where
    backpane () = [|()|]

instance (Backpane a1, Backpane a2) => Backpane (a1, a2) where
    backpane (x1, x2) = [| ($(backpane x1), $(backpane x2)) |]

instance (Backpane a1, Backpane a2, Backpane a3) => Backpane (a1, a2, a3) where
    backpane (x1, x2, x3) = [| ($(backpane x1), $(backpane x2), $(backpane x3)) |]

data Result = Result ExpQ

instance Backpane Result where
    backpane (Result e) = e

compile
    :: forall addr a b. (Backpane a)
    => (forall s. Addressing s addr a)
    -> Addr
    -> Dat
    -> Component
compile addressing addr wr = do
    (x, (decs, components -> comps, connections -> conns)) <- evalRWST (runAddressing addressing) (addr, wr) ()

    (outs, compDecs) <- fmap L.unzip $ forM (Map.toList comps) $ \(nm, (pat, mkComp)) -> do
        let addrInE = case Map.lookup nm conns of
                Just addrs -> [| muxA $(listE addrs) |]
                Nothing -> [| pure Nothing |]
        addrIn <- newName "addrIn"
        masked <- newName "masked"
        let def =
                [d|
                 $(varP addrIn) = $addrInE
                 $pat = $(mkComp (varE addrIn))
                 $(varP masked) = mask (delay False $ isJust <$> $(varE addrIn)) $(varE nm)
                |]
        return (varE masked, def)

    let out = [| fmap (fromMaybe (Just 0) . getFirst) . getAp $ mconcat $(listE outs) |]

    decs <- mconcat (decs:compDecs)
    letE (pure <$> decs) [| ($out, $(backpane x)) |]

memoryMap
    :: forall addr a. (Backpane a)
    => Addr
    -> Dat
    -> (forall s. Addressing s addr a)
    -> Component
memoryMap addr wr addressing =
    [| let addr' = $addr; wr' = $wr
        in $(compile addressing [| addr' |] [| wr' |])
    |]

memoryMap_
    :: forall addr dat. ()
    => Addr
    -> Dat
    -> (forall s. Addressing s addr ())
    -> Dat
memoryMap_ addr wr addressing = [| fst $(memoryMap addr wr addressing) |]

connect
    :: Handle s addr
    -> Addressing s addr ()
connect h@(Handle nm) = Addressing $ do
    (addr, _) <- ask
    tell (mempty, mempty, ConnectionMap $ Map.singleton nm [addr])

override
    :: ExpQ
    -> Addressing s addr ()
override sig = Addressing $ do
    nm <- lift $ newName "override"
    let comp = \_ -> [| weak $sig |]
    tell (mempty, ComponentMap $ Map.singleton nm (varP nm, comp), mempty)

matchAddr
    :: forall addr' addr a s dat. ()
    => ExpQ {-(addr -> Maybe addr')-}
    -> Addressing s addr' a
    -> Addressing s addr a
matchAddr match body = Addressing $ do
    nm <- lift $ newName "addr"
    let addr' = varE nm
    RWST $ \(addr, wr) s -> do
        let dec = [d| $(varP nm) = $(restrict addr) |]
        runRWST
          (tell (dec, mempty, mempty) >> runAddressing body)
          (addr', wr)
          s
  where
    restrict :: Addr -> Addr
    restrict addr = [| (>>= $match) <$> $addr |]

conduit
    :: forall addr' s addr. ()
    => ExpQ
    -> Addressing s addr (Handle s addr', Result, Result)
conduit rdExt = Addressing $ do
    h@(Handle nm) <- Handle <$> (lift $ newName "rd")
    (_, wr) <- ask
    wrExt <- lift $ newName "wrExt"
    addrExt <- lift $ newName "addrExt"
    let comp = \addr ->
          [| (strong $rdExt, $addr, $wr) |]
    tell (mempty, ComponentMap $ Map.singleton nm ([p|($(varP nm), $(varP addrExt), $(varP wrExt))|], comp), mempty)
    return (h, Result (varE addrExt), Result (varE wrExt))

readWrite
    :: forall addr' addr s dat. ()
    => (Addr -> Dat -> Component)
    -> Addressing s addr (Handle s addr', Result)
readWrite component = Addressing $ do
    h@(Handle nm) <- Handle <$> (lift $ newName "rd")
    (_, wr) <- ask
    back <- lift $ newName "back"
    let comp = \addr -> [| first strong $(component addr wr) |]
    tell (mempty, ComponentMap $ Map.singleton nm ([p|($(varP nm), $(varP back))|], comp), mempty)
    return (h, Result (varE back))

readWrite_
    :: forall addr' addr s dat. ()
    => (Addr -> Dat -> Dat)
    -> Addressing s addr (Handle s addr')
readWrite_ component = fmap fst $ readWrite $ \addr wr -> [| ($(component addr wr), ()) |]

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ {-(Vec n dat)-}
    -> Addressing s addr (Handle s (Index n))
romFromVec size@SNat xs = readWrite_ $ \addr _wr ->
    [| fmap Just $ rom $xs (bitCoerce . fromJustX <$> $addr) |]

romFromFile
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing s addr (Handle s (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr _wr ->
    [| fmap (Just . unpack) $ romFilePow2 $fileName (bitCoerce . fromJustX <$> $addr) |]

ram0
    :: (1 <= n)
    => SNat n
    -> Addressing s addr (Handle s (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
    [| fmap Just $ blockRam1 NoClearOnReset $(TH.lift size) 0 (fromJustX <$> $addr) (liftA2 (,) <$> $addr <*> $wr) |]

ramFromFile
    :: SNat n
    -> ExpQ {-FilePath-}
    -> Addressing s addr (Handle s (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \addr wr ->
    [| fmap (Just . unpack) $
     blockRamFile size $fileName
       (fromJustX <$> $addr)
       (liftA2 (,) <$> $addr <*> (fmap pack <$> $wr))
    |]

port
    :: forall addr' a s addr. ()
    => ExpQ
    -> Addressing s addr (Handle s addr', Result)
port mkPort = readWrite $ \addr wr ->
  [| let (read, x) = $mkPort $ portFromAddr $addr $wr
     in (delay Nothing read, x) |]

port_
    :: forall addr' s addr. ()
    => ExpQ
    -> Addressing s addr (Handle s addr')
port_ mkPort = readWrite_ $ \addr wr ->
  [| let read = $mkPort $ portFromAddr $addr $wr in delay Nothing read |]

from
    :: forall addr' s addr a. (Integral addr, Ord addr, Integral addr', Bounded addr', Lift addr, Lift addr')
    => addr
    -> Addressing s addr' a
    -> Addressing s addr a
from base = matchAddr [| from_ $(TH.lift (base :: addr)) $(TH.lift (maxBound :: addr')) |]

tag
    :: (Lift addr')
    => addr'
    -> Addressing s (addr', addr) a
    -> Addressing s addr a
tag t = matchAddr [| \addr -> Just ($(TH.lift t), addr) |]

matchLeft
    :: Addressing s addr1 a
    -> Addressing s (Either addr1 addr2) a
matchLeft = matchAddr [| either Just (const Nothing) |]

matchRight
    :: Addressing s addr2 a
    -> Addressing s (Either addr1 addr2) a
matchRight = matchAddr [| either (const Nothing) Just |]

from_ :: forall addr' addr. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr -> addr' -> addr -> Maybe addr'
from_ base lim addr = do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= fromIntegral lim
    return (fromIntegral offset)

strong :: (Functor f) => f a -> f `Ap` First a
strong = Ap . fmap pure

weak :: (Functor f) => f (Maybe a) -> f `Ap` First (Maybe a)
weak = Ap . fmap (maybe mempty (pure . Just))

mask :: (Applicative f) => f Bool -> f `Ap` First a -> f `Ap` First a
mask p x = Ap $ mux p (getAp x) (pure mempty)
