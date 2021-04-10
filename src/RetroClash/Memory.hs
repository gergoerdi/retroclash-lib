{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory
    ( RAM, ROM, Port, Port_
    , packRam

    , memoryMap_

    , romFromVec
    , ramFromFile
    , connect

    , from
    ) where

import Clash.Prelude hiding (Exp, lift)
import RetroClash.Utils
import RetroClash.Port
import Data.Maybe
import Control.Arrow (second)
import Control.Monad
import Control.Monad.RWS
import Data.Kind (Type)

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

type MkComponent s dom dat = ExpQ {-(Signal dom (Maybe addr))-} -> ExpQ {-(Signal dom (Maybe dat))-}
type AddrIn s dom = ExpQ {-(Signal dom (Maybe addr))-}

type AddrIns s dom = [AddrIn s dom]

newtype ComponentMap s dom dat = ComponentMap
    { components :: Map Name (MkComponent s dom dat) }
    deriving newtype (Semigroup, Monoid)

newtype ConnectionMap s dom = ConnectionMap
    { connections :: Map Name (AddrIns s dom) }
    deriving newtype (Monoid)

instance Semigroup (ConnectionMap s dom) where
    ConnectionMap m1 <> ConnectionMap m2 = ConnectionMap $ Map.unionWith (<>) m1 m2

newtype Addressing (s :: Type) (dom :: Domain) (dat :: Type) (addr :: Type) (a :: Type) = Addressing
    { runAddressing :: RWST
          (ExpQ {-(Signal dom (Maybe addr))-}, ExpQ {-(Signal dom (Maybe dat))-})
          (DecsQ, ComponentMap s dom dat, ConnectionMap s dom, [ExpQ {-(FanIn dom dat)-}])
          ()
          Q
          a
    }
    deriving newtype (Functor, Applicative, Monad)

compile
    :: forall addr dom dat a b. ()
    => (forall s. Addressing s dom dat addr ())
    -> ExpQ {-(Signal dom (Maybe addr))-}
    -> ExpQ {-(Signal dom (Maybe dat))-}
    -> ExpQ {-(Signal dom (Maybe dat))-}
compile addressing addr wr = do
    ((), (decs, components -> comps, connections -> conns, outs)) <- evalRWST (runAddressing addressing) (addr, wr) ()

    let compDecs = mconcat
            [ [d| $(varP nm) = $rd |]
            | (nm, mkComp) <- Map.toList comps
            , let addrIn = case Map.lookup nm conns of
                      Just addrs -> [| muxA $(listE addrs) |]
                      Nothing -> [| pure Nothing |]
            , let rd = mkComp addrIn
            ]

    let out = [| fmap (fromMaybe (Just 0) . getFirst) . getAp $ mconcat $(listE outs) |]

    decs <- decs <> compDecs
    letE (pure <$> decs) out

memoryMap_
    :: forall addr dat dom. ()
    => ExpQ {-(Signal dom (Maybe addr))-}
    -> ExpQ {-(Signal dom (Maybe dat))-}
    -> (forall s. Addressing s dom dat addr ())
    -> ExpQ {-(Signal dom (Maybe dat))-}
memoryMap_ addr wr addressing =
    [| let addr' = $addr; wr' = $wr
        in $(compile addressing [| addr' |] [| wr' |])
    |]

matchAddr
    :: forall addr' addr a s dom dat. ()
    => ExpQ {-(addr -> Maybe addr')-}
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ do
    nm <- lift $ newName "addr"
    let addr' = varE nm
    censor (\(decls, comps, conns, outs) -> (decls, comps, conns, [applyMask addr' outs])) $
        RWST $ \(addr, wr) s -> do
            let dec = [d| $(varP nm) = $(restrict addr) |]
            runRWST
              (tell (dec, mempty, mempty, mempty) >> runAddressing body)
              (addr', wr)
              s
  where
    restrict :: ExpQ {-(Signal dom (Maybe addr))-} -> ExpQ {-(Signal dom (Maybe addr'))-}
    restrict addr = [| (>>= $match) <$> $addr |]

    applyMask :: ExpQ {-(Signal dom (Maybe addr'))-} -> [ExpQ {-(FanIn dom dat)-}] -> ExpQ {-(FanIn dom dat)-}
    applyMask addr' outs = [| mask (delay False $ isJust <$> $addr') (mconcat $(listE outs)) |]

readWrite_
    :: forall addr' addr s dom dat. ()
    => (ExpQ {-(Signal dom (Maybe addr'))-} -> ExpQ {-(Signal dom (Maybe dat))-} -> ExpQ {-(Signal dom (Maybe dat))-})
    -> Addressing s dom dat addr (Handle s addr')
readWrite_ component = Addressing $ do
    h@(Handle nm) <- Handle <$> (lift $ newName "rd")
    (_, wr) <- ask
    let comp = \addr -> [| strong $(component addr wr) |]
    tell (mempty, ComponentMap $ Map.singleton nm comp, mempty, mempty)
    return h

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ {-(Vec n dat)-}
    -> Addressing s dom dat addr (Handle s (Index n))
romFromVec size@SNat xs = readWrite_ $ \addr _wr ->
    [| fmap Just $ rom $xs (bitCoerce . fromJustX <$> $addr) |]

ramFromFile
    :: SNat n
    -> ExpQ {-FilePath-}
    -> Addressing s dom dat addr (Handle s (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \addr wr ->
    [| fmap (Just . unpack) $
     blockRamFile size $fileName
       (fromJustX <$> $addr)
       (liftA2 (,) <$> $addr <*> (fmap pack <$> $wr))
    |]

from
    :: forall addr' s dom dat addr a. (Integral addr, Ord addr, Integral addr', Bounded addr', Lift addr, Lift addr')
    => addr
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
from base = matchAddr [| from_ $(TH.lift (base :: addr)) $(TH.lift (maxBound :: addr')) |]

connect
    :: Handle s addr
    -> Addressing s dom dat addr ()
connect h@(Handle nm) = Addressing $ do
    (addr, _) <- ask
    let conn = ConnectionMap $ Map.singleton nm [addr]
        out = varE nm
    tell (mempty, mempty, conn, [out])

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
