{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module RetroClash.Memory
    ( memoryMap_

    , romFromVec
    , ramFromFile
    , connect

    , from
    ) where

import Clash.Prelude hiding (Exp, lift)
import RetroClash.Utils
import RetroClash.Port
import Data.Maybe
import Control.Monad
import Control.Monad.RWS
import Data.Kind

import Data.Map as Map
import Data.List as L
import Data.Function (on)

import Data.Kind
import Data.Dependent.Map as DMap
import Data.Dependent.Sum as DSum
import Data.GADT.Compare
import Type.Reflection

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type, lift)
import qualified Language.Haskell.TH.Syntax as TH

data Handle s (addr :: Type) = Handle Name (TypeRep addr)

instance GEq (Handle s) where
    geq (Handle a ta) (Handle b tb) = do
        Refl <- geq ta tb
        guard $ a == b
        return Refl

instance GCompare (Handle s) where
    gcompare (Handle a ta) (Handle b tb) = case gcompare ta tb of
        GLT -> GLT
        GGT -> GGT
        GEQ -> case compare a b of
            LT -> GLT
            GT -> GGT
            EQ -> GEQ

type FanIn dom a = Signal dom `Ap` First (Maybe a)

newtype Component s dom dat addr = Component
    { mkComponent :: ExpQ {-(Signal dom (Maybe addr))-} -> ExpQ {-(Signal dom (Maybe dat))-} }

newtype Connections s dom addr = Connections
    { getAddrs :: [ExpQ {-(Signal dom (Maybe addr))-}] }

newtype ComponentMap s dom dat = ComponentMap
    { components :: DMap (Handle s) (Component s dom dat) }
    deriving newtype (Semigroup, Monoid)

newtype ConnectionMap s dom = ConnectionMap
    { connections :: DMap (Handle s) (Connections s dom) }
    deriving newtype (Monoid)

instance Semigroup (ConnectionMap s dom) where
    ConnectionMap m1 <> ConnectionMap m2 = ConnectionMap $
        DMap.unionWithKey (\_ c1 c2 -> Connections (getAddrs c1 <> getAddrs c2)) m1 m2

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
            | h@(Handle nm _) :=> comp <- DMap.toList comps
            , let addrIn = case DMap.lookup h conns of
                      Just (getAddrs -> addrs) -> [| muxA $(listE addrs) |]
                      Nothing -> [| pure Nothing |]
            , let rd = mkComponent comp addrIn
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
    :: forall addr' addr s dom dat. (Typeable addr')
    => (ExpQ {-(Signal dom (Maybe addr'))-} -> ExpQ {-(Signal dom (Maybe dat))-} -> ExpQ {-(Signal dom (Maybe dat))-})
    -> Addressing s dom dat addr (Handle s addr')
readWrite_ component = Addressing $ do
    h@(Handle nm _) <- Handle <$> (lift $ newName "rd") <*> pure typeRep
    (_, wr) <- ask
    let comp = Component $ \addr -> [| strong $(component addr wr) |]
    tell (mempty, ComponentMap $ DMap.singleton h comp, mempty, mempty)
    return h

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ {-(Vec n dat)-}
    -> Addressing s dom dat addr (Handle s (Index n))
romFromVec size@SNat xs = readWrite_ $ \addr _wr ->
    [| fmap Just $ rom $xs (maybe 0 bitCoerce <$> $addr) |]

ramFromFile
    :: SNat n
    -> ExpQ {-FilePath-}
    -> Addressing s dom dat addr (Handle s (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \addr wr ->
    [| fmap (Just . unpack) $
     singlePort (blockRamFile size $fileName)
       (fromMaybe 0 <$> $addr)
       (fmap pack <$> $wr)
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
connect h@(Handle nm _) = Addressing $ do
    (addr, _) <- ask
    let conn = ConnectionMap $ DMap.singleton h $ Connections [addr]
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
