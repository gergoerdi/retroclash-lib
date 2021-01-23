{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory.Core
    ( Addressing
    , Component

    , memoryMap, memoryMap_

    , readWrite
    , connect

    , override

    , matchAddr
    ) where

import Clash.Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import Data.Kind
import Data.Dependent.Map as DMap
import Data.Map as Map
import Data.GADT.Compare
import Type.Reflection

type Key = Int
data Component s (addr :: Type) = Component (TypeRep addr) Key

instance GEq (Component s) where
    geq (Component a x) (Component b y) = do
        p@Refl <- geq a b
        guard $ x == y
        return p

instance GCompare (Component s) where
    gcompare (Component a x) (Component b y) = case gcompare a b of
        GEQ -> case compare x y of
            LT -> GLT
            EQ -> GEQ
            GT -> GGT
        ord -> ord

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: DMap (Component s) (FanIn dom) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ DMap.unionWithKey (const mappend) map1 map2

newtype ReadMap s dom dat = ReadMap{ readMap :: Map Key (FanIn dom (Maybe dat)) }
    deriving newtype (Monoid)

instance Semigroup (ReadMap s dom dat) where
    ReadMap map1 <> ReadMap map2 = ReadMap $ Map.unionWithKey (const mappend) map1 map2

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn dom addr, Signal dom (Maybe dat), ReadMap s dom dat, AddrMap s dom)
          (FanIn dom (Maybe dat), ReadMap s dom dat, AddrMap s dom)
          Key
          a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr a)
    -> (Signal dom (Maybe dat), a)
memoryMap addr wr body = (join <$> firstIn read, x)
  where
    (x, (read, reads, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, wr, reads, conns) 0

memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr ())
    -> Signal dom (Maybe dat)
memoryMap_ addr wr body = fst $ memoryMap addr wr body

readWrite
    :: forall addr' a s dom dat addr. (HiddenClockResetEnable dom, Typeable addr')
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing s dom dat addr (Component s addr', a)
readWrite mkComponent = Addressing $ do
    component@(Component _ i) <- Component typeRep <$> get <* modify succ
    (_, wr, _, addrs) <- ask
    let addr = firstIn . fromMaybe (error "readWrite") $ DMap.lookup component (addrMap addrs)
        selected = isJust <$> addr
    let (read, x) = mkComponent addr wr
    tell (mempty, ReadMap $ Map.singleton i $ gated (delay False selected) (fanIn read), mempty)
    return (component, x)

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ rws $ \(addr, wr, reads, addrs) s ->
  let addr' = fanInMaybe . fmap (match =<<) . firstIn $ addr
      (x, s', (read, reads', components)) = runRWS (unAddressing body) (addr', wr, reads, addrs) s
  in (x, s', (read, reads', components))

gated :: Signal dom Bool -> FanIn dom a -> FanIn dom a
gated p sig = fanInMaybe $ mux p (firstIn sig) (pure Nothing)

override
    :: (Show dat)
    => Signal dom (Maybe dat)
    -> Addressing s dom dat addr a
    -> Addressing s dom dat addr a
override sig body = Addressing $ do
    tell (sig', mempty, mempty)
    -- local (\(addr, wr, reads, addrs) -> (gated (isNothing <$> sig) addr, wr, reads, addrs)) $ unAddressing body
    unAddressing body
  where
    sig' = gated (isJust <$> sig) (fanIn sig)

connect
    :: Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component _ i) = Addressing $ do
    (addr, _, reads, _) <- ask
    let read = fromMaybe (error "connect") $ Map.lookup i (readMap reads)
    tell (read, mempty, AddrMap $ DMap.singleton component addr)

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure
