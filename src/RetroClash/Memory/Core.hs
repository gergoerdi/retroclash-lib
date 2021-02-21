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

import RetroClash.Internal.Assoc as Map
import Unsafe.Coerce

type Key = Int
data Component s addr = Component Key

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: Map Key (FanIn dom ()) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ Map.unionWithKey (const mappend) map1 map2

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
    :: forall addr' a s dom dat addr. ()
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing s dom dat addr (Component s addr', a)
readWrite mkComponent = Addressing $ do
    component@(Component k) <- Component <$> get <* modify succ
    (_, wr, _, addrs) <- ask
    let addr = firstIn . unsafeCoerce $ Map.lookup k (addrMap addrs)
    let (read, x) = mkComponent addr wr
    tell (mempty, ReadMap $ Map.singleton k (fanIn read), mempty)
    return (component, x)

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ rws $ \(addr, wr, reads, addrs) s ->
  let addr' = fanInMaybe . fmap (match =<<) . firstIn $ addr
      selected = isJust <$> firstIn addr'
      (x, s', (read, reads', components)) = runRWS (unAddressing body) (addr', wr, reads, addrs) s
  in (x, s', (read, reads', components))

gated :: Signal dom Bool -> FanIn dom a -> FanIn dom a
gated p sig = fanInMaybe $ mux p (firstIn sig) (pure Nothing)

override
    :: (Show dat)
    => Signal dom (Maybe dat)
    -> Addressing s dom dat addr ()
override sig = Addressing $ do
    (addr, _, _, _) <- ask
    let selected = isJust <$> firstIn addr
        sig' = gated (selected .&&. isJust <$> sig) (fanIn sig)
    tell (sig', mempty, mempty)

connect
    :: (HiddenClockResetEnable dom)
    => Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component k) = Addressing $ do
    (addr, _, reads, _) <- ask
    let read = Map.lookup k (readMap reads)
        selected = isJust <$> firstIn addr
    tell (gated (delay False selected) read, mempty, AddrMap $ Map.singleton k $ unsafeCoerce addr)

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure
