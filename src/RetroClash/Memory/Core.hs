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
import Control.Arrow (first)
import Data.Maybe
import Control.Monad
import RetroClash.Internal.Monoid
import RetroClash.Internal.RWS
import RetroClash.Internal.Assoc as Map
import Unsafe.Coerce

type Key = Int
data Component s addr = Component Key

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: Map Key (FanIn dom ()) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn dom addr, Signal dom (Maybe dat), AddrMap s dom)
          (FanIn dom (Maybe dat), AddrMap s dom)
          Int
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
    (x, (read, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, wr, conns) 0

memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr ())
    -> Signal dom (Maybe dat)
memoryMap_ addr wr body = fst $ memoryMap addr wr body

readWrite
    :: forall addr' a s dom dat addr. (HiddenClockResetEnable dom)
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing s dom dat addr (Component s addr', a)
readWrite mkComponent = Addressing $ do
    component@(Component k) <- Component <$> get <* modify succ
    (_, wr, addrs) <- ask
    let addr = firstIn . unsafeCoerce $ Map.lookup k (addrMap addrs)
        selected = isJust <$> addr
    let (read, x) = mkComponent addr wr
    tell (gated (delay False selected) (fanIn read), mempty)
    return (component, x)

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ rws $ \(addr, wr, addrs) s ->
  let addr' = fanInMaybe . fmap (match =<<) . firstIn $ addr
      (x, s', (read, components)) = runRWS (unAddressing body) (addr', wr, addrs) s
  in (x, s', (read, components))

gated :: Signal dom Bool -> FanIn dom a -> FanIn dom a
gated p sig = fanInMaybe $ mux p (firstIn sig) (pure Nothing)

override
    :: Signal dom (Maybe dat)
    -> Addressing s dom dat addr a
    -> Addressing s dom dat addr a
override sig = Addressing . censor (first $ mappend sig') . unAddressing
  where
    sig' = gated (isJust <$> sig) (fanIn sig)

connect
    :: Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component k) = Addressing $ do
    (addr, _, _) <- ask
    tell (mempty, AddrMap $ Map.singleton k $ unsafeCoerce addr)

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure