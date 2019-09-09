{-# LANGUAGE ScopedTypeVariables, ApplicativeDo #-}
module RetroClash.Utils
    ( oneHot
    , activeLow, activeHigh
    , fromActiveLow, fromActiveHigh
    , countTo
    , nextIdx, prevIdx
    , succIdx, predIdx
    , shiftInLeft

    , mealyState
    , mealyStateSlow
    ) where

import Clash.Prelude
import Data.Maybe (fromMaybe)
import Control.Monad.State

oneHot :: forall n. (KnownNat n) => Index n -> Unsigned n
oneHot = bit . fromIntegral

activeHigh :: Bool -> Bit
activeHigh = boolToBit

activeLow :: Bool -> Bit
activeLow = complement . activeHigh

fromActiveHigh :: Bit -> Bool
fromActiveHigh = bitToBool

fromActiveLow :: Bit -> Bool
fromActiveLow = fromActiveHigh . complement

countTo :: (Eq a, Enum a, NFDataX a, HiddenClockResetEnable dom) => a -> a -> Signal dom a
countTo start target = counter
  where
    counter = register start $ mux (counter .==. pure target) (pure start) (succ <$> counter)

nextIdx :: (Eq a, Enum a, Bounded a) => a -> a
nextIdx = fromMaybe minBound . succIdx

prevIdx :: (Eq a, Enum a, Bounded a) => a -> a
prevIdx = fromMaybe maxBound . predIdx

succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x | x == maxBound = Nothing
          | otherwise = Just $ succ x

predIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
predIdx x | x == minBound = Nothing
          | otherwise = Just $ pred x

mealyState :: (HiddenClockResetEnable dom, NFDataX s)
           => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState = mealyStateSlow (pure True)

mealyStateSlow
    :: (HiddenClockResetEnable dom, NFDataX s)
    => Signal dom Bool
    -> (i -> State s o)
    -> s
    -> (Signal dom i -> Signal dom o)
mealyStateSlow tick f s0 x = mealy step s0 (bundle (tick, x))
  where
    step s (tick, x) = let (y, s') = runState (f x) s
                       in (if tick then s' else s, y)

shiftInLeft :: (BitPack a, KnownNat (BitSize a)) => Bit -> a -> (a, Bit)
shiftInLeft b bs = bitCoerce (b, bs)
