{-# LANGUAGE ScopedTypeVariables, ApplicativeDo, Rank2Types #-}
module RetroClash.Utils
    ( withResetEnableGen
    , withEnableGen

    , activeLow, activeHigh
    , fromActiveLow, fromActiveHigh
    , countTo
    , nextIdx, prevIdx
    , succIdx, predIdx
    , shiftInLeft

    , oneHot
    , roundRobin

    , mealyState
    , mealyStateSlow
    ) where

import Clash.Prelude
import Data.Maybe (fromMaybe)
import Control.Monad.State

oneHot :: forall n. (KnownNat n) => Index n -> Vec n Bool
oneHot = bitCoerce @(Unsigned n) . bit . fromIntegral

roundRobin
    :: forall n div dom a. (KnownNat n, Eq div, Enum div, Num div, NFDataX div, HiddenClockResetEnable dom)
    => div
    -> (Signal dom (Vec n Bool), Signal dom (Index n))
roundRobin div = (selector, i)
  where
    i = regEn (0 :: Index n) timer $ nextIdx <$> i
    selector = bitCoerce . oneHot <$> i
    counter = countTo 0 div
    timer = counter .==. pure 0

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

withResetEnableGen
    :: (KnownDomain dom)
    => (HiddenClockResetEnable dom => r)
    -> Clock dom -> r
withResetEnableGen board clk = withClockResetEnable clk resetGen enableGen board

withEnableGen
    :: (KnownDomain dom)
    => (HiddenClockResetEnable dom => r)
    -> Clock dom -> Reset dom -> r
withEnableGen board clk rst = withClockResetEnable clk rst enableGen board
