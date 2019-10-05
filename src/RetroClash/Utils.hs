{-# LANGUAGE ScopedTypeVariables, ApplicativeDo, Rank2Types #-}
module RetroClash.Utils
    ( withResetEnableGen
    , withEnableGen

    , activeLow, activeHigh
    , fromActiveLow, fromActiveHigh

    , (.==)
    , (==.)

    , unchanged
    , debounce

    , oneHot
    , roundRobin

    , countTo
    , nextIdx, prevIdx
    , succIdx, predIdx
    , moreIdx, lessIdx
    , shiftInFromLeft

    , mealyState
    , mealyStateSlow
    ) where

import Clash.Prelude
import Data.Maybe (fromMaybe)
import Control.Monad.State
import RetroClash.Clock

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

oneHot :: forall n. (KnownNat n) => Index n -> Vec n Bool
oneHot = bitCoerce . bit @(Unsigned n) . fromIntegral

unchanged :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => a -> Signal dom a -> Signal dom Bool
unchanged x0 x = x .==. register x0 x

debounce
    :: forall ps a dom. (Eq a, NFDataX a, HiddenClockResetEnable dom, KnownNat (ClockDivider dom ps))
    => SNat ps -> a -> Signal dom a -> Signal dom a
debounce _ init this = regEn init stable this
  where
    counter = register (0 :: Index (ClockDivider dom ps)) counter'
    counter' = mux (unchanged init this) (moreIdx <$> counter) 0
    stable = counter' .==. pure maxBound

roundRobin
    :: forall n dom a. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> (Signal dom (Vec n Bool), Signal dom (Index n))
roundRobin next = (selector, i)
  where
    i = regEn (0 :: Index n) next $ nextIdx <$> i
    selector = bitCoerce . oneHot <$> i

activeHigh :: Bool -> Bit
activeHigh = boolToBit

activeLow :: Bool -> Bit
activeLow = complement . activeHigh

fromActiveHigh :: Bit -> Bool
fromActiveHigh = bitToBool

fromActiveLow :: Bit -> Bool
fromActiveLow = fromActiveHigh . complement

infix 4 ==.
(==.) :: (Eq a, Applicative f) => a -> f a -> f Bool
x ==. fy = pure x .==. fy

infix 4 .==
(.==) :: (Eq a, Applicative f) => f a -> a -> f Bool
fx .== y = fx .==. pure y

countTo :: (Eq a, Enum a, NFDataX a, HiddenClockResetEnable dom) => a -> a -> Signal dom a
countTo start target = counter
  where
    counter = register start $ mux (counter .==. pure target) (pure start) (succ <$> counter)

nextIdx :: (Eq a, Enum a, Bounded a) => a -> a
nextIdx = fromMaybe minBound . succIdx

prevIdx :: (Eq a, Enum a, Bounded a) => a -> a
prevIdx = fromMaybe maxBound . predIdx

moreIdx :: (Eq a, Enum a, Bounded a) => a -> a
moreIdx = fromMaybe maxBound . succIdx

lessIdx :: (Eq a, Enum a, Bounded a) => a -> a
lessIdx = fromMaybe minBound . predIdx

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

shiftInFromLeft :: (BitPack a, KnownNat (BitSize a)) => Bit -> a -> (a, Bit)
shiftInFromLeft b bs = bitCoerce (b, bs)
