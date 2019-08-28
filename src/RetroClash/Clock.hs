{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module RetroClash.Clock
    ( FromHz
    , fromHz
    , ClockDivider
    , divider
    ) where

import Clash.Prelude
import RetroClash.Utils
import GHC.Natural

type FromHz rate = 1_000_000_000_000 `Div` rate

fromHz :: Integer -> Natural
fromHz hz = fromIntegral $ 1_000_000_000_000 `div` hz

type family ClockPeriod conf where
    ClockPeriod ('DomainConfiguration _ ps _ _ _ _) = ps

type ClockDivider dom n = n `Div` ClockPeriod (KnownConf dom)

divider
    :: forall n proxy dom.
       (KnownNat n, KnownNat (ClockDivider dom n), HiddenClockResetEnable dom)
    => proxy n -> Signal dom Bool
divider _ = countTo @(Index (ClockDivider dom n)) minBound maxBound .==. pure maxBound
