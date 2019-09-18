{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module RetroClash.Clock
    ( HzToPeriod
    , ClockDivider
    , divider
    ) where

import Clash.Prelude
import RetroClash.Utils
import GHC.Natural

type HzToPeriod (rate :: Nat) = 1_000_000_000_000 `Div` rate

type family ClockPeriod conf where
    ClockPeriod ('DomainConfiguration _ ps _ _ _ _) = ps

type ClockDivider dom n = n `Div` ClockPeriod (KnownConf dom)

divider
    :: forall n proxy dom.
       (KnownNat n, KnownNat (ClockDivider dom n), HiddenClockResetEnable dom)
    => proxy n -> Signal dom Bool
divider _ = countTo @(Index (ClockDivider dom n)) minBound maxBound .==. pure maxBound
