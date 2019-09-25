{-# LANGUAGE ScopedTypeVariables, NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module RetroClash.Clock
    ( HzToPeriod
    , ClockDivider
    , risePeriod
    ) where

import Clash.Prelude
import RetroClash.Utils
import GHC.Natural

type HzToPeriod (rate :: Nat) = 1_000_000_000_000 `Div` rate

type ClockDivider dom n = n `Div` DomainPeriod dom

risePeriod
    :: forall ps dom. (HiddenClockResetEnable dom, _)
    => SNat ps
    -> Signal dom Bool
risePeriod _ = riseEvery (SNat @(ClockDivider dom ps))
