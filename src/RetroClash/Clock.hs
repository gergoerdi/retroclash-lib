{-# LANGUAGE ScopedTypeVariables, NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module RetroClash.Clock
    ( HzToPeriod
    , ClockDivider
    ) where

import Clash.Prelude
import RetroClash.Utils
import GHC.Natural

type HzToPeriod (rate :: Nat) = 1_000_000_000_000 `Div` rate

type ClockDivider dom n = n `Div` DomainPeriod dom
