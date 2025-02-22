{-# LANGUAGE ScopedTypeVariables, NumericUnderscores, PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module RetroClash.Clock
    ( HzToPeriod

    , Seconds
    , Milliseconds
    , Microseconds
    , Nanoseconds
    , Picoseconds

    , ClockDivider
    , risePeriod
    , riseRate
    ) where

import Clash.Prelude

#if MIN_VERSION_clash_prelude(1,9,0)
#else
type HzToPeriod (rate :: Nat) = Seconds 1 `Div` rate

type Seconds      (s  :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds  (1_000 * us)
type Nanoseconds  (ns :: Nat) = Picoseconds  (1_000 * ns)
type Picoseconds  (ps :: Nat) = ps

type ClockDivider dom ps = ps `Div` DomainPeriod dom
#endif

risePeriod
    :: forall ps dom. (HiddenClockResetEnable dom, _)
    => SNat ps
    -> Signal dom Bool
risePeriod _ = riseEvery (SNat @(ClockDivider dom ps))

riseRate
    :: forall rate dom. (HiddenClockResetEnable dom, _)
    => SNat rate
    -> Signal dom Bool
riseRate _ = risePeriod (SNat @(HzToPeriod rate))
