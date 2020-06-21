{-# LANGUAGE RecordWildCards, RankNTypes #-}
module RetroClash.Delayed
    ( delayVGA
    , delayedRom
    , delayedRam
    , delayedBlockRam1

    , delayedRegister
    , isRisingD
    , changedD
    , matchDelay
    )
    where

import Clash.Prelude
import RetroClash.VGA
import Data.Function (fix)

delayVGA
    :: (KnownNat d, KnownNat r, KnownNat g, KnownNat b)
    => (HiddenClockResetEnable dom)
    => VGASync dom
    -> DSignal dom d (Unsigned r, Unsigned g, Unsigned b)
    -> VGAOut dom r g b
delayVGA VGASync{..} rgb = vgaOut vgaSync' (toSignal rgb)
  where
    vgaSync' = VGASync
        { vgaHSync = matchDelay rgb undefined vgaHSync
        , vgaVSync = matchDelay rgb undefined vgaVSync
        , vgaDE = matchDelay rgb False vgaDE
        }

matchDelay
    :: (KnownNat d, NFDataX a, HiddenClockResetEnable dom)
    => DSignal dom d any
    -> a
    -> Signal dom a
    -> Signal dom a
matchDelay d x0 x = toSignal $ d *> delayI x0 (fromSignal x)

delayedRam
    :: (HiddenClockResetEnable dom)
    => (forall dom'. (HiddenClockResetEnable dom') => Signal dom' addr -> Signal dom' wr -> Signal dom' a)
    -> DSignal dom d addr
    -> DSignal dom d wr
    -> DSignal dom (d + 1) a
delayedRam syncRam addr write = unsafeFromSignal $ syncRam (toSignal addr) (toSignal write)

delayedRom
    :: (HiddenClockResetEnable dom)
    => (forall dom'. (HiddenClockResetEnable dom') => Signal dom' addr -> Signal dom' a)
    -> DSignal dom d addr
    -> DSignal dom (d + 1) a
delayedRom syncRom addr = unsafeFromSignal $ syncRom (toSignal addr)

delayedBlockRam1
    :: (1 <= n, Enum addr, NFDataX a, HiddenClockResetEnable dom)
    => ResetStrategy r
    -> SNat n
    -> a
    -> DSignal dom d addr
    -> DSignal dom d (Maybe (addr, a))
    -> DSignal dom (d + 1) a
delayedBlockRam1 resetStrat size content = delayedRam (blockRam1 resetStrat size content)

delayedRegister
    :: (NFDataX a, HiddenClockResetEnable dom)
    => a
    -> (DSignal dom d a -> DSignal dom d a)
    -> DSignal dom (d + 1) a
delayedRegister initial feedback = fix $ unsafeFromSignal . register initial . toSignal . feedback . antiDelay d1

isRisingD
    :: (HiddenClockResetEnable dom, NFDataX a, Bounded a, Eq a)
    => a -> DSignal dom d a -> DSignal dom d Bool
isRisingD initial = unsafeFromSignal . isRising initial . toSignal

changedD
    :: (HiddenClockResetEnable dom, NFDataX a, Eq a)
    => a -> DSignal dom d a -> DSignal dom d Bool
changedD initial x = x ./=. (unsafeFromSignal . register initial . toSignal $ x)
