{-# LANGUAGE RecordWildCards #-}
module RetroClash.Delayed
    ( delayVGA
    , delayedBlockRam1
    )
    where

import Clash.Prelude
import RetroClash.VGA

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

delayedBlockRam1
    :: (1 <= n, Enum addr, NFDataX a, HiddenClockResetEnable dom)
    => ResetStrategy r
    -> SNat n
    -> a
    -> DSignal dom d addr
    -> DSignal dom d (Maybe (addr, a))
    -> DSignal dom (d + 1) a
delayedBlockRam1 resetStrat size content addr wr = unsafeFromSignal $
    blockRam1 resetStrat size content (toSignal addr) (toSignal wr)
