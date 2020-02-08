{-# LANGUAGE ScopedTypeVariables #-}
module RetroClash.Video
    ( maskStart, maskEnd, maskSides
    , center
    , scale
    ) where

import Clash.Prelude
import RetroClash.Utils
import Data.Maybe

maskStart
    :: forall k n dom. (KnownNat n, KnownNat k, 1 <= n)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index (n + k)))
    -> Signal dom (Maybe (Index n))
maskStart = maskSides (SNat @k) (SNat @0)

maskEnd
    :: forall k n dom. (KnownNat n, KnownNat k, 1 <= n)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index (n + k)))
    -> Signal dom (Maybe (Index n))
maskEnd = maskSides (SNat @0) (SNat @k)

center
    :: forall k n m dom. (KnownNat n, KnownNat k, 1 <= k, n ~ (k + 2 * m), KnownNat m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index n))
    -> Signal dom (Maybe (Index k))
center = maskSides (SNat @m) (SNat @m)

maskSides
    :: (KnownNat n, KnownNat m, KnownNat k, 1 <= k)
    => (HiddenClockResetEnable dom)
    => SNat n
    -> SNat m
    -> Signal dom (Maybe (Index (n + k + m)))
    -> Signal dom (Maybe (Index k))
maskSides start end raw = translated
  where
    changed = register Nothing raw ./=. raw
    started = raw .== Just (snatToNum start)

    r = register Nothing translated
    translated =
        mux (isNothing <$> raw) (pure Nothing) $
        mux (not <$> changed) r $
        mux started (pure $ Just 0) $
        (succIdx =<<) <$> r

scale
    :: forall k n m dom. (KnownNat n, KnownNat k, 1 <= k, 1 <= (n `Div` k))
    => (HiddenClockResetEnable dom)
    => SNat k
    -> Signal dom (Maybe (Index n))
    -> Signal dom (Maybe (Index (n `Div` k)))
scale k raw = scaled
  where
    changed = register Nothing raw ./=. raw
    cnt = register (0 :: Index k) $
        mux (isNothing <$> raw) (pure 0) $
        mux (not <$> changed) cnt $
        nextIdx <$> cnt

    r = register Nothing scaled
    scaled =
        mux (isNothing <$> raw) (pure Nothing) $
        mux (not <$> changed) r $
        mux (cnt .== 0) (maybe (Just 0) succIdx <$> r) $
        r
