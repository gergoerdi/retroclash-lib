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
    :: forall k n dom. (KnownNat n, KnownNat k)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index (k + n)))
    -> Signal dom (Maybe (Index n))
maskStart = maskSides (SNat @k)

maskEnd
    :: forall k n dom. (KnownNat n, KnownNat k)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index (n + k)))
    -> Signal dom (Maybe (Index n))
maskEnd = maskSides (SNat @0)

center
    :: forall n n0 k m dom. (KnownNat n, KnownNat n0, KnownNat k, KnownNat m)
    => (k ~ ((n0 - n) `Div` 2), n0 ~ (k + n + m))
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index n0))
    -> Signal dom (Maybe (Index n))
center = maskSides (SNat @k)

maskSides
    :: (KnownNat n, KnownNat m, KnownNat k)
    => (HiddenClockResetEnable dom)
    => SNat k
    -> Signal dom (Maybe (Index (k + n + m)))
    -> Signal dom (Maybe (Index n))
maskSides k raw = transformed
  where
    changed = register Nothing raw ./=. raw
    started = raw .== Just (snatToNum k)

    r = register Nothing transformed
    transformed =
        mux (not <$> changed) r $
        mux (isNothing <$> raw) (pure Nothing) $
        mux started (pure $ Just 0) $
        (succIdx =<<) <$> r

scale
    :: forall k n m dom. (KnownNat n, KnownNat k, 1 <= k, 1 <= (n `Div` k))
    => (HiddenClockResetEnable dom)
    => SNat k
    -> Signal dom (Maybe (Index n))
    -> Signal dom (Maybe (Index (n `Div` k)))
scale k raw = scaledNext
  where
    changed = register Nothing raw ./=. raw

    counter = regEn (0 :: Index k) changed $
        mux (isNothing <$> raw) (pure 0) $
        nextIdx <$> counter

    scaled = register Nothing scaledNext
    scaledNext =
        mux (not <$> changed) scaled $
        mux (counter .== 0) (maybe (Just 0) succIdx <$> scaled) $
        scaled
