{-# LANGUAGE RecordWildCards #-}
module RetroClash.SerialRx
    ( serialRx
    , serialRxDyn
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import RetroClash.Slow

import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.Word
import Data.Foldable (for_)

type RxState n = Slow (Maybe Bit, RxBit n)

data RxBit n
    = Idle
    | StartBit
    | DataBit (Index n) (BitVector n)
    | StopBit (BitVector n)
    deriving (Generic, Eq, Show, NFDataX)

rxStep :: (KnownNat n) => Word32 -> Bit -> State (RxState n) (Maybe (BitVector n))
rxStep bitDuration input = fmap getLast $ execWriterT $ do
    (slowly, (sample, s)) <- getSlow halfDuration
    slowly $ putSlow (sample <|> Just input, s)
    case s of
        Idle -> when (input == low) $ goto StartBit
        StartBit -> slowly $ for_ sample $ \sample -> do
            goto $ if sample == low then DataBit 0 0 else Idle
        DataBit i x -> slowly $ for_ sample $ \sample -> do
            let (x', _) = bvShiftR sample x
            goto $ maybe StopBit DataBit (succIdx i) x'
        StopBit x -> slowly $ for_ sample $ \sample -> do
            when (sample == high) $ tell $ Last . Just $ x
            goto Idle
  where
    goto s = putSlow (Nothing, s)
    halfDuration = bitDuration `div` 2

serialRxDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Word32
    -> Signal dom Bit
    -> Signal dom (Maybe (BitVector n))
serialRxDyn bitDuration input = mealyStateB (uncurry rxStep) (Slow 0 (Nothing, Idle)) (bitDuration, input)

serialRx
    :: forall n rate dom. (KnownNat n, KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom Bit
    -> Signal dom (Maybe (BitVector n))
serialRx rate = serialRxDyn $ pure bitDuration
  where
    bitDuration = fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod rate))
