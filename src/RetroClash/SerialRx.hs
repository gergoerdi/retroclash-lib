{-# LANGUAGE RecordWildCards, LambdaCase #-}
module RetroClash.SerialRx
    ( serialRx
    , serialRxDyn
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.Word

data RxState n
    = Idle
    | RxBit Word32 (Maybe Bit) (RxBit n)
    deriving (Generic, Eq, Show, NFDataX)

data RxBit n
    = StartBit
    | DataBit (BitVector n) (Index n)
    | StopBit (BitVector n)
    deriving (Generic, Eq, Show, NFDataX)

rxStep :: (KnownNat n) => Word32 -> Bit -> State (RxState n) (Maybe (BitVector n))
rxStep bitDuration input = fmap getLast . execWriterT $ get >>= \case
    Idle -> do
        when (input == low) $ goto StartBit
    RxBit cnt sample b | cnt < halfDuration -> do
        put $ RxBit (cnt + 1) sample b
    RxBit _ Nothing b -> do
        put $ RxBit 0 (Just input) b
    RxBit _ (Just sample) rx -> case rx of
        StartBit -> do
            if sample == low then goto (DataBit 0 0) else put Idle
        DataBit xs i -> do
            let (xs', _) = bvShiftR sample xs
            goto $ maybe (StopBit xs') (DataBit xs') $ succIdx i
        StopBit xs -> do
            when (sample == high) $ tell $ pure xs
            put Idle
  where
    halfDuration = bitDuration `shiftR` 2

    goto = put . RxBit 0 Nothing

serialRxDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Word32
    -> Signal dom Bit
    -> Signal dom (Maybe (BitVector n))
serialRxDyn bitDuration input = mealyStateB (uncurry rxStep) Idle (bitDuration, input)

serialRx
    :: forall n rate dom. (KnownNat n, KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom Bit
    -> Signal dom (Maybe (BitVector n))
serialRx rate = serialRxDyn $ pure bitDuration
  where
    bitDuration = fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod rate))
