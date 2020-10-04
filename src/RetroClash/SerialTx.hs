{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module RetroClash.SerialTx
    ( serialTx
    , serialTxDyn
    , fifo
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import RetroClash.Slow

import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Data.Foldable (for_)
import Data.Word

type TxState n = Slow (TxBit n)

data TxBit n
    = Idle
    | StartBit (BitVector n)
    | DataBit (BitVector n) (Index n)
    | StopBit
    deriving (Show, Eq, Generic, NFDataX)

txStep :: forall n. (KnownNat n) => Word32 -> Maybe (BitVector n) -> State (TxState n) (Bit, Bool)
txStep bitDuration input = runAck $ do
    (slowly, s) <- getSlow bitDuration
    case s of
        Idle -> do
            for_ input $ \x -> do
                tell $ Any True
                putSlow $ StartBit x
            return high
        StartBit x -> do
            slowly $ putSlow $ DataBit x 0
            return low
        DataBit x i -> do
            let (x', _) = bvShiftR 0 x
            slowly $ putSlow $ maybe StopBit (DataBit x') $ succIdx i
            return $ lsb x
        StopBit -> do
            slowly $ putSlow Idle
            return high
  where
    runAck = fmap (\(out, consumed) -> (out, getAny consumed)) . runWriterT

serialTxDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Word32
    -> Signal dom (Maybe (BitVector n))
    -> (Signal dom Bit, Signal dom Bool)
serialTxDyn bitDuration input = mealyStateB (uncurry txStep) (Slow 0 Idle) (bitDuration, input)

serialTx
    :: forall n rate dom. (KnownNat n, KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom (Maybe (BitVector n))
    -> (Signal dom Bit, Signal dom Bool)
serialTx rate = serialTxDyn $ pure . fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod rate))

fifo
    :: forall a dom. (NFDataX a, HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom Bool -> Signal dom (Maybe a)
fifo input outReady = r
  where
    r = register Nothing $ mux outReady input (mplus <$> r <*> input)
