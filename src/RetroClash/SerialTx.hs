{-# LANGUAGE RecordWildCards, LambdaCase #-}
module RetroClash.SerialTx
    ( serialTx
    , serialTxDyn
    , fifo
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Word

data TxState n
    = Idle
    | TxBit Word32 (TxBit n)
    deriving (Show, Eq, Generic, NFDataX)

data TxBit n
    = StartBit (BitVector n)
    | DataBit (BitVector n) (Index n)
    | StopBit
    deriving (Show, Eq, Generic, NFDataX)

txStep :: forall n. (KnownNat n) => Word32 -> Maybe (BitVector n) -> State (TxState n) (Bit, Bool)
txStep bitDuration input = fmap (fmap getAny) . runWriterT $ get >>= \case
    Idle -> do
        tell $ Any True
        traverse_ (goto . StartBit) input
        return high
    TxBit cnt tx -> slowly cnt tx $ case tx of
        StartBit xs -> do
            goto $ DataBit xs 0
            return low
        DataBit xs i -> do
            let (xs', _) = bvShiftR 0 xs
            goto $ maybe StopBit (DataBit xs') $ succIdx i
            return $ lsb xs
        StopBit -> do
            put Idle
            return high
  where
    goto = put . TxBit (bitDuration - 1)

    slowly cnt tx act
        | cnt > 0 = act <* put (TxBit (cnt - 1) tx)
        | otherwise = act

serialTxDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Word32
    -> Signal dom (Maybe (BitVector n))
    -> (Signal dom Bit, Signal dom Bool)
serialTxDyn bitDuration input = mealyStateB (uncurry txStep) Idle (bitDuration, input)

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
