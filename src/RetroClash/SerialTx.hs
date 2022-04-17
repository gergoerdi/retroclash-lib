{-# LANGUAGE RecordWildCards, LambdaCase #-}
module RetroClash.SerialTx
    ( serialTx
    , serialTxDyn
    , fifo
    , TxBit(..)
    , txStep
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import Control.Monad.State
import Data.Word
import Data.Maybe (isNothing)

data TxBit n
    = StartBit (BitVector n)
    | DataBit (BitVector n) (Index n)
    | StopBit
    deriving (Show, Eq, Generic, NFDataX)

txNext :: (KnownNat n) => Maybe (BitVector n) -> Maybe (TxBit n) -> Maybe (TxBit n)
txNext input = \case
    Nothing             -> StartBit <$> input
    Just (StartBit xs)  -> Just $ DataBit xs 0
    Just (DataBit xs i) -> Just $ let xs' = xs `shiftR` 1 in maybe StopBit (DataBit xs') $ succIdx i
    Just StopBit        -> Nothing

txOutput :: (KnownNat n) => Maybe (TxBit n) -> Bit
txOutput = \case
    Nothing             -> high
    Just StartBit{}     -> low
    Just (DataBit xs _) -> lsb xs
    Just StopBit{}      -> high

txStep :: forall n. (KnownNat n) => Bool -> Maybe (BitVector n) -> State (Maybe (TxBit n)) (Bit, Bool)
txStep tick input = do
    s <- get
    when tick $ modify $ txNext input
    s' <- get
    let ready = tick && isNothing s'
        out = txOutput s
    return (out, ready)

serialTxDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Word32
    -> Signal dom (Maybe (BitVector n))
    -> (Signal dom Bit, Signal dom Bool)
serialTxDyn bitDuration input = mealyStateB (uncurry txStep) Nothing (tick, input)
  where
    tick = riseEveryDyn bitDuration

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
