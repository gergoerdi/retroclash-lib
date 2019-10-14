{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module RetroClash.SerialTx
    ( TxOut(..)
    , serialTx
    , fifo
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import Control.Monad.State
import Data.Foldable (for_)
import Data.Word

data TxState n
    = Idle
    | Slowly Word32 (TxBit n)
    deriving (Show, Eq, Generic, NFDataX)

data TxBit n
    = StartBit (Vec n Bit)
    | DataBit (Vec n Bit) (Index n)
    | StopBit
    deriving (Show, Eq, Generic, NFDataX)

data TxOut dom = TxOut
    { txReady :: Signal dom Bool
    , txOut :: Signal dom Bit
    }

txStep :: forall n. (KnownNat n) => Word32 -> Maybe (Vec n Bit) -> State (TxState n) (Bool, Bit)
txStep periodLen input = do
    s <- get
    case s of
        Idle -> case input of
            Nothing -> return (False, high)
            Just x -> do
                goto $ StartBit x
                return (True, high)
        Slowly cnt txBit -> do
            let slowly k = if cnt == periodLen then k else put (Slowly (cnt + 1) txBit)
            case txBit of
                StartBit x -> do
                    slowly $ goto $ DataBit x 0
                    return (False, low)
                DataBit x i -> do
                    slowly $ goto $ maybe StopBit (DataBit $ rotateRightS x (SNat @1)) $ succIdx i
                    return (False, lsb x)
                StopBit -> do
                    slowly $ put Idle
                    return (False, high)
  where
    goto = put . Slowly 0

serialTxDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Word32
    -> Signal dom (Maybe (Vec n Bit))
    -> TxOut dom
serialTxDyn periodLen inp = TxOut{..}
  where
    (txReady, txOut) = mealyStateB (uncurry txStep) Idle (periodLen, inp)

serialTx
    :: forall n rate dom. (KnownNat n, KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom (Maybe (Vec n Bit))
    -> TxOut dom
serialTx rate = serialTxDyn $ pure . fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod rate))

fifo
    :: forall a dom. (NFDataX a, HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom Bool -> Signal dom (Maybe a)
fifo input outReady = r
  where
    r = register Nothing $ mux outReady input (mplus <$> r <*> input)
