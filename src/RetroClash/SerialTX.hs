{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module RetroClash.SerialTX
    ( TXOut(..)
    , serialTX
    , fifo
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import Control.Monad.State
import Data.Foldable (for_)

data St n = MkSt
    { cnt :: Int
    , state :: TXState n
    }
    deriving (Generic, Show, NFDataX)

data TXState n
    = Idle
    | StartBit (Vec n Bit)
    | DataBit (Vec n Bit) (Index n)
    | StopBit
    deriving (Show, Eq, Generic, NFDataX)

data TXOut dom = TXOut
    { txReady :: Signal dom Bool
    , txOut :: Signal dom Bit
    }

txStep :: forall n. (KnownNat n) => Int -> Maybe (Vec n Bit) -> State (St n) (Bool, Bit)
txStep periodLen input = do
    s@MkSt{..} <- get
    let slowly k = if cnt == periodLen then k else put s{ cnt = cnt + 1 }
    case state of
        Idle -> do
            for_ input $ goto . StartBit
            return (True, high)
        StartBit x -> do
            slowly $ goto $ DataBit x 0
            return (False, low)
        DataBit x i -> do
            let (x', b) = shiftInFromLeft low x
            slowly $ goto $ maybe StopBit (DataBit x') $ succIdx i
            return (False, b)
        StopBit -> do
            slowly $ goto Idle
            return (False, high)
  where
    goto s = put MkSt{ cnt = 0, state = s }

serialTXDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Int
    -> Signal dom (Maybe (Vec n Bit))
    -> TXOut dom
serialTXDyn periodLen inp = TXOut{..}
  where
    (txReady, txOut) = unbundle $ mealyState (txStep periodLen) s0 inp
    s0 = MkSt{ cnt = 0, state = Idle }

serialTX
    :: forall n rate dom. (KnownNat n, KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom (Maybe (Vec n Bit))
    -> TXOut dom
serialTX rate = serialTXDyn $ fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod rate))

fifo
    :: forall a dom. (NFDataX a, HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom Bool -> Signal dom (Maybe a)
fifo input outReady = r
  where
    r = register Nothing $ mux outReady input (mplus <$> r <*> input)
