{-# LANGUAGE StandaloneDeriving, LambdaCase #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module RetroClash.I2C (i2cMaster) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import Control.Monad.State
import Data.Maybe (isJust, isNothing)

type Message = (BitVector 8, BitVector 8, BitVector 8)

data MessageState
    = Init        (BitVector 8, BitVector 8, BitVector 8) Init
    | SendAddr    (BitVector 8, BitVector 8)              (SendBits 8)
    | SendSubaddr (BitVector 8)                           (SendBits 8)
    | SendDat                                             (SendBits 8)
    | Teardown                                            Teardown
    deriving (Show, Generic, BitPack, NFDataX)

data SendBits n
    = SendBit SendTransition (BitVector n) (Index n)
    | SendAck SendTransition
    deriving (Show, Generic, NFDataX)
deriving instance (KnownNat n, 1 <= n) => BitPack (SendBits n)

data SendTransition = SDASet | Tick
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

data Init = StartInit | SDALow | SCLLow
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

data Teardown = StartTeardown | SCLHigh | SDAHigh
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

startBit :: (KnownNat n) => BitVector n -> SendBits n
startBit xs = SendBit minBound xs minBound

succBit :: (KnownNat n) => SendBits n -> Maybe (SendBits n)
succBit (SendBit transition xs i) = Just $ case succIdx transition of
    Just transition' -> SendBit transition' xs i
    Nothing -> maybe (SendAck minBound) (SendBit minBound (xs `shiftL` 1)) $ succIdx i
succBit (SendAck transition) = SendAck <$> succIdx transition

shiftOut :: (KnownNat n) => SendBits n -> (Maybe Bit, Maybe Bit)
shiftOut (SendBit transition xs i) = (Just $ boolToBit $ transition == Tick, Just $ msb xs)
shiftOut (SendAck transition) = (Just $ boolToBit $ transition == Tick, Nothing)

-- We only drive clk (clock stretching not implemented), and we never query
-- peripherals over I2C, so we never actually use sdaIn and sclIn
i2cNext :: Maybe Message -> Bit -> Bit -> Maybe MessageState -> Maybe MessageState
i2cNext newMsg _sdaIn _sclIn = \case
    Nothing                                     -> Init <$> newMsg <*> pure StartInit

    Just (Init        xss@(xs1, xs2, xs3) ramp) -> Just $ maybe (SendAddr    (xs2, xs3) (startBit xs1)) (Init        xss) $ succIdx ramp
    Just (SendAddr    xss@(xs2, xs3)      b)    -> Just $ maybe (SendSubaddr xs3        (startBit xs2)) (SendAddr    xss) $ succBit b
    Just (SendSubaddr xss@xs3             b)    -> Just $ maybe (SendDat                (startBit xs3)) (SendSubaddr xss) $ succBit b
    Just (SendDat                         b)    -> Just $ maybe (Teardown StartTeardown)                SendDat           $ succBit b

    Just (Teardown ramp)                        -> Teardown <$> succIdx ramp

i2cOutput :: Maybe MessageState -> (Maybe Bit, Maybe Bit)
i2cOutput = \case
    Nothing                       -> (Just 1, Just 1)

    Just (Init _ StartInit)       -> (Just 1, Just 1)
    Just (Init _ SDALow)          -> (Just 1, Just 0)
    Just (Init _ SCLLow)          -> (Just 0, Just 0)

    Just (SendAddr _ b)           -> shiftOut b
    Just (SendSubaddr _ b)        -> shiftOut b
    Just (SendDat b)              -> shiftOut b

    Just (Teardown StartTeardown) -> (Just 0, Just 0)
    Just (Teardown SCLHigh)       -> (Just 1, Just 0)
    Just (Teardown SDAHigh)       -> (Just 1, Just 1)

i2cMaster
    :: (HiddenClockResetEnable dom, 1 <= i2cRate, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => SNat i2cRate
    -> "DATA"   ::: Signal dom (Maybe Message)
    -> "SCL_IN" ::: BiSignalIn 'PullUp dom (BitSize Bit)
    -> "SDA_IN" ::: BiSignalIn 'PullUp dom (BitSize Bit)
    -> ( "SCL_OUT" ::: BiSignalOut 'PullUp dom (BitSize Bit)
       , "SDA_OUT" ::: BiSignalOut 'PullUp dom (BitSize Bit)
       , "READY"   ::: Signal dom Bool
       )
i2cMaster i2cRate@SNat msg sclIn sdaIn = (sclOut, sdaOut, ready)
  where
    i2cClock = riseRate i2cRate
    sclIn' = readFromBiSignal sclIn
    sdaIn' = readFromBiSignal sdaIn

    (sclOut', sdaOut', ready) = mealyStateB step Nothing (i2cClock, msg, sclIn', sdaIn')
    sclOut = writeToBiSignal sclIn sclOut'
    sdaOut = writeToBiSignal sdaIn sdaOut'

    step :: (Bool, Maybe Message, Bit, Bit) -> State (Maybe MessageState) (Maybe Bit, Maybe Bit, Bool)
    step (tick, msg, sclIn, sdaIn) = do
        s <- get
        when tick $ modify $ i2cNext msg sdaIn sclIn
        s' <- get
        let ready = tick && isNothing s'
            (sclOut, sdaOut) = i2cOutput s
        return (sclOut, sdaOut, ready)
