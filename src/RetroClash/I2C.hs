{-# LANGUAGE StandaloneDeriving, LambdaCase #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module RetroClash.I2C (i2cMaster) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import Control.Monad.State
import Data.Maybe (isJust, isNothing)

type Message = (Unsigned 8, Unsigned 8, Unsigned 8)

data MessageState
    = Init Init
    | SendAddr (SendBits 8)
    | SendSubaddr (SendBits 8)
    | SendDat (SendBits 8)
    | Teardown Teardown
    deriving (Show, Generic, BitPack, NFDataX)

data SendBits n
  = SendBit SendTransition (Index n)
  | SendAck SendTransition
  deriving (Show, Generic, NFDataX)
deriving instance (KnownNat n, 1 <= n) => BitPack (SendBits n)

data SendTransition = SDASet | Tick
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

data Init = StartInit | SDALow | SCLLow
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

data Teardown = StartTeardown | SCLHigh | SDAHigh
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

startBit :: (KnownNat n) => SendBits n
startBit = SendBit minBound maxBound

nextBit :: (KnownNat n) => SendBits n -> Maybe (SendBits n)
nextBit (SendBit transition i) = Just $ case succIdx transition of
    Just transition' -> SendBit transition' i
    Nothing -> maybe (SendAck minBound) (SendBit minBound) $ predIdx i
nextBit (SendAck transition) = SendAck <$> succIdx transition

shiftOut :: (KnownNat n) => Unsigned n -> SendBits n -> (Maybe Bit, Maybe Bit)
shiftOut x (SendBit transition i) = (Just $ boolToBit $ transition == Tick, Just $ x ! i)
shiftOut _ (SendAck transition) = (Just $ boolToBit $ transition == Tick, Nothing)

-- We only drive clk (clock stretching not implemented), and we never query
-- peripherals over I2C, so we never actually use sdaIn and sclIn
i2cNext :: Bool -> Bit -> Bit -> Maybe MessageState -> Maybe MessageState
i2cNext startNew _sdaIn _sclIn = \case
    Nothing              -> Init StartInit <$ guard startNew

    Just (Init ramp)     -> Just $ maybe (SendAddr startBit) Init $ succIdx ramp

    Just (SendAddr b)    -> Just $ maybe (SendSubaddr startBit) SendAddr $ nextBit b
    Just (SendSubaddr b) -> Just $ maybe (SendDat startBit) SendSubaddr $ nextBit b
    Just (SendDat b)     -> Just $ maybe (Teardown StartTeardown) SendDat $ nextBit b

    Just (Teardown ramp) -> Teardown <$> succIdx ramp

i2cOutput :: Message -> Maybe MessageState -> (Maybe Bit, Maybe Bit)
i2cOutput (addr, subaddr, dat) = \case
    Nothing                       -> (Just 1, Just 1)

    Just (Init StartInit)         -> (Just 1, Just 1)
    Just (Init SDALow)            -> (Just 1, Just 0)
    Just (Init SCLLow)            -> (Just 0, Just 0)

    Just (SendAddr b)             -> shiftOut addr b
    Just (SendSubaddr b)          -> shiftOut subaddr b
    Just (SendDat b)              -> shiftOut dat b

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
        when tick $ modify $ i2cNext (isJust msg) sdaIn sclIn
        s' <- get
        let ready = tick && isNothing s'
            (sclOut, sdaOut) = i2cOutput (fromJustX msg) s
        return (sclOut, sdaOut, ready)
