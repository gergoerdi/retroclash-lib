{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module RetroClash.PS2
    ( PS2(..)
    , samplePS2

    , decodePS2

    , KeyEvent(..)
    , ScanCode(..)
    , KeyCode(..)
    , parseScanCode

    , keyPress
    , keyState
    ) where

import Clash.Prelude
import Clash.Class.HasDomain
import RetroClash.Utils
import RetroClash.Clock
import Control.Monad (guard, when)
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Monoid (Last(..))
import Data.Foldable (traverse_)

data PS2 dom = PS2
    { ps2Clk :: "CLK"   ::: Signal dom Bit
    , ps2Data :: "DATA" ::: Signal dom Bit
    }

type instance HasDomain dom1 (PS2 dom2) = DomEq dom1 dom2
type instance TryDomain t (PS2 dom) = Found dom

samplePS2
    :: forall dom. (HiddenClockResetEnable dom, KnownNat (ClockDivider dom (Microseconds 1)))
    => PS2 dom -> Signal dom (Maybe Bit)
samplePS2 PS2{..} =
    enable (isFalling low . lowpass $ ps2Clk) (lowpass ps2Data)
  where
    lowpass :: Signal dom Bit -> Signal dom Bit
    lowpass = debounce (SNat @(Microseconds 1)) low

data PS2State
    = Idle
    | Bit (BitVector 8) (Index 8)
    | Parity (BitVector 8)
    | Stop (Maybe (Unsigned 8))
    deriving (Show, Eq, Generic, NFDataX)

decoder :: Bit -> WriterT (Last (Unsigned 8)) (State PS2State) ()
decoder x = get >>= \case
    Idle -> do
        when (x == low) $ put $ Bit 0 0
    Bit xs i -> do
        let (xs', _) = bvShiftR x xs
        put $ maybe (Parity xs') (Bit xs') $ succIdx i
    Parity xs -> do
        put $ Stop $ unpack xs <$ guard (parity xs /= x)
    Stop b -> do
        when (x == high) $ tell . Last $ b
        put Idle

decodePS2
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe Bit) -> Signal dom (Maybe (Unsigned 8))
decodePS2 = mealyState sampleDecoder Idle
  where
    sampleDecoder = fmap getLast . execWriterT . traverse_ decoder

data KeyEvent = KeyPress | KeyRelease
    deriving (Generic, Eq, Show, NFDataX)

type KeyCode = Unsigned 9

data ScanCode = ScanCode KeyEvent KeyCode
    deriving (Generic, Eq, Show, NFDataX)

data ScanState
    = Init
    | Extended
    | Code KeyEvent Bit
    deriving (Show, Generic, NFDataX)

parser :: Unsigned 8 -> WriterT (Last ScanCode) (State ScanState) ()
parser raw = get >>= \case
    Init
        | raw == 0xe0 -> put $ Extended
        | raw == 0xf0 -> put $ Code KeyRelease 0
        | otherwise   -> finish KeyPress 0
    Extended
        | raw == 0xf0 -> put $ Code KeyRelease 1
        | otherwise   -> finish KeyPress 1
    Code ev ext       -> finish ev ext
  where
    finish ev ext = do
        tell $ Last . Just $ ScanCode ev $ bitCoerce (ext, raw)
        put Init

parseScanCode
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8)) -> Signal dom (Maybe ScanCode)
parseScanCode = mealyState byteParser Init
  where
    byteParser = fmap getLast . execWriterT . traverse_ parser

keyPress :: ScanCode -> Maybe KeyCode
keyPress (ScanCode KeyPress kc) = Just kc
keyPress _ = Nothing

keyState
    :: (HiddenClockResetEnable dom)
    => KeyCode
    -> Signal dom (Maybe ScanCode)
    -> Signal dom Bool
keyState target = regMaybe False . fmap fromScanCode
  where
    fromScanCode sc = do
        ScanCode ev kc <- sc
        guard $ kc == target
        return $ ev == KeyPress
