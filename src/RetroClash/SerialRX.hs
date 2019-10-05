{-# LANGUAGE RecordWildCards #-}
module RetroClash.SerialRX
    ( serialRX
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

import Control.Category ((>>>))
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Writer
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Monoid
import Data.Proxy

data RXState n = RXState
    { buf :: Maybe Bit
    , cnt :: Int
    , phase :: RXPhase n
    }
    deriving (Generic, NFData, Show, NFDataX)

data RXPhase n
    = Idle
    | Start
    | Bit (Index n) (Vec n Bit)
    | Stop (Vec n Bit)
    deriving (Generic, NFData, Show, NFDataX)

rx0 :: (KnownNat n) => Int -> Bit -> State (RXState n) (Maybe (Vec n Bit))
rx0 halfPeriod bit = do
    s@RXState{..} <- get
    sampled <- do
        let atMiddle = cnt == halfPeriod
        modify $ \s -> if atMiddle then s{ cnt = 0, buf = Just bit } else s{ cnt = cnt + 1}
        return $ if atMiddle then buf else Nothing

    fmap getLast $ execWriterT $ case phase of
        Idle    | bit == low -> goto Start
        Start   | Just read <- sampled -> goto $ if read == low then Bit 0 (repeat low) else Idle
        Bit i x | Just read <- sampled -> do
            let (x', _) = shiftInFromLeft read x
            goto $ maybe Stop Bit (succIdx i) x'
        Stop x  | Just read <- sampled -> do
            when (read == high) $ tell $ Last . Just $ x
            goto Idle
        _ -> return ()
  where
    goto ph = modify $ \s -> s{ cnt = 0, buf = Nothing, phase = ph }

serialRX
    :: forall n rate dom. (KnownNat n, KnownNat rate, KnownNat (ClockDivider dom (HzToPeriod (rate * 2))))
    => (HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom Bit
    -> Signal dom (Maybe (Vec n Bit))
serialRX rate = mealyState (rx0 $ fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod (rate * 2)))) s0
  where
    s0 = RXState{ cnt = 0, buf = Nothing, phase = Idle }
