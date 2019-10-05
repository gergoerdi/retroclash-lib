{-# LANGUAGE RecordWildCards #-}
module RetroClash.SerialRX
    ( serialRX
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import Control.Category ((>>>))
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Monoid
import Data.Foldable (for_)

data RXState n = RXState
    { cnt :: Int
    , buf :: Maybe Bit
    , phase :: RXPhase n
    }
    deriving (Generic, Show, NFDataX)

data RXPhase n
    = Idle
    | StartBit
    | DataBit (Index n) (Vec n Bit)
    | StopBit (Vec n Bit)
    deriving (Generic, Show, NFDataX)

rx0 :: (KnownNat n) => Int -> Bit -> State (RXState n) (Maybe (Vec n Bit))
rx0 halfPeriod input = fmap (getLast =<<) $ runMaybeT $ execWriterT $ do
    s@RXState{..} <- get
    let sample = do
            let atMiddle = cnt == halfPeriod
            put $ if atMiddle then s{ cnt = 0, buf = Just input } else s{ cnt = cnt + 1 }
            return $ if atMiddle then buf else Nothing
    case phase of
        Idle -> when (input == low) $ goto StartBit
        StartBit -> do
            Just bit <- sample
            goto $ if bit == low then DataBit 0 (repeat low) else Idle
        DataBit i x -> do
            Just bit <- sample
            let (x', _) = shiftInFromLeft bit x
            goto $ maybe StopBit DataBit (succIdx i) x'
        StopBit x -> do
            Just bit <- sample
            when (bit == high) $ tell $ Last . Just $ x
            goto Idle
  where
    goto ph = put RXState{ cnt = 0, buf = Nothing, phase = ph }

serialRX
    :: forall n rate dom. (KnownNat n, KnownNat rate, KnownNat (ClockDivider dom (HzToPeriod (rate * 2))))
    => (HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom Bit
    -> Signal dom (Maybe (Vec n Bit))
serialRX rate = mealyState (rx0 $ fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod (rate * 2)))) s0
  where
    s0 = RXState{ cnt = 0, buf = Nothing, phase = Idle }
