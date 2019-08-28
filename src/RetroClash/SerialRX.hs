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

data RXState = RXState
    { buf :: Maybe Bit
    , cnt :: Int
    , phase :: RXPhase
    }
    deriving (Generic, NFData, Show, Undefined)

data RXPhase
    = Idle
    | Start
    | Bit (Index 8) (Unsigned 8)
    | Stop (Unsigned 8)
    deriving (Generic, NFData, Show, Undefined)

rx0 :: Int -> Bit -> State RXState (Maybe (Unsigned 8))
rx0 halfRate bit = do
    s@RXState{..} <- get
    sampled <- do
        let atMiddle = cnt == halfRate
        modify $ \s -> if atMiddle then s{ cnt = 0, buf = Just bit } else s{ cnt = cnt + 1}
        return $ if atMiddle then buf else Nothing

    fmap getLast $ execWriterT $ case phase of
        Idle    | bit == low -> goto Start
        Start   | Just read <- sampled -> goto $ if read == low then Bit 0 0 else Idle
        Bit i x | Just read <- sampled -> do
            let (x', _) = shiftInLeft read x
            goto $ maybe Stop Bit (succIdx i) x'
        Stop x  | Just read <- sampled -> do
            when (read == high) $ tell $ Last . Just $ x
            goto Idle
        _ -> return ()
  where
    goto ph = modify $ \s -> s{ cnt = 0, buf = Nothing, phase = ph }

serialRX
    :: forall rate dom proxy. (KnownNat rate, KnownNat (ClockDivider dom (rate `Div` 2)))
    => (HiddenClockResetEnable dom)
    => proxy rate
    -> Signal dom Bit
    -> Signal dom (Maybe (Unsigned 8))
serialRX rate = mealyState (rx0 $ fromIntegral . natVal $ Proxy @(ClockDivider dom (rate `Div` 2))) s0
  where
    s0 = RXState{ cnt = 0, buf = Nothing, phase = Idle }
