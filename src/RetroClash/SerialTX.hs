{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeOperators, GADTs #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module RetroClash.SerialTX
    ( TXOut(..)
    , serialTX
    , fifo
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock

import Control.Category ((>>>))
import Control.Monad.State
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Foldable (for_)

data TXState n
    = TXIdle
    | TXStart (Vec n Bit)
    | TXBit (Vec n Bit) (Index n)
    deriving (Show, Eq, Generic, NFDataX)

data TXOut dom = TXOut
    { txReady :: Signal dom Bool
    , txOut :: Signal dom Bit
    }

tx :: forall n. (KnownNat n) => Maybe (Vec n Bit) -> State (TXState n) (Bool, Bit)
tx input = do
    s <- get
    case s of
        TXIdle -> do
            for_ input $ put . TXStart
            return (True, high)
        TXStart x -> do
            put $ TXBit x 0
            return (False, low)
        TXBit x i -> do
            let (x', b) = shiftInFromLeft low x
            put $ maybe TXIdle (TXBit x') $ succIdx i
            return (False, b)

serialTX'
    :: (KnownNat n, 1 <= n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (Vec n Bit))
    -> TXOut dom
serialTX' tick inp = TXOut{..}
  where
    (txReady, txOut) = unbundle $ mealyStateSlow tick tx TXIdle inp

serialTX
    :: (KnownNat n, HiddenClockResetEnable dom, _)
    => SNat rate
    -> Signal dom (Maybe (Vec n Bit))
    -> TXOut dom
serialTX rate = serialTX' (riseRate rate)

fifo
    :: forall a dom. (NFDataX a, HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom Bool -> Signal dom (Maybe a)
fifo input outReady = mooreB step fst (Nothing, Nothing) (input, outReady)
  where
    step (current, saved) (input, outReady) = if outReady then (next, next) else (current, Nothing)
      where
        next = input <|> saved
