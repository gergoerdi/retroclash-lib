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
    | TXStart (Unsigned n)
    | TXBit (Unsigned n) (Index n)
    deriving (Show, Eq, Generic, NFDataX)

data TXOut dom = TXOut{ txReady :: Signal dom Bool, txOut :: Signal dom Bit }

tx0 :: forall n. (KnownNat n, 1 <= n) => Maybe (Unsigned n) -> State (TXState n) (Bool, Bit)
tx0 input = do
    s <- get
    case s of
        TXIdle -> do
            for_ input $ put . TXStart
            return (True, high)
        TXStart x -> do
            put $ TXBit x 0
            return (False, low)
        TXBit x i -> do
            let (x', b) = shiftInLeft low x
            put $ maybe TXIdle (TXBit x') $ succIdx i
            return (False, b)

txDyn
    :: (KnownNat n, 1 <= n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (Unsigned n))
    -> TXOut dom
txDyn tick inp = TXOut{..}
  where
    (txReady, txOut) = unbundle $ mealyStateSlow tick tx0 TXIdle inp

serialTX
    :: (KnownNat n, HiddenClockResetEnable dom, _)
    => SNat rate
    -> Signal dom (Maybe (Unsigned n))
    -> TXOut dom
serialTX rate = txDyn (riseRate rate)

fifo
    :: forall a. (NFDataX a)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom Bool -> Signal dom (Maybe a)
fifo = curry $ mooreB step fst (Nothing, Nothing)
  where
    step (current, saved) (input, outReady) = if outReady then (next, next) else (current, Nothing)
      where
        next = input <|> saved
