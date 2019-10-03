module RetroClash.Keypad
    ( Matrix(..), KeyStates(..), KeyEvent(..), KeyEvents(..)
    , scanKeypad, keypadEvents
    , pressedKeys
    , firstJust2D
    , inputKeypad
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import Control.Monad (mplus)

type Matrix rows cols a = Vec rows (Vec cols a)

type KeyStates rows cols = Matrix rows cols Bool

data KeyEvent
    = Pressed
    | Released
    deriving (Show, Eq, Generic, NFDataX)

type KeyEvents rows cols = Matrix rows cols (Maybe KeyEvent)

scanKeypad
    :: forall rows cols dom. (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom)
    => Signal dom (Vec rows Bool)
    -> (Signal dom (Vec cols Bool), Signal dom (KeyStates rows cols))
scanKeypad rows = (cols, transpose <$> bundle state)
  where
    (cols, currentCol) = roundRobin nextCol
    nextCol = riseEvery (SNat @1000)

    state = map colState indicesI
      where
        colState thisCol = regEn (repeat False) (stable .&&. currentCol .== thisCol) $ rows

        stable = cnt .== maxBound
        cnt = register (0 :: Index 10) $ mux nextCol 0 (moreIdx <$> cnt)

keypadEvents
    :: (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom)
    => Signal dom (KeyStates rows cols)
    -> Signal dom (KeyEvents rows cols)
keypadEvents states = zipWith (zipWith event) <$> delayed <*> states
  where
    delayed = register (repeat $ repeat False) states

    event False True = Just Pressed
    event True False = Just Released
    event _ _ = Nothing

pressedKeys :: Matrix rows cols a -> KeyEvents rows cols  -> Matrix rows cols (Maybe a)
pressedKeys = zipWith (zipWith decode)
  where
    decode mapping (Just Pressed) = Just mapping
    decode _ _ = Nothing

firstJust2D :: Matrix rows cols (Maybe a) -> Maybe a
firstJust2D = foldl (foldl mplus) Nothing

inputKeypad
    :: (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom, KnownNat (ClockDivider dom (Milliseconds 5)))
    => Matrix rows cols a
    -> Signal dom (Vec rows Bool)
    -> (Signal dom (Vec cols Bool), Signal dom (Maybe a))
inputKeypad keymap rows = (cols, pressedKey)
  where
    (cols, keyState) = scanKeypad rows
    events = keypadEvents . debounce (SNat @(Milliseconds 5)) (repeat . repeat $ False) $ keyState
    pressedKey = firstJust2D . pressedKeys keymap <$> events

