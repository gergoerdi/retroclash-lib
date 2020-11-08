{-# LANGUAGE RecordWildCards, LambdaCase #-}
module RetroClash.PS2.ASCII
    ( Side(..), Modifier(..)
    , modMap
    , asciiMap
    ) where

import Clash.Prelude
import RetroClash.PS2
import Data.Char

data Side
    = OnLeft
    | OnRight
    deriving (Eq, Show, Enum, Bounded, Generic, NFDataX)

data Modifier
    = Shift
    | Ctrl
    | Alt
    | Win
    deriving (Eq, Show, Enum, Bounded, Generic, NFDataX)

modMap :: KeyCode -> Maybe (Modifier, Side)
modMap 0x012 = Just (Shift, OnLeft)
modMap 0x059 = Just (Shift, OnRight)
modMap 0x014 = Just (Ctrl, OnLeft)
modMap 0x114 = Just (Ctrl, OnRight)
modMap 0x011 = Just (Alt, OnLeft)
modMap 0x111 = Just (Alt, OnRight)
modMap 0x11f = Just (Win, OnLeft)
modMap 0x127 = Just (Win, OnRight)
modMap _ = Nothing

{-# INLINE asciiMap #-}
asciiMap :: KeyCode -> Maybe (Unsigned 7)
asciiMap = fmap fromChar . charMap
  where
    fromChar = fromIntegral . ord

{-# INLINE charMap #-}
charMap :: KeyCode -> Maybe Char
charMap 0x05a = Just '\r'
charMap 0x15a = Just '\r' -- Keypad
charMap 0x029 = Just ' '
charMap 0x04c = Just ';'
charMap 0x052 = Just '\''
charMap 0x045 = Just '0'
charMap 0x016 = Just '1'
charMap 0x01e = Just '2'
charMap 0x026 = Just '3'
charMap 0x025 = Just '4'
charMap 0x02e = Just '5'
charMap 0x036 = Just '6'
charMap 0x03d = Just '7'
charMap 0x03e = Just '8'
charMap 0x046 = Just '9'
charMap 0x01c = Just 'a'
charMap 0x032 = Just 'b'
charMap 0x021 = Just 'c'
charMap 0x023 = Just 'd'
charMap 0x024 = Just 'e'
charMap 0x02b = Just 'f'
charMap 0x034 = Just 'g'
charMap 0x033 = Just 'h'
charMap 0x043 = Just 'i'
charMap 0x03b = Just 'j'
charMap 0x042 = Just 'k'
charMap 0x04b = Just 'l'
charMap 0x03a = Just 'm'
charMap 0x031 = Just 'n'
charMap 0x044 = Just 'o'
charMap 0x04d = Just 'p'
charMap 0x015 = Just 'q'
charMap 0x02d = Just 'r'
charMap 0x01b = Just 's'
charMap 0x02c = Just 't'
charMap 0x03c = Just 'u'
charMap 0x02a = Just 'v'
charMap 0x01d = Just 'w'
charMap 0x022 = Just 'x'
charMap 0x035 = Just 'y'
charMap 0x01a = Just 'z'
charMap _ = Nothing
