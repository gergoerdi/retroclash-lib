{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, DeriveFoldable, DeriveTraversable #-}
module RetroClash.SevenSegment
    ( encodeHexSS
    , showSS
    , showSSs
    , muxRR
    -- , bytesSS
    ) where

import Clash.Prelude
import qualified Data.List as L
import RetroClash.Utils (countTo, oneHot, nextIdx)

muxRR
    :: forall n div dom a. (KnownNat n, Eq div, Enum div, Num div, NFDataX div, HiddenClockResetEnable dom)
    => div
    -> Signal dom (Vec n a)
    -> (Signal dom (Vec n Bool), Signal dom a)
muxRR div xs = (selected, current)
  where
    i = regEn (0 :: Index n) timer $ nextIdx <$> i
    selected = bitCoerce . oneHot <$> i
    current = (!!) <$> xs <*> i

    counter = countTo 0 div
    timer = counter .==. pure 0

bytesSS
    :: forall n div dom clk sync. (KnownNat n, KnownNat div, HiddenClockResetEnable dom)
    => Unsigned div
    -> Signal dom (Vec n (Maybe (Unsigned 8)))
    -> (Signal dom (Vec (n * 2) Bool), Signal dom (Vec 7 Bool))
bytesSS div bytes = (shownDigit, segments)
  where
    digit = regEn (0 :: Index (n * 2)) timer $ nextIdx <$> digit
      where
        counter = countTo 0 div
        timer = counter .==. pure 0

    shownDigit = bitCoerce . oneHot <$> digit
    segments = maybe (pure False) encodeHexSS <$> nibble

    nibble = (!!) <$> nibbles <*> digit

    nibbles = concatMap (traverse splitByte) <$> bytes

    splitByte :: Unsigned 8 -> Vec 2 (Unsigned 4)
    splitByte byte = hi :> lo :> Nil
      where
        (hi, lo) = bitCoerce byte

encodeHexSS :: Unsigned 4 -> Vec 7 Bool
encodeHexSS n = case n of
    --       a        b        c        d        e        f        g
    0x0 ->  True  :> True  :> True  :> True  :> True  :> True  :> False :> Nil
    0x1 ->  False :> True  :> True  :> False :> False :> False :> False :> Nil
    0x2 ->  True  :> True  :> False :> True  :> True  :> False :> True  :> Nil
    0x3 ->  True  :> True  :> True  :> True  :> False :> False :> True  :> Nil
    0x4 ->  False :> True  :> True  :> False :> False :> True  :> True  :> Nil
    0x5 ->  True  :> False :> True  :> True  :> False :> True  :> True  :> Nil
    0x6 ->  True  :> False :> True  :> True  :> True  :> True  :> True  :> Nil
    0x7 ->  True  :> True  :> True  :> False :> False :> False :> False :> Nil
    0x8 ->  True  :> True  :> True  :> True  :> True  :> True  :> True  :> Nil
    0x9 ->  True  :> True  :> True  :> True  :> False :> True  :> True  :> Nil
    0xa ->  True  :> True  :> True  :> False :> True  :> True  :> True  :> Nil
    0xb ->  False :> False :> True  :> True  :> True  :> True  :> True  :> Nil
    0xc ->  True  :> False :> False :> True  :> True  :> True  :> False :> Nil
    0xd ->  False :> True  :> True  :> True  :> True  :> False :> True  :> Nil
    0xe ->  True  :> False :> False :> True  :> True  :> True  :> True  :> Nil
    0xf ->  True  :> False :> False :> False :> True  :> True  :> True  :> Nil

showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) = unlines . L.concat $
    [ L.replicate 1 $ horiz   a
    , L.replicate 3 $ vert  f   b
    , L.replicate 1 $ horiz   g
    , L.replicate 3 $ vert  e   c
    , L.replicate 1 $ horiz   d
    ]
  where
    horiz True  = " ###### "
    horiz False = " ...... "

    vert b1 b2 = part b1 <> "      " <> part b2
      where
        part True  = "#"
        part False = "."

showSSs :: [Vec 7 Bool] -> String
showSSs = unlines . L.map (L.intercalate "  ") . L.transpose . L.map (lines . showSS)
