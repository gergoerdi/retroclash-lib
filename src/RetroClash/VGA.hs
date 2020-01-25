{-# LANGUAGE ScopedTypeVariables, NumericUnderscores, PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module RetroClash.VGA
    ( VGAOut(..)
    , VGADriver(..)
    , vgaDriver
    , VGATiming(..), VGATimings(..)
    , vga640x480at60
    , vga800x600at60
    , vga800x600at72
    , vga1024x768at60
    ) where

import RetroClash.Clock
import RetroClash.Utils
import Clash.Prelude

data VGAOut dom r g b = VGAOut
    { vgaRed   :: Signal dom (Unsigned r)
    , vgaGreen :: Signal dom (Unsigned g)
    , vgaBlue  :: Signal dom (Unsigned b)
    , vgaHSync :: Signal dom Bit
    , vgaVSync :: Signal dom Bit
    }

data VGATiming (visible :: Nat) = forall pre pulseWidth post. VGATiming
    { polarity :: Polarity
    , preWidth :: SNat pre
    , pulseWidth :: SNat pulseWidth
    , postWidth :: SNat post
    }
deriving instance Show (VGATiming vis)

data VGATimings (ps :: Nat) (w :: Nat) (h :: Nat) = VGATimings
    { vgaHorizTiming :: VGATiming w
    , vgaVertTiming :: VGATiming h
    }
    deriving (Show)

data VGADriver dom w h = VGADriver
    { vgaVSync :: Signal dom Bit
    , vgaHSync :: Signal dom Bit
    , vgaEndFrame :: Signal dom Bool
    , vgaEndLine :: Signal dom Bool
    , vgaX :: Signal dom (Maybe (Index w))
    , vgaY :: Signal dom (Maybe (Index h))
    }

data VGAState visible front pulse back
    = Visible (Index visible)
    | FrontPorch (Index front)
    | SyncPulse (Index pulse)
    | BackPorch (Index back)
    deriving (Show, Generic, NFDataX)

visible :: VGAState visible front pulse back -> Maybe (Index visible)
visible (Visible coord) = Just coord
visible _ = Nothing

sync :: VGAState visible front pulse back -> Bool
sync SyncPulse{} = True
sync _ = False

end :: (KnownNat back) => VGAState visible front pulse back -> Bool
end (BackPorch cnt) | cnt == maxBound = True
end _ = False

vgaCounter
    :: (HiddenClockResetEnable dom, KnownNat visible)
    => VGATiming visible
    -> Signal dom Bool
    -> (Signal dom (Maybe (Index visible)), Signal dom Bit, Signal dom Bool)
vgaCounter (VGATiming pol preWidth@SNat pulseWidth@SNat postWidth@SNat) tick =
    (visible <$> state, toActiveDyn pol . sync <$> state, end <$> state)
  where
    state = regEn (Visible 0) tick $ next <$> state

    next (Visible cnt) = maybe (FrontPorch $ the preWidth 0) Visible $ succIdx cnt
    next (FrontPorch cnt) = maybe (SyncPulse $ the pulseWidth 0) FrontPorch $ succIdx cnt
    next (SyncPulse cnt) = maybe (BackPorch $ the postWidth 0) SyncPulse $ succIdx cnt
    next (BackPorch cnt) = maybe (Visible 0) BackPorch $ succIdx cnt

    the :: SNat n -> Index n -> Index n
    the _ = id

vgaDriver
    :: (HiddenClockResetEnable dom, KnownNat w, KnownNat h)
    => (DomainPeriod dom ~ ps)
    => VGATimings ps w h
    -> VGADriver dom w h
vgaDriver VGATimings{..} = VGADriver{..}
  where
    (vgaX, vgaHSync, vgaEndLine) = vgaCounter vgaHorizTiming (pure True)
    (vgaY, vgaVSync, endFrame) = vgaCounter vgaVertTiming vgaEndLine
    vgaEndFrame = vgaEndLine .&&. endFrame

-- | VGA 640*480@60Hz, 25.175 MHz pixel clock
vga640x480at60 :: VGATimings (HzToPeriod 25_175_000) 640 480
vga640x480at60 = VGATimings
    { vgaHorizTiming = VGATiming Low (SNat @16) (SNat @96) (SNat @48)
    , vgaVertTiming  = VGATiming Low (SNat @11) (SNat @2)  (SNat @31)
    }

-- | VGA 800x600@72Hz, 50 MHz pixel clock
vga800x600at72 :: VGATimings (HzToPeriod 50_000_000) 800 600
vga800x600at72 = VGATimings
    { vgaHorizTiming = VGATiming High (SNat @56) (SNat @120) (SNat @64)
    , vgaVertTiming  = VGATiming High (SNat @37) (SNat @6)   (SNat @23)
    }

-- | VGA 800x600@60Hz, 40 MHz pixel clock
vga800x600at60 :: VGATimings (HzToPeriod 40_000_000) 800 600
vga800x600at60 = VGATimings
    { vgaHorizTiming = VGATiming High (SNat @40) (SNat @128) (SNat @88)
    , vgaVertTiming  = VGATiming High (SNat @1)  (SNat @4)   (SNat @23)
    }

-- | VGA 1024*768@60Hz, 65 MHz pixel clock
vga1024x768at60 :: VGATimings (HzToPeriod 65_000_000) 1024 768
vga1024x768at60 = VGATimings
    { vgaHorizTiming = VGATiming Low (SNat @24) (SNat @136) (SNat @160)
    , vgaVertTiming  = VGATiming Low (SNat @3)  (SNat @6)   (SNat @29)
    }
