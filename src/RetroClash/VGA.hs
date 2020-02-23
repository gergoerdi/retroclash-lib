{-# LANGUAGE ScopedTypeVariables, NumericUnderscores, PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module RetroClash.VGA
    ( VGASync(..)
    , VGADriver(..)
    , vgaDriver
    , VGAOut(..)
    , vgaOut
    , vgaPort
    , VGATiming(..), VGATimings(..)

    , vga640x480at60
    , vga800x600at60
    , vga800x600at72
    , vga1024x768at60
    ) where

import Clash.Prelude
import RetroClash.Clock
import RetroClash.Utils
import Data.Maybe (isJust)

data VGASync dom = VGASync
    { vgaHSync :: Signal dom Bit
    , vgaVSync :: Signal dom Bit
    , vgaDE :: Signal dom Bool
    }

data VGAOut dom r g b = VGAOut
    { vgaSync  :: VGASync dom
    , vgaR     :: Signal dom (Unsigned r)
    , vgaG     :: Signal dom (Unsigned g)
    , vgaB     :: Signal dom (Unsigned b)
    }

data VGADriver dom w h = VGADriver
    { vgaSync :: VGASync dom
    , vgaX :: Signal dom (Maybe (Index w))
    , vgaY :: Signal dom (Maybe (Index h))
    }

-- The weird prefix names used here are due to https://github.com/clash-lang/clash-compiler/issues/1041
vgaPort :: PortName
vgaPort = PortProduct ""
    [ PortProduct "VGA"
      [ PortName "HSYNC"
      , PortName "VSYNC"
      , PortName "DE"
      ]
    , PortName "VGA_RED"
    , PortName "VGA_GREEN"
    , PortName "VGA_BLUE"
    ]

data VGATiming (visible :: Nat) = forall front pulse back. VGATiming
    { polarity :: Polarity
    , preWidth :: SNat front
    , pulseWidth :: SNat pulse
    , postWidth :: SNat back
    }
deriving instance Show (VGATiming vis)

data VGATimings (ps :: Nat) (w :: Nat) (h :: Nat) = VGATimings
    { vgaHorizTiming :: VGATiming w
    , vgaVertTiming :: VGATiming h
    }
    deriving (Show)

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

data VGACounter visible
    = forall front pulse back. (KnownNat front, KnownNat pulse, KnownNat back)
    => VGACounter (VGAState visible front pulse back -> VGAState visible front pulse back)

vgaCounter :: (KnownNat visible) => VGATiming visible -> VGACounter visible
vgaCounter (VGATiming _ front@SNat pulse@SNat back@SNat) = VGACounter next
  where
    next (Visible cnt) = maybe (FrontPorch $ the front 0) Visible $ succIdx cnt
    next (FrontPorch cnt) = maybe (SyncPulse $ the pulse 0) FrontPorch $ succIdx cnt
    next (SyncPulse cnt) = maybe (BackPorch $ the back 0) SyncPulse $ succIdx cnt
    next (BackPorch cnt) = maybe (Visible 0) BackPorch $ succIdx cnt

    the :: SNat n -> Index n -> Index n
    the _ = id

vgaDriver
    :: (HiddenClockResetEnable dom, KnownNat w, KnownNat h)
    => (DomainPeriod dom ~ ps)
    => VGATimings ps w h
    -> VGADriver dom w h
vgaDriver VGATimings{..} = case (vgaCounter vgaHorizTiming, vgaCounter vgaVertTiming) of
    (VGACounter nextH, VGACounter nextV) -> VGADriver{ vgaSync = VGASync{..}, .. }
      where
        stateH = register (Visible 0) $ nextH <$> stateH
        stateV = regEn (Visible 0) endLine $ nextV <$> stateV

        vgaX = visible <$> stateH
        vgaHSync = toActiveDyn (polarity vgaHorizTiming) . sync <$> stateH
        endLine = end <$> stateH

        vgaY = visible <$> stateV
        vgaVSync = toActiveDyn (polarity vgaVertTiming) . sync <$> stateV

        vgaDE = isJust <$> vgaX .&&. isJust <$> vgaY


vgaOut
    :: (HiddenClockResetEnable dom, KnownNat r, KnownNat g, KnownNat b)
    => VGASync dom
    -> Signal dom (Unsigned r, Unsigned g, Unsigned b)
    -> VGAOut dom r g b
vgaOut vgaSync@VGASync{..} rgb = VGAOut{..}
  where
    (vgaR, vgaG, vgaB) = unbundle $ blank rgb

    blank = mux (not <$> vgaDE) (pure (0, 0, 0))

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
