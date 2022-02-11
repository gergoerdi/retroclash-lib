{-# LANGUAGE ScopedTypeVariables, NumericUnderscores #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module RetroClash.VGA
    ( VGASync(..)
    , VGADriver(..)
    , vgaDriver
    , VGAOut(..)
    , vgaOut
    , VGATiming(..), VGATimings(..)

    , vga640x480at60
    , vga800x600at60
    , vga800x600at72
    , vga1024x768at60
    ) where

import Clash.Prelude
import Clash.Class.HasDomain
import RetroClash.Clock
import RetroClash.Utils
import Data.Maybe (isJust)

data VGASync dom = VGASync
    { vgaHSync :: "HSYNC" ::: Signal dom Bit
    , vgaVSync :: "VSYNC" ::: Signal dom Bit
    , vgaDE :: "DE" ::: Signal dom Bool
    }

data VGAOut dom r g b = VGAOut
    { vgaSync  :: VGASync dom
    , vgaR     :: "RED" ::: Signal dom (Unsigned r)
    , vgaG     :: "GREEN" ::: Signal dom (Unsigned g)
    , vgaB     :: "BLUE" ::: Signal dom (Unsigned b)
    }

type instance HasDomain dom1 (VGAOut dom2 r g b) = DomEq dom1 dom2
type instance TryDomain t (VGAOut dom r g b) = Found dom

data VGADriver dom w h = VGADriver
    { vgaSync :: VGASync dom
    , vgaX :: Signal dom (Maybe (Index w))
    , vgaY :: Signal dom (Maybe (Index h))
    }

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

type Step a = a -> a

data VGACounter visible
    = forall front pulse back. (KnownNat front, KnownNat pulse, KnownNat back)
    => VGACounter (Step (VGAState visible front pulse back))

mkVGACounter
    :: SNat front -> SNat pulse -> SNat back
    -> Step (VGAState visible front pulse back)
    -> VGACounter visible
mkVGACounter SNat SNat SNat = VGACounter

vgaCounter :: (KnownNat visible) => VGATiming visible -> VGACounter visible
vgaCounter (VGATiming _ front@SNat pulse@SNat back@SNat) =
    mkVGACounter front pulse back $ \case
        Visible cnt    -> count Visible    FrontPorch cnt
        FrontPorch cnt -> count FrontPorch SyncPulse  cnt
        SyncPulse cnt  -> count SyncPulse  BackPorch  cnt
        BackPorch cnt  -> count BackPorch  Visible    cnt
  where
    count :: (KnownNat n, KnownNat m) => (Index n -> a) -> (Index m -> a) -> Index n -> a
    count this next = maybe (next 0) this . succIdx

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
