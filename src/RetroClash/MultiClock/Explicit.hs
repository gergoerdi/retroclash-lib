module RetroClash.MultiClock.Explicit where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import RetroClash.Utils

unsafeConvertJustSpike
    :: forall domA domB a. (NFDataX a)
    => (KnownDomain domA, KnownDomain domB)
    => Clock domA -> Clock domB
    -> Reset domA -> Reset domB
    -> Enable domA -> Enable domB
    -> Signal domA (Maybe a)
    -> Signal domB (Maybe a)
unsafeConvertJustSpike clkA clkB rstA rstB enA enB sigA = sigB
  where
    (lastJustB, emptyB, _) = E.asyncFIFOSynchronizer (SNat @2) clkA clkB rstA rstB enA enB (pure True) sigA
    sigB = enable (not <$> emptyB) lastJustB
