{-# LANGUAGE RankNTypes #-}
module RetroClash.CPU where

import Clash.Prelude
import RetroClash.Utils

import Data.Monoid (Last(..))
import Data.Functor.Identity

import Control.Monad.Writer
import Control.Monad.State
import Control.Lens (Setter', scribe, iso)

import Barbies
import Barbies.Bare

type Raw b = b Bare Identity
type SignalB dom b = b Covered (Signal dom)
type Partial b = Barbie (b Covered) Last

(.:=) :: (Applicative f, MonadWriter (Barbie b f) m) => Setter' (b f) (f a) -> a -> m ()
fd .:= x = scribe (iso getBarbie Barbie . fd) (pure x)

update :: (BareB b, ApplicativeB (b Covered)) => Raw b -> Partial b -> Raw b
update initials edits = bstrip $ bzipWith update1 (bcover initials) (getBarbie edits)
  where
    update1 :: Identity a -> Last a -> Identity a
    update1 initial edit = maybe initial Identity (getLast edit)

type CPUM s o = WriterT (Barbie (o Covered) Last) (State s)

mealyCPU
    :: (BareB i, TraversableB (i Covered))
    => (NFDataX s)
    => (BareB o, ApplicativeB (o Covered), DistributiveB (o Covered))
    => (HiddenClockResetEnable dom)
    => s
    -> (s -> Raw o)
    -> (Raw i -> CPUM s o ())
    -> SignalB dom i -> SignalB dom o
mealyCPU initState defaultOutput step =
    bunbundle . mealyState (runCPU defaultOutput . step) initState . bbundle

runCPU
    :: (BareB o, ApplicativeB (o Covered))
    => (s -> Raw o)
    -> CPUM s o ()
    -> State s (Raw o)
runCPU defaultOutput step = do
    edits <- execWriterT step
    out0 <- gets defaultOutput
    return $ update out0 edits
