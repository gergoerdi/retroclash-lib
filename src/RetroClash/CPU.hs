{-# LANGUAGE RankNTypes #-}
module RetroClash.CPU where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Barbies

import Data.Monoid (Last(..))
import Data.Functor.Identity

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens (Setter', scribe, iso)

import Barbies
import Barbies.Bare

infix 4 .:=
(.:=) :: (Applicative f, MonadWriter (Barbie b f) m) => Setter' (b f) (f a) -> a -> m ()
fd .:= x = scribe (iso getBarbie Barbie . fd) (pure x)

assignOut :: (Applicative f, MonadWriter (Barbie b f) m) => Setter' (b f) (f a) -> a -> m ()
assignOut fd x = fd .:= x

update :: (BareB b, ApplicativeB (b Covered)) => Pure b -> Partial b -> Pure b
update initials = bstrip . bzipWith update1 (bcover initials) . getBarbie
  where
    update1 :: Identity a -> Last a -> Identity a
    update1 initial edit = maybe initial Identity (getLast edit)

type Mask b = b Covered (Const Bool)

keep :: (BareB b, ApplicativeB (b Covered), Monoid (Partial b)) => Mask b -> Partial b -> Partial b
keep mask = Barbie . bzipWith3 keep1 mask (getBarbie mempty) . getBarbie
  where
    keep1 :: Const Bool a -> f a -> f a -> f a
    keep1 (Const mask) empty x = if mask then x else empty

type CPUM s o = WriterT (Barbie (o Covered) Last) (State s)

mealyCPU
    :: (BareB i, TraversableB (i Covered))
    => (NFDataX s)
    => (BareB o, ApplicativeB (o Covered), DistributiveB (o Covered))
    => (HiddenClockResetEnable dom)
    => s
    -> (s -> Pure o)
    -> (Pure i -> CPUM s o ())
    -> Signals dom i -> Signals dom o
mealyCPU initState defaultOutput step =
    bunbundle . mealyState (runCPU defaultOutput . step) initState . bbundle

stallable
    :: (BareB o, ApplicativeB (o Covered), Monoid (Partial o))
    => Mask o
    -> MaybeT (CPUM s o) ()
    -> State (s, Partial o) ()
stallable mask step = modify $ \(s0, out0) -> case runState (runWriterT . runMaybeT $ step) s0 of
    ((Nothing, _), _) -> (s0, keep mask out0)
    ((Just (), out), s) -> (s, out)

mooreCPU
    :: (BareB i, TraversableB (i Covered))
    => (NFDataX s, NFDataX (Partial o), Monoid (Partial o))
    => (BareB o, ApplicativeB (o Covered), DistributiveB (o Covered))
    => (HiddenClockResetEnable dom)
    => s
    -> (s -> Pure o)
    -> Mask o
    -> (Pure i -> MaybeT (CPUM s o) ())
    -> Signals dom i -> Signals dom o
mooreCPU initState defaultOutput mask step =
    bunbundle .
    mooreState
      (stallable mask . step)
      (\(s, o) -> update (defaultOutput s) o)
      (initState, mempty) .
    bbundle

runCPU
    :: (BareB o, ApplicativeB (o Covered))
    => (s -> Pure o)
    -> CPUM s o ()
    -> State s (Pure o)
runCPU defaultOutput step = do
    edits <- execWriterT step
    out0 <- gets defaultOutput
    return $ update out0 edits
