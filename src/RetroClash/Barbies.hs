module RetroClash.Barbies
    ( Pure
    , Partial
    , Signals
    , bbundle
    , bunbundle
    ) where

import Clash.Prelude
import Data.Monoid (Last(..))
import Data.Functor.Identity

import Barbies
import Barbies.Bare

type Pure b = b Bare Identity
type Partial b = Barbie (b Covered) Last
type Signals dom b = b Covered (Signal dom)

bbundle :: (Applicative f, BareB b, TraversableB (b Covered)) => b Covered f -> f (Pure b)
bbundle = fmap bstrip . bsequence'

bunbundle :: (Functor f, BareB b, DistributiveB (b Covered)) => f (Pure b) -> b Covered f
bunbundle = bdistribute' . fmap bcover
