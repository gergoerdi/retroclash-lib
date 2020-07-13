{-# LANGUAGE ScopedTypeVariables, ApplicativeDo, Rank2Types #-}
{-# LANGUAGE TupleSections #-}
module RetroClash.Utils
    ( withResetEnableGen
    , withEnableGen

    , withStart

    , Polarity(..), Active, active, IsActive(..)
    , toActiveDyn

    , (.==)
    , (==.)
    , (./=)
    , (/=.)
    , (.>)
    , (.>=)
    , (.<)
    , (.<=)

    , (.!!.)
    , (.!!)
    , (!!.)

    , changed
    , integrate
    , debounce

    , oneHot
    , roundRobin

    , countFromTo
    , nextIdx, prevIdx
    , succIdx, predIdx
    , moreIdx, lessIdx

    , mealyState
    , mealyStateB

    , enable
    , muxA
    , packWrite
    ) where

import Clash.Prelude
import RetroClash.Clock
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict
import qualified Data.Foldable as F
import RetroClash.Clock
import Barbies
import Barbies.Bare
import Data.Functor.Identity

withResetEnableGen
    :: (KnownDomain dom)
    => (HiddenClockResetEnable dom => r)
    -> Clock dom -> r
withResetEnableGen board clk = withClockResetEnable clk resetGen enableGen board

withEnableGen
    :: (KnownDomain dom)
    => (HiddenClockResetEnable dom => r)
    -> Clock dom -> Reset dom -> r
withEnableGen board clk rst = withClockResetEnable clk rst enableGen board

oneHot :: forall n. (KnownNat n) => Index n -> Vec n Bool
oneHot = reverse . bitCoerce . bit @(Unsigned n) . fromIntegral

changed :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => a -> Signal dom a -> Signal dom Bool
changed x0 x = x ./=. register x0 x

integrate
    :: (Monoid a, NFDataX a, HiddenClockResetEnable dom)
    => Signal dom Bool -> Signal dom a -> Signal dom a
integrate clear x = acc
  where
    acc = register mempty $ mux clear x $ mappend <$> acc <*> x

debounce
    :: forall ps a dom. (Eq a, NFDataX a, HiddenClockResetEnable dom, KnownNat (ClockDivider dom ps))
    => SNat ps -> a -> Signal dom a -> Signal dom a
debounce SNat start this = regEn start stable this
  where
    counter = register (0 :: Index (ClockDivider dom ps)) counterNext
    counterNext = mux (changed start this) 0 (moreIdx <$> counter)
    stable = counterNext .==. pure maxBound

roundRobin
    :: forall n dom a. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> (Signal dom (Vec n Bool), Signal dom (Index n))
roundRobin next = (selector, i)
  where
    i = regEn (0 :: Index n) next $ nextIdx <$> i
    selector = oneHot <$> i

data Polarity = High | Low
    deriving (Show, Eq)

newtype Active (p :: Polarity) = MkActive{ activeLevel :: Bit }
    deriving (Show, Eq, Ord, Generic, NFDataX, BitPack)

active :: Bit -> Active p
active = MkActive

toActiveDyn :: Polarity -> Bool -> Bit
toActiveDyn High = boolToBit
toActiveDyn Low = complement . boolToBit

fromActiveDyn :: Polarity -> Bit -> Bool
fromActiveDyn High = bitToBool
fromActiveDyn Low = bitToBool . complement

class IsActive p where
    fromActive :: Active p -> Bool
    toActive :: Bool -> Active p

instance IsActive High where
    fromActive = fromActiveDyn High . activeLevel
    toActive = MkActive . toActiveDyn High

instance IsActive Low where
    fromActive = fromActiveDyn Low . activeLevel
    toActive = MkActive . toActiveDyn Low

infix 4 ==.
(==.) :: (Eq a, Functor f) => a -> f a -> f Bool
x ==. fy = (x ==) <$> fy

infix 4 .==
(.==) :: (Eq a, Functor f) => f a -> a -> f Bool
fx .== y = (== y) <$> fx

infix 4 /=.
(/=.) :: (Eq a, Functor f) => a -> f a -> f Bool
x /=. fy = (x /=) <$> fy

infix 4 ./=
(./=) :: (Eq a, Functor f) => f a -> a -> f Bool
fx ./= y = (/= y) <$> fx

infix 4 .>
(.>) :: (Ord a, Functor f) => f a -> a -> f Bool
fx .> y = (> y) <$> fx

infix 4 .>=
(.>=) :: (Ord a, Functor f) => f a -> a -> f Bool
fx .>= y = (>= y) <$> fx

infix 4 .<
(.<) :: (Ord a, Functor f) => f a -> a -> f Bool
fx .< y = (< y) <$> fx

infix 4 .<=
(.<=) :: (Ord a, Functor f) => f a -> a -> f Bool
fx .<= y = (<= y) <$> fx

(.!!.) :: (KnownNat n, Enum i, Applicative f) => f (Vec n a) -> f i -> f a
(.!!.) = liftA2 (!!)

(!!.) :: (KnownNat n, Enum i, Functor f) => Vec n a -> f i -> f a
xs !!. i = (xs !!) <$> i

(.!!) :: (KnownNat n, Enum i, Functor f) => f (Vec n a) -> i -> f a
xs .!! i = (!! i) <$> xs

countFromTo :: (Eq a, Enum a, NFDataX a, HiddenClockResetEnable dom) => a -> a -> Signal dom Bool -> Signal dom a
countFromTo from to tick = counter
  where
    counter = regEn from tick $ mux (counter .==. pure to) (pure from) (succ <$> counter)

nextIdx :: (Eq a, Enum a, Bounded a) => a -> a
nextIdx = fromMaybe minBound . succIdx

prevIdx :: (Eq a, Enum a, Bounded a) => a -> a
prevIdx = fromMaybe maxBound . predIdx

moreIdx :: (Eq a, Enum a, Bounded a) => a -> a
moreIdx = fromMaybe maxBound . succIdx

lessIdx :: (Eq a, Enum a, Bounded a) => a -> a
lessIdx = fromMaybe minBound . predIdx

succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x | x == maxBound = Nothing
          | otherwise = Just $ succ x

predIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
predIdx x | x == minBound = Nothing
          | otherwise = Just $ pred x

mealyState
   :: (HiddenClockResetEnable dom, NFDataX s)
   => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)

mealyStateB
    :: (HiddenClockResetEnable dom, NFDataX s, Bundle i, Bundle o)
    => (i -> State s o) -> s -> (Unbundled dom i -> Unbundled dom o)
mealyStateB f s0 = unbundle . mealyState f s0 . bundle

enable :: (Applicative f) => f Bool -> f a -> f (Maybe a)
enable en x = mux en (Just <$> x) (pure Nothing)

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

muxA :: (Foldable t, Alternative m, Applicative f) => t (f (m a)) -> f (m a)
muxA = F.foldr (liftA2 (<|>)) (pure empty)

withStart :: (HiddenClockResetEnable dom) => a -> Signal dom a -> Signal dom a
withStart x0 = mux (register True $ pure False) (pure x0)
