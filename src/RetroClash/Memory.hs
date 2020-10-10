{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns, ApplicativeDo #-}
module RetroClash.Memory
    ( RAM(..), ROM(..)
    , packRam

    , Addressing
    , memoryMap, memoryMap_
    , (<||>)
    , mask
    , offset
    , ram, rom, port
    ) where

import Clash.Prelude hiding (rom)
import RetroClash.Utils
import RetroClash.Port
import Control.Arrow (first, second)
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

type RAM dom a d = Signal dom a -> Signal dom (Maybe (a, d)) -> Signal dom d
type ROM dom a d = Signal dom a ->                              Signal dom d

data Mem dom a d
    = RAM (Signal dom a -> Signal dom (Maybe (a, d)) -> Signal dom d)
    | ROM (Signal dom a ->                              Signal dom d)

packRam :: (BitPack d) => RAM dom a (BitVector (BitSize d)) -> RAM dom a d
packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

newtype CollectedSignal dom a = CollectedSignal{ getCollectedSignal :: Signal dom (Maybe a) }

instance Semigroup (CollectedSignal dom a) where
    x <> y = CollectedSignal $ mplus <$> getCollectedSignal x <*> getCollectedSignal y

instance Monoid (CollectedSignal dom a) where
    mempty = CollectedSignal $ pure Nothing

newtype Addressing dom addr dat res a = Addressing
    { unAddressing :: ReaderT
                      (Signal dom (Maybe addr), Signal dom (Maybe dat))
                      (Writer (CollectedSignal dom res))
                      a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat res out
    -> (Signal dom (Maybe res), out)
memoryMap addr write spec = (result, output)
  where
    (output, getCollectedSignal -> result) = runWriter $ runReaderT (unAddressing spec) (addr, write)

memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat res ()
    -> Signal dom (Maybe res)
memoryMap_ addr write spec = fst $ memoryMap addr write spec

mapAddr
    :: (HiddenClockResetEnable dom)
    => (addr -> Maybe addr') -> Addressing dom addr' dat out r -> Addressing dom addr dat out r
mapAddr f body = Addressing $ withReaderT (first $ fmap (f =<<)) $ do
    (addr, _) <- ask
    censor (CollectedSignal . fmap join . enable (delay False $ isJust <$> addr) . getCollectedSignal) $ unAddressing body

infix 1 <||>
(<||>)
    :: (HiddenClockResetEnable dom)
    => Addressing dom addr1 dat out r1
    -> Addressing dom addr2 dat out r2
    -> Addressing dom (Either addr1 addr2) dat out (r1, r2)
body1 <||> body2 = do
    x <- mapAddr (either Just (const Nothing)) body1
    y <- mapAddr (either (const Nothing) Just) body2
    return (x, y)

mask
    :: (KnownNat k, KnownNat n)
    => (HiddenClockResetEnable dom)
    => Unsigned (n + k)
    -> Addressing dom (Unsigned k)       dat out r
    -> Addressing dom (Unsigned (n + k)) dat out r
mask base = mapAddr (maskAddr base)

offset
    :: (Num addr, Ord addr)
    => (HiddenClockResetEnable dom)
    => addr
    -> Addressing dom addr dat out r
    -> Addressing dom addr dat out r
offset base = mapAddr $ \addr -> do
    guard $ base <= addr
    return $ addr - base

ram :: (Num addr) => RAM dom addr dat -> Addressing dom addr dat dat ()
ram mkRam = Addressing $ do
    (addr, w) <- ask
    let output = mkRam (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> w)
    tell $ CollectedSignal $ Just <$> output

rom :: (Num addr) => ROM dom addr dat -> Addressing dom addr w dat ()
rom mkRom = Addressing $ do
    (addr, w) <- ask
    let output = mkRom (fromMaybe 0 <$> addr)
    tell $ CollectedSignal $ Just <$> output

port
    :: (HiddenClockResetEnable dom, NFDataX out)
    => (Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe out), r))
    -> Addressing dom addr dat out r
port mkPort = Addressing $ do
    (addr, w) <- ask
    let (output, result) = mkPort $ portFromAddr addr w
    tell $ CollectedSignal $ delay Nothing output
    return result
