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

packRam :: (BitPack d) => RAM dom a (BitVector (BitSize d)) -> RAM dom a d
packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

newtype FirstSignal dom a = FirstSignal{ getFirstSignal :: Signal dom (First a) }

instance Semigroup (FirstSignal dom a) where
    x <> y = FirstSignal $ (<>) <$> getFirstSignal x <*> getFirstSignal y

instance Monoid (FirstSignal dom a) where
    mempty = FirstSignal $ pure mempty

newtype Addressing dom addr dat a = Addressing
    { unAddressing :: ReaderT
                      (Signal dom (Maybe addr), Signal dom (Maybe dat))
                      (Writer (FirstSignal dom dat))
                      a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat out
    -> (Signal dom (Maybe dat), out)
memoryMap addr write spec = (getFirst <$> getFirstSignal result, output)
  where
    (output, result) = runWriter $ runReaderT (unAddressing spec) (addr, write)

memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat ()
    -> Signal dom (Maybe dat)
memoryMap_ addr write spec = fst $ memoryMap addr write spec

mapAddr
    :: (HiddenClockResetEnable dom)
    => (addr -> Maybe addr') -> Addressing dom addr' dat r -> Addressing dom addr dat r
mapAddr f body = Addressing $ withReaderT (first $ fmap (f =<<)) $ do
    (addr, _) <- ask
    censor (FirstSignal . whenAddr addr . getFirstSignal) $ unAddressing body
  where
    whenAddr addr sig = mux (delay False $ isJust <$> addr) sig (pure mempty)

infix 1 <||>
(<||>)
    :: (HiddenClockResetEnable dom)
    => Addressing dom addr1 dat r1
    -> Addressing dom addr2 dat r2
    -> Addressing dom (Either addr1 addr2) dat (r1, r2)
body1 <||> body2 = do
    x <- mapAddr (either Just (const Nothing)) body1
    y <- mapAddr (either (const Nothing) Just) body2
    return (x, y)

mask
    :: (KnownNat k, KnownNat n)
    => (HiddenClockResetEnable dom)
    => Unsigned (n + k)
    -> Addressing dom (Unsigned k)       dat r
    -> Addressing dom (Unsigned (n + k)) dat r
mask base = mapAddr (maskAddr base)

offset
    :: (Num addr, Ord addr)
    => (HiddenClockResetEnable dom)
    => addr
    -> Addressing dom addr dat r
    -> Addressing dom addr dat r
offset base = mapAddr $ \addr -> do
    guard $ base <= addr
    return $ addr - base

ram :: (Num addr) => RAM dom addr dat -> Addressing dom addr dat ()
ram mkRam = Addressing $ do
    (addr, w) <- ask
    let output = mkRam (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> w)
    tell $ FirstSignal $ pure <$> output

rom :: (Num addr) => ROM dom addr dat -> Addressing dom addr dat ()
rom mkRom = Addressing $ do
    (addr, w) <- ask
    let output = mkRom (fromMaybe 0 <$> addr)
    tell $ FirstSignal $ pure <$> output

port
    :: (HiddenClockResetEnable dom, NFDataX dat)
    => (Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), r))
    -> Addressing dom addr dat r
port mkPort = Addressing $ do
    (addr, w) <- ask
    let (output, result) = mkPort $ portFromAddr addr w
    tell $ FirstSignal $ delay mempty $ First <$> output
    return result
