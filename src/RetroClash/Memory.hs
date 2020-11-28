{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns, ApplicativeDo #-}
module RetroClash.Memory
    ( RAM(..), ROM(..)
    , packRam

    , Addressing
    , memoryMap, memoryMap_
    , (<||>)
    , matchLeft, matchRight
    , mask
    , offset
    , override
    , readWrite, readOnly, port
    , romFromFile, ram0
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

newtype Addressing dom addr dat a = Addressing
    { unAddressing :: ReaderT
                      (Signal dom (Maybe addr), Signal dom (Maybe dat))
                      (Writer (Ap (Signal dom) (First dat)))
                      a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat a
    -> (Signal dom (Maybe dat), a)
memoryMap addr write spec = (read, result)
  where
    (result, output) = runWriter $ runReaderT (unAddressing spec) (addr, write)
    read = getFirst <$> getAp output

memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat ()
    -> Signal dom (Maybe dat)
memoryMap_ addr write spec = fst $ memoryMap addr write spec

mapAddr
    :: (HiddenClockResetEnable dom)
    => (addr -> Maybe addr') -> Addressing dom addr' dat a -> Addressing dom addr dat a
mapAddr f body = Addressing $ withReaderT (first $ fmap (f =<<)) $ do
    (addr, _) <- ask
    censor (Ap . whenAddr addr . getAp) $ unAddressing body
  where
    whenAddr addr sig = mux (delay False $ isJust <$> addr) sig (pure mempty)

infix 1 <||>
(<||>)
    :: (HiddenClockResetEnable dom)
    => Addressing dom addr1 dat a
    -> Addressing dom addr2 dat b
    -> Addressing dom (Either addr1 addr2) dat (a, b)
body1 <||> body2 = liftA2 (,) (matchLeft body1) (matchRight body2)

override
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe dat)
    -> Addressing dom addr dat a
    -> Addressing dom addr dat a
override sig = Addressing . censor (mappend sig') . withReaderT (first propagate) . unAddressing
  where
    sig' = Ap $ First <$> sig

    propagate = mux (isJust <$> sig) (pure Nothing)

matchLeft
    :: (HiddenClockResetEnable dom)
    => Addressing dom addr1 dat a
    -> Addressing dom (Either addr1 addr2) dat a
matchLeft = mapAddr (either Just (const Nothing))

matchRight
    :: (HiddenClockResetEnable dom)
    => Addressing dom addr2 dat b
    -> Addressing dom (Either addr1 addr2) dat b
matchRight = mapAddr (either (const Nothing) Just)

romFromFile
    :: (HiddenClockResetEnable dom, 1 <= n, BitPack dat)
    => SNat n -> FilePath -> Addressing dom (Unsigned (CLog 2 n)) dat ()
romFromFile SNat fileName = readOnly $ fmap unpack . romFilePow2 fileName

ram0
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, Num dat)
    => SNat n -> Addressing dom (Unsigned (CLog 2 n)) dat ()
ram0 size@SNat = readWrite $ blockRamU ClearOnReset size (const 0)

readWrite :: (Num addr) => RAM dom addr dat -> Addressing dom addr dat ()
readWrite ram = Addressing $ do
    (addr, w) <- ask
    let output = ram (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> w)
    tell $ Ap $ pure <$> output

readOnly :: (Num addr) => ROM dom addr dat -> Addressing dom addr dat ()
readOnly rom = Addressing $ do
    (addr, w) <- ask
    let output = rom (fromMaybe 0 <$> addr)
    tell $ Ap $ pure <$> output

port
    :: (HiddenClockResetEnable dom, NFDataX dat)
    => (Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a))
    -> Addressing dom addr dat a
port mkPort = Addressing $ do
    (addr, w) <- ask
    let (output, result) = mkPort $ portFromAddr addr w
    tell $ Ap $ delay mempty $ First <$> output
    return result

mask
    :: (KnownNat k, KnownNat n)
    => (HiddenClockResetEnable dom)
    => Unsigned (n + k)
    -> Addressing dom (Unsigned k)       dat a
    -> Addressing dom (Unsigned (n + k)) dat a
mask base = mapAddr $ \addr -> do
    let (space, offset) = splitAddr addr
    guard $ space == baseSpace
    return offset
  where
    (baseSpace, _) = splitAddr base

splitAddr :: (KnownNat n, KnownNat k) => Unsigned (n + k) -> (Unsigned n, Unsigned k)
splitAddr = bitCoerce

offset
    :: (Num addr, Ord addr)
    => (HiddenClockResetEnable dom)
    => addr
    -> Addressing dom addr dat a
    -> Addressing dom addr dat a
offset base = mapAddr $ \addr -> do
    guard $ base <= addr
    return $ addr - base
