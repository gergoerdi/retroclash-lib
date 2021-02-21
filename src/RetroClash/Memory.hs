{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory
    ( module RetroClash.Memory.Core

    , readWrite_, conduit
    , romFromVec, romFromFile
    , ram0, ramFromFile
    , port, port_

    , from
    , matchLeft, matchRight
    , tag
    ) where

import RetroClash.Memory.Core

import Clash.Prelude
import RetroClash.Port
import Data.Maybe
import Control.Monad

type Port dom addr dat a = Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a)
type Port_ dom addr dat = Signal dom (Maybe (PortCommand addr dat)) -> Signal dom (Maybe dat)

conduit
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe dat)
    -> Addressing s dom dat addr (Component s addr', Signal dom (Maybe addr'), Signal dom (Maybe dat))
conduit read = do
    (component, (addr, wr)) <- readWrite $ \addr wr -> (read, (addr, wr))
    return (component, addr, wr)

readWrite_
    :: forall addr' s dom addr dat. (HiddenClockResetEnable dom)
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> Signal dom (Maybe dat))
    -> Addressing s dom dat addr (Component s addr')
readWrite_ mkComponent = fmap fst $ readWrite $ \addr wr -> (mkComponent addr wr, ())

romFromVec
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, KnownNat n)
    => SNat (n + k)
    -> Vec n dat
    -> Addressing s dom dat addr (Component s (Index (n + k)))
romFromVec size@SNat xs = readWrite_ $ \addr _wr ->
    fmap Just $ rom xs (maybe 0 bitCoerce <$> addr)

romFromFile
    :: (HiddenClockResetEnable dom, 1 <= n, BitPack dat)
    => SNat n
    -> FilePath
    -> Addressing s dom dat addr (Component s (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr _wr ->
    fmap (Just . unpack) $ romFilePow2 fileName (maybe 0 bitCoerce <$> addr)

ram0
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, Num dat)
    => SNat n
    -> Addressing s dom dat addr (Component s (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
      fmap Just $ blockRam1 ClearOnReset size 0 (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> wr)

ramFromFile
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, BitPack dat)
    => SNat n
    -> FilePath
    -> Addressing s dom dat addr (Component s (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \addr wr ->
    fmap (Just . unpack) $ blockRamFile size fileName (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> (fmap pack <$> wr))

port
    :: (HiddenClockResetEnable dom, NFDataX dat)
    => Port dom addr' dat a
    -> Addressing s dom dat addr (Component s addr', a)
port mkPort = readWrite $ \addr wr ->
    let (read, x) = mkPort $ portFromAddr addr wr
    in (delay Nothing read, x)

port_
    :: (HiddenClockResetEnable dom, NFDataX dat)
    => Port_ dom addr' dat
    -> Addressing s dom dat addr (Component s addr')
port_ mkPort = readWrite_ $ \addr wr ->
    let read = mkPort $ portFromAddr addr wr
    in (delay Nothing read)

tag
    :: addr'
    -> Addressing s dom dat (addr', addr) a
    -> Addressing s dom dat addr a
tag t = matchAddr $ \addr -> Just (t, addr)

matchLeft
    :: Addressing s dom dat addr1 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchLeft = matchAddr $ either Just (const Nothing)

matchRight
    :: Addressing s dom dat addr2 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchRight = matchAddr $ either (const Nothing) Just

from
    :: forall addr' s dom dat addr a. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
from base = matchAddr $ \addr -> do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= lim
    return $ fromIntegral offset
  where
    lim = fromIntegral (maxBound :: addr')
