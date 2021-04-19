{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory
    ( RAM, ROM, Port, Port_
    , packRam

    , memoryMap_

    , readWrite_
    , romFromVec, romFromFile
    , ram0, ramFromFile
    , connect

    , from
    , matchLeft, matchRight
    , tag
    ) where

import Clash.Prelude hiding (Exp, lift)
import RetroClash.Utils
import RetroClash.Port
import Data.Maybe
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Kind (Type)

import Data.List as L
import Data.Map.Monoidal as Map

import Language.Haskell.TH hiding (Type)
import LiftType
import Type.Reflection (Typeable)
import qualified Language.Haskell.TH.Syntax as TH

type RAM dom addr dat = Signal dom addr -> Signal dom (Maybe (addr, dat)) -> Signal dom dat
type ROM dom addr dat = Signal dom addr ->                                   Signal dom dat
type Port dom addr dat a = Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a)
type Port_ dom addr dat = Signal dom (Maybe (PortCommand addr dat)) -> Signal dom (Maybe dat)

packRam :: (BitPack dat) => RAM dom addr (BitVector (BitSize dat)) -> RAM dom addr dat
packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

data Handle s (addr :: Type) = Handle Name Name

-- | type Addr dom addr = TExpQ (Signal dom (Maybe addr))
type Addr = ExpQ

-- | type Dat dom dat = TExpQ (Signal dom (Maybe dat))
type Dat = ExpQ

-- | type Component dom dat a = TExpQ (Signal dom (Maybe dat), a)
type Component = ExpQ

newtype Addressing (s :: Type) (addr :: Type) (a :: Type) = Addressing
    { runAddressing :: ReaderT (Addr, Dat) (WriterT (DecsQ, [Component], MonoidalMap Name [Addr]) Q) a }
    deriving newtype (Functor, Applicative, Monad)

compile
    :: (forall s. Addressing s addr ())
    -> Addr
    -> Dat
    -> Component
compile addressing addr wr = do
    (x, (decs, rds, conns)) <-
        runWriterT $ runReaderT (runAddressing addressing) (addr, wr)

    let compDecs = [ [d| $(varP nm) = muxA $(listE addrs) |]
                   | (nm, addrs) <- Map.toList conns
                   ]
    decs <- mconcat (decs:compDecs)

    let rd = [| muxA $(listE rds) .<| Just 0 |]
    letE (pure <$> decs) rd

memoryMap_
    :: Addr
    -> Dat
    -> (forall s. Addressing s addr ())
    -> Component
memoryMap_ addr wr addressing =
    [| let addr' = $addr; wr' = $wr
       in $(compile addressing [| addr' |] [| wr' |])
    |]

connect
    :: Handle s addr
    -> Addressing s addr ()
connect (Handle rd compAddr) = Addressing $ do
    (addr, _) <- ask
    let masked = [| enable (delay False $ isJust <$> $addr) $(varE rd) |]
    tell (mempty, [masked], Map.singleton compAddr [addr])

override
    :: ExpQ
    -> Addressing s addr ()
override sig = Addressing $ do
    rd <- lift . lift $ newName "rd"
    let decs = [d| $(varP rd) = weak $sig |]
    tell (decs, [varE rd], mempty)

weak :: (Functor f) => f (Maybe a) -> f (Maybe (Maybe a))
weak = fmap (maybe Nothing (Just . Just))

matchAddr
    :: forall addr' addr a s dat. ()
    => ExpQ {-(addr -> Maybe addr')-}
    -> Addressing s addr' a
    -> Addressing s addr a
matchAddr match body = Addressing $ do
    nm <- lift . lift $ newName "addr"
    let addr' = varE nm
    ReaderT $ \(addr, wr) -> do
        let dec = [d| $(varP nm) = ($match =<<) <$> $addr |]
        runReaderT
          (tell (dec, mempty, mempty) >> runAddressing body)
          (addr', wr)

readWrite_
    :: forall addr' addr s dat. ()
    => (Addr -> Dat -> Component)
    -> Addressing s addr (Handle s addr')
readWrite_ component = Addressing $ do
    rd <- lift . lift $ newName "rd"
    addr <- lift . lift $ newName "compAddr"
    (_, wr) <- ask
    let decs = [d| $(varP rd) = $(component (varE addr) wr) |]
    tell (decs, mempty, Map.singleton addr mempty)
    return $ Handle rd addr

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ {-(Vec n dat)-}
    -> Addressing s addr (Handle s (Index n))
romFromVec size@SNat xs = readWrite_ $ \addr _wr ->
    [| fmap Just $ rom $xs (bitCoerce . fromJustX <$> $addr) |]

romFromFile
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing s addr (Handle s (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr _wr ->
    [| fmap (Just . unpack) $ romFilePow2 $fileName (maybe 0 bitCoerce <$> $addr) |]

ram0
    :: (1 <= n)
    => SNat n
    -> Addressing s addr (Handle s (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
    [| fmap Just $ blockRam1 NoClearOnReset $(TH.lift size) 0 (fromMaybe 0 <$> $addr) (liftA2 (,) <$> $addr <*> $wr) |]

ramFromFile
    :: SNat n
    -> ExpQ {-FilePath-}
    -> Addressing s addr (Handle s (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \addr wr ->
    [| fmap (Just . unpack) $
     blockRamFile size $fileName
       (fromJustX <$> $addr)
       (liftA2 (,) <$> $addr <*> (fmap pack <$> $wr))
    |]

from
    :: forall addr' s addr a. (Integral addr, Ord addr, Integral addr', Bounded addr', Typeable addr', Lift addr)
    => addr
    -> Addressing s addr' a
    -> Addressing s addr a
from base = matchAddr [| from_ @($(liftTypeQ @addr')) $(TH.lift base) |]

tag
    :: (Lift addr')
    => addr'
    -> Addressing s (addr', addr) a
    -> Addressing s addr a
tag t = matchAddr [| \addr -> Just ($(TH.lift t), addr) |]

matchLeft
    :: Addressing s addr1 a
    -> Addressing s (Either addr1 addr2) a
matchLeft = matchAddr [| either Just (const Nothing) |]

matchRight
    :: Addressing s addr2 a
    -> Addressing s (Either addr1 addr2) a
matchRight = matchAddr [| either (const Nothing) Just |]

from_ :: forall addr' addr. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr -> addr -> Maybe addr'
from_ base addr = do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= fromIntegral (maxBound :: addr')
    return (fromIntegral offset)
