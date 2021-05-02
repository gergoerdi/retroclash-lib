{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module RetroClash.Memory
    ( RAM, ROM
    , packRam

    , memoryMap_

    , readWrite_
    , romFromVec, romFromFile
    , ram0, ramFromFile
    , connect

    , from
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

type RAM dom addr dat = Signal dom addr -> Signal dom (Maybe (addr, dat)) -> Signal dom dat
type ROM dom addr dat = Signal dom addr ->                                   Signal dom dat

packRam :: (BitPack dat) => RAM dom addr (BitVector (BitSize dat)) -> RAM dom addr dat
packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

data Handle addr = Handle Name Name

-- | type Addr dom addr = TExpQ (Signal dom (Maybe addr))
type Addr = ExpQ

-- | type Dat dom dat = TExpQ (Signal dom (Maybe dat))
type Dat = ExpQ

-- | type Component dom dat a = TExpQ (Signal dom (Maybe dat))
type Component = ExpQ

newtype Addressing addr a = Addressing
    { runAddressing :: ReaderT (Addr, Dat) (WriterT (DecsQ, MonoidalMap Name [Addr], [Component]) Q) a }
    deriving newtype (Functor, Applicative, Monad)

compile
    :: Addressing addr ()
    -> Addr
    -> Dat
    -> Component
compile addressing addr wr = do
    -- (x, (decs, conns, rds)) <-
    --     runWriterT $ runReaderT (runAddressing addressing) ([| Just <$> $addr |], wr)
    (decs, conns, outs) <-
        execWriterT $ runReaderT (runAddressing addressing) ([| Just <$> $addr |], wr)

    let compAddrs = [ [d| $(varP nm) = muxA $(listE addrs) |]
                    | (nm, addrs) <- Map.toList conns
                    ]
    decs <- mconcat (decs:compAddrs)
    letE (pure <$> decs) [| muxA $(listE outs) .<| 0 |]

memoryMap_
    :: Addr
    -> Dat
    -> Addressing addr ()
    -> Component
memoryMap_ addr wr addressing =
    [| let addr' = $addr; wr' = $wr
       in $(compile addressing [| addr' |] [| wr' |])
    |]

connect
    :: Handle addr
    -> Addressing addr ()
connect (Handle rd compAddr) = Addressing $ do
    (addr, _) <- ask
    let masked = [| enable (delay False $ isJust <$> $addr) $(varE rd) |]
    tell (mempty, Map.singleton compAddr [addr], [masked])

matchAddr
    :: ExpQ {-(addr -> Maybe addr')-}
    -> Addressing addr' a
    -> Addressing addr a
matchAddr match body = Addressing $ do
    nm <- lift . lift $ newName "addr"
    let addr' = varE nm
    ReaderT $ \(addr, wr) -> do
        let dec = [d| $(varP nm) = ($match =<<) <$> $addr |]
        runReaderT
          (tell (dec, mempty, mempty) >> runAddressing body)
          (addr', wr)

readWrite_
    :: (Addr -> Dat -> Component)
    -> Addressing addr (Handle addr')
readWrite_ component = Addressing $ do
    rd <- lift . lift $ newName "rd"
    addr <- lift . lift $ newName "compAddr"
    (_, wr) <- ask
    let decs = [d| $(varP rd) = $(component (varE addr) wr) |]
    tell (decs, Map.singleton addr mempty, mempty)
    return $ Handle rd addr

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ {-(Vec n dat)-}
    -> Addressing addr (Handle (Index n))
romFromVec size@SNat xs = readWrite_ $ \addr _wr ->
    [| rom $xs (bitCoerce . fromJustX <$> $addr) |]

romFromFile
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing addr (Handle (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr _wr ->
    [| fmap unpack $ romFilePow2 $fileName (bitCoerce . fromJustX <$> $addr) |]

ram0
    :: (1 <= n)
    => SNat n
    -> Addressing addr (Handle (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
    [| blockRam1 NoClearOnReset size 0 (fromJustX <$> $addr) (liftA2 (,) <$> $addr <*> $wr) |]

ramFromFile
    :: SNat n
    -> ExpQ {- FilePath -}
    -> Addressing addr (Handle (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \addr wr ->
    [| packRam (blockRamFile size $fileName)
           (fromJustX <$> $addr)
           (liftA2 (,) <$> $addr <*> $wr)
    |]

from
    :: forall addr' addr a. (Typeable addr', Lift addr)
    => (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> Addressing addr' a
    -> Addressing addr a
from base = matchAddr [| from' @($(liftTypeQ @addr')) base |]

from'
    :: forall addr' addr. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr -> addr -> Maybe addr'
from' base addr = do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= fromIntegral (maxBound :: addr')
    return (fromIntegral offset)
