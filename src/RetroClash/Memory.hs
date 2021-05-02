{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module RetroClash.Memory
    ( RAM, ROM, Port, Port_
    , packRam

    , memoryMap, memoryMap_

    , conduit, readWrite, readWrite_
    , romFromVec, romFromFile
    , ram0, ramFromFile
    , port, port_
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

type RAM dom addr dat = Signal dom addr -> Signal dom (Maybe (addr, dat)) -> Signal dom dat
type ROM dom addr dat = Signal dom addr ->                                   Signal dom dat
type Port dom addr dat a = Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a)
type Port_ dom addr dat = Signal dom (Maybe (PortCommand addr dat)) -> Signal dom (Maybe dat)

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

class Backpane a where
    backpane :: a -> ExpQ

instance Backpane () where
    backpane () = [|()|]

instance (Backpane a1, Backpane a2) => Backpane (a1, a2) where
    backpane (x1, x2) = [| ($(backpane x1), $(backpane x2)) |]

instance (Backpane a1, Backpane a2, Backpane a3) => Backpane (a1, a2, a3) where
    backpane (x1, x2, x3) = [| ($(backpane x1), $(backpane x2), $(backpane x3)) |]

data Result = Result ExpQ

instance Backpane Result where
    backpane (Result e) = e

compile
    :: forall addr a b. (Backpane a)
    => Addressing addr a
    -> Addr
    -> Dat
    -> Component
compile addressing addr wr = do
    (x, (decs, conns, rds)) <-
        runWriterT $ runReaderT (runAddressing addressing) ([| Just <$> $addr |], wr)

    let compAddrs = [ [d| $(varP nm) = muxA $(listE addrs) |]
                    | (nm, addrs) <- Map.toList conns
                    ]
    decs <- mconcat (decs:compAddrs)
    letE (pure <$> decs) [| (muxA $(listE rds) .<| 0, $(backpane x)) |]

memoryMap
    :: forall addr a. (Backpane a)
    => Addr
    -> Dat
    -> Addressing addr a
    -> Component
memoryMap addr wr addressing =
    [| let addr' = $addr; wr' = $wr
        in $(compile addressing [| addr' |] [| wr' |])
    |]

memoryMap_
    :: forall addr dat. ()
    => Addr
    -> Dat
    -> Addressing addr ()
    -> Dat
memoryMap_ addr wr addressing = [| fst $(memoryMap addr wr addressing) |]

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

readWrite
    :: (Addr -> Dat -> Component)
    -> Addressing addr (Handle addr', Result)
readWrite component = Addressing $ do
    rd <- lift . lift $ newName "rd"
    addr <- lift . lift $ newName "compAddr"
    result <- lift . lift $ newName "result"
    (_, wr) <- ask
    let decs = [d| ($(varP rd), $(varP result)) = $(component (varE addr) wr) |]
    tell (decs, Map.singleton addr mempty, mempty)
    return (Handle rd addr, Result (varE result))

readWrite_
    :: (Addr -> Dat -> Dat)
    -> Addressing addr (Handle addr')
readWrite_ component = fmap fst $ readWrite $ \addr wr -> [| ($(component addr wr), ()) |]

conduit
    :: forall addr' addr. ()
    => ExpQ
    -> Addressing addr (Handle addr', Result, Result)
conduit rdExt = do
    (h, Result x) <- readWrite $ \addr wr -> [| ($rdExt, ($addr, $wr)) |]
    return (h, Result [| fst $x |], Result [| snd $x |])

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ {-(Vec n dat)-}
    -> Addressing addr (Handle (Index n))
romFromVec size xs = readWrite_ $ \addr _wr ->
    [| rom $xs (bitCoerce . fromJustX <$> $addr) |]

romFromFile
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing addr (Handle (Index n))
romFromFile size fileName = readWrite_ $ \addr _wr ->
    [| fmap unpack $ romFilePow2 $fileName (bitCoerce . fromJustX <$> $addr) |]

ram0
    :: (1 <= n)
    => SNat n
    -> Addressing addr (Handle (Index n))
ram0 size = readWrite_ $ \addr wr ->
    [| blockRam1 NoClearOnReset size 0 (fromJustX <$> $addr) (liftA2 (,) <$> $addr <*> $wr) |]

ramFromFile
    :: SNat n
    -> ExpQ {- FilePath -}
    -> Addressing addr (Handle (Index n))
ramFromFile size fileName = readWrite_ $ \addr wr ->
    [| packRam (blockRamFile size $fileName)
           (fromJustX <$> $addr)
           (liftA2 (,) <$> $addr <*> $wr)
    |]

port
    :: forall addr' a addr. ()
    => ExpQ
    -> Addressing addr (Handle addr', Result)
port mkPort = readWrite $ \addr wr ->
  [| let (read, x) = $mkPort $ portFromAddr $addr $wr
     in (delay Nothing read, x) |]

port_
    :: forall addr' addr. ()
    => ExpQ
    -> Addressing addr (Handle addr')
port_ mkPort = readWrite_ $ \addr wr ->
  [| let read = $mkPort $ portFromAddr $addr $wr in delay Nothing read |]

from
    :: forall addr' addr a. (Typeable addr', Lift addr)
    => (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> Addressing addr' a
    -> Addressing addr a
from base = matchAddr [| from' @($(liftTypeQ @addr')) base |]

tag
    :: (Lift addr')
    => addr'
    -> Addressing (addr', addr) a
    -> Addressing addr a
tag t = matchAddr [| \addr -> Just (t, addr) |]

matchLeft
    :: Addressing addr1 a
    -> Addressing (Either addr1 addr2) a
matchLeft = matchAddr [| either Just (const Nothing) |]

matchRight
    :: Addressing addr2 a
    -> Addressing (Either addr1 addr2) a
matchRight = matchAddr [| either (const Nothing) Just |]

from'
    :: forall addr' addr. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr -> addr -> Maybe addr'
from' base addr = do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= fromIntegral (maxBound :: addr')
    return (fromIntegral offset)
