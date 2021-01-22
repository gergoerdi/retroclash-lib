{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module RetroClash.Memory
    ( memoryMap, memoryMap_

    , conduit, readWrite, readWrite_
    , romFromVec, romFromFile
    , ram0, ramFromFile
    , port, port_
    , connect

    , override

    , from
    , matchLeft, matchRight
    , tag
    ) where

import Clash.Prelude hiding (Exp, lift)
import RetroClash.Utils
import RetroClash.Port
import Control.Arrow (first)
import Data.Maybe
import Control.Monad
import Control.Monad.RWS
import Data.Kind

import Data.Map as Map
import Data.List as L
import Data.Function (on)

import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH.Syntax as TH

data Handle s addr = Handle Name

type FanIn dom dat = Signal dom `Ap` First (Maybe dat)

-- | (muxAddr :: Signal dom (Maybe addr)) -> (rd :: FanIn dom dat, x :: a)
type Component = Exp -> DecsQ

-- | (matcher :: Signal dom (Maybe addr) -> Signal dom (Maybe addr'))
type Matcher = ExpQ

-- | (addr :: Signal dom (Maybe addr))
type Connection = Name

-- | (out :: FanIn dom dat)
type Out = ExpQ

newtype Addressing (s :: Type) (addr :: Type) (a :: Type) = Addressing
    { runAddressing :: RWST
          (Exp, Matcher, Name)                                                  -- (wr, matcher, addr)
          ([(Name, Component)], [(Name, Matcher)], [(Name, Connection)], [Out]) -- (components, matcher binds, connections, outs)
          ()
          Q
          a
    }
    deriving newtype (Functor, Applicative, Monad)

listMap :: (Ord k) => [(k, v)] -> Map.Map k [v]
listMap =
    Map.fromList .
    fmap (\ kvs@((k, _):_) -> (k, fmap snd kvs)) .
    groupBy ((==) `on` fst) .
    sortBy (compare `on` fst)

class Backpane a where
    backpane :: a -> ExpQ

instance (Backpane a1, Backpane a2) => Backpane (a1, a2) where
    backpane (x1, x2) = [| ($(backpane x1), $(backpane x2)) |]

instance (Backpane a1, Backpane a2, Backpane a3) => Backpane (a1, a2, a3) where
    backpane (x1, x2, x3) = [| ($(backpane x1), $(backpane x2), $(backpane x3)) |]

instance Backpane () where
    backpane () = [|()|]

data Result = Result Name
    deriving Show

instance Backpane Result where
    backpane (Result nm) = varE nm

data Addr = Addr Name
    deriving Show

instance Backpane Addr where
    backpane (Addr nm) = varE nm

data WR = WR Name
    deriving Show

instance Backpane WR where
    backpane (WR nm) = varE nm

compile
    :: forall addr dom a. (Backpane a)
    => (forall s. Addressing s addr a)
    -> Q (ExpQ -> ExpQ, [Dec], ExpQ, ExpQ)
compile addressing = do
    addr <- newName "addr"
    wr <- newName "wr"
    (x, (coms, mbs, listMap -> conns, outs)) <- evalRWST (runAddressing addressing) (VarE wr, [|id|], addr) ()

    muxs <- forM conns $ \ addrs -> do
        mux <- newName "muxAddr"
        return (mux, [d| $(varP mux) = muxA $(listE $ varE <$> addrs) |])
    muxDecs <- fmap L.concat $ mapM snd $ Map.elems muxs
    comDecs <- fmap L.concat $ forM coms $ \(nm, mkCom) -> do
        mux <- case Map.lookup nm muxs of
            Just (mux, _) -> varE mux
            Nothing -> [| pure Nothing |]
        mkCom mux
    mbDecs <- fmap mconcat $ mapM (\(nm, matcher) -> [d| $(varP nm) = $matcher $(varE addr) |]) mbs

    let wrapper body = [| \ $(varP addr) $(varP wr) -> $body |]
        out = [| mconcat $(listE outs) |]

        decs = mconcat [muxDecs, comDecs, mbDecs]

    return (wrapper, decs, [| fmap (join . getFirst) (getAp $out) |], backpane x)

memoryMap :: forall addr a. Backpane a => ExpQ -> ExpQ -> (forall s. Addressing s addr a) -> ExpQ
memoryMap addr wr addressing = do
    (wrapper, decs, rd, x) <- compile addressing
    [| $(wrapper $ LetE decs <$> [| ($rd, $x) |]) $addr $wr |]

memoryMap_ :: forall addr. ExpQ -> ExpQ -> (forall s. Addressing s addr ()) -> ExpQ
memoryMap_ addr wr addressing = do
    (wrapper, decs, rd, _) <- compile addressing
    [| $(wrapper $ LetE decs <$> rd) $addr $wr |]

-- packRam :: (BitPack dat) => RAM dom addr (BitVector (BitSize dat)) -> RAM dom addr dat
-- packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

matchAddr
    :: ExpQ
    -> Addressing s addr' a
    -> Addressing s addr a
matchAddr match body = Addressing $ do
    addr' <- lift $ newName "addr"
    censor (\(coms, matchers, conns, outs) -> (coms, matchers, conns, [applyMask addr' outs])) $
        RWST $ \(wr, matcher, addr) s -> do
            let matcher' = restrict matcher
            runRWST (tell (mempty, [(addr', matcher')], mempty, mempty) >> runAddressing body) (wr, matcher', addr') s
  where
    restrict matcher = [| fmap ((=<<) $match) . $matcher |]
    applyMask addr' outs = [| mask (delay False $ isJust <$> $(varE addr')) $ mconcat $(listE outs) |]

readWrite
    :: forall addr' s addr. ()
    => (Exp -> Exp -> ExpQ)
    -> Addressing s addr (Handle s addr', Result)
readWrite component = Addressing $ do
    h@(Handle rd) <- Handle <$> (lift $ newName "rd")
    x <- lift $ newName "x"
    (wr, _, _) <- ask
    let comp = \muxAddr -> [d| ($(varP rd), $(varP x)) = first strong $(component muxAddr wr) |]
    tell ([(rd, comp)], mempty, mempty, mempty)
    return (h, Result x)

conduit
    :: forall addr' s addr. ()
    => ExpQ
    -> Addressing s addr (Handle s addr', Addr, WR)
conduit mkConduit = Addressing $ do
    h@(Handle rd) <- Handle <$> (lift $ newName "rd")
    addr' <- lift $ newName "conduitAddr"
    wr' <- lift $ newName "conduitWrite"
    (wr, _, _) <- ask
    let comp = \muxAddr ->
          [d|
             $(varP rd) = strong $mkConduit
             $(varP addr') = $(pure muxAddr)
             $(varP wr') = $(pure wr)
          |]
    tell ([(rd, comp)], mempty, mempty, mempty)
    return (h, Addr addr', WR wr')

readWrite_
    :: forall addr' s addr. ()
    => (Exp -> Exp -> ExpQ)
    -> Addressing s addr (Handle s addr')
readWrite_ component = fmap fst $ readWrite $ \addr wr -> [| ($(component addr wr), ()) |]

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing s addr (Handle s (Index n))
romFromVec size@SNat xs = readWrite_ $ \(pure -> addr) _wr ->
    [| fmap Just $ rom $xs (maybe 0 bitCoerce <$> $addr) |]

romFromFile
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing s addr (Handle s (Index n))
romFromFile size@SNat fileName = readWrite_ $ \(pure -> addr) _wr ->
    [| fmap (Just . unpack) $ romFilePow2 $fileName (maybe 0 bitCoerce <$> $addr) |]

ram0
    :: (1 <= n)
    => SNat n
    -> Addressing s addr (Handle s (Index n))
ram0 size@SNat = readWrite_ $ \(pure -> addr) (pure -> wr) ->
    [| fmap Just $ blockRam1 ClearOnReset $(TH.lift size) 0 (fromMaybe 0 <$> $addr) (liftA2 (,) <$> $addr <*> $wr) |]

ramFromFile
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing s addr (Handle s (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \(pure -> addr) (pure -> wr) ->
    [| fmap (Just . unpack) $ blockRamFile size $fileName (fromMaybe 0 <$> $addr) (liftA2 (,) <$> $addr <*> (fmap pack <$> $wr)) |]

port
    :: forall addr' a s addr. ()
    => ExpQ
    -> Addressing s addr (Handle s addr', Result)
port mkPort = readWrite $ \(pure -> addr) (pure -> wr) ->
  [| let (read, x) = $mkPort $ portFromAddr $addr $wr
     in (delay Nothing read, x) |]

port_
    :: forall addr' s addr. ()
    => ExpQ
    -> Addressing s addr (Handle s addr')
port_ mkPort = readWrite_ $ \(pure -> addr) (pure -> wr) ->
  [| let read = $mkPort $ portFromAddr $addr $wr in delay Nothing read |]

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

override
    :: ExpQ
    -> Addressing s addr ()
override sig = Addressing $ do
    tell (mempty, mempty, mempty, [ [| weak $sig |] ])

from
    :: forall addr' s dom dat addr a. (Integral addr, Ord addr, Integral addr', Bounded addr', Lift addr, Lift addr')
    => addr
    -> Addressing s addr' a
    -> Addressing s addr a
from base = matchAddr [| from_ $(TH.lift (base :: addr)) $(TH.lift (maxBound :: addr')) |]

connect
    :: Handle s addr
    -> Addressing s addr ()
connect (Handle comp) = Addressing $ do
    (_, _, addr) <- ask
    tell (mempty, mempty, [(comp, addr)], [varE comp])

from_ :: forall addr' addr. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr -> addr' -> addr -> Maybe addr'
from_ base lim addr = do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= fromIntegral lim
    return (fromIntegral offset)

strong :: (Functor f) => f a -> f `Ap` First a
strong = Ap . fmap pure

weak :: (Functor f) => f (Maybe a) -> f `Ap` First (Maybe a)
weak = Ap . fmap (maybe mempty (pure . Just))

mask :: (Applicative f) => f Bool -> f `Ap` First a -> f `Ap` First a
mask p x = Ap $ mux p (getAp x) (pure mempty)
