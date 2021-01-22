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
import Control.Arrow (first, second)
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import Control.Monad.State

import Data.Map as Map
import Data.List as L
import Data.Function (on)

import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH.Syntax as TH

data Handle s addr = Handle Int

newtype Addressing s addr a = Addressing
    { runAddressing :: RWS
          (Exp)                                  -- wr
          ([(Exp -> ExpQ, Bool)], [(Int, ExpQ)])  -- (components, matchers)
          Int                                    -- component key supply
          a
    }
    deriving newtype (Functor, Applicative, Monad)

listMap :: (Ord k) => [(k, v)] -> Map.Map k [v]
listMap =
    Map.fromList .
    fmap (\ kvs@((k, _):_) -> (k, fmap snd kvs)) .
    groupBy ((==) `on` fst) .
    sortBy (compare `on` fst)

runMatchers :: [(Int, ExpQ)] -> Exp -> Q ([Dec], Map.Map Int [Exp])
runMatchers matchers addr0 = do
    (addrs, decs) <- fmap L.unzip $ flip evalStateT addr0 $ forM matchers $ \(i, matcher) -> do
        addr <- get
        nm <- lift $ newName "addrIn"
        nm' <- lift $ newName "addrOut"
        put $ VarE nm'
        decs <- lift
            [d|
             $(varP nm) = $matcher $(pure addr)
             $(varP nm') = mux (isJust <$> $(varE nm)) (pure Nothing) $(pure addr)
            |]
        return ((i, VarE nm), decs)

    return (mconcat decs, listMap addrs)

data Resolved = Resolved
    { outs :: Int -> ExpQ
    , addrs :: Int -> ExpQ
    , wrs :: Int -> ExpQ
    }

class Backpane a where
    backpane :: a -> Resolved -> ExpQ

instance (Backpane a1, Backpane a2) => Backpane (a1, a2) where
    backpane (x1, x2) env = [| ($(backpane x1 env), $(backpane x2 env)) |]

instance (Backpane a1, Backpane a2, Backpane a3) => Backpane (a1, a2, a3) where
    backpane (x1, x2, x3) env = [| ($(backpane x1 env), $(backpane x2 env), $(backpane x3 env)) |]

instance Backpane () where
    backpane _ _ = [|()|]

data Out = Out Int
    deriving Show

instance Backpane Out where
    backpane x@(Out i) = ($ i) . outs

data Addr = Addr Int
    deriving Show

instance Backpane Addr where
    backpane (Addr i) = ($ i) . addrs

data WR = WR Int
    deriving Show

instance Backpane WR where
    backpane (WR i) = ($ i) . wrs

compile
    :: forall addr dom a. Backpane a
    => (forall s. Addressing s addr a)
    -> Q (ExpQ -> ExpQ, [Dec], ExpQ, ExpQ)
compile addressing = do
    addr <- newName "addr"
    wr <- newName "wr"

    let (x, (components, matchers)) = evalRWS (runAddressing addressing) (VarE wr) 0

    (addrDecs, addrs) <- runMatchers matchers $ VarE addr
    muxs <- forM addrs $ \addrs -> do
        mux <- newName "muxAddr"
        return (mux, [d| $(varP mux) = muxA $(ListE <$> pure addrs) |])
    noMux <- [|pure Nothing|]

    (rdDecs, rds, backpaneVars) <- fmap L.unzip3 $ forM (L.zip [0..] components) $ \(i, (component, periph)) -> do
        rd <- newName "rd"
        comp <- component $ maybe (error "muxs") (VarE . fst) $ Map.lookup i muxs
        (dec, x) <- case periph of
            False -> do
                dec <- [d| $(varP rd) = $(pure comp) |]
                return (dec, Nothing)
            True -> do
                x <- newName "x"
                dec <- [d| ($(varP rd), $(varP x)) = $(pure comp) |]
                return (dec, Just $ VarE x)
        return (dec, VarE rd, x)

    muxDecs <- mconcat <$> mapM snd (Map.elems muxs)

    let decs = addrDecs <> mconcat rdDecs <> muxDecs
        addrs i = pure $ maybe noMux (VarE . fst) $ Map.lookup i muxs
        resolved = Resolved
            { outs =
                   let backpaneMap = Map.fromList $ L.zip [0..] backpaneVars
                   in \i -> maybe [|()|] pure . join $ Map.lookup i backpaneMap
            , addrs = addrs
            , wrs = \i -> let addr = addrs i in [| mux (isJust <$> $addr) $(varE wr) (pure Nothing) |]
            }
        wrapper body = [| \ $(varP addr) $(varP wr) -> $body |]
        rd = [| muxA $(ListE <$> pure rds) |]

    return (wrapper, decs, [| muxA $(ListE <$> pure rds) |], backpane x resolved)

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

mask :: (Exp -> ExpQ) -> (Exp -> ExpQ)
mask body addr = [|mux (delay True $ isNothing <$> $(pure addr)) (pure Nothing) $(body addr) |]

matchAddr
    :: ExpQ
    -> Addressing s addr' a
    -> Addressing s addr a
matchAddr match body = Addressing $ rws $ \wr s ->
    let (x, s', (components, matchers)) = runRWS (runAddressing body) wr s
    in (x, s', (components, fmap (fmap restrict) matchers))
  where
    restrict matcher = [| $matcher . fmap ($match =<<) |]

readWrite
    :: forall addr' s addr. ()
    => (Exp -> Exp -> ExpQ)
    -> Addressing s addr (Handle s addr', Out)
readWrite component = Addressing $ do
    h@(Handle i) <- Handle <$> get <* modify succ
    wr <- ask
    tell ([(mask $ \addr -> component addr wr, True)], mempty)
    return (h, Out i)

conduit
    :: forall addr' s addr. ()
    => ExpQ
    -> Addressing s addr (Handle s addr', Addr, WR)
conduit mkConduit = Addressing $ do
    h@(Handle i) <- Handle <$> get <* modify succ
    wr <- ask
    tell ([(mask $ \addr -> mkConduit, False)], mempty)
    return (h, Addr i, WR i)

readWrite_
    :: forall addr' s addr. ()
    => (Exp -> Exp -> ExpQ)
    -> Addressing s addr (Handle s addr')
readWrite_ component = Addressing $ do
    h <- Handle <$> get <* modify succ
    wr <- ask
    tell ([(mask $ \addr -> component addr wr, False)], mempty)
    return h


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
    -> Addressing s addr (Handle s addr', Out)
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
    -> Addressing s addr a
    -> Addressing s addr a
override sig body = Addressing $ do
    i <- get <* modify succ
    tell ([(\_addr -> sig, False)], [(i, [|\_addr -> $sig |])])
    runAddressing body

from
    :: forall addr' s dom dat addr a. (Integral addr, Ord addr, Integral addr', Bounded addr', Lift addr, Lift addr')
    => addr
    -> Addressing s addr' a
    -> Addressing s addr a
from base = matchAddr [| \addr -> do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= $lim
    let fromIntegral' = the $(TH.lift (fromIntegral 0 :: addr')) . fromIntegral
    return (fromIntegral' offset) |]
  where
    lim = TH.lift $ fromIntegral @addr' @addr maxBound

the :: a -> a -> a
the _ x = x

connect
    :: Handle s addr
    -> Addressing s addr ()
connect (Handle i) = Addressing $ do
    tell (mempty, [(i, [|id|])])
