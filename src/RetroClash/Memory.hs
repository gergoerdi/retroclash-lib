{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module RetroClash.Memory
    ( memoryMap_

    , romFromVec
    , ramFromFile
    , connect

    , from
    ) where

import Clash.Prelude hiding (Exp, lift)
import RetroClash.Utils
import RetroClash.Port
import Data.Maybe
import Control.Monad
import Control.Monad.RWS
import Data.Kind

import Data.Map as Map
import Data.List as L
import Data.Function (on)

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type, lift)
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

newtype Addressing (s :: Type) (dom :: Domain) (addr0 :: Type) (addr :: Type) (a :: Type) = Addressing
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

compile
    :: forall addr dom a. ()
    => (forall s. Addressing s dom addr addr ())
    -> Q (ExpQ -> ExpQ, [Dec], ExpQ)
compile addressing = do
    addr <- newName "addr"
    wr <- newName "wr"
    ((), (coms, mbs, listMap -> conns, outs)) <- evalRWST (runAddressing addressing) (VarE wr, [|id|], addr) ()

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

    return (wrapper, decs, [| fmap (fromMaybe (Just 0) . getFirst) (getAp $out) |])

memoryMap_ :: forall addr dom. ExpQ -> ExpQ -> (forall s. Addressing s dom addr addr ()) -> ExpQ
memoryMap_ addr wr addressing = do
    (wrapper, decs, rd) <- compile addressing
    [| $(wrapper $ LetE decs <$> rd) $addr $wr |]

matchAddr
    :: forall addr' addr a s dom addr0. TExpQ (addr -> Maybe addr')
    -> Addressing s dom addr0 addr' a
    -> Addressing s dom addr0 addr a
matchAddr match body = Addressing $ do
    addr' <- lift $ newName "addr"
    censor (\(coms, matchers, conns, outs) -> (coms, matchers, conns, [applyMask addr' outs])) $
        RWST $ \(wr, matcher, addr) s -> do
            let matcher' = restrict (unsafeTExpCoerce matcher)
            runRWST (tell (mempty, [(addr', unTypeQ matcher')], mempty, mempty) >> runAddressing body) (wr, unTypeQ matcher', addr') s
  where
    restrict :: TExpQ (Signal dom addr0 -> Signal dom (Maybe addr)) -> TExpQ (Signal dom addr0 -> Signal dom (Maybe addr'))

    restrict matcher = [|| fmap ((=<<) $$match) . $$matcher ||]
    applyMask addr' outs = [| mask (delay False $ isJust <$> $(varE addr')) $ mconcat $(listE outs) |]

readWrite_
    :: forall addr' addr s dom addr0. ()
    => (Exp -> Exp -> ExpQ)
    -> Addressing s dom addr0 addr (Handle s addr')
readWrite_ component = Addressing $ do
    h@(Handle rd) <- Handle <$> (lift $ newName "rd")
    (wr, _, _) <- ask
    let comp = \muxAddr -> [d| $(varP rd) = strong $(component muxAddr wr) |]
    tell ([(rd, comp)], mempty, mempty, mempty)
    return h

romFromVec
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing s dom addr0 addr (Handle s (Index n))
romFromVec size@SNat xs = readWrite_ $ \(pure -> addr) _wr ->
    [| fmap Just $ rom $xs (maybe 0 bitCoerce <$> $addr) |]

ramFromFile
    :: (1 <= n)
    => SNat n
    -> ExpQ
    -> Addressing s dom addr0 addr (Handle s (Index n))
ramFromFile size@SNat fileName = readWrite_ $ \(pure -> addr) (pure -> wr) ->
    [| fmap (Just . unpack) $
       singlePort (blockRamFile size $fileName)
         (fromMaybe 0 <$> $addr)
         (fmap pack <$> $wr)
     |]

from
    :: forall addr' s dom addr0 addr a. (Integral addr, Ord addr, Integral addr', Bounded addr', Lift addr, Lift addr')
    => addr
    -> Addressing s dom addr0 addr' a
    -> Addressing s dom addr0 addr a
from base = matchAddr [|| from_ $$(liftTyped (base :: addr)) $$(liftTyped (maxBound :: addr')) ||]

connect
    :: Handle s addr
    -> Addressing s dom addr0 addr ()
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
