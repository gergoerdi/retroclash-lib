{-# LANGUAGE MultiParamTypeClasses #-}
module RetroClash.Internal.RWS
    ( RWS
    , rws, runRWS, evalRWS
    , module Control.Monad.Reader.Class
    , module Control.Monad.Writer.Class
    , module Control.Monad.State.Class
    ) where

import Prelude

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class

newtype RWS r w s a = RWS{ runRWS :: r -> s -> (a, s, w) }

rws :: (r -> s -> (a, s, w)) -> RWS r w s a
rws = RWS

evalRWS :: RWS r w s a -> r -> s -> (a, w)
evalRWS act r s = let (x, _s', w) = runRWS act r s in (x, w)

instance Functor (RWS r w s) where
    fmap f act = RWS $ \r s ->
      let (x, s', w) = runRWS act r s
      in (f x, s', w)

instance (Monoid w) => Applicative (RWS r w s) where
    pure x = RWS $ \r s -> (x, s, mempty)
    ff <*> fx = RWS $ \r s ->
      let (f, s', w1) = runRWS ff r s
          (x, s'', w2) = runRWS fx r s'
      in (f x, s'', w1 <> w2)

instance (Monoid w) => Monad (RWS r w s) where
    m >>= k = RWS $ \r s ->
      let (x, s', w1) = runRWS m r s
          (y, s'', w2) = runRWS (k x) r s'
      in (y, s'', w1 <> w2)

instance (Monoid w) => MonadReader r (RWS r w s) where
    ask = RWS $ \r s -> (r, s, mempty)
    local f act = RWS $ \r s -> runRWS act (f r) s

instance (Monoid w) => MonadState s (RWS r w s) where
    get = RWS $ \r s -> (s, s, mempty)
    put s = RWS $ \r _ -> ((), s, mempty)

instance (Monoid w) => MonadWriter w (RWS r w s) where
    tell w = RWS $ \r s -> ((), s, w)

    listen act = RWS $ \r s ->
      let (x, s', w) = runRWS act r s
      in ((x, w), s', w)

    pass act = RWS $ \r s ->
      let ((x, f), s', w) = runRWS act r s
      in (x, s', f w)
