module RetroClash.Slow
    ( Slow(..)
    , getSlow
    , putSlow
    ) where

import Clash.Prelude

import Control.Monad.State
import Data.Word

data Slow a = Slow Word32 a
    deriving (Show, Eq, Generic, NFDataX)

getSlow :: (MonadState (Slow a) m) => Word32 -> m (m () -> m (), a)
getSlow duration = do
    Slow cnt s <- get
    if cnt >= duration then return (\k -> k, s) else do
        put $ Slow (cnt + 1) s
        return (\k -> return (), s)

putSlow :: (MonadState (Slow a) m) => a -> m ()
putSlow = put . Slow 0
