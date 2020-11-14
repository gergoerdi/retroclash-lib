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

getSlow :: (MonadState (Slow s) m) => Word32 -> m (m () -> m (), s)
getSlow duration = do
    Slow cnt s <- get
    wrapper <- if cnt >= duration then return (\k -> k) else do
        put $ Slow (cnt + 1) s
        return (\k -> return ())
    return (wrapper, s)

putSlow :: (MonadState (Slow s) m) => s -> m ()
putSlow = put . Slow 0
