{-# LANGUAGE LambdaCase, ApplicativeDo #-}
module RetroClash.Port
    ( PortCommand(..)
    , portFromAddr
    , maskAddr
    , basedAt
    ) where

import Clash.Prelude

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Control.Monad

data PortCommand port a
    = ReadPort port
    | WritePort port a
    deriving (Generic, NFDataX, Show)

instance Functor (PortCommand port) where
    {-# INLINE fmap #-}
    fmap f = \case
        ReadPort port -> ReadPort port
        WritePort port val -> WritePort port (f val)

instance Bifunctor PortCommand where
    {-# INLINE bimap #-}
    bimap f g = \case
        ReadPort port -> ReadPort (f port)
        WritePort port val -> WritePort (f port) (g val)

    {-# INLINE second #-}
    second = fmap

instance Bifoldable PortCommand where
    {-# INLINE bifoldMap #-}
    bifoldMap f g = \case
        ReadPort port -> f port
        WritePort port val -> f port <> g val

instance Bitraversable PortCommand where
    {-# INLINE bitraverse #-}
    bitraverse f g = \case
        ReadPort port -> ReadPort <$> f port
        WritePort port val -> WritePort <$> f port <*> g val

portFromAddr :: Signal dom (Maybe port) -> Signal dom (Maybe a) -> Signal dom (Maybe (PortCommand port a))
portFromAddr addr w = do
    addr <- addr
    w <- w
    pure $ case (addr, w) of
        (Just addr, Nothing) -> Just $ ReadPort addr
        (Just addr, Just w) -> Just $ WritePort addr w
        _ -> Nothing

maskAddr :: forall n k. (KnownNat n, KnownNat k) => Unsigned (n + k) -> Unsigned (n + k) -> Maybe (Unsigned k)
maskAddr base0 addr = do
    let (space, offset) = split addr
    guard $ space == base
    return offset
  where
    (base, _) = split base0

    split :: Unsigned (n + k) -> (Unsigned n, Unsigned k)
    split = bitCoerce

basedAt
    :: forall n k a. (KnownNat n, KnownNat k)
    => Unsigned (n + k)
    -> Maybe (PortCommand (Unsigned (n + k)) a)
    -> Maybe (PortCommand (Unsigned k) a)
basedAt base cmd = bitraverse (maskAddr base) pure =<< cmd
