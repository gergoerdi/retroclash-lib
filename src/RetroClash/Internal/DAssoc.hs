{-# LANGUAGE RankNTypes #-}
module RetroClash.Internal.DAssoc where

import Data.Kind
import Data.Dependent.Sum as DSum
import Data.GADT.Compare
import Type.Reflection

import Clash.Prelude hiding (lookup, fold)

newtype DMap k f = MkDMap { dassocs :: [DSum k f] }

instance (GCompare k) => Semigroup (DMap k f) where
    (<>) = union

instance (GCompare k) => Monoid (DMap k f) where
    mempty = MkDMap []

unionWithKey :: (GCompare k) => (forall a. k a -> f a -> f a -> f a) -> DMap k f -> DMap k f -> DMap k f
unionWithKey f l r = go l (dassocs r)
  where
    go l [] = l
    go l ((k :=> v) : xs) = go (insertWithKey f k v l) xs

union :: (GCompare k) => DMap k f -> DMap k f -> DMap k f
union = unionWithKey $ \ _ old new -> old

insertWithKey :: (GCompare k) => (k a -> f a -> f a -> f a) -> k a -> f a -> DMap k f -> DMap k f
insertWithKey f k0 v0 = MkDMap . go . dassocs
  where
    go [] = [k0 :=> v0]
    go ((k1 :=> v1) : xs) = case k0 `gcompare` k1 of
        GLT -> (k0 :=> v0) : (k1 :=> v1) : xs
        GEQ -> (k0 :=> f k0 v1 v0) : xs
        GGT -> (k1 :=> v1) : go xs

lookup :: (GCompare k) => k a -> DMap k f -> Maybe (f a)
lookup k0 = go . dassocs
  where
    go [] = Nothing
    go ((k :=> v) : xs) = case geq k0 k of
        Just Refl -> Just v
        Nothing -> go xs

singleton :: k a -> f a -> DMap k f
singleton k v = MkDMap [k :=> v]
