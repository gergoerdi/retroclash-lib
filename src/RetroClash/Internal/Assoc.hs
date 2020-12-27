module RetroClash.Internal.Assoc where

import Clash.Prelude hiding (lookup, fold)

newtype Map k a = MkMap{ assocs :: [(k, a)] }
    deriving Show

instance (Ord k) => Semigroup (Map k a) where
    (<>) = union

instance (Ord k) => Monoid (Map k a) where
    mempty = MkMap []

unionWithKey :: (Ord k) => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f l r = fold (insertWithKey f) l (assocs r)

union :: (Ord k) => Map k a -> Map k a -> Map k a
union = unionWithKey (\ _ old new -> old)

fold :: (a -> b -> b) -> b -> [a] -> b
fold f = go
  where
    go z [] = z
    go z (x:xs) = go (f x z) xs

insertWithKey :: (Ord k) => (k -> a -> a -> a) -> (k,a) -> Map k a -> Map k a
insertWithKey f x0@(k0, v0) = MkMap . go . assocs
  where
    go [] = [x0]
    go (x1@(k1, v1) : xs) = case compare k0 k1 of
        LT -> x0 : x1 : xs
        EQ -> (k0, f k0 v1 v0) : xs
        GT -> x1 : go xs

lookup :: (Eq k) => k -> Map k a -> Maybe a
lookup k0 = go . assocs
  where
    go [] = Nothing
    go ((k1,v1):xs) = if k1 == k0 then Just v1 else go xs

singleton :: k -> a -> Map k a
singleton k a = MkMap [(k, a)]
