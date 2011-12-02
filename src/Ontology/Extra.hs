-- |Random add-on functions to various collection packages.
{-# LANGUAGE PackageImports, RankNTypes, ScopedTypeVariables #-}
module Ontology.Extra
    ( ixSetUnions
    , partitionM
    , minBy
    , maxBy
    ) where

import "mtl" Control.Monad.State
import Data.Data (Data)
import Data.IxSet (Indexable, IxSet, union, empty)
-- import Ontology.Graph ()

-- |Belongs in Data.IxSet
ixSetUnions :: (Data a, Ord a, Indexable a) => [IxSet a] -> IxSet a
ixSetUnions = foldr union empty

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs =
    foldM f ([], []) xs
    where f (ts, fs) x = p x >>= \ flag -> return $ if flag then (x : ts, fs) else (ts, x : fs)

minBy :: Ord a => (a -> a -> Ordering) -> a -> a -> a
minBy cmp a b =
    case cmp a b of
      LT -> a
      _ -> b

maxBy :: Ord a => (a -> a -> Ordering) -> a -> a -> a
maxBy cmp a b =
    case cmp a b of
      GT -> a
      _ -> b
