{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task4.PQueue where

import Common.PriorityQueue ( PriorityQueue(..) )
import Common.MonoidalTree ( MonoidalTree((|>)) )

import Task1 ( Measured(..), MinMax(getMinMax) )
import Task4.Tree ( (><), splitTree, Split(Split), Tree(Empty) )

newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v) }
  deriving (Show, Eq)

-- | Measures given entry using both minimum and maximum priority 'k'
instance Ord k => Measured (MinMax k) (Entry k v) where
  measure = measure . fst . getEntry

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldr (\(k, v) pq -> insert k v pq) empty
  entries = foldMap (\(Entry (k, v)) -> [(k, v)]) . getTree
  insert k v (PQueue t) = PQueue (t |> Entry (k, v))
  extractMin (PQueue q) = case splitTree isMin mempty q of
    Nothing                         -> Nothing
    Just (Split l (Entry (_, v)) r) -> Just (v, PQueue (l >< r))
    where
      minKey = (fst . getMinMax . measure) q
      isMin mm = fst (getMinMax mm) == minKey
  extractMax (PQueue q) = case splitTree isMax mempty q of
    Nothing                         -> Nothing
    Just (Split l (Entry (_, v)) r) -> Just (v, PQueue (l >< r))
    where
      maxKey = (snd . getMinMax . measure) q
      isMax mm = snd (getMinMax mm) == maxKey 
