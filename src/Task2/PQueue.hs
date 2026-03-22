{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.PQueue where

import Common.PriorityQueue ( PriorityQueue(..) )
import Common.MonoidalTree ( MonoidalTree((|>)) )

import Task1 (Measured(..), MinMax(..), Min (..), Max(..))
import Task2.Tree ( Tree(..), branch )

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v) }
  deriving (Show, Eq)

instance Ord k => Measured (MinMax k) (Entry k v) where
  measure = measure . fst . getEntry

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty

  toPriorityQueue = foldl (\pq (k, v) -> insert k v pq) empty

  entries = foldMap (\(Entry (k, v)) -> [(k, v)]) . getTree

  insert k v (PQueue t) = PQueue (t |> Entry (k, v))

  extractMin = extract minKey . getTree
    where
      minKey :: Ord k => Tree (MinMax k) (Entry k v) -> Min k
      minKey = fst . getMinMax . measure
      
  extractMax = extract maxKey . getTree
    where
      maxKey :: Ord k => Tree (MinMax k) (Entry k v) -> Max k
      maxKey = snd . getMinMax . measure

extract :: (Ord k, Eq b, Monoid b) 
        => (Tree (MinMax k) (Entry k v) -> b)
        -> Tree (MinMax k) (Entry k v)
        -> Maybe (v, PQueue k v)
extract minMaxKey t = case go minMaxKey t of
    Nothing      -> Nothing
    Just (v, t') -> Just (v, PQueue t')
  where
    go _ Empty                 = Nothing
    go _ (Leaf (Entry (_, v))) = Just (v, Empty)
    go f (Branch _ l r)        = case (f l, f r) of
      (k1, k2)
        | k1 == mempty && k2 == mempty     -> Nothing
        | k2 == mempty || (k1 <> k2) == k1 -> fromSide (`branch` r) l r
        | otherwise                        -> fromSide (branch l) r l
      where
        fromSide createBranch l' r' = case go f l' of
          Nothing         -> Nothing
          Just (v, Empty) -> Just (v, r')
          Just (v, l'')   -> Just (v, createBranch l'')
