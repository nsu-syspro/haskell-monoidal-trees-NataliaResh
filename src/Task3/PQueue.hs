{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.PQueue where

import Common.PriorityQueue ( PriorityQueue(..) )
import Common.MonoidalTree ( MonoidalTree((|>)) )

import Task1 ( MinMax(getMinMax), Max, Min, Measured(..) )
import Task3.Tree
    ( Tree(..),
      node2,
      node3,
      RemoveResult(Hole, Balanced),
      balance2Left,
      balance2Right,
      balance3Left,
      balance3Middle,
      balance3Right )

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
extract minMaxKey t = case go t of
    Nothing      -> Nothing
    Just (v, Balanced t') -> Just (v, PQueue t')
    Just (v, Hole t') -> Just (v, PQueue t')
  where
    go Empty                 = Nothing
    go (Leaf (Entry (_, v))) = Just (v, Hole Empty)
    go (Node2 _ l r)
        | minMaxLeft l r = case go l of
          Nothing -> Nothing
          Just (v, Balanced l') -> Just (v, Balanced $ node2 l' r)
          Just (v, Hole l') -> Just (v, balance2Left l' r)
        | otherwise      = case go r of 
          Nothing -> Nothing
          Just (v, Balanced r') -> Just (v, Balanced $ node2 l r')
          Just (v, Hole r') -> Just (v, balance2Right l r')
    go (Node3 _ l m r)
        | minMaxLeft3 l m r  = case go l of
          Nothing -> Nothing
          Just (v, Balanced l') -> Just (v, Balanced $ node3 l' m r)
          Just (v, Hole l') -> Just (v, balance3Left l' m r)
        | minMaxRight3 l m r = case go r of
          Nothing -> Nothing
          Just (v, Balanced r') -> Just (v, Balanced $ node3 l m r')
          Just (v, Hole r') -> Just (v, balance3Right l m r')
        | otherwise          = case go m of 
          Nothing -> Nothing
          Just (v, Balanced m') -> Just (v, Balanced $ node3 l m' r)
          Just (v, Hole m') -> Just (v, balance3Middle l m' r)

    minMaxLeft l r = let
      k1 = minMaxKey l
      k2 = minMaxKey r
      in  k1 <> k2 == k1

    minMaxLeft3 l m r = let
      k1 = minMaxKey l
      k2 = minMaxKey m
      k3 = minMaxKey r
      in  k1 <> k2 <> k3 == k1
    
    minMaxRight3 l m r = let
      k1 = minMaxKey l
      k2 = minMaxKey m
      k3 = minMaxKey r
      in  k1 <> k2 <> k3 == k3
