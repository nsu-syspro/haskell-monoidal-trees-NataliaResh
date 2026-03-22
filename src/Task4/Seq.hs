{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task4.Seq where

import Common.Sequence ( Sequence(..) )
import Common.MonoidalTree ( MonoidalTree((|>), (<|)) )

import Task1 (Measured(..), Size(..))
import Task4.Tree
    ( (><), single, split, splitTree, Split(Split), Tree(Empty) )

-- * Sequence definition

-- | Random-access sequence based on binary tree
newtype Seq a = Seq { getTree :: Tree (Size a) (Elem a) }
  deriving (Show, Eq)

-- | Sequence element wrapper
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Eq)

-- | Measures given element as 'Size 1'
instance Measured (Size a) (Elem a) where
  measure _ = Size 1

instance Foldable Seq where
  foldMap f = foldMap (f . getElem) . getTree

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length (Seq t) = getSize (measure t :: Size a)

-- * Sequence instance

instance Sequence Seq where
  empty = Seq Empty
  toSequence = foldr (+|) empty
  x +| (Seq t) = Seq (Elem x <| t)
  Seq t |+ x = Seq (t |> Elem x)
  insertAt i x (Seq xs) = let 
    j = max 0 (min i (getLength xs))
    (l, r) = split ((j < ). getSize) xs
    in Seq (l >< single (Elem x) >< r)

  removeAt i (Seq xs)
    | 0 <= i && i < getLength xs = let
      (l, r)  = split ((i < ). getSize) xs
      (_, r') = split ((1 < ) . getSize) r
      in Seq (l >< r')
    | otherwise                 = Seq xs
  elemAt i (Seq xs)
    | 0 <= i && i < getLength xs = case splitTree ((i < ) . getSize) (Size 0) xs of
      Just (Split _ x _) -> Just (getElem x)
      Nothing            -> Nothing
    | otherwise                 = Nothing

getLength :: forall a. Tree (Size a) (Elem a) -> Int
getLength t = getSize (measure t :: Size a)