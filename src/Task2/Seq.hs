{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.Seq where

import Common.Sequence ( Sequence(..) )
import Common.MonoidalTree ( MonoidalTree((|>), (<|)) )

import Task1 (Measured(..), Size(..))
import Task2.Tree ( Tree(..), leaf, branch )

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

  insertAt j e (Seq tr) = let idx = max 0 (min j (length (Seq tr))) in
    Seq (go idx (Elem e) tr)
    where
      go _ y Empty = leaf y
      go i y (Leaf x)
        | i == 0    = branch (leaf y) (leaf x)
        | otherwise = branch (leaf x) (leaf y)
      go i y (Branch _ l r)
        | i <= getLength l = branch (go i y l) r
        | otherwise         = branch l (go (i - getLength l) y r)

  removeAt j (Seq tr)
    | 0 <= j && j < getLength tr = Seq (go j tr)
    | otherwise               = Seq tr
    where
      go _ Empty    = Empty
      go _ (Leaf _) = Empty
      go i (Branch _ l r)
        | i < getLength l = case go i l of 
          Empty -> r
          l'    -> branch l' r
        | otherwise = case go (i - getLength l) r of 
          Empty -> l
          r'    -> branch l r'
    
  elemAt i (Seq t) = case t of
    Empty -> Nothing
    Leaf x
       | i == 0    -> Just $ getElem x
       | otherwise -> Nothing
    Branch _ l r
      | i < getLength l -> elemAt i $ Seq l
      | otherwise     -> elemAt (i - getLength l) $ Seq r

getLength :: forall a. Tree (Size a) (Elem a) -> Int
getLength t = getSize (measure t :: Size a)