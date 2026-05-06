{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Seq where

import Common.Sequence ( Sequence(..) )
import Common.MonoidalTree ( MonoidalTree((|>), (<|)) )

import Task1 (Measured(..), Size(..))
import Task3.Tree
    ( InsertResult(Split, Done),
      Tree(..),
      leaf,
      node2,
      node3,
      RemoveResult(Hole, Balanced),
      balance2Left,
      balance2Right,
      balance3Left,
      balance3Middle,
      balance3Right )

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
    case go idx (Elem e) tr of
      Done t' -> Seq t'
      Split l r -> Seq $ node2 l r
    where
      go _ y Empty = Done $ leaf y
      go i y (Leaf x)
        | i == 0    = Done $ node2 (leaf y) (leaf x)
        | otherwise = Done $ node2 (leaf x) (leaf y)
      go i y (Node2 _ l r)
        | i <= getLength l = case go i y l of
          Done l'     -> Done $ node2 l' r
          Split l' r' -> Done $ node3 l' r' r
        | otherwise         = case go (i - getLength l) y r of
          Done r'     -> Done $ node2 l r'
          Split l' r' -> Done $ node3 l l' r'
      go i y (Node3 _ l m r)
        | i <= getLength l               = case go i y l of
          Done l'     -> Done $ node3 l' m r
          Split l' r' -> Split (node2 l' r') (node2 m r)
        | i <= getLength l + getLength m = case go (i - getLength l) y m of
          Done m'     -> Done $ node3 l m' r
          Split l' r' -> Split (node2 l l') (node2 r' r)
        | otherwise                     = case go (i - (getLength l + getLength m)) y r of
          Done r'     -> Done $ node3 l m r'
          Split l' r' -> Split (node2 l m) (node2 l' r')

  removeAt j (Seq tr)
    | 0 <= j && j < getLength tr = case go j tr of
      Balanced t -> Seq t
      Hole t -> Seq t
    | otherwise               = Seq tr
    where
      go _ Empty    = Balanced Empty
      go _ (Leaf _) = Hole Empty
      go i (Node2 _ l r)
        | i < getLength l = case go i l of
          Balanced l' -> Balanced $ node2 l' r
          Hole l'     -> balance2Left l' r
        | otherwise      = case go (i - getLength l) r of 
          Balanced r' -> Balanced $ node2 l r'
          Hole r'     -> balance2Right l r'
      go i (Node3 _ l m r)
        | i < getLength l               = case go i l of
          Balanced l' -> Balanced $ node3 l' m r
          Hole l'     -> balance3Left l' m r
        | i < getLength l + getLength m = case go (i - getLength l) m of
          Balanced m' -> Balanced $ node3 l m' r
          Hole m'     -> balance3Middle l m' r
        | otherwise                     = case go (i - (getLength l + getLength m)) r of
          Balanced r' -> Balanced $ node3 l m r'
          Hole r'     -> balance3Right l m r'

  elemAt i (Seq t) = case t of
    Empty -> Nothing
    Leaf x
       | i == 0    -> Just $ getElem x
       | otherwise -> Nothing
    Node2 _ l r
      | i < getLength l -> elemAt i $ Seq l
      | otherwise       -> elemAt (i - getLength l) $ Seq r
    Node3 _ l m r
      | i < getLength l               -> elemAt i $ Seq l
      | i < getLength l + getLength m -> elemAt (i - getLength l) $ Seq m
      | otherwise                     -> elemAt (i - (getLength l + getLength m)) $ Seq r

getLength :: forall a. Tree (Size a) (Elem a) -> Int
getLength t = getSize (measure t :: Size a)
