{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.Tree where

import Common.MonoidalTree ( MonoidalTree(..) )

import Task1 (Measured(..))

-- * Binary tree definition

-- | Binary tree with values 'a' in leaves
-- Intermediate branches contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Branch m (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty          = mempty
  measure (Leaf x)       = measure x
  measure (Branch m _ _) = m 

instance Foldable (Tree m) where
  foldMap _ Empty          = mempty
  foldMap f (Leaf x)       = f x
  foldMap f (Branch _ l r) = (foldMap . foldMap) f [l, r]

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

branch :: Measured m a => Tree m a -> Tree m a -> Tree m a
branch l r = Branch (measure [l, r]) l r

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = foldr (<|) Empty
  x <| Empty        = leaf x
  x <| Leaf y       = branch (leaf x) (leaf y)
  x <| Branch _ l r = branch (x <| l) r
  
  Empty        |> x = leaf x
  Leaf y       |> x = branch (leaf y) (leaf x)
  Branch _ l r |> x = branch l (r |> x)