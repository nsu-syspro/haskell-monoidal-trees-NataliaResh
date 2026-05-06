{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Tree where

import Common.MonoidalTree ( MonoidalTree(..) )

import Task1 (Measured(..))

-- * 2-3 tree definition

-- | 2-3 tree with values 'a' in leaves
-- Intermediate nodes contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Node2 m (Tree m a) (Tree m a)
  | Node3 m (Tree m a) (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty           = mempty
  measure (Leaf x)        = measure x
  measure (Node2 m _ _)   = m
  measure (Node3 m _ _ _) = m

instance Foldable (Tree m) where
  foldMap _ Empty          = mempty
  foldMap f (Leaf x)       = f x
  foldMap f (Node2 _ l r) = (foldMap . foldMap) f [l, r]
  foldMap f (Node3 _ l m r) = (foldMap . foldMap) f [l, m, r]

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

node2 :: Measured m a => Tree m a -> Tree m a -> Tree m a
node2 l r = Node2 (measure [l, r]) l r

node3 :: Measured m a => Tree m a -> Tree m a -> Tree m a -> Tree m a
node3 l m r = Node3 (measure [l, m, r]) l m r

-- * Monoidal tree instance

data InsertResult m a =
  Done (Tree m a)
  | Split (Tree m a) (Tree m a)

data RemoveResult m a =
  Balanced (Tree m a)
  | Hole (Tree m a)

instance MonoidalTree Tree where
  toTree = foldr (<|) Empty
  
  x <| t = case insertL x t of
    Done t' -> t'
    Split l r -> node2 l r

  t |> x = case insertR x t of
    Done t' -> t'
    Split l r -> node2 l r

insertL :: Measured m a => a -> Tree m a -> InsertResult m a
insertL x Empty = Done $ leaf x
insertL x (Leaf y) = Split (leaf x) (leaf y)
insertL x (Node2 _ l r) = case insertL x l of
  (Done l') -> Done $ node2 l' r
  (Split l' r') -> Done $ node3 l' r' r
insertL x (Node3 _ l m r) = case insertL x l of
  (Done l') -> Done $ node3 l' m r
  (Split l' r') -> Split (node2 l' r') (node2 m r)

insertR :: Measured m a => a -> Tree m a -> InsertResult m a
insertR x Empty = Done $ leaf x
insertR x (Leaf y) = Split (leaf y) (leaf x)
insertR x (Node2 _ l r) = case insertR x r of
  (Done r') -> Done $ node2 l r'
  (Split l' r') -> Done $ node3 l l' r'
insertR x (Node3 _ l m r) = case insertR x r of
  (Done r') -> Done $ node3 l m r'
  (Split l' r') -> Split (node2 l m) (node2 l' r')

balance2Left :: Measured m a => Tree m a -> Tree m a -> RemoveResult m a
balance2Left l' r = case r of
  Empty               -> Hole l'
  Leaf x              -> Hole $ leaf x
  Node2 _ l'' r''     -> Hole $ node3 l' l'' r''
  Node3 _ l'' m'' r'' -> Balanced $ node2 (node2 l' l'') (node2 m'' r'')
      
balance2Right :: Measured m a => Tree m a -> Tree m a -> RemoveResult m a
balance2Right l r' = case l of
  Empty               -> Hole r'
  Leaf x              -> Hole $ leaf x
  Node2 _ l'' r''     -> Hole $ node3 l'' r'' r'
  Node3 _ l'' m'' r'' -> Balanced $ node2 (node2 l'' m'') (node2 r'' r')

balance3Left :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
balance3Left l' m r = case m of
  Empty               -> Balanced r
  Leaf x              -> Balanced $ node2 (leaf x) r
  Node2 _ l'' r''     -> Balanced $ node2 (node2 l' l'') (node2 r'' r)
  Node3 _ l'' m'' r'' -> Balanced $ node3 (node2 l' l'') (node2 m'' r'') r

balance3Middle :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
balance3Middle l m' r = case l of
  Empty               -> Balanced r
  Leaf x              -> Balanced $ node2 (leaf x) r
  Node2 _ l'' r''     -> Balanced $ node2 (node2 l'' r'') (node2 m' r)
  Node3 _ l'' m'' r'' -> Balanced $ node3 (node2 l'' m'') (node2 r'' m') r

balance3Right :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
balance3Right l m r' = case m of
  Empty               -> Balanced l
  Leaf x              -> Balanced $ node2 l (leaf x)
  Node2 _ l'' r''     -> Balanced $ node2 (node2 l l'') (node2 r'' r')
  Node3 _ l'' m'' r'' -> Balanced $ node3 l (node2 l'' m'') (node2 r'' r')