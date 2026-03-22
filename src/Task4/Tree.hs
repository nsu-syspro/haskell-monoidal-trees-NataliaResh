{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task4.Tree where

import Common.MonoidalTree

import Task1

-- * Finger tree definition

-- | Finger tree with values 'a' in leaves
-- Intermediate branches contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Single a
  | Deep m (Digit a) (Tree m (Node m a)) (Digit a)
  deriving (Show, Eq)

-- | 2-3 node of finger tree
data Node m a
  = Node2 m a a
  | Node3 m a a a
  deriving (Show, Eq)

-- | Finger tree digit
data Digit a
  = One   a
  | Two   a a
  | Three a a a
  | Four  a a a a
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty           = mempty
  measure (Single x)      = measure x
  measure (Deep m _  _ _) = m

-- | Measures given node using provided measure of 'a'
instance Measured m a => Measured m (Node m a) where
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m

-- | Measures given digit using provided measure of 'a'
instance Measured m a => Measured m (Digit a) where
  measure :: Measured m a => Digit a -> m
  measure = foldMap measure

instance Foldable (Tree m) where
  foldMap _ Empty          = mempty
  foldMap f (Single x)       = f x
  foldMap f (Deep _ x t y) = foldMap f x <>  (foldMap . foldMap) f t <> foldMap f y

instance Foldable (Node m) where
  foldMap f (Node2 _ l r) = f l <> f r
  foldMap f (Node3 _ l m r) = f l <> f m <> f r

instance Foldable Digit where
  foldMap :: Monoid m => (a -> m) -> Digit a -> m
  foldMap f (One x) = f x
  foldMap f (Two x y) = f x <> f y
  foldMap f (Three x y z) = f x <> f y <> f z
  foldMap f (Four x y z w) = f x <> f y <> f z <> f w

-- * Smart constructors

single :: a -> Tree m a
single = Single
node2 :: Measured m a => a -> a -> Node m a
node2 x y = Node2 (measure [x, y]) x y

node3 :: Measured m a => a -> a -> a -> Node m a
node3 x y z = Node3 (measure [x, y, z]) x y z

deep :: Measured m a => Digit a -> Tree m (Node m a) -> Digit a -> Tree m a
deep x t y = Deep (foldMap measure x <> measure t <> foldMap measure y) x t y

prependDigit :: a -> Digit a -> Digit a
prependDigit a (One b) = Two a b
prependDigit a (Two b c) = Three a b c
prependDigit a (Three b c d) = Four a b c d
prependDigit _ (Four {}) = error ""

appendDigit :: a -> Digit a -> Digit a
appendDigit a (One b) = Two b a
appendDigit a (Two b c) = Three b c a
appendDigit a (Three b c d) = Four b c d a
appendDigit _ (Four {}) = error ""
-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = foldr (<|) Empty
  a <| Empty                      = single a
  a <| Single b                   = deep (One a) Empty (One b)
  a <| Deep _ (Four b c d e) m sf = deep (Two a b) (node3 c d e <| m) sf
  a <| Deep _ pr m sf             = deep (prependDigit a pr) m sf

  Empty |> a = single a
  Single b |> a = deep (One b) Empty (One a)
  Deep _ pr m (Four b c d e) |> a = deep pr (m |> node3 b c d) (Two e a)
  Deep _ pr m sf |> a = deep pr m (appendDigit a sf)

-- * Utility functions

-- | Split result with left part, middle element and right part
data Split f a = Split (f a) a (f a)
  deriving (Show, Eq)

digitToList :: Digit a -> [a]
digitToList (One a) = [a]
digitToList (Two a b) = [a, b]
digitToList (Three a b c) = [a, b, c]
digitToList (Four a b c d) = [a, b, c, d]


listToDigit :: [a] -> Digit a
listToDigit [a] = One a
listToDigit [a, b] = Two a b
listToDigit [a, b, c] = Three a b c
listToDigit [a, b, c, d] = Four a b c d
listToDigit _ = error ""

headDigit :: Digit c -> c
headDigit (One a) = a
headDigit (Two a _) = a
headDigit (Three a _ _) = a
headDigit (Four a _ _ _) = a

tailDigit :: Digit a -> [a]
tailDigit = drop 1 . digitToList

lastDigit :: Digit c -> c
lastDigit = last . digitToList

initDigit :: Digit a -> [a]
initDigit = init . digitToList

splitDigit :: Measured m a => (m -> Bool) -> m -> Digit a -> Split [] a
splitDigit p j = go j . digitToList
  where
    go _ [] = error ""
    go _  [a] = Split [] a []
    go i (a:as)
      | p i' = Split [] a as
      | otherwise = let 
        Split l x r = go i' as 
        in Split (a : l) x r
      where
        i' = i <> measure a

nodeToDigit :: Node m a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

data ViewL s a = NilL | ConsL a (s a)
data ViewR s a = NilR | ConsR (s a) a

viewL :: Measured m a => Tree m a -> ViewL (Tree m) a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep _ pr m sf) = ConsL (headDigit pr) (deepL (tailDigit pr) m sf)

deepL :: Measured m a => [a] -> Tree m (Node m a) -> Digit a -> Tree m a
deepL [] m sf = case viewL m of
  NilL -> toTree sf
  ConsL a m' -> deep (nodeToDigit a) m' sf
deepL pr m sf = deep (listToDigit pr) m sf

viewR :: Measured m a => Tree m a -> ViewR (Tree m) a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep _ pr m sf) = ConsR (deepR pr m (initDigit sf)) (lastDigit sf)

deepR :: Measured m a => Digit a -> Tree m (Node m a) -> [a] -> Tree m a
deepR pr m [] = case viewR m of
  NilR -> toTree pr
  ConsR m' a -> deep pr m' (nodeToDigit a)
deepR pr m sf = deep pr m (listToDigit sf)

-- | Helper function for spliting tree based on given predicate and starting accumulator value
splitTree :: Measured m a => (m -> Bool) -> m -> Tree m a -> Maybe (Split (Tree m) a)
splitTree _ _ Empty = Nothing
splitTree _ _ (Single x) = Just (Split Empty x Empty)
splitTree p i (Deep _ pr m sf)
  | p mpr = let Split l x r = splitDigit p i pr in Just (Split (toTree l) x (deepL r m sf))
  | p mm  = case splitTree p mpr m of
              Nothing -> Nothing
              Just (Split ml xs mr) ->
                let Split l x r = splitDigit p (mpr <> measure ml) (nodeToDigit xs)
                in Just (Split (deepR pr ml l) x (deepL r mr sf))
  | otherwise = let Split l x r = splitDigit p mm sf
                in Just (Split (deepR pr m l) x (toTree r))
  where
    mpr = i <>  foldMap measure pr
    mm = mpr <> measure m

-- | Splits tree based on given predicate
split :: Measured m a => (m -> Bool) -> Tree m a -> (Tree m a, Tree m a)
split _ Empty = (Empty, Empty)
split p xs
  | p (measure xs) = case splitTree p mempty xs of
    Just (Split l x r) -> (l, x <| r)
    Nothing -> (xs, Empty)
  | otherwise = (xs, Empty)

-- | Concatenates two trees
infixr 6 ><
(><) :: Measured m a => Tree m a -> Tree m a -> Tree m a
xs >< ys = app3 xs [] ys

appends :: (Foldable f, Measured m a) => f a -> Tree m a -> Tree m a
appends ts xs = foldr (<|) xs ts

prepends :: (Foldable f, Measured m a) => f a -> Tree m a -> Tree m a
prepends ts xs = foldl (|>) xs ts

app3 :: Measured m a => Tree m a -> [a] -> Tree m a -> Tree m a
app3 Empty ts xs = appends ts xs
app3 xs ts Empty = prepends ts xs
app3 (Single x) ts xs = x <| appends ts xs
app3 xs ts (Single x) = prepends ts xs |> x
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) = deep pr1 (app3 m1 (nodes (digitToList sf1 ++ ts ++ digitToList pr2)) m2) sf2


nodes :: Measured m a => [a] -> [Node m a]
nodes [] = []
nodes [a, b] = [node2 a b]
nodes [a, b, c] = [node3 a b c]
nodes [a, b, c, d] = [node2 a b, node2 c d]
nodes (a:b:c:xs) = node3 a b c : nodes xs
nodes [_] = []