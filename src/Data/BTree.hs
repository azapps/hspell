module Data.BTree where

import Prelude hiding (head,last,init)

data Tree a = Node a (Tree a) (Tree a) | Leaf
                                         deriving (Eq, Show)
-- | Insert an element in the sorted tree
insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node a Leaf Leaf
insert a (Node n l r)
  | a < n = Node n (insert a l) r
  | otherwise = Node n l (insert a r)

-- | An empty tree
empty :: Tree a
empty = Leaf

--  | Converts a Tree to a sorted list
toList :: Tree a -> [a]
toList Leaf = []
toList (Node n l r) = toList l ++ [n] ++ toList r

-- | Test whether a list is empty. 
null :: Tree a -> Bool
null Leaf = True
null _ = False

-- | Number of elements in the Tree
size :: Tree a -> Int
size Leaf = 0
size (Node _ l r) = 1 + size l + size r

-- | Extract the first element of a Tree
-- | which must be non-empty
head :: Tree a -> a
head (Node n Leaf _) = n
head (Node _ l _) = head l
head Leaf = error "Empty tree"

-- | Extract the last element of a Tree
-- | which must be non-empty
last :: Tree a -> a
last (Node n _ Leaf) = n
last (Node _ _ r) = last r
last Leaf = error "Empty tree"

-- | Return all the elements of a Tree
-- | except the last one. The Tree can be empty
init :: Tree a -> Tree a
init Leaf = Leaf
init (Node _ Leaf Leaf) = Leaf
init (Node _ l Leaf) = l
init (Node n l r) = Node n l (init r)

-- | Return a human-readable Tree
prettyPrint :: Show a => Tree a -> String
prettyPrint = unlines . treeIndent
  where
    treeIndent Leaf          = ["-- /-"]
    treeIndent (Node v lb rb) =
      ["--" ++ (show v)] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent $ rb
        ls     = treeIndent $ lb
