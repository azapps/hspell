module SpellChecker.Trie where
import Data.Map (Map)
import qualified Data.Map as M
import Data.Foldable (foldr')

-- | A simple trie which only stores keys
data Trie k = Trie { isFinal :: Bool
                   , subTries :: Map k (Trie k) }
            deriving (Show, Eq)

-- | returns an empty trie
empty :: Trie k
empty = Trie False M.empty

-- | Add a word to a trie
addWord :: (Eq k, Ord k) => [k] -> Trie k -> Trie k
addWord [] t = t { isFinal = True }
addWord (k:ks) trie = trie { subTries = M.insert k nt (subTries trie)}
  where nt = case M.lookup k (subTries trie) of
          Nothing -> addWord ks empty
          Just subtrie -> addWord ks subtrie

-- | Add many words to a trie
addWords :: (Ord k) => Trie k -> [[k]] -> Trie k
addWords = foldr' addWord

-- | Returns all childrens of the current trie
getChildrens :: Ord k => Trie k -> [(k,Trie k)]
getChildrens = M.toList . subTries

-- | Computes the height of the Trie
height :: Trie k -> Int
height (Trie _ subtries) = 1 + maximum' (map height $ M.elems subtries)
                           where 
                             maximum' [] = 0
                             maximum' xs = maximum xs

-- | Computes the number of elements of the trie
size :: Trie k -> Int
size (Trie _ subtries) = 1 + sum (map size $ M.elems subtries)

-- | Count all words in the trie
countWords :: Trie k -> Int
countWords (Trie f subtries) = let s = if f then 1 else 0 in
  s + sum (map countWords $ M.elems subtries)
