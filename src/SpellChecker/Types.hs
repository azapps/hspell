module SpellChecker.Types where
import Data.Trie
import qualified Data.PQueue.Prio.Min as Q
import Data.Vector (Vector)
import Data.BTree

data Op = Subst Char Char | Del Char | Ins Char | Rev Char Char Op | No
        deriving (Eq, Show)

type Penalties = Op -> Int

getPenalty :: Int -> Penalties -> Op -> (Int,Op)
getPenalty i p op = (i + p op, op)

type WMColumn = Vector (Char,Int,Op)

-- | The last two columns of the Weight matrix
type WeightMatrix = (Maybe WMColumn,WMColumn)



-- | Wrapper for a word – I want to generalize the datatypes later
type Word = String

-- | ADT for saving suggested words in the Tree
data SuggWord = SuggWord {suggInt :: Int, suggWord :: Word}
                deriving (Eq, Ord)

-- | Saves the current threshold
data Threshold = Threshold {
  hWords :: Tree SuggWord -- ^ The best `hMaxLength` words 
  , hMaxLength :: Int -- ^ The max length of `hWords`
  , hDefault :: Int -- ^ The default threshold which is used when `hWords` is shorter then `hMaxLength`
  , hWord :: Word
  }

-- | The alias for the queue used for saving the next possible nodes
type Queue = Q.MinPQueue Int Current

-- | Saves all needed informations for expanding a node
data Current = Current { currentWord :: Word -- ^ The word which was read so far from the tried
                       , currentMatrix :: WeightMatrix -- ^ The weight matrix for the current word
                       , currentChar :: Char -- ^ The char which is on top of the trie
                       , currentTrie :: Trie  Char -- ^ The subtrie
                       }
             deriving (Eq,Show)

