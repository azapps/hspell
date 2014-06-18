module SpellChecker.Types where
import SpellChecker.Trie
import qualified Data.PQueue.Prio.Min as Q
import Data.Vector (Vector)

data Penalties = Penalties {
  penaltyInsertion :: Char -> Char -> Int
  , penaltyDeletion :: Char -> Char -> Int
  , penaltySubstitution :: Char -> Char -> Int
  , penaltyReversal :: Char -> Char -> Int
  }

type WMColumn = Vector (Char,Int)

-- | The last two columns of the Weight matrix
type WeightMatrix = (Maybe WMColumn,WMColumn)



-- | Wrapper for a word – I want to generalize the datatypes later
type Word = String

-- | Saves the current threshold
data Threshold = Threshold {
  hWords :: Vector (Word,Int) -- ^ The best `hMaxLength` words 
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

