module SpellChecker.AStar where

import SpellChecker.Types
import qualified Data.PQueue.Prio.Min as Q
import Data.Trie
import SpellChecker.DynamicProg
import Data.Foldable (foldr')
import SpellChecker.AStarHelper
import qualified Data.Vector as V
import qualified Data.BTree as BT

-- | Performs an A* search on the trie and returns the n best results
aStar :: Int -- ^ How many results do you need? (n)
         -> Penalties -- ^ Penalties for insertion, deletion …
         -> Trie Char -- ^ The Trie with all possible words
         -> Word -- ^ The word you are searching for
         -> [(Word,Int)] -- ^ List of n best words with theire weights
aStar suggestions penalties trie w' =
  let
    w = ' ' : w'
    defaultThreshold = ceiling $ (realToFrac (length w) :: Double)
    threshold = Threshold BT.empty suggestions  defaultThreshold w'
    curr = Current "" (Nothing, V.fromList $ zip w [0..]) ' ' trie
    queue = Q.fromList [(getHeuristicScore curr threshold,curr)]
    toTuple (SuggWord i sw) = (sw,i)
  in
   map toTuple $ BT.toList $ hWords $ fst $ step penalties (threshold, queue)

-- | This is actual algorithm for the A*
step :: Penalties -> (Threshold, Queue) -- ^ Takes a threshold and a queue
        -> (Threshold, Queue) -- ^ returns a new threshold and new queue
step penalties (h,q) =
  case Q.getMin q of
    Nothing -> (h,q)
    Just (_, current) ->
      let expandedQueue = expand penalties h current
          newQueue :: Queue
          newQueue = foldr' (uncurry $ flip Q.insert) (Q.deleteMin q) expandedQueue
          newThreshold = updateThreshold current h
      in
       if matchThreshold h current && not (hasFinalResult h) then
         step penalties (newThreshold, newQueue)
       else
         (h,q)

-- | Checks if the word we want to check is correct.
--
-- This optimizes the computation a bit
hasFinalResult :: Threshold -> Bool
hasFinalResult threshold =
  let w = hWords threshold in
  not (BT.null w) && suggWord (BT.head w) == hWord threshold

-- | Calculates a new threshold from the current element and the current threshold
updateThreshold :: Current -> Threshold -> Threshold
updateThreshold current threshold =
  if not $ isFinal $ currentTrie current then
    -- If its not a final word, we ignore it
    threshold
  else
    let currentScore = getFinalScore current in
    if BT.size (hWords threshold) < hMaxLength threshold then
      -- If hWords is not full
      if currentScore > hDefault threshold then
        -- and the current score is bigger as the default one we ignore it
        threshold
      else
        -- else we insert it to the threshold
        insertSorted current threshold
    else
      -- hWords is full
      let maxElem = BT.last $ hWords threshold in
      if  currentScore >= suggInt maxElem then
        -- if the current score is bigger we ignore it
        threshold
      else
        -- we delete the biggest element and insert our
        insertSorted current $ threshold { hWords = BT.init $ hWords threshold}

-- | Insert the current element in the threshold
insertSorted :: Current -> Threshold -> Threshold
insertSorted current threshold =
  let e = SuggWord (getFinalScore current) (currentWord current) in
  threshold { hWords = BT.insert e $ hWords threshold}

-- | The expansion function for the A*
expand :: Penalties -> Threshold -> Current -> [(Current,Int)]
expand penalties threshold current =
  let subt = getChildrens $ currentTrie current
      elems :: [(Char,Trie Char)]
      elems = filter (matchThreshold threshold . calcCurrent penalties current) subt in
  toCurrentList elems
  where
    toCurrentList :: [(Char,Trie Char)] -> [(Current,Int)]
    toCurrentList [] = []
    toCurrentList (x:xs) =
      let
        newScore = getHeuristicScore curr threshold
        curr = calcCurrent penalties current x
      in
      (curr,newScore) : toCurrentList xs

-- | Calculate a new current element from an old one and the SubTrie of that
calcCurrent :: Penalties -- ^ Penalties
               -> Current -- ^ Current element
               -> (Char,Trie Char) -- ^ One element of the return of getChildrens
               -> Current -- ^ New current element
calcCurrent penalties current (c,trie)= let 
  newWord = currentWord current ++ [c]
  newWM = calcWeights penalties (lastChar current) c (currentMatrix current)
  in
   Current newWord newWM c trie

-- | Checks if the weight of the current is smaller than the treshold
matchThreshold :: Threshold -> Current -> Bool
matchThreshold threshold current =
  let currentScore = getHeuristicScore current threshold
      matchMaxDiff = currentScore  < 4
      maxElem = BT.last $ hWords threshold
      matchHeuristic = currentScore < suggInt maxElem
  in
  if (BT.size (hWords threshold)) < hMaxLength threshold then
    currentScore < hDefault threshold
  else
    matchMaxDiff && matchHeuristic

-- | Returns the last character of the current word. Space if its empty because it must not occur in a word
lastChar :: Current -> Char
lastChar Current { currentWord = w } = last $ ' ' : w
    
