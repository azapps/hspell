module SpellChecker.AStar where

import SpellChecker.Types
import qualified Data.PQueue.Prio.Min as Q
import SpellChecker.Trie
import SpellChecker.DynamicProg
import Data.Foldable (foldr')
import Data.List
import Data.Ord
import SpellChecker.AStarHelper
import qualified Data.Vector as V

-- | Performs an A* search on the trie and returns the n best results
aStar :: Int -- ^ How many results do you need? (n)
         -> Trie Char -- ^ The Trie with all possible words
         -> Word -- ^ The word you are searching for
         -> [(Word,Int)] -- ^ List of n best words with theire weights
aStar suggestions trie w' =
  let
    w = ' ' : w'
    defaultThreshold = ceiling $ (realToFrac (length w) :: Double)
    threshold = Threshold V.empty suggestions  defaultThreshold w'
    curr = Current "" (V.fromList $ zip w [0..]) ' ' trie
    queue = Q.fromList [(getHeuristicScore curr threshold,curr)]
  in
   V.toList $ hWords $ fst $ step (threshold, queue)

-- | This is actual algorithm for the A*
step :: (Threshold, Queue) -- ^ Takes a threshold and a queue
        -> (Threshold, Queue) -- ^ returns a new threshold and new queue
step (h,q) =
  case Q.getMin q of
    Nothing -> (h,q)
    Just (_, current) ->
      let expandedQueue = expand h current
          newQueue :: Queue
          newQueue = foldr' (uncurry $ flip Q.insert) (Q.deleteMin q) expandedQueue
          newThreshold = updateThreshold current h
      in
       if matchThreshold h current && not (hasFinalResult h) then
         step (newThreshold, newQueue)
       else
         (h,q)

-- | Checks if the word we want to check is correct.
--
-- This optimizes the computation a bit
hasFinalResult :: Threshold -> Bool
hasFinalResult threshold =
  let w = hWords threshold in
  V.length w > 0 && fst (V.head w) == hWord threshold

-- | Calculates a new threshold from the current element and the current threshold
updateThreshold :: Current -> Threshold -> Threshold
updateThreshold current threshold =
  if not $ isFinal $ currentTrie current then
    -- If its not a final word, we ignore it
    threshold
  else
    let currentScore = getFinalScore current in
    if V.length (hWords threshold) < hMaxLength threshold then
      -- If hWords is not full
      if currentScore > hDefault threshold then
        -- and the current score is bigger as the default one we ignore it
        threshold
      else
        -- else we insert it to the threshold
        insertSorted current threshold
    else
      -- hWords is full
      let maxElem = V.last $ hWords threshold in
      if  currentScore >= snd maxElem then
        -- if the current score is bigger we ignore it
        threshold
      else
        -- we delete the biggest element and insert our
        insertSorted current $ threshold { hWords = V.init $ hWords threshold}

-- | Insert the current element in the threshold
insertSorted :: Current -> Threshold -> Threshold
insertSorted current threshold =
  let e = (currentWord current, getFinalScore current) in
  addToThreshold e threshold


addToThreshold :: (Word,Int) -> Threshold -> Threshold
addToThreshold e threshold =
  threshold { hWords = V.fromList $ insertBy (comparing snd) e $ V.toList $ hWords threshold}
  
-- | The expansion function for the A*
expand :: Threshold -> Current -> [(Current,Int)]
expand threshold current =
  let subt = getChildrens $ currentTrie current
      elems :: [(Char,Trie Char)]
      elems = filter (matchThreshold threshold . calcCurrent current) subt in
  toCurrentList elems
  where
    toCurrentList :: [(Char,Trie Char)] -> [(Current,Int)]
    toCurrentList [] = []
    toCurrentList (x:xs) =
      let
        newScore = getHeuristicScore curr threshold
        curr = calcCurrent current x
      in
      (curr,newScore) : toCurrentList xs

-- | Calculate a new current element from an old one and the SubTrie of that
calcCurrent :: Current -- ^ Current element
               -> (Char,Trie Char) -- ^ One element of the return of getChildrens
               -> Current -- ^ New current element
calcCurrent current (c,trie)= let 
  newWord = currentWord current ++ [c]
  newWM = calcWeights c (currentMatrix current)
  in
   Current newWord newWM c trie

-- | Checks if the weight of the current is smaller than the treshold
matchThreshold :: Threshold -> Current -> Bool
matchThreshold threshold current =
  let currentScore = getHeuristicScore current threshold in
  if (V.length (hWords threshold)) < hMaxLength threshold then
    currentScore < hDefault threshold
  else
    let maxElem = V.last $ hWords threshold in
    currentScore < snd maxElem
    
