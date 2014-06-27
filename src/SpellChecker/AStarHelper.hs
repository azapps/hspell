module SpellChecker.AStarHelper where

import SpellChecker.Types
import qualified Data.Vector as V
import Data.Ord

-- | Returns the final score of a current element
getFinalScore :: Current -> Int
getFinalScore = snd3 . V.last . snd . currentMatrix

-- | get the score (old costs + heuristic)
getHeuristicScore :: Current -> Threshold -> Int
getHeuristicScore current threshold =
  let costs = getCurrentCosts current threshold
      total = costs + 0
  in
  total
  --trace (show costs ++ " " ++ show (half heuristic) ++ "  " ++ currentWord current ++ "  " ++ (show $ hWords threshold)) total

-- | snd function for triple
snd3 :: (a,b,c) -> b
snd3 (_,a,_) = a

-- | Returns the current costs
-- | This is the minimum of the current Weightmatrix
getCurrentCosts :: Current -> Threshold -> Int
getCurrentCosts current _ =
  let currentMat = snd $ currentMatrix current
  in
   snd3 $ V.minimumBy (comparing snd3) currentMat
  

-- | Wrapper for (/2) :: Int -> Int
half :: Int -> Int
half i = ceiling $ (realToFrac i :: Double) /2
