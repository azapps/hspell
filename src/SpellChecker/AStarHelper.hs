module SpellChecker.AStarHelper where

import SpellChecker.Types
import qualified Data.Vector.Unboxed as V
--import Debug.Trace

-- | Returns the final score of a current element
getFinalScore :: Current -> Int
getFinalScore = snd . V.last . currentMatrix

-- | get the score (old costs + heuristic)
getHeuristicScore :: Current -> Threshold -> Int
getHeuristicScore current threshold =
  let costs = getCurrentCosts current threshold
      currentLength = length $ currentWord current
      heuristic = max 0 $ length (hWord threshold) - currentLength
      total = costs + 0 --(half heuristic)
  in
  total
  --trace (show costs ++ " " ++ show (half heuristic) ++ "  " ++ currentWord current ++ "  " ++ (show $ hWords threshold)) total

-- | Returns the current costs
getCurrentCosts :: Current -> Threshold -> Int
getCurrentCosts current threshold =
  let wlength = length $ currentWord current
      l = V.length $ currentMatrix current
      in
  snd $ ( V.! (min wlength (l-1)))  $ currentMatrix current

-- | Wrapper for (/2) :: Int -> Int
half :: Int -> Int
half i = ceiling $ (realToFrac i :: Double) /2
