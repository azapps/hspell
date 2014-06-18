module SpellChecker.AStarHelper where

import SpellChecker.Types
import qualified Data.Vector as V
--import Debug.Trace

-- | Returns the final score of a current element
getFinalScore :: Current -> Int
getFinalScore = snd . V.last . snd . currentMatrix

-- | get the score (old costs + heuristic)
getHeuristicScore :: Current -> Threshold -> Int
getHeuristicScore current threshold =
  let costs = getCurrentCosts current threshold
      total = costs + 0
  in
  total
  --trace (show costs ++ " " ++ show (half heuristic) ++ "  " ++ currentWord current ++ "  " ++ (show $ hWords threshold)) total

-- | Returns the current costs
getCurrentCosts :: Current -> Threshold -> Int
getCurrentCosts current threshold =
  let l = V.length $ snd $ currentMatrix current
      currentLength = length $ currentWord current
      wlength = length $ hWord threshold
      costs = if currentLength < wlength then
                snd $ ( V.! (min currentLength (l-1)))  $ snd $ currentMatrix current
              else
                snd $ V.last $ snd $ currentMatrix current
  in
   costs
  

-- | Wrapper for (/2) :: Int -> Int
half :: Int -> Int
half i = ceiling $ (realToFrac i :: Double) /2
