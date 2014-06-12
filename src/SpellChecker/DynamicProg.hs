module SpellChecker.DynamicProg (calcWeights) where

import SpellChecker.Types
import qualified Data.Vector.Unboxed as V
import Data.Monoid

{-import Data.Foldable (foldl')

-- Only a function for testing
calcWeightsForWord :: Word -> Word -> WeightMatrix
calcWeightsForWord w1 w2 = let wm = zip w1 [0..] in
  foldl' (flip calcWeights) wm w2
-}

-- | Updates the WeightMatrix by adding a new Char to the word
calcWeights :: Char -- ^ Current char
            -> WeightMatrix -- ^ weight matrix
            -> WeightMatrix -- ^ New weight matrix
calcWeights c' weightMatrix = let
  (cx',x') = V.head weightMatrix
  newV = x'+1 in
  V.singleton (cx',newV) <> go newV c' weightMatrix
go :: Int -> Char -> WeightMatrix -> WeightMatrix
go i c weightMatrix
  | V.length weightMatrix < 2 = V.empty
  | otherwise = let
    (_,x) = weightMatrix V.! 0
    (cy,y) = weightMatrix V.! 1
    subW = if c==cy then 0 else 1
    newV = minimum [x+subW,y+1,i+1] in
    V.singleton (cy,newV) <> (go newV c $ V.tail weightMatrix)
