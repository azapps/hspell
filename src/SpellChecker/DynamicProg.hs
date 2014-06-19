module SpellChecker.DynamicProg (calcWeights, calcWeightsForWord) where

import SpellChecker.Types
import qualified Data.Vector as V
import Data.Monoid
import Control.Applicative

-- | Calculates a weightMatrix for two words
-- 
-- This function is not used here but its a great way to test the calcWeights
calcWeightsForWord :: Penalties -> Word -> Word -> WeightMatrix
calcWeightsForWord penalties w1 w2 = let wm = (Nothing,V.fromList $ zip (' ' : w1) [0..]) in
  calcMat penalties ' '  w2 wm

-- | The "go" function for calcWeightsForWord
calcMat :: Penalties -> Char -> Word -> WeightMatrix -> WeightMatrix
calcMat penalties oldChar (c:cx) wm = calcMat penalties c cx $ calcWeights penalties oldChar c wm
calcMat _ _ [] wm = wm


-- | Updates the WeightMatrix by adding a new Char to the word
calcWeights :: Penalties
            -> Char
            -> Char -- ^ Current char
            -> WeightMatrix -- ^ weight matrix
            -> WeightMatrix -- ^ New weight matrix
calcWeights penalties cOld c (fstWM,sndWM) = let
  (cx',x') = V.head sndWM
  weightMatrix' = (((' ',1000) `V.cons`) <$> fstWM,sndWM)
  newV = x'+1 in
  (Just sndWM,V.singleton (cx',newV) <> go penalties newV cOld c weightMatrix')
  
go :: Penalties -> Int -> Char -> Char -> WeightMatrix -> WMColumn
go penalties i cOld c weightMatrix@(fstWM,sndWM)
  | V.length sndWM < 2 = V.empty
  | otherwise = let
    (_,x) = sndWM V.! 0
    (cy,y) = sndWM V.! 1
    newV = minimum [
        x + penaltySubstitution penalties c cy
      , y + penaltyDeletion penalties c cy
      , i + penaltyInsertion penalties c cy
      , calcReversion penalties cOld c weightMatrix
      ] in
    V.singleton (cy,newV) <> go penalties newV cOld c (V.tail <$> fstWM,V.tail sndWM)

calcReversion :: Penalties -> Char -> Char -> WeightMatrix -> Int
calcReversion _ _ _ (Nothing,_) = 1000 -- something big
calcReversion penalties cOld c (Just fstWM,sndWM) = let
    (cy,_) = sndWM V.! 1
    (cr,r) = fstWM V.! 0
    in
     if cr==cOld && cy == c then r + penaltyReversal penalties cOld c else 1000
     -- This does not work with arbitrary scores!
