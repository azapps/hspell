module SpellChecker.DynamicProg where

import SpellChecker.Types
import qualified Data.Vector as V
import Data.Monoid
import Control.Applicative
--import Debug.Trace
import Data.List
import Data.Ord

-- | Calculates a weightMatrix for two words
-- 
-- This function is not used here but its a great way to test the calcWeights
calcWeightsForWord :: Penalties -> Word -> Word -> WeightMatrix
calcWeightsForWord penalties w1 w2 = let wm = (Nothing,V.fromList $ zip3 (' ' : w1) [0..] (repeat No)) in
  calcMat penalties ' '  w2 wm

-- | The "go" function for calcWeightsForWord
calcMat :: Penalties -> Char -> Word -> WeightMatrix -> WeightMatrix
calcMat penalties oldChar (c:cx) wm = calcMat penalties c cx $ calcWeights penalties oldChar c wm
calcMat _ _ [] wm = wm


-- | Updates the WeightMatrix by adding a new Char to the word
calcWeights :: Penalties -- ^ Penalties for insertion…
            -> Char -- ^ Last char
            -> Char -- ^ Current char
            -> WeightMatrix -- ^ weight matrix
            -> WeightMatrix -- ^ New weight matrix
calcWeights penalties cOld c (fstWM,sndWM) = let
  (cx',x',op') = V.head sndWM
  weightMatrix' = (((' ',1000,No) `V.cons`) <$> fstWM,sndWM)
  newV = x'+1 in
  (Just sndWM,V.singleton (cx',newV,op') <> go penalties newV cOld c weightMatrix')

--Debugging stuff
--trace' x = trace ("\n" ++ show x ++ "\n") x
--pp (x,y)= "///\n" ++ (show $ V.toList <$> x) ++ "\n" ++ (show $ V.toList y) ++ "\n\\\\\\\\\n"

-- | The go function for calculating the weights
go :: Penalties -> Int -> Char -> Char -> WeightMatrix -> WMColumn
go penalties i cOld c weightMatrix@(fstWM,sndWM)
  | V.length sndWM < 2 = V.empty
  | otherwise = let
    (_,x,_) = sndWM V.! 0
    (cy,y,_) = sndWM V.! 1
    (newV,op) = minimumBy (comparing fst) [
        getPenalty y penalties (Ins c)
      , getPenalty i penalties (Del c)
      , getPenalty x penalties (Subst c cy)
      , calcReversion penalties cOld c weightMatrix
      ] in
    V.singleton (cy,newV,op) <> go penalties newV cOld c (V.tail <$> fstWM,V.tail sndWM)

-- | Calculate the costs for flipped characters
calcReversion :: Penalties -- ^ Penalties
                 -> Char -- ^ Old char
                 -> Char -- ^ Current char
                 -> WeightMatrix -- ^ Weight matrix
                 -> (Int,Op) -- ^ Score
calcReversion _ _ _ (Nothing,_) = (1000,No) -- something big
calcReversion penalties cOld c (_,sndWM) = let
    (cr,_,_) = sndWM V.! 0
    (cy,r,lastOp) = sndWM V.! 1
    --oldPenalty = fst $ getPenalty 0 penalties lastOp
    in
     if cr==c && cy == cOld then
        --trace "WTFFFF" $ trace' $
        getPenalty (r-1) penalties (Rev cOld c lastOp) else (1000,No)
