module SpellChecker.CheckFile where

import SpellChecker.Trie
import SpellChecker.Types
import SpellChecker.AStar
import Data.Char
import Text.Read
import Control.Applicative

defaultPenalties :: Penalties
defaultPenalties = Penalties {
  penaltyInsertion = \_ _ -> 1
  , penaltyDeletion = \_ _ -> 1
  , penaltySubstitution = \x y -> if x == y then 0 else 1
  , penaltyReversal = \_ _ -> 1
  }
                   
-- | Represents a part of text
data Token = Token {
  tWord :: Word -- ^ one word
  , tIntermediate :: String -- ^ intermediate stuff
    } deriving Show

-- | Silent spell checker: Takes always the first option
checkSilent :: String -> Trie Char -> String
checkSilent str trie =
  let tokens = splitString str
      newTokens = map (correctTokenSilent trie) tokens in
  joinTokens newTokens

-- | Takes the first option for one token
correctTokenSilent :: Trie Char -> Token -> Token
correctTokenSilent _ t@(Token "" _) = t
correctTokenSilent trie token =
  let word = tWord token
      results = aStar 10 defaultPenalties trie word
      first = if length results == 0 then word else fst $ head results in
  if first == word then
    token
  else token { tWord = first }

-- | Checks a text interactively
checkString :: String -> Trie Char -> IO (Maybe String)
checkString str trie = do
  let tokens = splitString str
  newTokens <- mapM (correctToken trie) tokens
  let newText = joinTokens newTokens
  return $ Just newText

-- | Handles the correction of a token
correctToken :: Trie Char -> Token -> IO Token
correctToken _ t@(Token "" _) = return t
correctToken trie token = do
  let word = tWord token
      results = aStar 10 defaultPenalties trie word
      first = fst $ head results
  if first == word then
    return token
  else do
    putStrLn $ word ++ " is misspelled. Suggestions:"
    selectWord token results

-- | Asks the user for the best correction
selectWord :: Token -> [(Word, Int)] -> IO Token
selectWord token results = do
  let options = zip ([0..] :: [Int]) results
  mapM_ printOption options
  res <- (readMaybe <$> getLine) :: IO (Maybe Int)
  case res of
    Nothing -> tryAgain
    Just i -> case lookup i options of
      Nothing -> tryAgain
      Just w -> return $ token {tWord = fst w}
  where
    printOption (i,(w,weight)) = putStrLn $ (show i) ++ ") " ++ w ++ " weight: (" ++ show weight ++ ")"
    tryAgain = do
      putStrLn "Wrong option – Please try again"
      selectWord token results

-- | Join tokens to a string
joinTokens :: [Token] -> String
joinTokens [] = ""
joinTokens (x:xs) = tWord x ++ tIntermediate x ++ joinTokens xs

-- | Splits a string to a list of Tokens
splitString :: String -> [Token]
splitString s = go s $ Token "" ""
              where
                newGo xs = (go xs $ Token "" "")
                
                go :: String -> Token -> [Token]
                go [] t
                  | null $ tWord t = []
                  | otherwise = [t]
                go (x:xs) t
                  | isSpace x = t { tIntermediate = (tIntermediate t) ++ [x] }  : newGo xs
                  | isAlpha x = go xs $ t { tWord = (tWord t) ++ [x] }
                  | otherwise =  t { tIntermediate = (tIntermediate t) ++ [x] } : newGo xs
