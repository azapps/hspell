{-#LANGUAGE TupleSections #-}
module Main where

import Data.Trie as Trie
import SpellChecker.Types
import Options.Applicative
import           Data.Monoid
import SpellChecker.CheckFile

data SpellCheckerConfig = SpellCheckerConfig {
  cfgDictPath :: FilePath
  , cfgCheckPath :: FilePath
  , cfgOutPath :: FilePath
  , cfgQuiet :: Bool
  , cfgNumSuggestions :: Int
    }

-- | Creates a Trie with empty Weight matrices from a List of Words
createTrie :: [Word] -- ^ List of Words
              -> Trie Char -- ^ Trie
createTrie = addWords Trie.empty

-- | Reads a dictionary from a file and creates a trie
trieFromFile :: FilePath -> IO (Trie Char)
trieFromFile f = createTrie <$> (concat . map words . lines) <$> readFile f

-- | Executes the checking
runChecker :: SpellCheckerConfig -> IO ()
runChecker (SpellCheckerConfig dictPath fPath oFile quiet numSugg) = do
  trie <- trieFromFile dictPath
  file <- readFile fPath
  let
    checkF = if quiet then checkSilentIO else checkString numSugg
  corrected <- checkF file trie
  writeFile oFile corrected

-- | Parse the CLI arguments and call runChecker
main :: IO ()
main = execParser opts >>= runChecker
       where
         opts = info parser mempty
         parser = SpellCheckerConfig
                  <$> strOption
                  ( short 'd'
                    <> long "dictFile"
                    <> metavar "DICTFILE"
                    <> help "The dictionary file. The items must be seperated by space and/or newline"
                    )
                  <*> strOption
                  ( short 'i'
                    <> long "file"
                    <> metavar "FILE"
                    <> help "The file to check"
                    )
                  <*> strOption
                  ( short 'o'
                    <> long "out"
                    <> metavar "OUT"
                    <> help "File to write")
                  <*> switch
                  ( short 'q'
                    <> long "quiet"
                    <> help "Take the best match for every Word" )
                  <*> option (short 's'
                    <> long "suggestions"
                    <> metavar "NUMBER"
                    <> help "Show n suggestions"
                    <> value 10)

