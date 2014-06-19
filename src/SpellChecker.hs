{-#LANGUAGE TupleSections #-}
module Main where

import SpellChecker.Trie
import SpellChecker.Types
import Options.Applicative
import           Data.Monoid
import SpellChecker.CheckFile

data SpellCheckerConfig = SpellCheckerConfig {
  cfgDictPath :: FilePath
  , cfgCheckPath :: FilePath
  , cfgOutPath :: FilePath
  , cfgQuiet :: Bool
    }

-- | Creates a Trie with empty Weight matrices from a List of Words
createTrie :: [Word] -- ^ List of Words
              -> Trie Char -- ^ Trie
createTrie = addWords SpellChecker.Trie.empty

-- | Executes the checking
runChecker :: SpellCheckerConfig -> IO ()
runChecker (SpellCheckerConfig dictPath fPath oFile quiet) = do
  dict <- readFile dictPath
  file <- readFile fPath
  let
    checkF = if quiet then checkSilentIO else checkString
    trie = createTrie $ concat $ map words $ lines dict
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

