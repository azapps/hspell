{-#LANGUAGE TupleSections #-}
module Main where

import SpellChecker.Trie
import SpellChecker.Types
import Options.Applicative
import           Data.Monoid
import SpellChecker.CheckFile

data SpellCheckerConfig = SpellCheckerConfig {
  dictPath :: FilePath
  , checkPath :: FilePath
  , outPath :: FilePath
    }

-- | Creates a Trie with empty Weight matrices from a List of Words
createTrie :: [Word] -- ^ List of Words
              -> Trie Char -- ^ Trie
createTrie = addWords SpellChecker.Trie.empty

runChecker :: SpellCheckerConfig -> IO ()
runChecker (SpellCheckerConfig dPath fPath oFile) = do
  dict <- readFile dPath
  file <- readFile fPath
  let trie = createTrie $ concat $ map words $ lines dict
  corrected <- checkString file trie
  case corrected of
    Nothing -> return ()
    Just c -> writeFile oFile c

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

