module Main where 

import System.Environment           (getArgs)
import Data.Char                    (toLower)
import qualified Data.Set as Set

import Trie                         (Trie, TrieSet, notMember', fromList')
import qualified Trie as Trie

type Dictionary = TrieSet Char

spellCheck :: String -> Dictionary -> [String]
spellCheck xs dict = filter (`notMember'` dict) (map (removePunc . map toLower) $ asWords xs)
    where asWords    = concatMap words . lines
          removePunc = filter (`notElem` "!@#$%^&*(),.;'\"?")

spellCheckFiles :: FilePath -> FilePath -> IO ()
spellCheckFiles input dict = do
    inputTxt <- readFile input
    dictTxt  <- readFile dict
    let incorrectWords = spellCheck inputTxt (fromList' $ lines dictTxt)
    mapM_ putStrLn incorrectWords
    return ()

main :: IO ()
main = do
    args <- getArgs
    let inFile = head args
    let dict   = args !! 1

    spellCheckFiles inFile dict 

-- Testing

testInput = "hi this is my wonky hilux"
testDict  = fromList' ["hi", "hilux", "these", "wonky", "words"]

