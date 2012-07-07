module Main where 

import System.Environment           (getArgs)
import Data.Char                    (toLower)
import Data.Set                     (Set, notMember, fromDistinctAscList)
import qualified Data.Set as Set

type Dictionary = Set String

spellCheck :: String -> Dictionary -> [String]
spellCheck xs dict = filter (`notMember` dict) (map (removePunc . map toLower) $ asWords xs)
    where asWords    = concatMap words . lines
          removePunc = filter (`notElem` "!@#$%^&*(),.;'\"?")

spellCheckFiles :: FilePath -> FilePath -> IO ()
spellCheckFiles input dict = do
    inputTxt <- readFile input
    dictTxt  <- readFile dict
    let incorrectWords = spellCheck inputTxt (fromDistinctAscList $ lines dictTxt)
    mapM_ putStrLn incorrectWords
    return ()

main :: IO ()
main = do
    args <- getArgs
    let inFile = head args
    let dict   = args !! 1

    spellCheckFiles inFile dict 


