module Main where 

import System.Environment (getArgs)
import Data.Char          (toLower)

type Dictionary = [String]

spellCheck :: String -> Dictionary -> [String]
spellCheck xs dict = filter (`notElem` dict) (map (removePunc . map toLower) $ asWords xs)
    where asWords    = concatMap words . lines 
          removePunc = filter (`notElem` "!@#$%^&*(),.;'\"?")

spellCheckFiles :: FilePath -> FilePath -> IO ()
spellCheckFiles input dict = do
    inputTxt <- readFile input
    dictTxt  <- readFile dict
    let incorrectWords = spellCheck inputTxt (lines dictTxt)
    mapM_ putStrLn incorrectWords

main :: IO ()
main = do
    args <- getArgs
    let inFile = head args
    let dict   = args !! 1

    spellCheckFiles inFile dict 


