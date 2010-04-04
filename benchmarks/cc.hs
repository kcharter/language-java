module Main where

import System.Environment

import CharCount

main :: IO ()
main =
    do args <- getArgs
       lengths <- mapM countCharsInFile args
       mapM_ (\(f,l) -> putStr f >> putStr ": " >> print l) (zip args lengths)

       