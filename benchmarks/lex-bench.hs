module Main where

import Criterion
import Criterion.Main

import Language.Java.Lexer.UnicodeEscapes

escape006xToChar :: Char -> IO Char
escape006xToChar = escapeToChar '0' '0' '6'

main = defaultMain [bench "escapeToChar" (escape006xToChar '7'),
                    bench "charToEscape" (nf charToEscape 'a')]
