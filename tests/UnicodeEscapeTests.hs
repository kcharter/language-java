module UnicodeEscapeTests (run) where

import Test.QuickCheck

import Language.Java.Lexer.UnicodeEscapes

run :: IO ()
run = quickCheck prop_charToEscapeToChar

prop_charToEscapeToChar x =
    let (c1,c2,c3,c4) = charToEscape x
        errOrChar = escapeToChar c1 c2 c3 c4 :: Either String Char
    in either (const False) (x ==) errOrChar
