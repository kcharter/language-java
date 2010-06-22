module UnicodeEscapeTests (run) where

import Control.Monad (liftM, liftM4)
import Data.Char (toLower)
import Test.QuickCheck

import Language.Java.Lexer.UnicodeEscapes

run :: IO ()
run = do quickCheck prop_charToEscapeToChar
         quickCheck $ forAll escapes prop_escapeToCharToEscape

prop_charToEscapeToChar x =
    let (c1,c2,c3,c4) = charToEscape x
        errOrChar = escapeToChar c1 c2 c3 c4 :: Either String Char
    in either (const False) (x ==) errOrChar

prop_escapeToCharToEscape digits@(d1, d2, d3, d4) =
    rightOrFalse (do c <- escapeToChar d1 d2 d3 d4 :: Either String Char
                     return (toLower4 digits == charToEscape c))

escapes = liftM4 (,,,) h h h h where h = hexDigits

hexDigits =
    frequency [(10, choose ('0','9')),
               (6, oneof [choose ('a','f'), choose ('A','F')])]

rightOrFalse :: Either a Bool -> Bool
rightOrFalse = either (const False) id

toLower4 (a,b,c,d) = (toLower a, toLower b, toLower c, toLower d)