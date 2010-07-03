module UnicodeEscapeTests (run) where

import Control.Monad (liftM, liftM4)
import Data.Char (toLower, ord)
import Test.QuickCheck

import Language.Java.Lexer.UnicodeEscapes

run :: IO ()
run = do quickCheck prop_charToEscapeToChar
         quickCheck $ forAll escapes prop_escapeToCharToEscape
         quickCheck $ forAll bogusEscapes prop_badEscapeHasError

prop_charToEscapeToChar x =
    let (c1,c2,c3,c4) = charToEscape x
        errOrChar = escapeToChar c1 c2 c3 c4 :: Either String Char
    in either (const False) (x ==) errOrChar

prop_escapeToCharToEscape digits@(d1, d2, d3, d4) =
    rightOrFalse (do c <- escapeToChar d1 d2 d3 d4 :: Either String Char
                     return (toLower4 digits == charToEscape c))

prop_badEscapeHasError digits@(d1, d2, d3, d4) =
    either (const True) (const False) (escapeToChar d1 d2 d3 d4 :: Either String Char)

escapes = liftM4 (,,,) h h h h where h = hexDigits

hexDigits =
    frequency [(10, choose ('0','9')),
               (6, oneof [choose ('a','f'), choose ('A','F')])]

bogusEscapes =
    -- We generate bad characters for each position with a frequency
    -- of 1/4. If there are no bad characters, keep generating.
    do candidate <- liftM4 (,,,) d d d d
       if legalEscape candidate then bogusEscapes else return candidate
    where d = frequency [(75, hexDigits), (25, badDigits)]
          legalEscape (a,b,c,d) =
              isHexDigit a && isHexDigit b && isHexDigit c && isHexDigit d
          isHexDigit x = between '0' '9' x || between 'a' 'f' (toLower x)
          between lo hi x = x >= lo && x <= hi
          badDigits = weightedChoices [('\0', pred '0'),
                                       ('G', 'Z'),
                                       ('g', 'z')]
          weightedChoices = frequency . map weightedChoice
          weightedChoice (x, y) = (max (ord y - ord x + 1) 0, choose (x,y))
          
rightOrFalse :: Either a Bool -> Bool
rightOrFalse = either (const False) id

toLower4 (a,b,c,d) = (toLower a, toLower b, toLower c, toLower d)