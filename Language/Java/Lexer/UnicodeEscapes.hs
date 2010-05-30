{-# LANGUAGE BangPatterns #-}

-- | Conversion from Java Unicode escapes to Unicode characters, and back.

module Language.Java.Lexer.UnicodeEscapes (escapeToChar, charToEscape) where

import Control.Monad.Error (Error, MonadError, throwError, strMsg)
import Data.Bits (shift, shiftR, (.|.), (.&.))
import Data.Char (ord, chr)


-- | Converts four characters, assumed to represent the four
-- hexidecimal digits for a Java Unicode escape, into the
-- corresponding Unicode character.
--
-- This function is meant to be used either in a lexer, or when
-- producing a stream of characters for consumption by a lexer, which
-- is why it takes four characters rather than four numbers, and why
-- we use four arguments rather than a string.
escapeToChar :: (Error e, MonadError e m) => Char -> Char -> Char -> Char -> m Char
escapeToChar !d1 !d2 !d3 !d4 =
    -- The code here needs a little explanation, particularly the
    -- 'AsHex' type, which is essentially a special-purpose 'Either',
    -- but with strict fields. Using Criterion, I found that I could
    -- avoid using 'error', and make an implementation in the IO monad
    -- very fast, by using this approach -- much faster than if
    -- 'toHex' returned its result in the monad. I had a version of
    -- this code that had 'Char' as the return type; it computed the
    -- character on success, but called 'error' on failure, and for
    -- some weird reason it was slower than this version executed in
    -- the IO monad, using -O2 and -funbox-strict-fields. Almost twice
    -- as slow, in fact.
    case (toHex d1)
    of HexDigit i1 ->
           case (toHex d2)
           of HexDigit i2 ->
                  case (toHex d3)
                  of HexDigit i3 ->
                         case (toHex d4)
                         of HexDigit i4 ->
                                return $ chr (((i1 `shift` 4 .|. i2) `shift` 4 .|. i3) `shift` 4 .|. i4)
                            NonHex c -> notAHexDigit c
                     NonHex c -> notAHexDigit c
              NonHex c -> notAHexDigit c
       NonHex c -> notAHexDigit c

notAHexDigit d = throwError $ strMsg $ "Not a hexidecimal digit: " ++ show d ++ "."

toHex :: Char -> AsHex
toHex !c =
    case c of '0' -> HexDigit 0
              '1' -> HexDigit 1
              '2' -> HexDigit 2
              '3' -> HexDigit 3
              '4' -> HexDigit 4
              '5' -> HexDigit 5
              '6' -> HexDigit 6
              '7' -> HexDigit 7
              '8' -> HexDigit 8
              '9' -> HexDigit 9
              'A' -> HexDigit 10
              'B' -> HexDigit 11
              'C' -> HexDigit 12
              'D' -> HexDigit 13
              'E' -> HexDigit 14
              'F' -> HexDigit 15
              'a' -> HexDigit 10
              'b' -> HexDigit 11
              'c' -> HexDigit 12
              'd' -> HexDigit 13
              'e' -> HexDigit 14
              'f' -> HexDigit 15
              _   -> NonHex c

data AsHex = HexDigit !Int | NonHex !Char

-- TODO: try using an unboxed tuple. This restricts how the function
-- result can be used, but for unparsing applications it should be
-- fine.

charToEscape :: Char -> (Char, Char, Char, Char)
charToEscape !c =
    (toHexChar i1, toHexChar i2, toHexChar i3, toHexChar i4) 
    where !i1 = (i .&. 0xf000) `shiftR` 12
          !i2 = (i .&. 0x0f00) `shiftR` 8
          !i3 = (i .&. 0x00f0) `shiftR` 4
          !i4 = (i .&. 0x000f)
          !i = ord c

toHexChar !i =
    if i > 9
    then chr (o_a + i - 10)
    else chr (o_0 + i)
    where o_a = ord 'a'
          o_0 = ord '0'
