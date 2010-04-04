{-# LANGUAGE BangPatterns, MagicHash #-}

-- |
--
-- A module containing simple functions for counting the (Unicode)
-- characters in a file. We should be able to do this relatively
-- quickly, so this should be a good upper speed bound for lexing a
-- Java file.

module CharCount (hCountChars, countCharsInFile) where

import Control.Monad.Error (throwError)
import System.IO
import System.IO.Error

import GHC.Prim (Int#,(+#))
import GHC.Exts

-- | Counts the characters read from a given handle, adding the total
-- to a given starting point.
hCountChars :: Int -> Handle -> IO Int
hCountChars = hCountChars1

-- The three different implementations of hCountChars below all give
-- pretty much the same performance. According to Criterion, they
-- report the charcter count for /usr/share/dict/words in between
-- 130ms and 140ms, which is around the stable time reported by
-- /usr/bin/time. This is about three or four times slower than 'wc'.
--
-- I experimented with the different approaches trying to find a way
-- to make the performance closer to 'wc'. I suspect that the
-- conversion to Unicode characters might be imposing some performance
-- overhead.

-- Count characters using ordinary Ints, and a CharOrEOF data type
-- that lets us represent EOF.
hCountChars1 :: Int -> Handle -> IO Int
hCountChars1 !sofar h =
    loop sofar
    where loop !sofar =
              hSafeGetChar1 h >>= \cOrEOF ->
                  case cOrEOF of AChar _ -> loop (sofar+1)
                                 EOF -> return sofar

-- Counts characters using ordinary ints, but turns characters and the
-- EOF condition into Ints; EOF is represented by -1.
hCountChars2 :: Int -> Handle -> IO Int
hCountChars2 !sofar h =
    loop sofar
    where loop !sofar =
              hSafeGetChar2 h >>= \i -> if i < 0 then return sofar else loop (sofar + 1)

-- Counts characters using GHC primitive ints and CharOrEOF.
hCountChars3 :: Int -> Handle -> IO Int
hCountChars3 !(I# sofar) h =
    loop sofar
    where loop :: Int# -> IO Int
          loop sofar =
              hSafeGetChar1 h >>= \cOrEOF ->
                  case cOrEOF of AChar _ -> loop (sofar +# 1#)
                                 EOF -> return $ I# sofar

hSafeGetChar1 :: Handle -> IO CharOrEOF
hSafeGetChar1 h =
    (hGetChar h >>= return . AChar)
    `catch` (\e -> if isEOFError e then return EOF else throwError e)

data CharOrEOF = AChar !Char | EOF;

hSafeGetChar2 :: Handle -> IO Int
hSafeGetChar2 h =
    (hGetChar h >>= return . fromEnum)
    `catch` (\e -> if isEOFError e then return (negate 1) else throwError e)

countCharsInFile :: FilePath -> IO Int
countCharsInFile fn =
    -- This isn't actually any faster than using 'withFile', so block
    -- buffering is obviously on by default.
    do h <- openFile fn ReadMode
       (do hSetBuffering h (BlockBuffering $ Just $ 64 * 1024)
           c <- (hCountChars 0 h)
           hClose h
           return c) `catch`  (\e -> hClose h >> throwError e)

