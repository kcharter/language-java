module Main where

import Criterion
import Criterion.Main
import CharCount

main = defaultMain [bench "cc CharCount.hs" (countCharsInFile "CharCount.hs"),
                    bench "cc /usr/share/dict/words" (countCharsInFile "/usr/share/dict/words")]
