{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- Module : Language.Java.AST.Lexical
--
-- Data types for representing the lexical elements of the Java
-- programming language as described in Chapter 3 of JLS3.

module Language.Java.AST.Lexical where

import Control.DeepSeq
import qualified Data.Text as T

-- | An input element includes segments of whitespace, comments, and
-- the tokens of the Java language. We include all three to support
-- tools that want to preserve or analyze comments.
data JInputElement = JieWhiteSpace T.Text |
                     -- ^ A sequence of whitespace characters
                     JieComment JComment |
                     -- ^ A Java comment
                     JieToken JToken
                     -- ^ A token of the Java language
                     deriving (Eq, Ord, Show)

instance NFData JInputElement where
    rnf e = e `seq` case e of
                      JieWhiteSpace t -> rnf t
                      JieComment c -> rnf c
                      JieToken t -> rnf t

-- | Single-line and multi-line comments.
data JComment = JcSingleLine T.Text |
                -- ^ A single-line comment. The text should include
                -- the leading @//@ and the trailing line terminator
                -- characters.
                JcMultiLine T.Text
                -- ^ A multi-line comment. The text should include
                -- the @/*@ and @*/@ comment delimiters.
                deriving (Eq, Ord, Show)

instance NFData JComment where
    rnf c = c `seq` case c of
                      JcSingleLine t -> rnf t
                      JcMultiLine t -> rnf t

data JToken = JtIdentifier JIdentifier |
              JtKeyword JKeyword |
              JtLiteral JLiteral |
              JtSeparator JSeparator |
              JtOperator JOperator
              deriving (Eq, Ord, Show)

instance NFData JToken where
    rnf t = t `seq` case t of
                      JtIdentifier i -> rnf i
                      JtKeyword k -> rnf k
                      JtLiteral l -> rnf l
                      JtSeparator s -> rnf s
                      JtOperator o -> rnf o

newtype JIdentifier = JIdentifier T.Text deriving (Eq, Ord, Show, NFData)

data JKeyword = JkAbstract |
                JkAssert |
                JkBoolean |
                JkBreak |
                JkByte |
                JkCase |
                JkCatch |
                JkChar |
                JkClass |
                JkConst |
                JkContinue |
                JkDefault |
                JkDo |
                JkDouble |
                JkElse |
                JkEnum |
                JkExtends |
                JkFinal |
                JkFinally |
                JkFloat |
                JkFor |
                JkIf |
                JkGoto |
                JkImplements |
                JkImport |
                JkInstanceof |
                JkInt |
                JkInterface |
                JkLong |
                JkNative |
                JkNew |
                JkPackage |
                JkPrivate |
                JkProtected |
                JkPublic |
                JkReturn |
                JkShort |
                JkStatic |
                JkStrictfp |
                JkSuper |
                JkSwitch |
                JkSynchronized |
                JkThis |
                JkThrow |
                JkThrows |
                JkTransient |
                JkTry |
                JkVoid |
                JkVolatile |
                JkWhile
                deriving (Eq, Ord, Show)

instance NFData JKeyword

data JLiteral = JlNull |
                JlTrue |
                JlFalse |
                JlCharLiteral Char |
                JlStringLiteral T.Text |
                JlIntegerLiteral T.Text Integer |
                JlFloatLiteral T.Text Double
                deriving (Eq, Ord, Show)

instance NFData JLiteral where
    rnf jl = jl `seq` case jl of
                        JlCharLiteral c -> rnf c
                        JlStringLiteral t -> rnf t
                        JlIntegerLiteral t i -> rnf t `seq` rnf i
                        JlFloatLiteral t d -> rnf t `seq` rnf d
                        _ -> ()

data JSeparator = JsLParen |
                  JsRParen |
                  JsLCurly |
                  JsRCurly |
                  JsLSquare |
                  JsRSquare |
                  JsSemicolon |
                  JsComma |
                  JsPeriod
                  deriving (Eq, Ord, Show)

instance NFData JSeparator

data JOperator = JopAssign |
                 JopLess |
                 JopGreater |
                 JopNot |
                 JopBwCompliment |
                 JopQuestion |
                 JopColon |
                 JopEqual |
                 JopLessOrEqual |
                 JopGreaterOrEqual |
                 JopNotEqual |
                 JopAnd |
                 JopOr |
                 JopIncr |
                 JopDecr |
                 JopAdd |
                 JopSub |
                 JopMul |
                 JopDiv |
                 JopBwAnd |
                 JoBwOr |
                 JopBwXor |
                 JopMod |
                 JopLeftShift |
                 JopRightShift |
                 JopUnsignedRightShift |
                 JopAddAssign |
                 JopSubAssign |
                 JopMulAssign |
                 JopDivAssign |
                 JopBwAndAssign |
                 JopBwOrAssign |
                 JopBwXorAssign |
                 JopModAssign |
                 JopLeftShiftAssign |
                 JopRightShiftAssign |
                 JopUnsignedRightShiftAssign
                 deriving (Eq, Ord, Show)

instance NFData JOperator

