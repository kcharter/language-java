{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- Module : Language.Java.AST.Lexical
--
-- Data types for representing the lexical elements of the Java
-- programming language as described in Chapter 3 of JLS3.
--
-- Informally, there are two kinds of \"tokens\" here, 'JInputElement'
-- and 'JToken'. A 'JInputElement' includes comments and expanses of
-- whitespace. A lexer that preserves comments and white space should
-- produce a stream of 'JInputElement' values. 'JToken' includes only
-- the elements of the Java language itself. A lexer that discards
-- comments and whitespace should produce a stream of 'JToken' values.
--
-- Given a stream of tokens, if you unparse the stream and lex the
-- output text, you should get the same stream of tokens
-- back. However, note that the types here are not designed to
-- preserve Unicode escapes as they appeared in the input. In
-- particular we provide no way to record the exact input text for
-- keywords, operators, separators, or the @null@, @true@, @false@,
-- and character literals. Unicode escapes that appear in original
-- program source may be replaced by their direct representations in
-- whatever encoding is used for output text.
--
-- We use 'T.Text' to represent sequences of Unicode characters from
-- the input.  When producing 'T.Text' values, a lexer must transform
-- Unicode escapes into the corresponding unicode characters. For
-- character and string literals, the lexer must also replace the
-- standard character and string escapes with the characters they
-- represent.


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
                -- neither the leading @\/\/@ nor the line terminator
                -- characters, but it should include all the internal
                -- white space.
                JcMultiLine T.Text
                -- ^ A multi-line comment. The text should include
                -- neither the leading @\/*@ delimiter nor the
                -- trailing @*\/@ delimiter, but it should include all
                -- the internal white space.
                deriving (Eq, Ord, Show)

instance NFData JComment where
    rnf c = c `seq` case c of
                      JcSingleLine t -> rnf t
                      JcMultiLine t -> rnf t

-- | Java language tokens, the lexical elements of the executable
-- language. Tokens include identifiers, reserved keywords, literals,
-- separators, and operator symbols.
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

-- | Keywords. Each constructor has the form @Jk/Keyword/@ where
-- @/Keyword/@ is the same as the language keyword, except it's
-- capitalized. 
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

-- | Literals, constant values of different types.
data JLiteral = JlNull |
                -- ^ The polymorphic @null@ literal.
                JlTrue |
                -- ^ The boolean @true@ literal.
                JlFalse |
                -- ^ The boolean @false@ literal.
                JlCharLiteral Char |
                -- ^ A character literal. The single character cannot
                -- represent character escapes in their source form,
                -- so they must be converted to the corresponding
                -- character.
                JlStringLiteral T.Text |
                -- ^ A string literal. The text should not include the
                -- surrounding quotation marks, and all character
                -- escapes should be converted to their corresponding
                -- characters.
                JlIntegerLiteral T.Text Integer |
                -- ^ An integer literal. This includes @int@ and
                -- @long@ literals. We include the original source
                -- text to help with unparsing.
                JlFloatLiteral T.Text Double
                -- ^ A floating point literal. This includes @float@
                -- and @double@ literals. We include the original
                -- source text to help with unparsing.
                deriving (Eq, Ord, Show)

instance NFData JLiteral where
    rnf jl = jl `seq` case jl of
                        JlCharLiteral c -> rnf c
                        JlStringLiteral t -> rnf t
                        JlIntegerLiteral t i -> rnf t `seq` rnf i
                        JlFloatLiteral t d -> rnf t `seq` rnf d
                        _ -> ()

-- | Special symbols used as punctuation.
data JSeparator = JsLParen |
                  -- ^ The left parenthesis @\'(\'@.
                  JsRParen |
                  -- ^ The right parenthesis @\')\'@.
                  JsLCurly |
                  -- ^ The left curly brace @\'{\'@.
                  JsRCurly |
                  -- ^ The right curly brace @\'}\'@.
                  JsLSquare |
                  -- ^ The left square bracket @\'[\'@.
                  JsRSquare |
                  -- ^ The right square bracket @\']\'@.
                  JsSemicolon |
                  -- ^ The semicolon @\';\'@.
                  JsComma |
                  -- ^ The comma @\',\'@.
                  JsPeriod
                  -- ^ The period @\'.\'@.
                  deriving (Eq, Ord, Show)

instance NFData JSeparator

-- | Java operators. Note that the ternary conditional operator @x ? y
-- | : z@ is represented by a pair of operators.
data JOperator = JopAssign |
                 -- ^ Assignment @(=)@.
                 JopLess |
                 -- ^ Less than @(<)@.
                 JopGreater |
                 -- ^ Greater than @(>)@.
                 JopNot |
                 -- ^ Boolean not @(!)@.
                 JopBwCompliment |
                 -- ^ Bitwise compliment @(~)@.
                 JopQuestion |
                 -- ^ First part of the ternary conditional operator @(?)@.
                 JopColon |
                 -- ^ Second part of the ternary conditional operator @(:)@.
                 JopEqual |
                 -- ^ Primitive equality @(==)@.
                 JopLessOrEqual |
                 -- ^ Less than or equal @(<=)@.
                 JopGreaterOrEqual |
                 -- ^ Greater than or equal @(>=)@.
                 JopNotEqual |
                 -- ^ Not equal @(!=)@.
                 JopAnd |
                 -- ^ Boolean and @(&&)@.
                 JopOr |
                 -- ^ Boolean or @(||)@.
                 JopIncr |
                 -- ^ Prefix or postfix increment @(++)@.
                 JopDecr |
                 -- ^ Prefix or postfix decrement @(--)@.
                 JopAdd |
                 -- ^ Addition or string concatenation @(+)@.
                 JopSub |
                 -- ^ Subtraction or negation @(-)@.
                 JopMul |
                 -- ^ Multiplication @(*)@.
                 JopDiv |
                 -- ^ Division @(\/)@.
                 JopBwAnd |
                 -- ^ Bitwise and @(&)@.
                 JoBwOr |
                 -- ^ Bitwise or @(|)@.
                 JopBwXor |
                 -- ^ Bitwise exclusive or @(^)@.
                 JopMod |
                 -- ^ Modulus @(%)@.
                 JopLeftShift |
                 -- ^ Left shift @(<<)@.
                 JopRightShift |
                 -- ^ Signed right shift @(>>)@.
                 JopUnsignedRightShift |
                 -- ^ Unsigned right shift @(>>>)@.
                 JopAddAssign |
                 -- ^ Add or concatenate and assign @(+=)@.
                 JopSubAssign |
                 -- ^ Subtract and assign @(-=)@.
                 JopMulAssign |
                 -- ^ Multiply and assign @(*=)@.
                 JopDivAssign |
                 -- ^ Divide and assign @(\/=)@.
                 JopBwAndAssign |
                 -- ^ Bitwise \"and\" and assign @(&=)@.
                 JopBwOrAssign |
                 -- ^ Bitwise \"or\" and assign @(|=)@.
                 JopBwXorAssign |
                 -- ^ Bitwise \"exclusive or\" and assign @(^=)@.
                 JopModAssign |
                 -- ^ Modulus and assign @(%=)@.
                 JopLeftShiftAssign |
                 -- ^ Left shift and assign @(<<=)@.
                 JopRightShiftAssign |
                 -- ^ Signed right shift and assign @(>>=)@.
                 JopUnsignedRightShiftAssign
                 -- ^ Unsigned right shift and assign @(>>>=)@.
                 deriving (Eq, Ord, Show)

instance NFData JOperator

