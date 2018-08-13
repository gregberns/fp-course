{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.JsParser where

import Course.Core
import Course.Parser
import Course.MoreParser
import Course.JsonValue
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional

-- var, let, const
-- doubleEqTok, tripleEqTok, semicolonTok
-- && ||

jsTrue :: Parser Chars
jsTrue = stringTok "true"

jsFalse :: Parser Chars
jsFalse = stringTok "false"

-- data JsExpression 

  -- operator operand

data ComparisonOperator =
  Equal
  | NotEqual
  | StrictEqual
  | StrictNotEqual
  | GreaterThan
  | GreaterThanOrEqual
  | LessThan
  | LessThanOrEqual
  deriving (Eq, Ord, Show)

-- toComparisonOperator ::
--   Chars
--   -> Optional ComparisonOperator
-- toComparisonOperator c =
--   let table = ("==", Equal) :.
--               ("!=", NotEqual) :.
--               ("===", StrictEqual) :.
--               ("!==", StrictNotEqual) :.
--               (">", GreaterThan) :.
--               (">=", GreaterThanOrEqual) :.
--               ("<", LessThan) :.
--               ("<=" , LessThanOrEqual) :.
--               Nil
--   in snd <$> find ((==) c . fst) table


jsEqual = stringTok "=="
jsNotEqual = stringTok "!="
jsStrictEqual = stringTok "==="
jsStrictNotEqual = stringTok "!=="
jsGreaterThan = stringTok ">"
jsGreaterThanOrEqual = stringTok ">="
jsLessThan = stringTok "<"
jsLessThanOrEqual = stringTok "<="

jsComparisonOperator = 
  (Equal <$ NotEqual
  ||| NotEqual <$ jsNotEqual
  ||| StrictEqual <$ jsStrictEqual
  ||| StrictNotEqual <$ jsStrictNotEqual
  )

jsExpression =
  do
    b1 <- jsTrue ||| jsFalse
    o <- jsComparisonOperator
    b2 <- jsTrue ||| jsFalse
    (o, b1, b2)


