{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# language DataKinds, KindSignatures #-}

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

data LogicalOperator =
  LogicalAnd
  | LogicalOr
  | LogicalNot
  deriving (Eq, Ord, Show)

data Boolean =
  JsTrue
  | JsFalse
  deriving (Eq, Ord, Show)


jsTrue :: Parser Chars
jsTrue = stringTok "true"

jsFalse :: Parser Chars
jsFalse = stringTok "false"

jsBoolean :: Parser Boolean
jsBoolean =
  JsTrue <$ jsTrue ||| JsFalse <$ jsFalse

jsEqual = stringTok "=="
jsNotEqual = stringTok "!="
jsStrictEqual = stringTok "==="
jsStrictNotEqual = stringTok "!=="
jsGreaterThan = stringTok ">"
jsGreaterThanOrEqual = stringTok ">="
jsLessThan = stringTok "<"
jsLessThanOrEqual = stringTok "<="

jsComparisonOperator = 
  spaces *> (
    StrictNotEqual <$ jsStrictNotEqual
    ||| StrictEqual <$ jsStrictEqual
    ||| NotEqual <$ jsNotEqual
    ||| Equal <$ jsEqual
    ||| GreaterThanOrEqual <$ jsGreaterThanOrEqual
    ||| GreaterThan <$ jsGreaterThan
    ||| LessThanOrEqual <$ jsLessThanOrEqual
    ||| LessThan <$ jsLessThan
  )

jsLogicalAnd = stringTok "&&"
jsLogicalOr = stringTok "||"
jsLogicalNot = stringTok "!"

jsLogicalOperator =
  spaces *> (
    LogicalAnd <$ jsLogicalAnd
    ||| LogicalOr <$ jsLogicalOr
    ||| LogicalNot <$ jsLogicalNot
  )


data Expr (v :: [*]) =
  Bool Boolean
  | String --todo
  | UniExp LogicalOperator (Expr v)
  | BinExp LogicalOperator (Expr v) (Expr v)
  deriving (Eq, Ord, Show)

-- parse jsExpression "!true"
-- Result >< UniExp LogicalAnd (Bool JsTrue) (Bool JsTrue)
jsUniExp :: Parser (Expr v)
jsUniExp =
  do
    o <- jsLogicalOperator
    b <- jsBoolean
    pure (UniExp o (Bool b))
  
-- parse jsExpression "true && true"
-- Result >< BinExp LogicalAnd (Bool JsTrue) (Bool JsTrue)
jsBinExp :: Parser (Expr v)
jsBinExp =
  do
    b1 <- jsBoolean
    _ <- spaces
    o <- jsLogicalOperator -- ||| jsComparisonOperator
    _ <- spaces
    b2 <- jsBoolean
    pure (BinExp o (Bool b1) (Bool b2))

-- jsExpression :: Parser Expr
-- jsExpression =
--   do
--     b1 <- jsBoolean
--     _ <- spaces
--     o <- jsLogicalOperator
--     _ <- spaces
--     b2 <- jsBoolean
--     pure (BinExp o (Bool b1) (Bool b2))


