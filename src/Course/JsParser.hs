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

data JsBinOp =
  JsBinEq
  | JsBinNotEq
  | JsBinStrictEq
  | JsBinStrictNotEq
  | JsBinGt
  | JsBinGe
  | JsBinLt
  | JsBinLe
  | JsBinAnd
  | JsBinOr
  deriving (Eq, Ord, Show)

data JsUnaryOp =
  JsUnaryOpNot
  deriving (Eq, Ord, Show)

data JsBoolean =
  JsTrue
  | JsFalse
  deriving (Eq, Ord, Show)

jsTrue :: Parser Chars
jsTrue = stringTok "true"

jsFalse :: Parser Chars
jsFalse = stringTok "false"

jsBoolean :: Parser JsBoolean
jsBoolean =
  JsTrue <$ jsTrue ||| JsFalse <$ jsFalse

jsBinEqual = stringTok "=="
jsBinNotEqual = stringTok "!="
jsBinStrictEqual = stringTok "==="
jsBinStrictNotEqual = stringTok "!=="
jsBinGreaterThan = stringTok ">"
jsBinGreaterThanOrEqual = stringTok ">="
jsBinLessThan = stringTok "<"
jsBinLessThanOrEqual = stringTok "<="
jsBinAnd = stringTok "&&"
jsBinOr = stringTok "||"
jsBinNot = stringTok "!"

jsBinOperator :: Parser JsBinOp
jsBinOperator = 
  spaces *> (
    JsBinStrictNotEq <$ jsBinStrictNotEqual
    ||| JsBinStrictEq <$ jsBinStrictEqual
    ||| JsBinNotEq <$ jsBinNotEqual
    ||| JsBinEq <$ jsBinEqual
    ||| JsBinGe <$ jsBinGreaterThanOrEqual
    ||| JsBinGt <$ jsBinGreaterThan
    ||| JsBinLe <$ jsBinLessThanOrEqual
    ||| JsBinLt <$ jsBinLessThan
    ||| JsBinAnd <$ jsBinAnd
    ||| JsBinOr <$ jsBinOr
  )

jsUnaryOp :: Parser JsUnaryOp
jsUnaryOp = 
  spaces *> (
    JsUnaryOpNot <$ jsBinNot
  )

data JsExpr =
  JsBoolExpr JsBoolean
  | JsParenExpr JsExpr
  | JsUnaryExpr (JsUnaryOp, JsExpr)
  | JsBinExpr (JsExpr, JsBinOp, JsExpr)
  deriving (Eq, Ord, Show)

-- parse jsUnaryExpr "!true"
-- Result >< JsUnaryExp LogicalAnd (Bool JsTrue) (Bool JsTrue)
jsUnaryExpr :: Parser (JsUnaryOp, JsExpr)
jsUnaryExpr =
  do
    o <- jsUnaryOp
    b <- jsExpr
    pure (o, b)
  
-- parse jsBinExpr "true && true"
-- Result >< BinExp LogicalAnd (Bool JsTrue) (Bool JsTrue)
-- parse jsBinExpr "true && (true || false)"

jsBinExpr :: Parser (JsExpr, JsBinOp, JsExpr)
jsBinExpr =
  do
    b1 <- jsExpr
    _ <- spaces
    o <- jsBinOperator
    _ <- spaces
    b2 <- jsExpr
    pure (b1, o, b2)

jsExpr :: Parser JsExpr
jsExpr =
  (
    JsBoolExpr <$> jsBoolean
    ||| JsBinExpr <$> jsBinExpr
    ||| JsUnaryExpr <$> jsUnaryExpr
    
  )
