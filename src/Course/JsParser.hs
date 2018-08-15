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

data JsBoolean =
  JsTrue
  | JsFalse
  deriving (Eq, Ord, Show)

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

data JsExpr =
  JsBoolExpr JsBoolean
  | JsParenExpr JsExpr
  | JsUnaryExpr (JsUnaryOp, JsExpr)
  | JsBinExpr (JsExpr, JsBinOp, JsExpr)
  deriving (Eq, Ord, Show)

jsCommaTok :: Parser Chars
jsCommaTok = stringTok ";"

jsTrue :: Parser Chars
jsTrue = stringTok "true"

jsFalse :: Parser Chars
jsFalse = stringTok "false"

jsBoolean :: Parser JsBoolean
jsBoolean = JsTrue <$ jsTrue ||| JsFalse <$ jsFalse

jsBinEqual :: Parser Chars
jsBinEqual = stringTok "=="

jsBinNotEqual :: Parser Chars
jsBinNotEqual = stringTok "!="

jsBinStrictEqual :: Parser Chars
jsBinStrictEqual = stringTok "==="

jsBinStrictNotEqual :: Parser Chars
jsBinStrictNotEqual = stringTok "!=="

jsBinGreaterThan :: Parser Chars
jsBinGreaterThan = stringTok ">"

jsBinGreaterThanOrEqual :: Parser Chars
jsBinGreaterThanOrEqual = stringTok ">="

jsBinLessThan :: Parser Chars
jsBinLessThan = stringTok "<"

jsBinLessThanOrEqual :: Parser Chars
jsBinLessThanOrEqual = stringTok "<="

jsBinAnd :: Parser Chars
jsBinAnd = stringTok "&&"

jsBinOr :: Parser Chars
jsBinOr = stringTok "||"

jsBinNot :: Parser Chars 
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

-- parse jsExpr "true !== false"
jsExpr :: Parser JsExpr
jsExpr =
  (
    JsBoolExpr <$> jsBoolean
    |||  JsBinExpr <$> jsBinExpr
    ||| JsUnaryExpr <$> jsUnaryExpr
  )

jsIfTok :: Parser Chars
jsIfTok = stringTok "if"

jsLtParen :: Parser Chars
jsLtParen = stringTok "("

jsRtParen :: Parser Chars
jsRtParen = stringTok ")"

jsLtBrace :: Parser Chars
jsLtBrace = stringTok "{"

jsRtBrace :: Parser Chars
jsRtBrace = stringTok "}"

data JsStatement =
  JsStmtBlock (List JsStatement)
  | JSExpressionStatement JsExpr
  | JsIfStmt (JsExpr, JsStatement)
  deriving (Eq, Ord, Show)


-- parse jsIfStmt "if(true){true;}"
-- parse jsIfStmt "if (true) { true; }"
-- parse jsIfStmt "if(true===true){true;}"
jsIfStmt :: Parser (JsExpr, JsStatement)
jsIfStmt =
  do
    _ <- jsIfTok
    e1 <- betweenCharTok '(' ')' jsExpr
    e2 <- betweenCharTok '{' '}' jsStmtBlock
    pure (e1, JsStmtBlock e2)
 
jsStmtBlock :: Parser (List JsStatement)
jsStmtBlock =
  do
    s <- jsStatement
    _ <- jsCommaTok
    pure (s :. Nil)


jsStatement :: Parser JsStatement
jsStatement =
  (
    JSExpressionStatement <$> jsExpr
    ||| JsIfStmt <$> jsIfStmt  
    ||| JsStmtBlock <$> jsStmtBlock
  )
