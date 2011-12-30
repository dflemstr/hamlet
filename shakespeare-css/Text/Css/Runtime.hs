{-# LANGUAGE TemplateHaskell, GADTs, FlexibleInstances, StandaloneDeriving #-}
module Text.Css.Runtime where

import Data.Text.Lazy.Builder (Builder)

import Text.ParserCombinators.Parsec (parse)

import Text.Css.Ast
import qualified Text.Css.Parser as CssParser

-- | Runtime representation of a Css expression,
-- parameterized by the expression type it represents.
data CssExpr a where
  StylesheetE
    :: [CssExpr Statement] -> CssExpr Stylesheet

  ImportStatementE
    :: CssExpr Uri -> Builder -> CssExpr Statement
  MediaStatementE
    :: Builder -> [CssExpr Ruleset] -> CssExpr Statement
  PageStatementE
    :: Builder -> [CssExpr Declaration] -> CssExpr Statement
  RulesetStatementE
    :: CssExpr Ruleset -> CssExpr Statement
  AtRuleStatementE
    :: Builder -> [CssExpr Declaration] -> CssExpr Statement

  RulesetE
    :: [CssExpr Selector] -> [CssExpr Declaration] -> CssExpr Ruleset
  MixinDefExtE
    :: Builder -> [CssExpr MixinArgument]
       -> [CssExpr Declaration] -> CssExpr Ruleset
  VarDeclStatementExtE
    :: Builder -> [CssExpr Term] -> CssExpr Ruleset

  MixinArgumentE
    :: Builder -> [CssExpr Value] -> CssExpr MixinArgument

  PropertyDeclarationE
    :: Builder -> CssExpr Expression -> Bool -> CssExpr Declaration
  RulesetDeclarationExtE
    :: CssExpr Ruleset -> CssExpr Declaration
  MixinApplicationExtE
    :: Builder -> CssExpr Declaration

  SelectorE
    :: [ Either
         Builder -- Prepend parent to this dynamic chunk
         Builder -- Rendered non-dynamic chunk
       ] -> CssExpr Selector

  ExpressionE
    :: [Either (CssExpr Term) ExprOperator] -> CssExpr Expression

  ValueTermE
    :: CssExpr Value -> CssExpr Term
  NegateTermE
    :: CssExpr Term -> CssExpr Term
  AbsTermE
    :: CssExpr Term -> CssExpr Term
  FunctionTermE
    :: Builder -> CssExpr Expression -> CssExpr Term
  ParensTermExtE
    :: CssExpr Term -> CssExpr Term
  AddTermExtE
    :: CssExpr Term -> CssExpr Term -> CssExpr Term
  SubTermExtE
    :: CssExpr Term -> CssExpr Term -> CssExpr Term
  MulTermExtE
    :: CssExpr Term -> CssExpr Term -> CssExpr Term
  DivTermExtE
    :: CssExpr Term -> CssExpr Term -> CssExpr Term

  NumberValueE
    :: Double -> CssExpr Value
  PercentageValueE
    :: Double -> CssExpr Value
  UnitValueE
    :: Unit -> Double -> CssExpr Value
  DimensionValueE
    :: Builder -> Double -> CssExpr Value
  StringValueE
    :: Builder -> CssExpr Value
  IdentValueE
    :: Builder -> CssExpr Value
  UriValueE
    :: CssExpr Uri -> CssExpr Value
  HexcolorValueE
    :: Color -> CssExpr Value
  EscapedStringValueExtE
    :: Builder -> CssExpr Value
  VariableValueExtE
    :: Variable -> CssExpr Value

  PlainUriE
    :: Builder -> CssExpr Uri

  PlainVariableE
    :: Builder -> CssExpr Variable
  VariableRefE
    :: CssExpr Variable -> CssExpr Variable

deriving instance Show a => Show (CssExpr a)

class CssValue a where
  toCssValue :: a -> Value

instance CssValue [Char] where
  toCssValue = parseValue

class CssUri a where
  toCssUri :: a -> Uri

instance CssUri [Char] where
  toCssUri = parseUri

parseValue :: String -> Value
parseValue =
  handleResult . parse CssParser.valueParser "value splice"
  where
    handleResult (Right v) = v
    handleResult (Left err) = error . show $ err

parseUri :: String -> Uri
parseUri =
  handleResult . parse CssParser.uriParser "uri splice"
  where
    handleResult (Right u) = u
    handleResult (Left err) = error . show $ err
