{-# LANGUAGE TemplateHaskell, GADTs #-}
module Text.Css.Runtime where

import Data.String
import Data.Text.Lazy.Builder (Builder)

import Text.ParserCombinators.Parsec (parse)

import Text.Css.Ast
import qualified Text.Css.Parser as CssParser

-- | Runtime representation of a Css expression,
-- parameterized by the expression type it represents.
data CssExpr a where
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
    :: Builder -> [CssExpr Declaration] -> CssExpr Ruleset
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

parseValue :: String -> CssExpr Value
parseValue =
  handleResult . parse CssParser.valueParser "value splice"
  where
    handleResult (Right v) =
      case v of
        NumberValue d -> NumberValueE d
        PercentageValue d -> PercentageValueE d
        UnitValue u d -> UnitValueE u d
        DimensionValue dim d ->
          DimensionValueE (fromString dim) d
        StringValue s ->
          StringValueE . fromString $ s
        IdentValue idf ->
          IdentValueE . fromString $ idf
        UriValue (PlainUri uri) ->
          UriValueE . PlainUriE . fromString $ uri
        UriValue _ ->
          error $
          "Cannot refer to URI splices from the result of a value splice"
        HexcolorValue color -> HexcolorValueE color
        EscapedStringValueExt esc ->
          EscapedStringValueExtE . fromString $ esc
        VariableValueExt var ->
          VariableValueExtE var
        SplicedValueExt _ ->
          error $
          "Cannot refer to value splices from the result of a value splice"
    handleResult (Left err) = error . show $ err

parseUri :: String -> CssExpr Uri
parseUri =
  handleResult . parse CssParser.uriParser "uri splice"
  where
    handleResult (Right u) =
      case u of
        PlainUri uri ->
          PlainUriE . fromString $ uri
        _ ->
          error "Cannot refer to URI splices from the result of an URI splice"
    handleResult (Left err) = error . show $ err
