{-# LANGUAGE TemplateHaskell, GADTs #-}
module Text.Css.Runtime where

import Data.String
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder

import Text.ParserCombinators.Parsec (parse)

import Text.Css.Ast
import qualified Text.Css.Parser as CssParser
import Text.Css.Render

-- | Runtime representation of a Css expression,
-- parameterized by the expression type it represents, and the type
-- that the AST has been partially rendered to.
data CssExpr a s where
  ImportStatementE
    :: CssExpr Uri s -> s -> CssExpr Statement s
  MediaStatementE
    :: s -> [CssExpr Ruleset s] -> CssExpr Statement s
  PageStatementE
    :: s -> [CssExpr Declaration s] -> CssExpr Statement s
  RulesetStatementE
    :: CssExpr Ruleset s -> CssExpr Statement s
  AtRuleStatementE
    :: s -> [CssExpr Declaration s] -> CssExpr Statement s

  RulesetE
    :: s -> [CssExpr Declaration s] -> CssExpr Ruleset s
  MixinDefExtE
    :: s -> [CssExpr MixinArgument s]
       -> [CssExpr Declaration s] -> CssExpr Ruleset s
  VarDeclStatementExtE
    :: s -> [CssExpr Term s] -> CssExpr Ruleset s

  MixinArgumentE
    :: s -> [CssExpr Value s] -> CssExpr MixinArgument s

  PropertyDeclarationE
    :: s -> CssExpr Expression s -> Bool -> CssExpr Declaration s
  RulesetDeclarationExtE
    :: CssExpr Ruleset s -> CssExpr Declaration s
  MixinApplicationExtE
    :: s -> CssExpr Declaration s

  ExpressionE
    :: [Either (CssExpr Term s) ExprOperator] -> CssExpr Expression s

  ValueTermE
    :: CssExpr Value s -> CssExpr Term s
  NegateTermE
    :: CssExpr Term s -> CssExpr Term s
  AbsTermE
    :: CssExpr Term s -> CssExpr Term s
  FunctionTermE
    :: s -> CssExpr Expression s -> CssExpr Term s
  ParensTermExtE
    :: CssExpr Term s -> CssExpr Term s
  AddTermExtE
    :: CssExpr Term s -> CssExpr Term s -> CssExpr Term s
  SubTermExtE
    :: CssExpr Term s -> CssExpr Term s -> CssExpr Term s
  MulTermExtE
    :: CssExpr Term s -> CssExpr Term s -> CssExpr Term s
  DivTermExtE
    :: CssExpr Term s -> CssExpr Term s -> CssExpr Term s

  NumberValueE
    :: Double -> CssExpr Value s
  PercentageValueE
    :: Double -> CssExpr Value s
  UnitValueE
    :: Unit -> Double -> CssExpr Value s
  DimensionValueE
    :: s -> Double -> CssExpr Value s
  StringValueE
    :: s -> CssExpr Value s
  IdentValueE
    :: s -> CssExpr Value s
  UriValueE
    :: CssExpr Uri s -> CssExpr Value s
  HexcolorValueE
    :: Color -> CssExpr Value s
  EscapedStringValueExtE
    :: s -> CssExpr Value s
  VariableValueExtE
    :: Variable -> CssExpr Value s

  PlainUriE
    :: s -> CssExpr Uri s

  PlainVariableE
    :: s -> CssExpr Variable s
  VariableRefE
    :: CssExpr Variable s -> CssExpr Variable s

parseValue :: (CssValue a, IsString s) => a -> CssExpr Value s
parseValue =
  handleResult . parse CssParser.valueParser "value splice" .
  Text.unpack . Builder.toLazyText . renderCss
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

parseUri :: (CssValue a, IsString s) => a -> CssExpr Uri s
parseUri =
  handleResult . parse CssParser.uriParser "uri splice" .
  Text.unpack . Builder.toLazyText . renderCss
  where
    handleResult (Right u) =
      case u of
        PlainUri uri ->
          PlainUriE . fromString $ uri
        _ ->
          error "Cannot refer to URI splices from the result of an URI splice"
    handleResult (Left err) = error . show $ err
