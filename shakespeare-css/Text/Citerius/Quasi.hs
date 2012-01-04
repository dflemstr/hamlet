{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Text.Citerius.Quasi where

import Data.List
import qualified Data.Text.Lazy.Builder as Builder

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Text.Shakespeare.Base

import Text.ParserCombinators.Parsec (parse)

import Text.Citerius.Parser
import Text.Css.Ast
import Text.Css.Convert
import Text.Css.Render
import Text.Css.Runtime

citerius :: QuasiQuoter
citerius =
  QuasiQuoter
  { quoteExp  = citeriusFromString
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

citeriusFromString :: String -> Q Exp
citeriusFromString =
  either (error . show) runSpliceResolver .
  parse stylesheetParser "quasiquote"

parseValue :: String -> Value
parseValue =
  either (error . show) id .  parse valueParser "value string"

parseUri :: String -> Uri
parseUri =
  either (error . show) id . parse uriParser "uri string"

runSpliceResolver :: Spliced a => a -> Q Exp
runSpliceResolver a = do
  n <- newName "_render"
  lamE [varP n] $ resolveSplices n a

class Spliced a where
  resolveSplices :: Name -> a -> Q Exp

instance Spliced Stylesheet where
  resolveSplices n (Stylesheet smts) =
    [| StylesheetE $(resolveSplices n smts) |]

instance Spliced Statement where
  resolveSplices n smt =
    case smt of
      ImportStatement uri qs ->
        [| ImportStatementE
           $(resolveSplices n uri)
           $(builderFromString $
             if null qs
             then ""
             else " " <> intercalate "," qs)
         |]
      MediaStatement qs rs ->
        [| MediaStatementE
           $(builderFromString . intercalate "," $ qs)
           $(resolveSplices n rs)
         |]
      PageStatement nam decls ->
        [| PageStatementE
           $(builderFromString $ maybe "" ((":"<>) . renderIdent) nam)
           $(resolveSplices n decls)
         |]
      RulesetStatement r ->
        [| RulesetStatementE $(resolveSplices n r) |]
      AtRuleStatement name decls ->
        [| AtRuleStatementE
           $(builderFromString . renderIdent $ name)
           $(resolveSplices n decls)
         |]

instance Spliced Ruleset where
  resolveSplices n rs =
    case rs of
      Ruleset sels decls ->
        [| RulesetE
           $(resolveSplices n sels)
           $(resolveSplices n decls)
         |]
      MixinDefExt sel args decls ->
        [| MixinDefExt
           $(resolveSplices n sel)
           $(resolveSplices n args)
           $(resolveSplices n decls)
         |]
      VarDeclStatementExt name term ->
        [| VarDeclStatementExtE
           $(lift name)
           $(resolveSplices n term)
         |]

instance Spliced MixinArgument where
  resolveSplices n (MixinArgument name val) =
    [| MixinArgumentE
       $(lift name)
       $(resolveSplices n val)
     |]

instance Spliced Declaration where
  resolveSplices n decl =
    case decl of
      PropertyDeclaration name expr bool ->
        [| PropertyDeclarationE
           $(builderFromString . renderIdent $ name)
           $(resolveSplices n expr)
           $(lift bool)
         |]
      RulesetDeclarationExt rs ->
        [| RulesetDeclarationExtE $(resolveSplices n rs) |]
      MixinApplicationExt sel args ->
        [| MixinApplicationExtE
           $(builderFromString . renderCss $ sel)
           $(resolveSplices n args)
         |]

instance Spliced Selector where
  resolveSplices _ (Selector elems) =
    [| SelectorE
       $(fmap ListE . mapM mkBuilders .
         squashRight . map annotElem $ elems)
     |]
    where
      annotElem (Right c) = Right . renderCss $ c
      annotElem (Left (SimpleSelector SelectorParentExt specs)) =
        Left . concatMapB renderCss $ specs
      annotElem (Left s) =
        Right . renderCss $ s
      mkBuilders (Left s)  = [| Left  $(builderFromString .
                                        renderIdent $ s) |]
      mkBuilders (Right s) = [| Right $(builderFromString .
                                        renderIdent $ s) |]

instance Spliced Expression where
  resolveSplices n (Expression elems) =
    [| ExpressionE $(resolveSplices n elems) |]

instance Spliced ExprOperator where
  resolveSplices = const lift

instance Spliced Term where
  resolveSplices n term =
    case term of
      ValueTerm val -> [| ValueTermE $(resolveSplices n val) |]
      NegateTerm t -> [| NegateTermE $(resolveSplices n t) |]
      AbsTerm t -> [| AbsTermE $(resolveSplices n t) |]
      FunctionTerm name e ->
        [| FunctionTermE
           $(lift name)
           $(resolveSplices n e)
         |]
      ParensTermExt t -> [| ParensTermExtE $(resolveSplices n t) |]
      AddTermExt t1 t2 ->
        [| AddTermExtE
           $(resolveSplices n t1)
           $(resolveSplices n t2)
         |]
      SubTermExt t1 t2 ->
        [| SubTermExtE
           $(resolveSplices n t1)
           $(resolveSplices n t2)
         |]
      MulTermExt t1 t2 ->
        [| MulTermExtE
           $(resolveSplices n t1)
           $(resolveSplices n t2)
         |]
      DivTermExt t1 t2 ->
        [| DivTermExtE
           $(resolveSplices n t1)
           $(resolveSplices n t2)
         |]

instance Spliced Value where
  resolveSplices n val =
    case val of
      NumberValue d ->
        [| NumberValueE (fromRational $(rationalLit d)) |]
      PercentageValue d ->
        [| PercentageValueE (fromRational $(rationalLit d)) |]
      UnitValue unit d ->
        [| UnitValueE $(lift unit) (fromRational $(rationalLit d)) |]
      DimensionValue dim d ->
        [| DimensionValueE
           $(builderFromString . renderIdent $ dim)
           (fromRational $(rationalLit d))
         |]
      StringValue s ->
        [| StringValueE $(builderFromString . renderString $ s) |]
      IdentValue i ->
        [| IdentValueE $(builderFromString . renderIdent $ i) |]
      UriValue uri ->
        [| UriValueE $(resolveSplices n uri) |]
      HexcolorValue v ->
        [| HexcolorValueE $(lift v) |]
      EscapedStringValueExt esc ->
        [| EscapedStringValueExtE $(builderFromString esc) |]
      VariableValueExt var ->
        [| VariableValueExtE $(lift var) |]
      SplicedValueExt ref ->
        [| liftValue . toCssValue |] `appE` return (derefToExp [] ref)

instance Spliced Uri where
  resolveSplices n uri =
    case uri of
      PlainUri u ->
        [| PlainUriE $(builderFromString . renderString $ u) |]
      SplicedUriExt ref ->
        [| liftUri . toCssUri |] `appE` return (derefToExp [] ref)
      SplicedUriParamExt ref ->
        [| liftUri . toCssUri |] `appE`
        ([| uncurry |] `appE` varE n `appE` return (derefToExp [] ref))

-- We don't want "instance Lift a => Spliced a" because
-- UndecidableInstances is evil and can't possibly be combined with
-- the class specializations we do above, so do some gymnastics instead

instance Spliced a => Spliced [a] where
  resolveSplices n = fmap ListE . mapM (resolveSplices n)

instance (Spliced a, Spliced b) => Spliced (Either a b) where
  resolveSplices n e =
    case e of
      Left  x -> [| Left  $(resolveSplices n x) |]
      Right x -> [| Right $(resolveSplices n x) |]

instance Lift ExprOperator where
  lift SplitExprOperator = [| SplitExprOperator |]
  lift SeqExprOperator   = [| SeqExprOperator |]
  lift SpaceExprOperator = [| SpaceExprOperator |]

instance Lift Variable where
  lift (PlainVariable var) =
    [| PlainVariableE $(lift var) |]
  lift (VariableRef var) = [| VariableRefE $(lift var) |]

instance Lift Color where
  lift (Color r g b a) =
    [| Color
       (fromRational $(rationalLit r))
       (fromRational $(rationalLit g))
       (fromRational $(rationalLit b))
       (fromRational $(rationalLit a))
     |]

instance Lift Unit where
  lift LengthPixels = [| LengthPixels |]
  lift LengthCentimeters = [| LengthCentimeters |]
  lift LengthMillimeters = [| LengthMillimeters |]
  lift LengthInches = [| LengthInches |]
  lift LengthPoints = [| LengthPoints |]
  lift LengthPica = [| LengthPica |]
  lift AngleDegrees = [| AngleDegrees |]
  lift AngleRadians = [| AngleRadians |]
  lift AngleGradians = [| AngleGradians |]
  lift AngleTurns = [| AngleTurns |]
  lift TimeSeconds = [| TimeSeconds |]
  lift TimeMilliseconds = [| TimeMilliseconds |]
  lift FreqHerz = [| FreqHerz |]
  lift FreqKiloHerz = [| FreqKiloHerz |]
  lift Ems = [| Ems |]
  lift Exs = [| Exs |]

builderFromString :: String -> Q Exp
builderFromString s =
  [| Builder.fromString |] `appE` litE (StringL s)

rationalLit :: (Real a) => a -> Q Exp
rationalLit = return . LitE . RationalL . toRational
