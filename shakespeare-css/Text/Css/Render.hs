module Text.Css.Render where

import Data.List
import Data.Maybe

import Numeric (showHex)

import Text.Css.Ast

class CssValue a where
  renderCss :: a -> String

instance CssValue Stylesheet where
  renderCss = concatMap renderCss . stylesheetStatements

instance CssValue Statement where
  renderCss (ImportStatement url queries) =
    "@import url(" ++ url ++ ")" ++
    (if null queries then "" else (" " ++ unwords queries))
    ++ ";"
  renderCss (MediaStatement types rules) =
    "@media " ++ intercalate "," types ++ "{" ++
    concatMap renderCss rules ++ "}"
  renderCss (PageStatement name decls) =
    "@page " ++ maybe "" (":" ++) name ++ "{" ++
    concatMap renderCss decls ++ "}"
  renderCss (RulesetStatement rs) =
    renderCss rs
  renderCss (AtRuleStatement name decls) =
    '@' : name ++ "{" ++
    concatMap renderCss decls ++ "}"

instance CssValue Ruleset where
  renderCss (Ruleset sel decls) =
    (unwords . map renderCss $ sel) ++ "{" ++
    (concatMap renderCss $ decls) ++ "}"
  renderCss (MixinDefExt sel args decls) =
    renderCss sel ++ "(" ++ concatMap renderCss args ++
    "){" ++ concatMap renderCss decls ++ "}"
  renderCss (VarDeclStatementExt name term) =
    '@' : name ++ ":" ++ renderCss term ++ ";"

instance CssValue MixinArgument where
  renderCss (MixinArgument name value) =
    name ++ ":" ++ renderCss value

instance CssValue Declaration where
  renderCss (PropertyDeclaration name expr prio) =
    name ++ ":" ++ renderCss expr ++
    (if prio then "!important" else "") ++ ";"
  renderCss (RulesetDeclarationExt rs) = renderCss rs
  renderCss (MixinApplicationExt selector) =
    renderCss selector ++ ";"

instance CssValue Selector where
  renderCss =
    concatMap renderElem . selectorElems
    where
      renderElem (Left sel) = renderCss sel
      renderElem (Right comb) = renderCss comb

instance CssValue SimpleSelector where
  renderCss (SimpleSelector ns name specs) =
    maybe "" (++"|") ns ++ fromMaybe "" name ++ concatMap renderCss specs

instance CssValue SimpleSelectorSpecifier where
  renderCss (AttributeSelector ns name mOp) =
    '[' : maybe "" (++"|") ns ++ name ++ maybe "" renderOp mOp ++ "]"
    where
      renderOp (operator, operand) =
        renderCss operator ++ renderString operand
  renderCss (ClassSelector name) =
    '.' : name
  renderCss (IDSelector name) =
    '#' : name
  renderCss (PseudoClassSelector name args) =
    ':' : name ++ (maybe "" (\ a -> '(' : a ++ ")") args)
  renderCss (PseudoElementSelector name args) =
    ':' : ':' : name ++ (maybe "" (\ a -> '(' : a ++ ")") args)
  renderCss (NotSelector selector) =
    ":not(" ++ renderCss selector ++ ")"
  renderCss (SuperblockSelectorExt) =
    "&"

instance CssValue AttributeOp where
  renderCss ExactlyOp = "="
  renderCss ElemSpaceOp = "~="
  renderCss ElemDashOp = "|="
  renderCss BeginsWithOp = "^="
  renderCss EndsWithOp = "$="
  renderCss ContainsOp = "*="

instance CssValue Combinator where
  renderCss DescendantCombinator = " "
  renderCss ChildCombinator = ">"
  renderCss SiblingCombinator = "+"
  renderCss GeneralSiblingCombinator = "~"

instance CssValue Expression where
  renderCss (Expression elems) =
    concatMap renderTermExpr elems
    where
      renderTermExpr (Left term) = renderCss term
      renderTermExpr (Right op) = renderCss op

instance CssValue ExprOperator where
  renderCss SplitExprOperator = "/"
  renderCss SeqExprOperator = ","
  renderCss SpaceExprOperator = " "

instance CssValue Term where
  renderCss (ValueTerm v) = renderCss v
  renderCss (NegateTerm t) = "-" ++ renderCss t
  renderCss (AbsTerm t) = "+" ++ renderCss t
  renderCss (FunctionTerm name expr) =
    name ++ '(' : renderCss expr ++ ")"
  renderCss (ParensTermExt term) =
    '(' : renderCss term ++ ")"
  renderCss (AddTermExt t1 t2) =
    renderCss t1 ++ "+" ++ renderCss t2
  renderCss (SubTermExt t1 t2) =
    renderCss t1 ++ "-" ++ renderCss t2
  renderCss (MulTermExt t1 t2) =
    renderCss t1 ++ "*" ++ renderCss t2
  renderCss (DivTermExt t1 t2) =
    renderCss t1 ++ "/" ++ renderCss t2

instance CssValue Value where
  renderCss (NumberValue d) = renderDouble d
  renderCss (PercentageValue d) = renderDouble d ++ "%"
  renderCss (LengthValue unit d) = renderDouble d ++ renderCss unit
  renderCss (EmsValue d) = renderDouble d ++ "em"
  renderCss (ExsValue d) = renderDouble d ++ "ex"
  renderCss (AngleValue unit d) = renderDouble d ++ renderCss unit
  renderCss (TimeValue unit d) = renderDouble d ++ renderCss unit
  renderCss (FreqValue unit d) = renderDouble d ++ renderCss unit
  renderCss (DimensionValue dim d) = renderDouble d ++ dim
  renderCss (StringValue str) = renderString str
  renderCss (IdentValue ident) = renderIdent ident
  renderCss (UriValue uri) = "uri(" ++ renderString uri ++ ")"
  renderCss (HexcolorValue c) = renderCss c
  renderCss (VariableValueExt v) = renderCss v
  renderCss (EscapedStringValueExt e) = e

instance CssValue Variable where
  renderCss (PlainVariable name) = '@' : name
  renderCss (VariableRef var) = '@' : renderCss var

instance CssValue Color where
  renderCss (Color rd gd bd ad) =
    if ad == 1
    then renderHash rd gd bd
    else renderRgba rd gd bd ad
    where
      renderHash r g b = '#' : toHex r ++ toHex g ++ toHex b
      renderRgba r g b a = "rgba(" ++
                           show8bit r ++ "," ++ show8bit g ++ "," ++
                           show8bit b ++ "," ++ show8bit a ++ ")"
      toHex = pad '0' 2 . flip showHex "" . to8bit
      show8bit = show . to8bit
      to8bit :: (RealFrac a) => a -> Int
      to8bit = (* 255) . round . clamp 0 1
      pad a n xs =
        let len = length xs
        in if len < n then replicate (n - len) a ++ xs else xs
      clamp mi ma v
        | v < mi = mi
        | v > ma = ma
        | otherwise = v

instance CssValue LengthUnit where
  renderCss LengthPixels = "px"
  renderCss LengthCentimeters = "cm"
  renderCss LengthMillimeters = "mm"
  renderCss LengthInches = "in"
  renderCss LengthPoints = "pt"
  renderCss LengthPica = "pc"

instance CssValue AngleUnit where
  renderCss AngleDegrees = "deg"
  renderCss AngleRadians = "rad"
  renderCss AngleGradians = "grad"
  renderCss AngleTurns = "turns"

instance CssValue TimeUnit where
  renderCss TimeSeconds = "s"
  renderCss TimeMilliseconds = "ms"

instance CssValue FreqUnit where
  renderCss FreqHerz = "hz"
  renderCss FreqKiloHerz = "khz"

renderDouble :: Double -> String
renderDouble d =
  if roundedD == d
  then show rounded
  else show d
  where
    rounded = round d :: Integer
    roundedD = fromIntegral rounded

renderString :: String -> String
renderString =
  (++"\"") . ('"':) . concatMap addQuotes
  where
    addQuotes '"' = "\\\""
    addQuotes x = [x]

renderIdent :: String -> String
renderIdent =
  id -- TODO escape chars that are not ("a-zA-Z_0-9-" or non-ascii),
     -- and escape "0-9-" in the beginning
