{-# LANGUAGE OverloadedStrings #-}
module Text.Css.Render where

import Data.Bits
import Data.List (intersperse)
import Data.Monoid
import Data.String
import Data.Text.Lazy.Builder (Builder)

import Text.Css.Ast

-- | Something that can be rendered as CSS
class CssValue a where
  -- | Renders a value to CSS
  renderCss :: a -> Builder

instance CssValue Stylesheet where
  renderCss = concatMapB renderCss . stylesheetStatements

instance CssValue Statement where
  renderCss (ImportStatement url queries) =
    "@import " <> renderCss url <>
    (if null queries then "" else (" " <> unwordsB (strL queries)))
    <> ";"
  renderCss (MediaStatement types rules) =
    "@media " <> intercalateB "," (strL types) <> "{" <>
    concatMapB renderCss rules <> "}"
  renderCss (PageStatement name decls) =
    "@page " <> maybe "" ((":" <>) . str) name <> "{" <>
    concatMapB renderCss decls <> "}"
  renderCss (RulesetStatement rs) =
    renderCss rs
  renderCss (AtRuleStatement name decls) =
    "@" <> str name <> "{" <>
    concatMapB renderCss decls <> "}"

instance CssValue Ruleset where
  renderCss (Ruleset sel decls) =
    (unwordsB . map renderCss $ sel) <> "{" <>
    (concatMapB renderCss $ decls) <> "}"
  renderCss (MixinDefExt sel args decls) =
    renderCss sel <> "(" <> concatMapB renderCss args <>
    "){" <> concatMapB renderCss decls <> "}"
  renderCss (VarDeclStatementExt name term) =
    "@" <> str name <> ":" <> renderCss term <> ";"

instance CssValue MixinArgument where
  renderCss (MixinArgument name value) =
    str name <> ":" <> renderCss value

instance CssValue Declaration where
  renderCss (PropertyDeclaration name expr prio) =
    str name <> ":" <> renderCss expr <>
    (if prio then "!important" else "") <> ";"
  renderCss (RulesetDeclarationExt rs) = renderCss rs
  renderCss (MixinApplicationExt selector) =
    renderCss selector <> ";"

instance CssValue Selector where
  renderCss =
    concatMapB renderElem . selectorElems
    where
      renderElem (Left sel) = renderCss sel
      renderElem (Right comb) = renderCss comb

instance CssValue SimpleSelector where
  renderCss (SimpleSelector ns name specs) =
    maybe "" ((<>"|") . str) ns <> maybe "" str name <>
    concatMapB renderCss specs

instance CssValue SimpleSelectorSpecifier where
  renderCss (AttributeSelector ns name mOp) =
    "[" <> maybe "" ((<>"|") . str) ns <> str name <>
    maybe "" renderOp mOp <> "]"
    where
      renderOp (operator, operand) =
        renderCss operator <> renderString operand
  renderCss (ClassSelector name) =
    "." <> str name
  renderCss (IDSelector name) =
    "#" <> str name
  renderCss (PseudoClassSelector name args) =
    ":" <> str name <> (maybe "" (\ a -> "(" <> str a <> ")") args)
  renderCss (PseudoElementSelector name args) =
    "::" <> str name <> (maybe "" (\ a -> "(" <> str a <> ")") args)
  renderCss (NotSelector selector) =
    ":not(" <> renderCss selector <> ")"
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
    concatMapB renderTermExpr elems
    where
      renderTermExpr (Left term) = renderCss term
      renderTermExpr (Right op) = renderCss op

instance CssValue ExprOperator where
  renderCss SplitExprOperator = "/"
  renderCss SeqExprOperator = ","
  renderCss SpaceExprOperator = " "

instance CssValue Term where
  renderCss (ValueTerm v) = renderCss v
  renderCss (NegateTerm t) = "-" <> renderCss t
  renderCss (AbsTerm t) = "+" <> renderCss t
  renderCss (FunctionTerm name expr) =
    str name <> "(" <> renderCss expr <> ")"
  renderCss (ParensTermExt term) =
    "(" <> renderCss term <> ")"
  renderCss (AddTermExt t1 t2) =
    renderCss t1 <> "+" <> renderCss t2
  renderCss (SubTermExt t1 t2) =
    renderCss t1 <> "-" <> renderCss t2
  renderCss (MulTermExt t1 t2) =
    renderCss t1 <> "*" <> renderCss t2
  renderCss (DivTermExt t1 t2) =
    renderCss t1 <> "/" <> renderCss t2

instance CssValue Value where
  renderCss (NumberValue d) = renderDouble d
  renderCss (PercentageValue d) = renderDouble d <> "%"
  renderCss (UnitValue unit d) = renderDouble d <> renderCss unit
  renderCss (DimensionValue dim d) = renderDouble d <> str dim
  renderCss (StringValue s) = renderString s
  renderCss (IdentValue ident) = renderIdent ident
  renderCss (UriValue uri) = renderCss uri
  renderCss (HexcolorValue c) = renderCss c
  renderCss (EscapedStringValueExt e) = str e
  renderCss (VariableValueExt v) = renderCss v
  renderCss (SplicedValueExt _) = "?value splice?"

instance CssValue Uri where
  renderCss (PlainUri uri) = "url(" <> renderString uri <> ")"
  renderCss (SplicedUriExt _) = "?uri splice?"
  renderCss (SplicedUriParamExt _) = "?uri param splice?"

instance CssValue Variable where
  renderCss (PlainVariable name) = "@" <> str name
  renderCss (VariableRef var) = "@" <> renderCss var

instance CssValue Color where
  renderCss (Color rd gd bd ad) =
    if ad == 1
    then renderHash rd gd bd
    else renderRgba rd gd bd ad
    where
      renderHash r g b = "#" <> reduce (toHex r, toHex g, toHex b)
      renderRgba r g b a =
        "rgba(" <>
        show8bit r <> "," <> show8bit g <> "," <>
        show8bit b <> "," <> show8bit a <> ")"
      reduce ((r1, r2), (g1, g2), (b1, b2))
        | r1 == r2 &&
          g1 == g2 &&
          b1 == b2  = concatMapB (str . return) [r1, g1, b1]
        | otherwise = concatMapB (str . return) [r1, r2, g1, g2, b1, b2]
      show8bit = str . show . to8bit
      to8bit :: (RealFrac a) => a -> Int
      to8bit = (* 255) . round . clamp 0 1
      toHex = (\ x -> (toChar $ shiftR x 4, toChar $ x .&. 15)) . to8bit
      toChar c
        | c < 10    = mkChar c 0 '0'
        | otherwise = mkChar c 10 'a'
      mkChar a b' c =
        toEnum $ fromIntegral $ a - b' + fromIntegral (fromEnum c)
      clamp mi ma v
        | v < mi = mi
        | v > ma = ma
        | otherwise = v

instance CssValue Unit where
  renderCss LengthPixels = "px"
  renderCss LengthCentimeters = "cm"
  renderCss LengthMillimeters = "mm"
  renderCss LengthInches = "in"
  renderCss LengthPoints = "pt"
  renderCss LengthPica = "pc"
  renderCss AngleDegrees = "deg"
  renderCss AngleRadians = "rad"
  renderCss AngleGradians = "grad"
  renderCss AngleTurns = "turns"
  renderCss TimeSeconds = "s"
  renderCss TimeMilliseconds = "ms"
  renderCss FreqHerz = "hz"
  renderCss FreqKiloHerz = "khz"
  renderCss Ems = "em"
  renderCss Exs = "ex"

renderDouble :: (IsString a) => Double -> a
renderDouble d =
  if roundedD == d
  then str . show $ rounded
  else str . show $ d
  where
    rounded = round d :: Integer
    roundedD = fromIntegral rounded

renderString :: (IsString a, Monoid a) => String -> a
renderString =
  (<>"\"") . ("\""<>) . concatMapB addQuotes
  where
    addQuotes '"' = str "\\\""
    addQuotes x = str [x]

renderIdent :: (IsString a) => String -> a
renderIdent =
  str
  -- TODO escape chars that are not ("a-zA-Z_0-9-" or non-ascii),
  -- and escape "0-9-" in the beginning

unwordsB :: (IsString a, Monoid a) => [a] -> a
unwordsB [] = ""
unwordsB ws = foldr1 (\ w s -> w <> " " <> s) ws

intercalateB :: (IsString a, Monoid a) => a -> [a] -> a
intercalateB b = concatB . intersperse b

concatB :: (IsString a, Monoid a) => [a] -> a
concatB = foldr (<>) ""

concatMapB :: (IsString b, Monoid b) => (a -> b) -> [a] -> b
concatMapB f = concatB . map f

strL :: (IsString a) => [String] -> [a]
strL = map str

str :: (IsString a) => String -> a
str = fromString

(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend
infixr 4 <> -- D.T.L.Builder is right-assoc optimized, etc
