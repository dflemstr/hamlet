{-# LANGUAGE OverloadedStrings #-}
module Text.Css.Runtime where

import Data.String
import Data.Text.Lazy.Builder (Builder)

import Text.Css.Ast
import Text.Css.Render

-- Builder: Value is fully escaped but not "quoted" or w/ context
-- String: Value is used at runtime for something other than
-- concatenation and Eq

newtype StylesheetR =
  StylesheetE [StatementR]
  deriving (Show)

data StatementR
  = ImportStatementE UriR Builder
  | MediaStatementE Builder [RulesetR]
  | PageStatementE Builder [DeclarationR]
  | RulesetStatementE RulesetR
  | AtRuleStatementE Builder [DeclarationR]
  deriving (Show)

data RulesetR
  = RulesetE [SelectorR] [DeclarationR]
  | MixinDefExtE SelectorR [MixinArgumentR] [DeclarationR]
  | VarDeclStatementExtE String TermR
  deriving (Show)

data MixinArgumentR
  = MixinArgumentE String ValueR
  deriving (Show)

data DeclarationR
  = PropertyDeclarationE Builder ExpressionR Bool
  | RulesetDeclarationExtE RulesetR
  | MixinApplicationExtE SelectorR [MixinArgumentR]
  deriving (Show)

data SelectorR
  = SelectorE
    [ Either
      Builder -- Prepend parent to this dynamic chunk
      Builder -- Rendered non-dynamic chunk
    ]
  deriving (Show)

data ExpressionR
  = ExpressionE [Either (TermR) ExprOperator]
  deriving (Show)

data TermR
  = ValueTermE ValueR
  | NegateTermE TermR
  | AbsTermE TermR
  | FunctionTermE String ExpressionR
  | ParensTermExtE TermR
  | AddTermExtE TermR TermR
  | SubTermExtE TermR TermR
  | MulTermExtE TermR TermR
  | DivTermExtE TermR TermR
  deriving (Show)

data ValueR
  = NumberValueE Double
  | PercentageValueE Double
  | UnitValueE Unit Double
  | DimensionValueE Builder Double
  | StringValueE Builder
  | IdentValueE Builder
  | UriValueE UriR
  | HexcolorValueE Color
  | EscapedStringValueExtE Builder
  | VariableValueExtE VariableR
  deriving (Show)

newtype UriR
  = PlainUriE Builder
  deriving (Show)

data VariableR
  = PlainVariableE String
  | VariableRefE VariableR
  deriving (Show)

class IsRuntimeCss a where
  renderRuntimeCss :: a -> Builder

instance IsRuntimeCss StylesheetR where
  renderRuntimeCss (StylesheetE smts) = concatMapB renderRuntimeCss smts

instance IsRuntimeCss StatementR where
  renderRuntimeCss (ImportStatementE url queries) =
    "@import " <> renderRuntimeCss url <> queries <> ";"
  renderRuntimeCss (MediaStatementE queries rules) =
    "@media " <> queries <> "{" <> concatMapB renderRuntimeCss rules <> "}"
  renderRuntimeCss (PageStatementE name decls) =
    "@page " <> name <> "{" <> concatMapB renderRuntimeCss decls <>
    "}"
  renderRuntimeCss (RulesetStatementE rs) = renderRuntimeCss rs
  renderRuntimeCss (AtRuleStatementE name decls) =
    "@" <> name <> "{" <> concatMapB renderRuntimeCss decls <> "}"

instance IsRuntimeCss RulesetR where
  renderRuntimeCss (RulesetE sels decls) =
    (intercalateB "," . map renderRuntimeCss $ sels) <> "{" <>
    concatMapB renderRuntimeCss decls <> "}"
  renderRuntimeCss (MixinDefExtE sel args decls) =
    renderRuntimeCss sel <> "(" <>
    (intercalateB "," . map renderRuntimeCss $ args) <> "){" <>
    concatMapB renderRuntimeCss decls <> "}"
  renderRuntimeCss (VarDeclStatementExtE name term) =
    "@" <> renderIdent name <> ":" <> renderRuntimeCss term <> ";"

instance IsRuntimeCss MixinArgumentR where
  renderRuntimeCss (MixinArgumentE name value) =
    renderIdent name <> ":" <> renderRuntimeCss value

instance IsRuntimeCss DeclarationR where
  renderRuntimeCss (PropertyDeclarationE name expr prio) =
    name <> ":" <> renderRuntimeCss expr <>
    (if prio then "!important" else "") <> ";"
  renderRuntimeCss (RulesetDeclarationExtE rs) =
    renderRuntimeCss rs
  renderRuntimeCss (MixinApplicationExtE selector args) =
    renderRuntimeCss selector <> "(" <>
    (intercalateB "," . map renderRuntimeCss $ args) <> ";"

instance IsRuntimeCss SelectorR where
  renderRuntimeCss (SelectorE chunks) =
    concatMapB renderChunk chunks
    where
      renderChunk (Right chunk) = chunk
      renderChunk (Left dynamic) = "&" <> dynamic

instance IsRuntimeCss ExpressionR where
  renderRuntimeCss (ExpressionE elems) =
    concatMapB renderTermExpr elems
    where
      renderTermExpr (Left term) = renderRuntimeCss term
      renderTermExpr (Right op) = renderCss op

instance IsRuntimeCss TermR where
  renderRuntimeCss (ValueTermE val) =
    renderRuntimeCss val
  renderRuntimeCss (NegateTermE term) =
    "-" <> renderRuntimeCss term
  renderRuntimeCss (AbsTermE term) =
    "+" <> renderRuntimeCss term
  renderRuntimeCss (FunctionTermE name expr) =
    renderIdent name <> "(" <> renderRuntimeCss expr <> ")"
  renderRuntimeCss (ParensTermExtE term) =
    "(" <> renderRuntimeCss term <> ")"
  renderRuntimeCss (AddTermExtE t1 t2) =
    renderRuntimeCss t1 <> "+" <> renderRuntimeCss t2
  renderRuntimeCss (SubTermExtE t1 t2) =
    renderRuntimeCss t1 <> "-" <> renderRuntimeCss t2
  renderRuntimeCss (MulTermExtE t1 t2) =
    renderRuntimeCss t1 <> "*" <> renderRuntimeCss t2
  renderRuntimeCss (DivTermExtE t1 t2) =
    renderRuntimeCss t1 <> "/" <> renderRuntimeCss t2

instance IsRuntimeCss ValueR where
  renderRuntimeCss (NumberValueE d) = renderDouble d
  renderRuntimeCss (PercentageValueE d) = renderDouble d <> "%"
  renderRuntimeCss (UnitValueE unit d) = renderDouble d <> renderCss unit
  renderRuntimeCss (DimensionValueE dim d) = renderDouble d <> dim
  renderRuntimeCss (StringValueE s) = "\"" <> s <> "\""
  renderRuntimeCss (IdentValueE ident) = ident
  renderRuntimeCss (UriValueE uri) = renderRuntimeCss uri
  renderRuntimeCss (HexcolorValueE c) = renderCss c
  renderRuntimeCss (EscapedStringValueExtE e) = e
  renderRuntimeCss (VariableValueExtE v) = renderRuntimeCss v

instance IsRuntimeCss UriR where
  renderRuntimeCss (PlainUriE uri) =
    "url(\"" <> uri <> "\")"

instance IsRuntimeCss VariableR where
  renderRuntimeCss (PlainVariableE name) =
    "@" <> renderIdent name
  renderRuntimeCss (VariableRefE var) =
    "@" <> renderRuntimeCss var

liftValue :: Value -> ValueR
liftValue v =
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
      UriValueE . PlainUriE . renderString $ uri
    UriValue _ ->
      error "Cannot resolve URI splices at runtime"
    HexcolorValue color -> HexcolorValueE color
    EscapedStringValueExt esc ->
      EscapedStringValueExtE . fromString $ esc
    VariableValueExt var ->
      VariableValueExtE . liftVariable $ var
    SplicedValueExt _ ->
      error "Cannot resolve value splices at runtime"

liftUri :: Uri -> UriR
liftUri u =
  case u of
    PlainUri uri ->
      PlainUriE . fromString $ uri
    _ ->
      error "Cannot resolve URI splices at runtime"

liftVariable :: Variable -> VariableR
liftVariable v =
  case v of
    PlainVariable n -> PlainVariableE . fromString $ n
    VariableRef r -> VariableRefE . liftVariable $ r
