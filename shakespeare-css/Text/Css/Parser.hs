-- | CSS parsers for CSS level 2.0, 2.1 and 3.0. These parsers are
-- *NOT* forwards-compatible, since they need access to expression
-- tokens directly -- conforming to the forwards-compatible CSS
-- standard would make variable interpolation etc. impractical.
--
-- We do manual whitespace management, because whitespace is
-- significant in some places of CSS stylesheets.
--
-- *TODO*: There might be issues with comments not being parseable in
-- some obscure locations
module Text.Css.Parser where

import Control.Applicative ((<*>), (<$>), (*>), (<*))
import Control.Arrow (first)
import Control.Monad (void)

import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Shakespeare.Base

import Text.Css.Ast
import Text.Css.Token
import Text.Css.Util

-- | A complete CSS stylesheet
stylesheetParser :: Parser Stylesheet
stylesheetParser =
  ( do
       void $ optionMaybe (try charsetSymToken *> stringToken *> semicolonToken)
       skipMany (try spaceToken *> sParser <|> try cdoToken <|> cdcToken)
       statements <- statementParser `sepEndBy` tailToken
       return $ Stylesheet statements
  ) <?> "stylesheet"
  where
    tailToken = skipMany ((try cdoToken <|> cdcToken) *> sParser)
    statementParser =
      try importParser <|>
      RulesetStatement <$> try rulesetParser <|>
      try atRuleParser <|>
      try mediaParser <|>
      pageParser

-- | A single import statement
importParser :: Parser Statement
importParser =
  ( do
       importSymToken
       sParser
       uri <- try (PlainUri <$> stringToken) <|>
              try uriParser <|> fail "Invalid import uri"
       sParser
       queries <- queryListParser
       semicolonToken
       sParser
       return $ ImportStatement uri queries
  ) <?> "import statement"

-- | An arbitrary @-rule statement. We assume that the rule contains
-- declarations, so that we can perform variable interpolations inside
atRuleParser :: Parser Statement
atRuleParser =
  ( do
       name <- atkeywordToken
       sParser
       declarations <- declarationsParser
       sParser
       return . AtRuleStatement name . catMaybes $ declarations
  ) <?> "@-rule statement"

-- | A media @-rule. Queries are not parsed further at all.
mediaParser :: Parser Statement
mediaParser =
  ( do
       mediaSymToken
       sParser
       queries <- queryListParser
       rulesets <- between openBraceToken closeBraceToken $ do
         sParser
         many rulesetParser
       sParser
       return $ MediaStatement queries rulesets
  ) <?> "media statement"

-- | A list of media queries
queryListParser :: Parser [String]
queryListParser =
  filter (not . null) . map strip <$>
  queryParser `sepBy` (commaToken *> sParser)
  <?> "query list"

-- | Any media query
queryParser :: Parser String
queryParser = queryContentToken <* sParser <?> "query"

-- | A page layout @-rule statement
pageParser :: Parser Statement
pageParser =
  ( do
       pageSymToken
       sParser
       pseudoName <- optionMaybe . try $ pseudoPageParser
       openBraceToken
       sParser
       declarations <- declarationParser `sepEndBy`
                       many1 (semicolonToken *> sParser)
       closeBraceToken
       sParser
       return $ PageStatement pseudoName declarations
  ) <?> "page statement"

-- | A named pseudo-page like ':left', ':right', etc.
pseudoPageParser :: Parser String
pseudoPageParser = colonToken *> identToken <* sParser <?> "pseudo page"

-- | An expression operator that separates terms.
--
-- The string 'border-radius: 1em 2em / 0.4em 3em;' has 3 operators:
-- A space operator, slash operator and another space operator.
operatorParser :: Parser ExprOperator
operatorParser =
  (slashToken *> return SplitExprOperator <|>
   commaToken *> return SeqExprOperator <|>
   return SpaceExprOperator
  ) <* sParser
  <?> "operator"

-- | A selector combinator. Does not assume that whitespace has been consumed
combinatorParser :: Parser Combinator
combinatorParser =
  (try (sParser *> greaterToken *> return ChildCombinator) <|>
   try (sParser *> plusToken *> return SiblingCombinator) <|>
   try (sParser *> tildeToken *> return GeneralSiblingCombinator) <|>
   spaceToken *> return DescendantCombinator
  ) <* sParser
  <?> "combinator"

-- | A declaration property
propertyParser :: Parser String
propertyParser = identToken <* sParser <?> "property"

-- | A set of rules for a selector sequence
rulesetParser :: Parser Ruleset
rulesetParser =
  ( do
       selectors <- selectorParser `sepBy1` (commaToken *> sParser)
       sParser
       declarations <- declarationsParser
       sParser
       return . Ruleset selectors . catMaybes $ declarations
  ) <?> "ruleset"

-- | A list of declarations. Two consecutive semicolons produce 'Nothing'.
declarationsParser :: Parser [Maybe Declaration]
declarationsParser =
  between openBraceToken closeBraceToken $ do
    sParser
    (optionMaybe . try $ declarationParser) `sepEndBy`
      (semicolonToken *> sParser)

-- | A selector (not a selector sequence; does not parse ',')
selectorParser :: Parser Selector
selectorParser = Selector <$> retSepBy1' simpleSelectorParser combinatorParser

-- | An explicit simple selector
simpleSelectorParser :: Parser SimpleSelector
simpleSelectorParser =
  ( do
       namespace <- optionMaybe (try namespacePrefixParser)
       name <- optionMaybe (try elementNameParser)
       specifiers <-
         if isJust name
         then many selectorSpecifierParser
         else many1 selectorSpecifierParser
       return $ SimpleSelector namespace name specifiers
  ) <?> "simple selector"

-- | A specifier that further constrains a simple selector
selectorSpecifierParser :: Parser SimpleSelectorSpecifier
selectorSpecifierParser =
  try (IDSelector <$> hashToken) <|>
  try (ampersandToken *> return SuperblockSelectorExt) <|>
  try (ClassSelector <$> classParser) <|>
  try (attribParser) <|>
  try (negationParser) <|>
  pseudoParser
  <?> "selector specifier"

-- | A class selector '.bla'
classParser :: Parser String
classParser = periodToken *> identToken <?> "class"

-- | An attribute selector like '[href*=\'google.com\']'
attribParser :: Parser SimpleSelectorSpecifier
attribParser =
  ( do
       openBracketToken
       sParser
       (namespace, name) <-
         try ((,) <$> (Just <$> namespacePrefixParser) <*> identToken) <|>
         (,) <$> return Nothing <*> identToken <?> "identifier"
       sParser
       operation <- optionMaybe . try $ do
         operator <-
           equalsToken *> return ExactlyOp <|>
           prefixmatchToken *> return BeginsWithOp <|>
           suffixmatchToken *> return EndsWithOp <|>
           substringmatchToken *> return ContainsOp <|>
           includesToken *> return ElemSpaceOp <|>
           dashmatchToken *> return ElemDashOp
         sParser
         operand <- try identToken <|> try stringToken <?> "operand"
         sParser
         return (operator, operand)
       closeBracketToken
       return $ AttributeSelector namespace name operation
  ) <?> "attribute specifier"

uriParser :: Parser Uri
uriParser =
  try
  ( do
       urlSymToken
       sParser
       Right (d, param) <- parseAt
       sParser
       closeParenToken
       return $
         if param
         then SplicedUriParamExt d
         else SplicedUriExt d
  ) <|> PlainUri <$> uriToken

-- | A pseudo class or pseudo element selector like ':first' or
-- '::before'. Arguments to pseudo functions are not parsed further.
pseudoParser :: Parser SimpleSelectorSpecifier
pseudoParser =
  ( do
       colonToken
       isPseudoElement <- colonToken *> return True <|> return False
       (name, content) <- try matchFunction <|> matchIdent <?>
                          "pseudo identifier or function"
       return $
         if isPseudoElement
         then PseudoElementSelector name content
         else PseudoClassSelector name content
  ) <?> "pseudo class or element"
  where
    matchFunction = do
      name <- functionToken
      sParser
      content <- functionContentToken
      closeParenToken
      return (name, Just . strip $ content)
    matchIdent = (\ i -> (i, Nothing)) <$> identToken

-- | Any expression like '1px solid red'
expressionParser :: Parser Expression
expressionParser =
  Expression <$> retSepBy1 termExprParser operatorParser
  <?> "expression"

-- | Operator description for extended expressions
-- TODO investigate what else should be added
termTable :: OperatorTable Char () Term
termTable =
  [ [ bi starToken  MulTermExt AssocLeft
    , bi slashToken DivTermExt AssocLeft
    ]
  , [ bi plusToken  AddTermExt AssocLeft
    , bi minusToken SubTermExt AssocLeft
    ]
  , [ un plusToken  AbsTerm
    , un minusToken NegateTerm
    ]
  ]
  where
    bi m f assoc = Infix  (m *> sParser *> return f) assoc
    un m f       = Prefix (m *> sParser *> return f)

-- | A term that includes infix operators
termExprParser :: Parser Term
termExprParser = buildExpressionParser termTable termParser

-- | A term like '1px', 'solid', 'red', '(3 + 4)'
-- 'desaturate(@color, 30% * 0.3)'. Some extension terms might need to be
-- reduced to proper CSS terms.
termParser :: Parser Term
termParser =
  (try (ParensTermExt <$>
        between openParenToken closeParenToken termExprParser) <|>
   try functionParser <|>
   ValueTerm <$> valueParser
  ) <* sParser <?> "plain term"

-- | Any value that cannot possibly be reduced further (without
-- variable resolution -> extension)
valueParser :: Parser Value
valueParser =
  (try (UriValue <$> uriParser) <|>
   try (splicedValueParser) <|>
   try (VariableValueExt <$> variableParser) <|>
   try (HexcolorValue <$> colorParser) <|>
   try (StringValue <$> stringToken) <|>
   try (mkLength <$> lengthToken) <|>
   try (UnitValue Ems <$> emsToken) <|>
   try (UnitValue Exs <$> exsToken) <|>
   try (mkAngle <$> angleToken) <|>
   try (mkTime <$> timeToken) <|>
   try (mkFreq <$> freqToken) <|>
   try (mkDimension <$> dimensionToken) <|>
   try (NumberValue <$> numberToken) <|>
   try (PercentageValue <$> percentageToken) <|>
   IdentValue <$> identToken
  ) <?> "value"
  where
    mkSomething b = uncurry UnitValue . first b
    mkLength = mkSomething mkLenUnit
    mkAngle = mkSomething mkAngleUnit
    mkTime = mkSomething mkTimeUnit
    mkFreq = mkSomething mkFreqUnit
    mkDimension = uncurry DimensionValue
    mkLenUnit "px" = LengthPixels
    mkLenUnit "cm" = LengthCentimeters
    mkLenUnit "mm" = LengthMillimeters
    mkLenUnit "in" = LengthInches
    mkLenUnit "pt" = LengthPoints
    mkLenUnit "pc" = LengthPica
    mkLenUnit  _   = error "valueParser: Invalid length unit"
    mkAngleUnit "deg"   = AngleDegrees
    mkAngleUnit "rad"   = AngleRadians
    mkAngleUnit "grad"  = AngleGradians
    mkAngleUnit "turns" = AngleTurns
    mkAngleUnit  _      = error "valueParser: Invalid angle unit"
    mkTimeUnit "ms" = TimeMilliseconds
    mkTimeUnit "s"  = TimeSeconds
    mkTimeUnit  _   = error "valueParser: Invalid time unit"
    mkFreqUnit "hz"  = FreqHerz
    mkFreqUnit "khz" = FreqKiloHerz
    mkFreqUnit  _    = error "valueParser: Invalid frequency unit"

-- | A variable/variable reference like '@myvar' or '@@myvar' or '@@@myvar'
variableParser :: Parser Variable
variableParser = do
  atToken
  try (VariableRef <$> variableParser) <|>
    (PlainVariable <$> identToken) <?> "variable name"

-- | A variable splice like '#{myvar}'
splicedValueParser :: Parser Value
splicedValueParser = do
  (Right d) <- parseHash
  return $ SplicedValueExt d

-- | A negation selector like ':not(.bla)'
negationParser :: Parser SimpleSelectorSpecifier
negationParser =
  ( do
       notFuncSymToken
       sParser
       arg <- simpleSelectorParser
       sParser
       closeParenToken
       return $ NotSelector arg
  ) <?> "negation selector"

-- | A namespace in front of an identifier
namespacePrefixParser :: Parser String
namespacePrefixParser =
  (try identToken <|> try starStringToken <|> return "") <* pipeToken
  <?> "namespace prefix"

-- | An element name
elementNameParser :: Parser String
elementNameParser = try identToken <|> starStringToken <?> "element name"

-- | Some declaration like 'foo: bar'
declarationParser :: Parser Declaration
declarationParser =
  try (RulesetDeclarationExt <$> rulesetParser) <|>
  ( do
       name <- propertyParser
       colonToken
       sParser
       expr <- expressionParser
       isPrio <- importantSymToken *> return True <|> return False
       return $ PropertyDeclaration name expr isPrio
  ) <?> "declaration"

-- | A function term like 'foo(bar)'
functionParser :: Parser Term
functionParser =
  ( do
       name <- functionToken
       sParser
       expr <- expressionParser
       closeParenToken
       sParser
       return $ FunctionTerm name expr
  ) <?> "function"

-- | A hexadecimal color à la '#abc' or '#abcdef'
colorParser :: Parser Color
colorParser =
  ( do
       hash <- colorToken
       sParser
       case hash of
         [r, g, b] ->
           return $ Color (colorOf1 r) (colorOf1 g) (colorOf1 b) 1
         [r1, r2, g1, g2, b1, b2] ->
           return $ Color (colorOf2 r1 r2) (colorOf2 g1 g2) (colorOf2 b1 b2) 1
         _ -> fail "invalid color hash code"
  ) <?> "color"
  where
    colorOf1 c = valueOf c / 15
    colorOf2 c1 c2 = (16 * valueOf c1 + valueOf c2) / 255
    valueOf c
      | c >= 'a' && c <= 'f' = fromIntegral (10 + cv - fromEnum 'a')
      | c >= '0' && c <= '9' = fromIntegral (cv - fromEnum '0')
      | otherwise            = error "valueOf: not hex digit"
      where
        cv = fromEnum c

-- | Whitespace and comments
sParser :: Parser ()
sParser = skipMany (spaceToken <|> commentToken)
