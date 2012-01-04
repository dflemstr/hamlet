-- | CSS parsers for CSS level 2.0, 2.1 and 3.0 with extensions.
--
-- These parsers are
-- *NOT* forwards-compatible, since they need access to expression
-- tokens directly -- conforming to the forwards-compatible CSS
-- standard would make variable interpolation etc. impractical.
--
-- We do manual whitespace management, because whitespace is
-- significant in some places of CSS stylesheets.
--
-- *TODO*: There might be issues with comments not being parseable in
-- some obscure locations
module Text.Citerius.Parser where

import Control.Applicative ((<*>), (<$>), (*>), (<*), pure)

import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Shakespeare.Base

import Text.Css.Ast
import Text.Css.Token
import Text.Css.Util

-- | A complete Citerius stylesheet
stylesheetParser :: Parser Stylesheet
stylesheetParser =
  ( do
       optional (atToken *> charsetSymToken *> stringToken *> semicolonToken)
       skipMany ( spaceToken <|>
                  commentToken <|>
                  cdoSymToken <|>
                  cdcSymToken
                )
       imports <- try importParser `sepEndBy` tailToken
       statements <- statementParser `sepEndBy` tailToken
       return $ Stylesheet (imports ++ statements)
  ) <?> "stylesheet"
  where
    tailToken = skipMany ((cdoSymToken <|> cdcSymToken) *> sParser)
    statementParser =
      atToken *>
      ( mediaRestParser <|>
        pageRestParser <|>
        atRuleRestParser
      ) <|>
      RulesetStatement <$> rulesetParser

-- | A single import statement
importParser :: Parser Statement
importParser = atToken *> importRestParser

importRestParser :: Parser Statement
importRestParser = flip label "import statement" $ do
  importSymToken
  sParser
  uri <- try (PlainUri <$> stringToken) <|>
         uriParser <|> fail "Invalid import uri"
  sParser
  queries <- queryListParser
  semicolonToken
  sParser
  return $ ImportStatement uri queries

-- | An arbitrary @-rule statement. We assume that the rule contains
-- declarations, so that we can perform variable interpolations inside
atRuleParser :: Parser Statement
atRuleParser = atToken *> atRuleRestParser

atRuleRestParser :: Parser Statement
atRuleRestParser = flip label "@-rule statement" $ do
  name <- identToken
  sParser
  declarations <- declarationsParser
  sParser
  return . AtRuleStatement name . catMaybes $ declarations

-- | A media @-rule. Queries are not parsed further at all.
mediaParser :: Parser Statement
mediaParser = atToken *> mediaRestParser

mediaRestParser :: Parser Statement
mediaRestParser = flip label "media statement" $ do
  mediaSymToken
  sParser
  queries <- queryListParser
  sParser
  rulesets <- between openBraceToken closeBraceToken $ do
    sParser
    many rulesetParser
  sParser
  return $ MediaStatement queries rulesets

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
pageParser = atToken *> pageRestParser

pageRestParser :: Parser Statement
pageRestParser = flip label "page statement" $ do
  pageSymToken
  sParser
  pseudoName <- optionMaybe pseudoPageParser
  declarations <- declarationsParser
  sParser
  return . PageStatement pseudoName . catMaybes $ declarations

-- | A named pseudo-page like ':left', ':right', etc.
pseudoPageParser :: Parser String
pseudoPageParser = colonToken *> identToken <* sParser <?> "pseudo page"

-- | An expression operator that separates terms.
--
-- The string 'border-radius: 1em 2em / 0.4em 3em;' has 3 operators:
-- A space operator, split operator and another space operator.
operatorParser :: Parser ExprOperator
operatorParser =
  ( slashToken *> pure SplitExprOperator <|>
    commaToken *> pure SeqExprOperator <|>
    spaceToken *> return SpaceExprOperator
  ) <* sParser
  <?> "operator"

-- | A selector combinator. Does not assume that whitespace has been consumed
combinatorParser :: Parser Combinator
combinatorParser =
  ( try (sParser *> greaterToken *> return ChildCombinator) <|>
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
rulesetParser = flip label "ruleset" $ do
  selectors <- selectorParser `sepBy1` (commaToken *> sParser)
  sParser
  declarations <- declarationsParser
  sParser
  return . Ruleset selectors . catMaybes $ declarations

-- | A list of declarations. Two consecutive semicolons produce 'Nothing'.
declarationsParser :: Parser [Maybe Declaration]
declarationsParser =
  between openBraceToken closeBraceToken $ do
    sParser
    (optionMaybe declarationParser) `sepEndBy` (semicolonToken *> sParser)

-- | A selector (not a selector sequence; does not parse ',')
selectorParser :: Parser Selector
selectorParser = Selector <$> retSepBy1' simpleSelectorParser combinatorParser

-- | An explicit simple selector
simpleSelectorParser :: Parser SimpleSelector
simpleSelectorParser = flip label "simple selector" $ do
  namespace <- optionMaybe (try namespacePrefixParser)
  n <- optionMaybe ( ampersandToken *> return Nothing <|>
                     Just <$> try elementNameParser
                   )
  specifiers <-
    if isJust n
    then many selectorSpecifierParser
    else many1 selectorSpecifierParser
  name <-
    case (namespace, n) of
      (Just ns, Just (Just e)) -> return $ SelectorNsElem ns e
      (Nothing, Just (Just e)) -> return $ SelectorElem e
      (Nothing, Just Nothing)  -> return $ SelectorParentExt
      (Nothing, Nothing)       -> return $ SelectorNothing
      (Just _,  Nothing)       ->
        fail "Cannot have namespace without identifier"
      (Just _,  Just Nothing)  ->
        fail "Parent selectors cannot have namespaces"
  return $ SimpleSelector name specifiers

-- | A specifier that further constrains a simple selector
selectorSpecifierParser :: Parser SimpleSelectorSpecifier
selectorSpecifierParser =
  (IDSelector <$> (hashToken *> identToken)) <|>
  (ClassSelector <$> (periodToken *> identToken)) <|>
  attribParser <|>
  try negationParser <|>
  pseudoParser
  <?> "selector specifier"

-- | An attribute selector like '[href*=\'google.com\']'
attribParser :: Parser SimpleSelectorSpecifier
attribParser = flip label "attribute specifier" $ do
  between openBracketToken closeBracketToken $ do
    sParser
    (namespace, name) <-
      try
      ((,) <$> (Just <$> namespacePrefixParser) <*> identToken) <|>
      ((,) <$> pure Nothing <*> identToken)
    sParser
    operation <- optionMaybe $ do
      operator <-
        equalsToken *> return ExactlyOp <|>
        prefixmatchSymToken *> return BeginsWithOp <|>
        suffixmatchSymToken *> return EndsWithOp <|>
        substringmatchSymToken *> return ContainsOp <|>
        includesSymToken *> return ElemSpaceOp <|>
        dashmatchSymToken *> return ElemDashOp
      sParser
      operand <- stringToken <|> identToken <?> "operand"
      sParser
      return (operator, operand)
    return $ AttributeSelector namespace name operation

-- | An URI like 'url(bla.txt)' or 'url("http://bla.com/")' or 'url(@{splice})'
uriParser :: Parser Uri
uriParser = do
  urlSymToken
  between openParenToken closeParenToken $
    sParser *> (uriSplice <|> PlainUri <$> uriToken) <* sParser
  where
    uriSplice = do
      Right (d, param) <- parseAt
      sParser
      return $
        if param
        then SplicedUriParamExt d
        else SplicedUriExt d

-- | A pseudo class or pseudo element selector like ':first' or
-- '::before'. Arguments to pseudo functions are not parsed further.
pseudoParser :: Parser SimpleSelectorSpecifier
pseudoParser = flip label "pseudo class or element" $ do
  colonToken
  isPseudoElement <- colonToken *> return True <|> return False
  (name, content) <-
    try matchFunction <|> matchIdent <?> "pseudo identifier or function"
  return $
    if isPseudoElement
    then PseudoElementSelector name content
    else PseudoClassSelector name content
  where
    matchFunction = do
      name <- functionToken
      content <- between openParenToken closeParenToken $ do
        sParser
        functionContentToken
      return (name, Just . strip $ content)
    matchIdent = (\ i -> (i, Nothing)) <$> identToken

-- | Any expression like '1px solid red'
expressionParser :: Parser Expression
expressionParser =
  Expression <$>
  ( try (retSepBy1 termParser operatorParser) <|>
    (return . Left) <$> termExprParser
  ) <* sParser <?> "expression"

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
termExprParser =
  buildExpressionParser
  termTable
  (termParser <* many (spaceToken <|> try commentToken))

-- | A term like '1px', 'solid', 'red', '(3 + 4)'
-- 'desaturate(@color, 30% * 0.3)'. Some extension terms might need to be
-- reduced to proper CSS terms.
termParser :: Parser Term
termParser =
  ( ParensTermExt <$>
    between openParenToken closeParenToken termExprParser
  ) <|>
  AbsTerm <$> (plusToken *> termParser) <|>
  NegateTerm <$> (minusToken *> termParser) <|>
  try functionParser <|>
  ValueTerm <$> valueParser

-- | Any value that cannot possibly be reduced further (without
-- variable resolution -> extension)
valueParser :: Parser Value
valueParser = -- TODO share parsers so that lookahead isn't needed
  ( try splicedValueParser <|>
    try (VariableValueExt <$> variableParser) <|>
    try (HexcolorValue <$> colorParser) <|>
    try (StringValue <$> stringToken) <|>
    try quantityParser <|>
    try (PercentageValue <$> numberToken <* percentageToken) <|>
    try (NumberValue <$> numberToken) <|>
    try (UriValue <$> uriParser) <|>
    IdentValue <$> identToken
  ) <?> "value"
  where
    quantityParser = do
      d <- numberToken
      unitParser d <|> dimensionParser d
    dimensionParser d = do
      dimension <- identToken
      return $ DimensionValue dimension d
    unitParser d = do
      unit <- -- TODO make a tree parser so that no lookahead is needed
        try pxSymToken    *> pure LengthPixels <|>
        try cmSymToken    *> pure LengthCentimeters <|>
        try mmSymToken    *> pure LengthMillimeters <|>
        try inSymToken    *> pure LengthInches <|>
        try ptSymToken    *> pure LengthPoints <|>
        try pcSymToken    *> pure LengthPica <|>
        try degSymToken   *> pure AngleDegrees <|>
        try radSymToken   *> pure AngleRadians <|>
        try gradSymToken  *> pure AngleGradians <|>
        try turnsSymToken *> pure AngleTurns <|>
        try msSymToken    *> pure TimeMilliseconds <|>
        try sSymToken     *> pure TimeSeconds <|>
        try hzSymToken    *> pure FreqHerz <|>
        try khzSymToken   *> pure FreqKiloHerz
      return $ UnitValue unit d

-- | A variable/variable reference like '@myvar' or '@@myvar' or '@@@myvar'
variableParser :: Parser Variable
variableParser = do
  atToken
  (VariableRef <$> variableParser) <|>
    (PlainVariable <$> identToken) <?> "variable name"

-- | A variable splice like '#{myvar}'
splicedValueParser :: Parser Value
splicedValueParser = do
  (Right d) <- parseHash
  return $ SplicedValueExt d

-- | A negation selector like ':not(.bla)'
negationParser :: Parser SimpleSelectorSpecifier
negationParser = flip label "negation selector" $ do
  notFuncSymToken
  arg <- between openParenToken closeParenToken $ do
    sParser
    selectorParser
  return $ NotSelector arg

-- | A namespace in front of an identifier
namespacePrefixParser :: Parser String
namespacePrefixParser =
  (starToken *> pure "*" <|> identToken <|> pure "") <* pipeToken
  <?> "namespace prefix"

-- | An element name
elementNameParser :: Parser String
elementNameParser = starToken *> pure "*" <|> identToken <?> "element name"

-- | Some declaration like 'foo: bar'
declarationParser :: Parser Declaration
declarationParser =
  try (RulesetDeclarationExt <$> rulesetParser) <|>
  ( flip label "declaration" $ do
       name <- propertyParser
       colonToken
       sParser
       expr <- expressionParser
       isPrio <- importantSymToken *> return True <|> return False
       return $ PropertyDeclaration name expr isPrio
  )

-- | A function term like 'foo(bar)'
functionParser :: Parser Term
functionParser = flip label "function" $ do
  name <- functionToken
  expr <- between openParenToken closeParenToken $ do
    sParser
    expressionParser
  return $ FunctionTerm name expr

-- | A hexadecimal color à la '#abc' or '#abcdef'
colorParser :: Parser Color
colorParser = flip label "color" $ do
  hashToken
  hash <- colorToken
  sParser
  case hash of
    [r, g, b] ->
      return $ Color (colorOf1 r) (colorOf1 g) (colorOf1 b) 1
    [r1, r2, g1, g2, b1, b2] ->
      return $ Color (colorOf2 r1 r2) (colorOf2 g1 g2) (colorOf2 b1 b2) 1
    _ -> fail "Invalid color: must be 3 or 6 digits"
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
