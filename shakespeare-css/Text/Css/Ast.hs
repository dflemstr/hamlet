-- | The abstract syntax tree for CSS-like stylesheets
module Text.Css.Ast where

-- | A CSS stylesheet
newtype Stylesheet =
  Stylesheet
  { stylesheetStatements :: [Statement]
  }
  deriving (Show)

-- | A stylesheet top level statement
data Statement =
  -- | '@import' statement
  ImportStatement
  { importUrl :: String
  , importQueries ::[String]
  } |
  -- | '@media' statement
  MediaStatement
  { mediaQueries :: [String]
  , mediaRules :: [Ruleset]
  } |
  -- | '@page' statement
  PageStatement
  { pagePseudoName :: Maybe String
  , pageDeclarations :: [Declaration]
  } |
  -- | A ruleset with declarations
  RulesetStatement
  { ruleset :: Ruleset
  } |
  -- | Any @-rule like '@font-face'
  AtRuleStatement
  { atRuleName :: String
  , atRuleDeclarations :: [Declaration]
  }
  deriving (Show)

-- | A block- or top level rule
data Ruleset =
  -- | A selector group
  Ruleset
  { rulesetSelectors :: [Selector]
  , rulesetDeclarations :: [Declaration]
  } |
  -- | Extension: A mixin definition
  MixinDefExt -- TODO
  { mixinSelector :: Selector
  , mixinArgs :: [MixinArgument]
  , mixinDeclarations :: [Declaration]
  } |
  -- | Extension: A variable declaration
  VarDeclStatementExt -- TODO
  { varDeclName :: String
  , varDeclTerm :: Term
  }
  deriving (Show)

-- | An argument to a mixin definition
data MixinArgument =
  MixinArgument -- TODO
  { mixinArgumentName :: String
  , mixinArgumentDefValue :: Value
  }
  deriving (Show)

-- | A block-level declaration
data Declaration =
  -- | A property declaration like 'foo: bar !important'
  PropertyDeclaration
  { declarationName :: String
  , declarationExpression :: Expression
  , declarationPrio :: Bool
  } |
  -- | Extension: A ruleset sub-declaration
  RulesetDeclarationExt
  { declarationRuleset :: Ruleset
  } |
  -- | Extension: An application of a previous mixin
  MixinApplicationExt -- TODO
  { mixinAppSelector :: Selector
  }
  deriving (Show)

-- | A selector that matches elements
newtype Selector =
  Selector
  { selectorElems :: [Either SimpleSelector Combinator]
  }
  deriving (Show)

-- | A simple selector without constraints
data SimpleSelector =
  SimpleSelector
  { selectorNamespace :: Maybe String
  , selectorName :: Maybe String
  , selectorSpecifiers :: [SimpleSelectorSpecifier]
  }
  deriving (Show)

-- | A specifier that adds constraints to a simple selector
data SimpleSelectorSpecifier
  = AttributeSelector (Maybe String) String (Maybe (AttributeOp, String))
  | ClassSelector String
  | IDSelector String
  | PseudoClassSelector String (Maybe String)
  | PseudoElementSelector String (Maybe String)
  | NotSelector SimpleSelector
    -- | Extension: Refers to the selector of the parent block
  | SuperblockSelectorExt
  deriving (Show)

-- | An operator for attribute selectors
data AttributeOp
  = ExactlyOp
  | ElemSpaceOp
  | ElemDashOp
  | BeginsWithOp
  | EndsWithOp
  | ContainsOp
  deriving (Show)

-- | A combinator for two specified simple selectors
data Combinator
  = DescendantCombinator
  | ChildCombinator
  | SiblingCombinator
  | GeneralSiblingCombinator
  deriving (Show)

-- | An expression for a property declaration
data Expression =
  Expression
  { expressionElems :: [Either Term ExprOperator]
  }
  deriving (Show)

-- | An expression delimiter operator
data ExprOperator
  = SplitExprOperator
  | SeqExprOperator
  | SpaceExprOperator
  deriving (Show)

-- | A combinator term for values
data Term
  = ValueTerm Value
  | NegateTerm Term
  | AbsTerm Term
  | FunctionTerm String Expression
    -- | Extension: Group terms with parens
  | ParensTermExt Term
    -- | Extension: Add terms
  | AddTermExt Term Term
    -- | Extension: Subtract terms
  | SubTermExt Term Term
    -- | Extension: Multiply terms
  | MulTermExt Term Term
    -- | Extension: Divide terms
  | DivTermExt Term Term
  deriving (Show)

-- | A value that cannot be reduced
data Value
  = NumberValue Double
  | PercentageValue Double
  | LengthValue LengthUnit Double
  | EmsValue Double
  | ExsValue Double
  | AngleValue AngleUnit Double
  | TimeValue TimeUnit Double
  | FreqValue FreqUnit Double
  | DimensionValue String Double
  | StringValue String
  | IdentValue String
  | UriValue String
  | HexcolorValue Color
    -- | Extension: Raw CSS value that should be inserted verbatim
  | EscapedStringValueExt String -- TODO
    -- | Extension: Variable reference
  | VariableValueExt Variable
  deriving (Show)

-- | A variable or variable reference
data Variable =
  PlainVariable
  { variableName :: String
  } |
  VariableRef
  { variableReference :: Variable
  }
  deriving (Show)

-- | An ARGB color
data Color =
  Color
  { colorRed :: Double
  , colorGreen :: Double
  , colorBlue :: Double
  , colorAlpha :: Double
  }
  deriving (Show)

-- | Units of length
data LengthUnit
  = LengthPixels
  | LengthCentimeters
  | LengthMillimeters
  | LengthInches
  | LengthPoints
  | LengthPica
  deriving (Show)

-- | Angle units
data AngleUnit
  = AngleDegrees
  | AngleRadians
  | AngleGradians
  | AngleTurns
  deriving (Show)

-- | Time units
data TimeUnit
  = TimeSeconds
  | TimeMilliseconds
  deriving (Show)

-- | Frequency units
data FreqUnit
  = FreqHerz
  | FreqKiloHerz
  deriving (Show)
