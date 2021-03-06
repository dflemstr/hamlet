{-# LANGUAGE TemplateHaskell #-}
-- | The abstract syntax tree for CSS-like stylesheets
module Text.Css.Ast where

import Text.Shakespeare.Base

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
  { importUrl :: Uri
  , importQueries :: [String]
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
  MixinDefExt -- TODO parser
  { mixinSelector :: Selector
  , mixinArgs :: [MixinArgument]
  , mixinDeclarations :: [Declaration]
  } |
  -- | Extension: A variable declaration
  VarDeclStatementExt -- TODO parser
  { varDeclName :: String
  , varDeclTerm :: Term
  }
  deriving (Show)

-- | An argument to a mixin definition
data MixinArgument =
  MixinArgument -- TODO parser
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
  MixinApplicationExt -- TODO parser
  { mixinAppSelector :: Selector
  , mixinAppArgs :: [Term]
  }
  deriving (Show)

-- | A selector that matches elements
data Selector =
  Selector
  { selectorElems :: [Either SimpleSelector Combinator]
  }
  deriving (Show)

-- | A simple selector without constraints
data SimpleSelector =
  SimpleSelector
  { selectorName :: SimpleSelectorName
  , selectorSpecifiers :: [SimpleSelectorSpecifier]
  }
  deriving (Show)

data SimpleSelectorName
  = SelectorNothing
  | SelectorNsElem String String
  | SelectorElem String
    -- | Extension: Insert the parent selector as the name
  | SelectorParentExt
  deriving (Show)

-- | A specifier that adds constraints to a simple selector
data SimpleSelectorSpecifier
  = AttributeSelector (Maybe String) String (Maybe (AttributeOp, String))
  | ClassSelector String
  | IDSelector String
  | PseudoClassSelector String (Maybe String)
  | PseudoElementSelector String (Maybe String)
  | NotSelector Selector
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
  | UnitValue Unit Double
  | DimensionValue String Double
  | StringValue String
  | IdentValue String
  | UriValue Uri
  | HexcolorValue Color
    -- | Extension: Raw CSS value that should be inserted verbatim
  | EscapedStringValueExt String -- TODO parser
    -- | Extension: Variable reference
  | VariableValueExt Variable
    -- | Extension: Hamlet-spliced value
  | SplicedValueExt Deref
  deriving (Show)

data Uri
  = PlainUri String
    -- | Extension: Hamlet-spliced URLs
  | SplicedUriExt Deref
    -- | Extension: Hamlet-spliced URL params
  | SplicedUriParamExt Deref
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

-- | Units of dimensions
data Unit
  = LengthPixels
  | LengthCentimeters
  | LengthMillimeters
  | LengthInches
  | LengthPoints
  | LengthPica
  | AngleDegrees
  | AngleRadians
  | AngleGradians
  | AngleTurns
  | TimeSeconds
  | TimeMilliseconds
  | FreqHerz
  | FreqKiloHerz
  | Ems -- Does not represent length according to standard!
  | Exs -- ditto
  deriving (Show)
