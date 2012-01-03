{-# LANGUAGE FlexibleInstances #-}
module Text.Css.Convert where

import Text.ParserCombinators.Parsec (parse)

import Text.Css.Ast
import qualified Text.Css.Parser as CssParser

class CssValue a where
  toCssValue :: a -> Value

instance CssValue [Char] where
  toCssValue = parseValue

class CssUri a where
  toCssUri :: a -> Uri

instance CssUri [Char] where
  toCssUri = PlainUri

parseValue :: String -> Value
parseValue =
  handleResult . parse CssParser.valueParser "value string"
  where
    handleResult (Right v) = v
    handleResult (Left err) = error . show $ err

parseUri :: String -> Uri
parseUri =
  handleResult . parse CssParser.uriParser "uri string"
  where
    handleResult (Right u) = u
    handleResult (Left err) = error . show $ err
