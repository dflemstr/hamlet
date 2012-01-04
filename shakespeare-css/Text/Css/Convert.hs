{-# LANGUAGE FlexibleInstances #-}
module Text.Css.Convert where

import Text.Css.Ast

class CssValue a where
  toCssValue :: a -> Value

instance CssValue [Char] where
  toCssValue = StringValue

class CssUri a where
  toCssUri :: a -> Uri

instance CssUri [Char] where
  toCssUri = PlainUri
