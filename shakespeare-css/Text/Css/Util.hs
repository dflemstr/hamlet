-- | Parsing utility functions
module Text.Css.Util where

import Control.Applicative ((<$>))

import Data.Char (isSpace)
import Data.Monoid

import Text.ParserCombinators.Parsec

-- | Match the predicate a number of times specified by a range
takeWhileBetween :: (Char -> Bool)  -- ^ Predicate
                    -> Int          -- ^ Least number of matches
                    -> Int          -- ^ Max number of matches
                    -> Parser String
takeWhileBetween predicate =
  go
  where
    go 0 0 = return ""
    go 0 e = try (consumeOne <+> go 0 (e - 1)) <|> return ""
    go b e = consumeOne <+> go (b - 1) (e - 1)
    consumeOne = satisfyChar predicate

-- | Like 'many1', but creates a 'String'
many1String :: Parser String -> Parser String
many1String = fmap concat . many1

-- | Like 'many', but creates a 'String'
manyString :: Parser String -> Parser String
manyString = fmap concat . many

-- | Like 'satisfy' and 'inClass', but also converts to a 'String'
satisfyInClass :: String -> Parser String
satisfyInClass = satisfyChar . inClass

-- | Like 'satisfy' and 'notInClass', but also converts to a 'String'
satisfyNotInClass :: String -> Parser String
satisfyNotInClass c = satisfyChar $ not . inClass c

-- | Like 'satisfy', but converts to a 'String'
satisfyChar :: (Char -> Bool) -> Parser String
satisfyChar f = return <$> satisfy f

-- | Matches the given 'String', or nothing.
optionalString :: String -> Parser String
optionalString = option "" . string

-- | Helper from attoparsec
takeWhile :: (Char -> Bool) -> Parser String
takeWhile = many . satisfy

-- | Helper from attoparsec
takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 = many1 . satisfy

-- | Like 'sepBy1', but retains the separators
retSepBy1 :: Parser a -> Parser sep -> Parser [Either a sep]
retSepBy1 p sep = do
  x <- p
  xs <- fmap concat . many . sequence $ [Right <$> sep, Left <$> p]
  return $ (Left x) : xs

-- | Like 'sepBy', but retains the separators
retSepBy :: Parser a -> Parser sep -> Parser [Either a sep]
retSepBy p sep = retSepBy1 p sep <|> return []

-- | Like 'sepBy1', but retains separators, and makes a 'try' excursion
-- for each new (sep, a) pair after the first obligatory match
retSepBy1' :: Parser a -> Parser sep -> Parser [Either a sep]
retSepBy1' p sep = do
  x <- p
  xs <- fmap concat . many . try . sequence $ [Right <$> sep, Left <$> p]
  return $ (Left x) : xs

-- | Like 'sepBy', but retains separators, and makes a 'try' excursion
-- for each new (sep, a) pair after the first match
retSepBy' :: Parser a -> Parser sep -> Parser [Either a sep]
retSepBy' p sep = retSepBy1 p sep <|> return []

-- | Like 'sepEndBy1', but retains separators
retSepEndBy1 :: Parser a -> Parser sep -> Parser [Either a sep]
retSepEndBy1 p sep = do
  x <- p
  ( do
       y <- sep
       xs <- retSepEndBy p sep
       return $ (Left x) : (Right y) : xs
    ) <|> return [Left x]

-- | Like 'sepEndBy', but retains separators
retSepEndBy :: Parser a -> Parser sep -> Parser [Either a sep]
retSepEndBy p sep = retSepEndBy1 p sep <|> return []

-- | Creates a class matcher for the given class definition.
-- The class specification specifies alternatives and ranges.
-- Ranges have the syntax \"a-x\", alternatives are literal
-- characters. To match a literal \'-\', put it at the beginning or
-- the end of the class specification
inClass :: String -> Char -> Bool
inClass "" =
  const False
inClass (a : '-' : b : xs) =
  \ c -> (c >= a && c <= b) || f c
  where
    f = inClass xs
inClass (x : xs) =
  \ c -> c == x || f c
  where
    f = inClass xs

-- | Removes whitespace at the beginning and end of the 'String'
strip :: String -> String
strip =
  f . f
  where
    f = reverse . dropWhile isSpace

-- | Maps the specified 'Left' or does nothing
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right x) = Right x

-- | Maps the specified 'Right' or does nothing
mapRight :: (a -> b) -> Either c a -> Either c b
mapRight f (Right a) = Right (f a)
mapRight _ (Left x)  = Left x

-- | Returns 'True' for 'Left', 'False' otherwise
isLeft :: Either a b -> Bool
isLeft Left {} = True
isLeft _       = False

-- | Returns 'True' for 'Right', 'False' otherwise
isRight :: Either a b -> Bool
isRight Right {} = True
isRight _        = False

-- | Appends the results of one parser to the results of another
(<+>) :: Monoid a => Parser a -> Parser a -> Parser a
(<+>) firstParser secondParser = do
  first <- firstParser
  second <- secondParser
  return $ first `mappend` second
infixl 4 <+>
