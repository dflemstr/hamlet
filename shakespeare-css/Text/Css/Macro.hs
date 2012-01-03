-- | Character macros taken directly from the CSS2 specification
module Text.Css.Macro where

import Prelude hiding (takeWhile)

import Control.Applicative ((<$>), (*>), (<*), pure)

import Data.Char

import Numeric (readHex, showHex)

import Text.ParserCombinators.Parsec

import Text.Css.Util

{-------------------------------------------------------------------------------

   See the CSS specification for an explanation of these character matchers.

-------------------------------------------------------------------------------}

ident :: Parser String
ident = optionalString "-" <+> (pure <$> nmstart) <+> many nmchar
        <?> "identifier"

name :: Parser String
name = many1 nmstart <?> "name"

nmstart :: Parser Char
nmstart = satisfyInClass "_a-zA-Z" <|> nonascii <|> escape
          <?> "start of a name"

nonascii :: Parser Char
nonascii =
  satisfy isNotAscii <?> "non-ASCII character"
  where
    isNotAscii x = fromEnum x > 159

escape :: Parser Char
escape =
  char '\\' *> escapeValue

escapeValue :: Parser Char
escapeValue =
  ( ( satisfyNotInClass "\n\r\f0-9a-fA-F"
      <?> "escaped char"
    ) <|>
    toUnicode <$>
    ( ( takeWhileBetween (inClass "0-9a-fA-F") 1 6
        <?> "hexadecimal Unicode code point"
      ) <*
      optional
      ( (char '\r' <* optional (char '\n')) <|>
        char '\n' <|> char '\t' <|> char '\f' <|> char ' '
        <?> "white space after Unicode sequence"
      )
    )
  )
  <?> "escape sequence"

nmchar :: Parser Char
nmchar =
  satisfyInClass "_a-zA-Z0-9-" <|> nonascii <|> escape
  <?> "name character"

num :: Parser Double
num =
  toDouble <$>
  ( try (takeWhile isDigit <+> string "." <+> takeWhile1 isDigit) <|>
    (takeWhile1 isDigit)
  )
  <?> "numeral"

string' :: Parser String
string' = string1 <|> string2 <?> "string"

string1 :: Parser String
string1 =
  char '"' *> many stringContent1 <*
  (char '"' <|> fail "Unclosed double-quoted string literal")
  <?> "double-quoted string"

string2 :: Parser String
string2 =
  char '\'' *> many stringContent2 <*
  (char '\'' <|> fail "Unclosed single-quoted string literal")
  <?> "single-quoted string"

stringContent1 :: Parser Char
stringContent1 =
  satisfyNotInClass "\n\r\f\\\"" <|>
  char '\\' *> (nl *> stringContent1 <|> escapeValue)
  <?> "string character in a double-quoted string"

stringContent2 :: Parser Char
stringContent2 =
  satisfyNotInClass "\n\r\f\\'" <|>
  char '\\' *> (nl *> stringContent2 <|> escapeValue)
  <?> "string character in a single-quoted string"

comment :: Parser String
comment =
  string "/*" *> many (noneOf "*") <+> many1 (char '*') <+>
  manyString commentEndElem <*
  (char '/' <|> fail "Unclosed comment: missing '/'")
  <?> "comment"
  where
    commentEndElem =
      pure <$> noneOf "/*" <+>
      many (noneOf "*") <+>
      ( init <$> takeWhile1 (== '*') <|>
        fail "Unclosed comment: missing '*'"
      )

uri :: Parser String
uri =
  string' <|> many uriContentElem <?> "uri"
  where
    uriContentElem =
      satisfyInClass "!#$%&*-[]-~" <|> nonascii <|> escape

nl :: Parser ()
nl =
  ((char '\r' <* optional (char '\n')) <|> char '\n' <|> char '\f') *> pure ()
  <?> "newline"

w :: Parser ()
w = (takeWhile $ inClass " \t\r\n\f") *> pure () <?> "whitespace"

-- | Creates specialized matchers for a single character
mkCharP :: Char -> Parser Char
mkCharP c =
  try
  ( ( char c <|>
      char (toUpper c) <|>
      char '\\' *> escaped
    ) *> pure c
  ) <?> ('\'' : c : "'-like character")
  where
    escaped =
      if c > 'f'
      then char c <|> char (toUpper c) <|> hexSequence *> pure c
      else hexSequence *> pure c
    hexStr = showHex (fromEnum c) ""
    upperHexStr = (showHex . fromEnum . toUpper $ c) ""
    hexSequence =
      takeWhileBetween (== '0') 0 4 *>
      (permuteStr hexStr <|> permuteStr upperHexStr) *>
      optional (char '\r' <* optional (char '\n') <|>
                satisfyInClass " \t\n\f")

-- These letters are used in case-insensitive CSS keywords
lA, lC, lD, lE, lG, lH, lI, lK, lL, lM :: Parser Char
lN, lO, lP, lR, lS, lT, lU, lX, lY, lZ :: Parser Char

lA = mkCharP 'a'
lC = mkCharP 'c'
lD = mkCharP 'd'
lE = mkCharP 'e'
lG = mkCharP 'g'
lH = mkCharP 'h'
lI = mkCharP 'i'
lK = mkCharP 'k'
lL = mkCharP 'l'
lM = mkCharP 'm'
lN = mkCharP 'n'
lO = mkCharP 'o'
lP = mkCharP 'p'
lR = mkCharP 'r'
lS = mkCharP 's'
lT = mkCharP 't'
lU = mkCharP 'u'
lX = mkCharP 'x'
lY = mkCharP 'y'
lZ = mkCharP 'z'

-- | Parses a CSS-style number
toDouble :: String -> Double
toDouble =
  -- token can have shape: ".432", "1335.2453" or "123"
  read . fixToken
  where
    fixToken ""          = "0"
    fixToken t @ ('.':_) = '0' : t
    fixToken t           = t

-- | Parses an Unicode hex sequence
toUnicode :: String -> Char
toUnicode str =
  let hexValue = head [x | (x, "") <- readHex str]
  in toEnum (hexValue :: Int)
