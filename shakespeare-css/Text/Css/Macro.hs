-- | Character macros taken directly from the CSS2 specification
module Text.Css.Macro where

import Prelude hiding (takeWhile)

import Control.Applicative ((<$>), (*>), (<*))
import Control.Monad (void)

import Data.Char

import Numeric (readHex, showHex)

import Text.ParserCombinators.Parsec

import Text.Css.Util

{-------------------------------------------------------------------------------

   See the CSS specification for an explanation of these character matchers.

-------------------------------------------------------------------------------}

ident :: Parser String
ident = optionalString "-" <+> nmstart <+> manyString nmchar <?> "identifier"

name :: Parser String
name = many1String nmstart <?> "name"

nmstart :: Parser String
nmstart =
  satisfyInClass "_a-zA-Z" <|>
  nonascii <|>
  try escape <?> "start of a name"

nonascii :: Parser String
nonascii =
  satisfyChar isNotAscii <?> "non-ASCII character"
  where
    isNotAscii x = fromEnum x > 159

unicode :: Parser String
unicode =
  ( do
       void $ string "\\"
       hexStr <- takeWhileBetween (inClass "0-9a-fA-F") 1 6
       void $ option "" (try (string "\r\n") <|> satisfyInClass " \n\r\t\f")
       let hexValue = head [x | (x, "") <- readHex hexStr]
       return . return . toEnum $ (hexValue :: Int)
  ) <?> "unicode escape sequence"

escape :: Parser String
escape =
  try unicode <|>
  string "\\" *> satisfyNotInClass "\n\r\f0-9a-fA-F"
  <?> "escape sequence"

nmchar :: Parser String
nmchar =
  satisfyInClass "_a-zA-Z0-9-" <|> nonascii <|> try escape <?>
  "name character"

num :: Parser Double
num =
  toDouble <$>
  (try (takeWhile isDigit <+> string "." <+> takeWhile1
        isDigit) <|>
   try (takeWhile1 isDigit))
  <?> "numeral"

string' :: Parser String
string' = try string1 <|> try string2 <?> "string"

string1 :: Parser String
string1 = string "\"" *> stringContents1 <* string "\""

string2 :: Parser String
string2 = string "'" *> stringContents2 <* string "'"

badstring :: Parser String
badstring = try badstring1 <|> try badstring2 <?> "bad string"

badstring1 :: Parser String
badstring1 =
  string "\"" *>
  stringContents1 <+> optionalString "\\"

badstring2 :: Parser String
badstring2 =
  string "'" *>
  stringContents2 <+> optionalString "\\"

stringContents1 :: Parser String
stringContents1 =
  manyString (satisfyNotInClass "\n\r\f\\\"" <|>
              try (string "\\" >> nl >> return "") <|>
              escape)

stringContents2 :: Parser String
stringContents2 =
  manyString (satisfyNotInClass "\n\r\f\\'" <|>
              try (string "\\" >> nl >> return "") <|>
              escape)

comment :: Parser String
comment =
  string "/*" *> takeWhile (/= '*') <+> takeWhile1 (== '*') <+>
  manyString commentEndElem <* string "/" <?> "comment"
  where
    commentEndElem =
      satisfyNotInClass "/*" <+>
      takeWhile (/= '*') <+>
      (init <$> takeWhile1 (== '*'))

badcomment :: Parser String
badcomment = try badcomment1 <|> try badcomment2 <?> "bad comment"

badcomment1 :: Parser String
badcomment1 =
  string "/*" <+> takeWhile (/= '*') <+> takeWhile1 (== '*')
  <+> manyString brokenEndElem
  where
    brokenEndElem =
      satisfyNotInClass "/*" <+> takeWhile (/= '*') <+> takeWhile1 (== '*')

badcomment2 :: Parser String
badcomment2 =
  string "/*" <+> takeWhile (/= '*') <+> manyString brokenEndElem
  where
    brokenEndElem =
      takeWhile1 (== '*') <+> satisfyNotInClass "/*" <+> takeWhile (/= '*')

uri :: Parser String
uri =
  try (url *> w *> string' <* w <* string ")") <|>
  try (url *> w *> manyString uriContentElem <* w <* string ")")
  <?> "uri"
  where
    uriContentElem =
      satisfyInClass "!#$%&*-[]-~" <|> nonascii <|> escape

url :: Parser ()
url = string "url(" *> return ()

baduri :: Parser String
baduri = try baduri1 <|> try baduri2 <|> try baduri3 <?> "bad uri"

baduri1 :: Parser String
baduri1 =
  string "url(" *> w *> manyString uriContentElem <* w
  where
    uriContentElem =
      satisfyInClass "!#$%&*-~" <|> nonascii <|> escape

baduri2 :: Parser String
baduri2 = string "url(" *> w *> string' <* w

baduri3 :: Parser String
baduri3 = string "url(" *> w *> badstring

nl :: Parser ()
nl = void (string "\n" <|> try (string "\r\n") <|> string "\r" <|> string "\f")
     <?> "newline"

w :: Parser ()
w = (takeWhile $ inClass " \t\r\n\f") *> return () <?> "w"

-- | Creates specialized matchers for a single character
mkCharP :: Char -> Parser String
mkCharP c =
  (if c > 'e' then (<|> string ('\\' : cString)) else id)
  ( string cString <|>
    string cUpperString <|>
    try hexSequence
  ) *> return cString <?> "'" ++ cString ++ "'-like character"
  where
    cString = [c]
    cUpper = toUpper c
    cUpperString = [cUpper]
    capsHex = showHex (fromEnum cUpper) ""
    smallHex = showHex (fromEnum c) ""
    hexSequence =
      string "\\" <+> takeWhileBetween (== '0') 0 4 <+>
      (try (string smallHex) <|> string capsHex) <+>
      option "" (try (string "\r\n") <|> satisfyInClass " \t\r\n\f")

-- These letters are used in case-insensitive CSS keywords
lA, lC, lD, lE, lG, lH, lI, lK, lL, lM :: Parser String
lN, lO, lP, lR, lS, lT, lU, lX, lY, lZ :: Parser String

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
