-- | CSS token specifiers. Every token that is used in CSS+extensions
-- is listed here.
module Text.Css.Token where

import Prelude hiding (takeWhile)

import Control.Applicative ((<*), (*>))

import Text.ParserCombinators.Parsec

import Text.Css.Macro
import Text.Css.Util

identToken :: Parser String
identToken = ident <?> "ident"

atkeywordToken :: Parser String
atkeywordToken = char '@' *> ident <?> "atkeyword"

stringToken :: Parser String
stringToken = string' <?> "string"

badStringToken :: Parser String
badStringToken = badstring <?> "bad string"

badUriToken :: Parser String
badUriToken = baduri <?> "bad uri"

badCommentToken :: Parser String
badCommentToken = badcomment <?> "bad comment"

hashToken :: Parser String
hashToken = char '#' *> name <?> "hash"

numberToken :: Parser Double
numberToken = num <?> "number"

percentageToken :: Parser Double
percentageToken = num <* char '%' <?> "percentage"

importSymToken :: Parser ()
importSymToken =
  char '@' *> lI *> lM *> lP *> lO *> lR *> lT *> return ()
  <?> "@import symbol"

pageSymToken :: Parser ()
pageSymToken =
  char '@' *> lP *> lA *> lG *> lE *> return ()
  <?> "@page symbol"

mediaSymToken :: Parser ()
mediaSymToken =
  char '@' *> lM *> lE *> lD *> lI *> lA *> return ()
  <?> "@media symbol"

charsetSymToken :: Parser ()
charsetSymToken =
  string "@charset " *> return ()
  <?> "@charset symbol"

importantSymToken :: Parser ()
importantSymToken =
  char '!' *> skipMany (try w <|> commentToken) *>
  lI *> lM *> lP *> lO *> lR *> lT *> lA *> lN *> lT *>
  return () <?> "!important symbol"

notFuncSymToken :: Parser ()
notFuncSymToken =
  char ':' *> lN *> lO *> lT *> char '(' *> return ()
  <?> ":not( symbol"

functionContentToken :: Parser String
functionContentToken = takeWhile (/= ')') <?> "function content"

queryContentToken :: Parser String
queryContentToken =
  many (noneOf "{,;") <?> "query content"

emsToken :: Parser Double
emsToken = try (num <* lE <* lM) <?> "ems"

exsToken :: Parser Double
exsToken = try (num <* lE <* lX) <?> "exs"

lengthToken :: Parser (String, Double)
lengthToken =
  try
  ( do
       n <- num
       d <- try (lP <+> lX) <|>
            try (lC <+> lM) <|>
            try (lM <+> lM) <|>
            try (lI <+> lN) <|>
            try (lP <+> lT) <|>
            lP <+> lC
       return (d, n)
  ) <?> "length"

angleToken :: Parser (String, Double)
angleToken =
  try
  ( do
       n <- num
       d <- try (lD <+> lE <+> lG) <|>
            try (lR <+> lA <+> lD) <|>
            try (lG <+> lR <+> lA <+> lD) <|>
            lT <+> lU <+> lR <+> lN <+> lS
       return (d, n)
  ) <?> "angle"

timeToken :: Parser (String, Double)
timeToken =
  try
  ( do
       n <- num
       d <- try (lM <+> lS) <|>
            lS
       return (d, n)
  ) <?> "time"

freqToken :: Parser (String, Double)
freqToken =
  try
  ( do
       n <- num
       d <- try (lH <+> lZ) <|>
            lK <+> lH <+> lZ
       return (d, n)
  ) <?> "freq"

dimensionToken :: Parser (String, Double)
dimensionToken =
  try
  ( do
       n <- num
       d <- ident
       return (d, n)
  ) <?> "dimension"

uriToken :: Parser String
uriToken = uri <?> "uri"

unicodeRangeToken :: Parser String
unicodeRangeToken =
  string "u+" <+> takeWhileBetween (inClass "0-9a-fA-F?") 1 6 <+>
  option "" (string "-" <+> takeWhileBetween (inClass "0-9a-fA-F") 1 6)
  <?> "unicode range"

colorToken :: Parser String
colorToken =
  char '#' *> takeWhileBetween (inClass "0-9a-fA-F") 3 6
  <?> "color"

cdoToken :: Parser ()
cdoToken = try (string "<!--") *> return () <?> "cdo"

cdcToken :: Parser ()
cdcToken = try (string "-->") *> return () <?> "cdc"

colonToken :: Parser ()
colonToken = char ':' *> return () <?> "colon"

semicolonToken :: Parser ()
semicolonToken = char ';' *> return () <?> "semicolon"

commaToken :: Parser ()
commaToken = char ',' *> return () <?> "comma"

slashToken :: Parser ()
slashToken = char '/' *> return () <?> "slash"

greaterToken :: Parser ()
greaterToken = char '>' *> return () <?> "greater than"

plusToken :: Parser ()
plusToken = char '+' *> return () <?> "plus"

tildeToken :: Parser ()
tildeToken = char '~' *> return () <?> "tilde"

minusToken :: Parser ()
minusToken = char '-' *> return () <?> "minus"

ampersandToken :: Parser ()
ampersandToken = char '&' *> return () <?> "ampersand"

starToken :: Parser ()
starToken = char '*' *> return () <?> "star"

starStringToken :: Parser String
starStringToken = string "*" <?> "star"

periodToken :: Parser ()
periodToken = char '.' *> return () <?> "period"

pipeToken :: Parser ()
pipeToken = char '|' *> return () <?> "pipe"

equalsToken :: Parser ()
equalsToken = char '=' *> return () <?> "equals"

hashSymbolToken :: Parser ()
hashSymbolToken = char '#' *> return () <?> "hash"

atToken :: Parser ()
atToken = char '@' *> return () <?> "at"

openBraceToken :: Parser ()
openBraceToken = char '{' *> return () <?> "open brace"

closeBraceToken :: Parser ()
closeBraceToken = char '}' *> return () <?> "close brace"

openParenToken :: Parser ()
openParenToken = char '(' *> return () <?> "open paren"

closeParenToken :: Parser ()
closeParenToken = char ')' *> return () <?> "close paren"

openBracketToken :: Parser ()
openBracketToken = char '[' *> return () <?> "open bracket"

closeBracketToken :: Parser ()
closeBracketToken = char ']' *> return () <?> "close bracket"

spaceToken :: Parser ()
spaceToken = try (takeWhile1 $ inClass " \t\r\n\f") *> return () <?> "space"

commentToken :: Parser ()
commentToken = try comment *> return () <?> "comment"

functionToken :: Parser String
functionToken = try (ident <* char '(') <?> "function"

prefixmatchToken :: Parser ()
prefixmatchToken = try (string "^=") *> return () <?> "prefix match operator"

suffixmatchToken :: Parser ()
suffixmatchToken = try (string "$=") *> return () <?> "suffix match operator"

substringmatchToken :: Parser ()
substringmatchToken =
  try (string "*=") *> return () <?> "substring match operator"

includesToken :: Parser ()
includesToken = try (string "~=") *> return () <?> "includes operator"

dashmatchToken :: Parser ()
dashmatchToken = try (string "|=") *> return () <?> "dash match operator"
