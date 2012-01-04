-- | CSS token specifiers. Every token that is used in CSS+extensions
-- is listed here.
module Text.Css.Token where

import Prelude hiding (takeWhile)

import Control.Applicative ((*>), pure)

import Text.ParserCombinators.Parsec

import Text.Css.Macro
import Text.Css.Util

--------------------------------------------------------------------------------
-- Dynamic tokens
--------------------------------------------------------------------------------

identToken :: Parser String
identToken = ident <?> "identifier"

stringToken :: Parser String
stringToken = string' <?> "string"

hashToken :: Parser ()
hashToken = char '#' `disregardWith` "hash"

numberToken :: Parser Double
numberToken = num <?> "number"

unicodeRangeToken :: Parser String
unicodeRangeToken =
  string "u+" <+> takeWhileBetween (inClass "0-9a-fA-F?") 1 6 <+>
  option "" (string "-" <+> takeWhileBetween (inClass "0-9a-fA-F") 1 6)
  <?> "unicode range"

functionToken :: Parser String
functionToken = ident <?> "function"

colorToken :: Parser String
colorToken = takeWhileBetween (inClass "0-9a-fA-F") 3 6 <?> "color"

uriToken :: Parser String
uriToken = uri <?> "uri"

functionContentToken :: Parser String
functionContentToken = takeWhile (/= ')') <?> "function content"

queryContentToken :: Parser String
queryContentToken =
  many1 (noneOf "{,;") <?> "query content"

commentToken :: Parser ()
commentToken = comment `disregardWith` "comment"

spaceToken :: Parser ()
spaceToken = many1 (oneOf " \t\r\n\f") `disregardWith` "whitespace"

--------------------------------------------------------------------------------
-- Constant symbol tokens
--------------------------------------------------------------------------------

importantSymToken :: Parser ()
importantSymToken =
  ( char '!' *> skipMany (w <|> commentToken) *>
    lI *> lM *> lP *> lO *> lR *> lT *> lA *> lN *> lT)
  `disregardWith` "!important"

charsetSymToken :: Parser ()
charsetSymToken =
  string "charset " *> pure ()

prefixmatchSymToken :: Parser ()
prefixmatchSymToken = string "^=" `disregardWith` "prefix match operator"

suffixmatchSymToken :: Parser ()
suffixmatchSymToken = string "$=" `disregardWith` "suffix match operator"

substringmatchSymToken :: Parser ()
substringmatchSymToken = string "*=" `disregardWith` "substring match operator"

includesSymToken :: Parser ()
includesSymToken = string "~=" `disregardWith` "includes operator"

dashmatchSymToken :: Parser ()
dashmatchSymToken = string "|=" `disregardWith` "dash match operator"

cdoSymToken :: Parser ()
cdoSymToken = string "<!--" `disregardWith` "cdo"

cdcSymToken :: Parser ()
cdcSymToken = string "-->" `disregardWith` "cdc"

importSymToken :: Parser ()
importSymToken = (lI *> lM *> lP *> lO *> lR *> lT) `disregardWith` "import"

pageSymToken :: Parser ()
pageSymToken = (lP *> lA *> lG *> lE) `disregardWith` "page"

mediaSymToken :: Parser ()
mediaSymToken = (lM *> lE *> lD *> lI *> lA) `disregardWith` "media"

notFuncSymToken :: Parser ()
notFuncSymToken = (lN *> lO *> lT) `disregardWith` "not"

emSymToken :: Parser ()
emSymToken = (lE *> lM) `disregardWith` "em"

exSymToken :: Parser ()
exSymToken = (lE *> lX) `disregardWith` "ex"

pxSymToken :: Parser ()
pxSymToken = (lP *> lX) `disregardWith` "px"

cmSymToken :: Parser ()
cmSymToken = (lC *> lM) `disregardWith` "cm"

mmSymToken :: Parser ()
mmSymToken = (lM *> lM) `disregardWith` "mm"

inSymToken :: Parser ()
inSymToken = (lI *> lN) `disregardWith` "in"

ptSymToken :: Parser ()
ptSymToken = (lP *> lT) `disregardWith` "pt"

pcSymToken :: Parser ()
pcSymToken = (lP *> lC) `disregardWith` "pc"

degSymToken :: Parser ()
degSymToken = (lD *> lE *> lG) `disregardWith` "deg"

radSymToken :: Parser ()
radSymToken = (lR *> lA *> lD) `disregardWith` "rad"

gradSymToken :: Parser ()
gradSymToken = (lG *> lR *> lA *> lD) `disregardWith` "grad"

turnsSymToken :: Parser ()
turnsSymToken = (lT *> lU *> lR *> lN *> lS) `disregardWith` "turns"

msSymToken :: Parser ()
msSymToken = (lM *> lS) `disregardWith` "ms"

sSymToken :: Parser ()
sSymToken = lS `disregardWith` "s"

hzSymToken :: Parser ()
hzSymToken = (lH *> lZ) `disregardWith` "hz"

khzSymToken :: Parser ()
khzSymToken = (lK *> lH *> lZ) `disregardWith` "khz"

urlSymToken :: Parser ()
urlSymToken = (lU *> lR *> lL) `disregardWith` "url"

--------------------------------------------------------------------------------
-- Single character tokens
--------------------------------------------------------------------------------

colonToken :: Parser ()
colonToken = char ':' `disregardWith` "colon"

semicolonToken :: Parser ()
semicolonToken = char ';' `disregardWith` "semicolon"

commaToken :: Parser ()
commaToken = char ',' `disregardWith` "comma"

slashToken :: Parser ()
slashToken = char '/' `disregardWith` "slash"

greaterToken :: Parser ()
greaterToken = char '>' `disregardWith` "greater than"

plusToken :: Parser ()
plusToken = char '+' `disregardWith` "plus"

tildeToken :: Parser ()
tildeToken = char '~' `disregardWith` "tilde"

minusToken :: Parser ()
minusToken = char '-' `disregardWith` "minus"

ampersandToken :: Parser ()
ampersandToken = char '&' `disregardWith` "ampersand"

starToken :: Parser ()
starToken = char '*' `disregardWith` "star"

starStringToken :: Parser String
starStringToken = string "*" <?> "star"

periodToken :: Parser ()
periodToken = char '.' `disregardWith` "period"

pipeToken :: Parser ()
pipeToken = char '|' `disregardWith` "pipe"

equalsToken :: Parser ()
equalsToken = char '=' `disregardWith` "equals"

hashSymbolToken :: Parser ()
hashSymbolToken = char '#' `disregardWith` "hash"

atToken :: Parser ()
atToken = char '@' `disregardWith` "at"

percentageToken :: Parser ()
percentageToken = char '%' `disregardWith` "percentage"

openBraceToken :: Parser ()
openBraceToken = char '{' `disregardWith` "open brace"

closeBraceToken :: Parser ()
closeBraceToken = char '}' `disregardWith` "close brace"

openParenToken :: Parser ()
openParenToken = char '(' `disregardWith` "open paren"

closeParenToken :: Parser ()
closeParenToken = char ')' `disregardWith` "close paren"

openBracketToken :: Parser ()
openBracketToken = char '[' `disregardWith` "open bracket"

closeBracketToken :: Parser ()
closeBracketToken = char ']' `disregardWith` "close bracket"
