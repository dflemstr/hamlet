{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Lucius
    ( -- * Parsing
      lucius
    , luciusFile
    , luciusFileDebug
    , luciusFileReload
      -- ** Runtime
    , luciusRT
    , luciusRT'
      -- * Re-export cassius
    , module Text.Cassius
    ) where

import Text.Cassius hiding (cassius, cassiusFile, cassiusFileDebug, cassiusFileReload)
import Text.Shakespeare.Base
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import Text.ParserCombinators.Parsec hiding (Line)
import Text.Css
import Data.Char (isSpace)
import Control.Applicative ((<$>))
import Data.Either (partitionEithers)
import Data.Text.Lazy.Builder (fromText)

-- |
--
-- >>> renderLucius undefined [lucius|foo{bar:baz}|]
-- "foo{bar:baz}"
lucius :: QuasiQuoter
lucius = QuasiQuoter { quoteExp = luciusFromString }

luciusFromString :: String -> Q Exp
luciusFromString s =
    topLevelsToCassius
  $ either (error . show) id $ parse parseTopLevels s s

whiteSpace :: Parser ()
whiteSpace = many
    ((oneOf " \t\n\r" >> return ()) <|> (parseComment >> return ()))
    >> return () -- FIXME comments, don't use many

parseBlock :: Parser Block
parseBlock = do
    sel <- parseSelector
    _ <- char '{'
    whiteSpace
    pairsBlocks <- parsePairsBlocks id
    let (pairs, blocks) = partitionEithers pairsBlocks
    whiteSpace
    return $ Block sel pairs blocks

parseSelector :: Parser Selector
parseSelector =
    go id
  where
    go front = do
        c <- parseContents "{,"
        let front' = front . (:) (trim c)
        (char ',' >> go front') <|> return (front' [])

trim :: Contents -> Contents
trim =
    reverse . trim' False . reverse . trim' True
  where
    trim' _ [] = []
    trim' b (ContentRaw s:rest) =
        let s' = trimS b s
         in if null s' then trim' b rest else ContentRaw s' : rest
    trim' _ x = x
    trimS True = dropWhile isSpace
    trimS False = reverse . dropWhile isSpace . reverse

type PairBlock = Either Pair Block
parsePairsBlocks :: ([PairBlock] -> [PairBlock]) -> Parser [PairBlock]
parsePairsBlocks front = (char '}' >> return (front [])) <|> (do
    isBlock <- lookAhead checkIfBlock
    x <- if isBlock
            then (do
                b <- parseBlock
                whiteSpace
                return $ Right b)
            else Left <$> parsePair
    parsePairsBlocks $ front . (:) x)
  where
    checkIfBlock = do
        skipMany $ noneOf "#@{};"
        (parseHash >> checkIfBlock)
            <|> (parseAt >> checkIfBlock)
            <|> (char '{' >> return True)
            <|> (oneOf ";}" >> return False)
            <|> (anyChar >> checkIfBlock)
            <|> fail "checkIfBlock"

parsePair :: Parser Pair
parsePair = do
    key <- parseContents ":"
    _ <- char ':'
    whiteSpace
    val <- parseContents ";}"
    (char ';' >> return ()) <|> return ()
    whiteSpace
    return (key, val)

parseContents :: String -> Parser Contents
parseContents = many1 . parseContent

parseContent :: String -> Parser Content
parseContent restricted =
    parseHash' <|> parseAt' <|> parseComment <|> parseChar
  where
    parseHash' = either ContentRaw ContentVar `fmap` parseHash
    parseAt' =
        either ContentRaw go `fmap` parseAt
      where
        go (d, False) = ContentUrl d
        go (d, True) = ContentUrlParam d
    parseChar = (ContentRaw . return) `fmap` noneOf restricted

parseComment :: Parser Content
parseComment = do
    _ <- try $ string "/*"
    _ <- manyTill anyChar $ try $ string "*/"
    return $ ContentRaw ""

luciusFile :: FilePath -> Q Exp
luciusFile fp = do
    contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    luciusFromString contents

luciusFileDebug, luciusFileReload :: FilePath -> Q Exp
luciusFileDebug = cssFileDebug [|parseTopLevels|] parseTopLevels
luciusFileReload = luciusFileDebug

parseTopLevels :: Parser [TopLevel]
parseTopLevels =
    go id
  where
    go front = do
        whiteSpace
        ((charset <|> media <|> var <|> fmap TopBlock parseBlock) >>= \x -> go (front . (:) x))
            <|> (return $ map compressTopLevel $ front [])
    charset = do
        _ <- try $ string "@charset "
        cs <- many1 $ noneOf ";"
        _ <- char ';'
        return $ TopCharset cs
    media = do
        _ <- try $ string "@media "
        name <- many1 $ noneOf "{"
        _ <- char '{'
        b <- parseBlocks id
        return $ MediaBlock name b
    var = try $ do
        _ <- char '@'
        k <- many1 $ noneOf ":"
        _ <- char ':'
        v <- many1 $ noneOf ";"
        _ <- char ';'
        let trimS = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        return $ TopVar (trimS k) (trimS v)
    parseBlocks front = do
        whiteSpace
        (char '}' >> return (map compressBlock $ front []))
            <|> (parseBlock >>= \x -> parseBlocks (front . (:) x))

luciusRT' :: TL.Text -> Either String ([(Text, Text)] -> Either String Css)
luciusRT' tl =
    case parse parseTopLevels (TL.unpack tl) (TL.unpack tl) of
        Left s -> Left $ show s
        Right tops -> Right $ \scope -> go scope tops
  where
    go :: [(Text, Text)] -> [TopLevel] -> Either String Css
    go _ [] = Right []
    go scope (TopCharset cs:rest) = do
        rest' <- go scope rest
        Right $ Charset cs : rest'
    go scope (TopBlock b:rest) = do
        b' <- goBlock scope b
        rest' <- go scope rest
        Right $ map Css b' ++ rest'
    go scope (MediaBlock m bs:rest) = do
        bs' <- mapM (goBlock scope) bs
        rest' <- go scope rest
        Right $ Media m (concat bs') : rest'
    go scope (TopVar k v:rest) = go ((pack k, pack v):scope) rest

    goBlock :: [(Text, Text)] -> Block -> Either String [Css']
    goBlock scope =
        either Left (Right . ($[])) . blockRuntime scope' (error "luciusRT has no URLs")
      where
        scope' = map goScope scope
        goScope (k, v) = (DerefIdent (Ident $ unpack k), CDPlain $ fromText v)

luciusRT :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRT tl scope = either Left (Right . renderCss) $ either Left ($ scope) (luciusRT' tl)
