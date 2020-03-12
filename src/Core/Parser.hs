{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

module Untyped.Parser where

import Data.Text hiding (empty, foldr, foldl')
import Text.Megaparsec hiding (sepEndBy1)
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Prelude hiding (unlines)
import Control.Monad (void)
import Data.Char
import Control.Applicative (liftA2)
import Data.List (foldl')

import Untyped.Expression


type Parser = Parsec Void Text


-- |
-- This ignores spaces and tabs.
spaceP :: Parser ()
spaceP = void $ takeWhileP (Just "space chars") (liftA2 (||) (== ' ') (== '\t'))

-- |
-- This ignores all whitespace.
wSpaceP :: Parser ()
wSpaceP = space1

-- |
wSpaceSomeP :: Parser ()
wSpaceSomeP = C.space

lineComment :: Parser ()
lineComment = empty

blockComment :: Parser ()
blockComment = empty

--wsIgnore :: Parser ()
--wsIgnore = L.space wSpaceP lineComment blockComment

--sIgnore :: Parser ()
--sIgnore = L.space spaceP lineComment blockComment

lexemeS :: Parser a -> Parser a
lexemeS = lexeme spaceP

lexemeSomeWS :: Parser a -> Parser a
lexemeSomeWS = lexeme wSpaceSomeP

lexemeWS :: Parser a -> Parser a
lexemeWS = lexeme wSpaceP

ident :: Parser Text
ident = lexemeS (cons <$> lowerChar <*> takeWhileP Nothing (isAlphaNum))

equals :: Parser ()
equals = void $ lexemeS (char ('=')) 

leftCurly :: Parser ()
leftCurly = void $ lexemeSomeWS (char ('{')) 

rightCurly :: Parser ()
rightCurly = void $ lexemeSomeWS (char ('}'))

leftBracket :: Parser ()
leftBracket = void $ lexemeS (char ('('))

rightBracket :: Parser ()
rightBracket = void $ lexemeSomeWS (char (')'))

bracketed :: Parser a -> Parser a
bracketed p =
  do
    _   <- try leftBracket
    res <- p
    _   <- rightBracket
    pure res

keyword :: Text -> Parser ()
keyword t = void $ lexemeS (chunk t)

var :: Parser Expression
var = Var <$> ident


lam :: Parser Expression
lam =
  do
    void $ lexemeS (char '\\')
    vars <- some ident
    void $ lexemeS (char '.')
    body <- parseExpr
    let lamExpr = foldr Lam body vars
    pure lamExpr


parseUnit :: Parser Expression
parseUnit = bracketed parseExpr <|> lam <|> var

app :: Parser Expression
app =
  do
    left  <- parseUnit
    rest  <- some parseUnit
    let appExpr = foldl' App left rest
    pure appExpr


letBlock :: Parser [Block]
letBlock =
  do
    try (keyword "let")
    leftCurly
    vals <- sepBy1 (parseDefinition <* optional (some space1)) (lexemeWS (char ';'))
    rightCurly
    _ <- optional $ keyword "in"
    pure vals


parseDefinition :: Parser Block
parseDefinition  = parseDecl parseExpr


parseExpr :: Parser Expression
parseExpr =
  choice
    [ lam
    , (try app)
    , var
    , bracketed parseExpr
    ]


parseBlock :: Parser Block
parseBlock =
  do
    lhs <- ident
    equals
    lBlock <- optional letBlock
    sLine  <- parseExpr
    case lBlock of
      Nothing -> pure (lhs, sLine)
      Just lb -> error ("to do when we add let expr" <> show lb) -- pure (lhs, Let lb sLine)


parseDecl :: Parser a -> Parser (Identifier, a)
parseDecl p =
  do
    lhs <- ident
    equals
    parsed <- p
    pure (lhs, parsed)


parseProgram :: Parser [Block]
parseProgram =
  do
    let sep = some (char '\n')
    sepEndBy1 parseBlock sep

    
    
textInput :: Parser Text
textInput = lexeme spaceP (cons <$> lowerChar <*> takeWhileP Nothing (isAlphaNum))

textLines :: Parser [Text]
textLines =
  let sep = char '\n' in
  sepEndBy1 textInput sep


sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do{ x <- p
                    ; do{ _ <- sep
                        ; xs <- sepEndBy p sep
                        ; return (x:xs)
                        }
                      <|> return [x]
                    }

{-

programExample :: Text
programExample = unlines ["f = {1}", "", "g = {let {x = 1; y = 2} 2}"]

blockExample :: Text
blockExample = "g = let {x = a; y = b} w"

lamEx :: Text
lamEx = "\\x . x"
-}
