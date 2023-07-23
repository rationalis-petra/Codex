{-# OPTIONS_GHC -Wno-orphans #-}
module Glint.Parse
  ( ParseOption(..)
  , glint
  , glint_doc
  , runParser
  ) where

import Data.Text (Text, pack, unpack)

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec hiding (runParser)
import Prettyprinter  

import Glint.Syntax


type Parser = Parsec Text Text

-- Which markup/markdown features we want. 
data ParseOption = Paragraph | Bold | Italic
  deriving (Eq, Ord)
  
--data ParserContext = ParserContext [ParseOption] 

sc :: Parser () 
sc = L.space space1 (fail "") (L.skipBlockComment "[!|" "!]")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc

-- symbol :: Text -> Parser Text
-- symbol = L.symbol sc
glint_doc :: Parser [GlnRaw]
glint_doc = many1 glint <* eof

glint :: Parser GlnRaw
glint = do 
  sc
  len <- lexeme $ length <$> many1 (char '[')
  name <- glname
  args <- pargs
  kwargs <- try pkwargs <|> pure []
  _ <- lexeme $ char '|'
  body <- many (try (Left <$> glint)
                <|> (Right <$> non_block_str))
    
  _ <- parsen (char ']') len
  pure $ Node name args kwargs body

glname :: Parser Text
glname = lexeme $ 
  try (between (char '\'') (char '\'') (pack <$> many (satisfy (/= '\''))))
  <|> pack <$> many1 (satisfy (\v -> v /= '|' && v /= ' ' && v /= '=' && v /= ','))

arg_str :: Parser Text
arg_str = lexeme $ 
  try (between (char '\'') (char '\'') (pack <$> many (satisfy (/= '\''))))
  <|> pack <$> many1 (satisfy (\v -> v /= '|' && v /= ' ' && v /= '=' && v /= ','))

pargs :: Parser [Text]
pargs = many arg_str

pkwargs :: Parser [(Text, Text)]
pkwargs = do
  _ <- lexeme $ char ',' 
  many ((,) <$> arg_str <*> (lexeme (char '=') *> arg_str))

non_block_str :: Parser Text
non_block_str = pack <$> many1 (satisfy (\v -> (v /= ']') && (v /= '[')))

-- delimiters :: Int -> Parser Text
-- delimiters =
--   pack <$> many1 (satisfy (\v -> (v /= ']') && (v /= '[')))

parsen :: Parser a -> Int -> Parser [a]
parsen p n = case n of
  0 -> pure []
  k -> (:) <$> p <*> parsen p (k - 1)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

runParser :: Parser a -> Text -> Text -> Either (Doc ann) a
runParser p file input = case Megaparsec.runParser p (unpack file) input of
  Left err -> Left $ pretty $ errorBundlePretty err
  Right val -> Right val

instance ShowErrorComponent Text where 
  showErrorComponent = unpack
