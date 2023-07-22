{-# OPTIONS_GHC -Wno-orphans #-}
module Glint.Parse
  ( ParseOption(..)
  , glint
  , runParser
  ) where

import Data.Text (Text, pack, unpack)

--import qualified Text.Megaparsec.Char.Lexer as L
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

-- sc :: Parser () 
-- sc = L.space space1 (fail "") (fail "")

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc

-- symbol :: Text -> Parser Text
-- symbol = L.symbol sc

glint :: Parser GlnRaw
glint = do 
  len <- length <$> many1 (char '[')
  name <- glname
  args <- pure []
  kwargs <- pure []
  _ <- char '|'
  body <- many ((Left <$> glint) <|>
                (Right <$> non_block_str))
  _ <- parsen (char ']') len
  pure $ Node name args kwargs body

non_block_str :: Parser Text
non_block_str = pack <$> many1 (satisfy nonspecial)

glname :: Parser Text
glname = pack <$> many (satisfy nonspecial)


nonspecial :: Char -> Bool
nonspecial v = (v /= '[') && (v /= '|') && (v /= ']')

parsen :: Parser a -> Int -> Parser [a]
parsen p n = case n of
  0 -> pure []
  k -> (:) <$> p <*> parsen p (k - 1)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p
-- non_block_str :: Parser Text
-- non_block_str = pack <$> many (satisfy (/= '['))

runParser :: Parser a -> Text -> Text -> Either (Doc ann) a
runParser p file input = case Megaparsec.runParser p (unpack file) input of
  Left err -> Left $ pretty $ errorBundlePretty err
  Right val -> Right val

instance ShowErrorComponent Text where 
  showErrorComponent = unpack
