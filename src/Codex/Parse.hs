{-# OPTIONS_GHC -Wno-orphans #-}
module Codex.Parse
  ( ParseOption(..)
  , codex
  , codex_doc
  , runParser
  ) where

import qualified Data.Text as Text
import Data.Text (Text, pack, unpack, singleton)

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec hiding (runParser)
import Prettyprinter  

import Codex.Syntax


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
codex_doc :: Parser [GlnRaw]
codex_doc = many1 codex <* eof

codex :: Parser GlnRaw
codex = do 
  sc
  len <- lexeme $ length <$> many1 (char '[')
  name <- glname
  args <- pargs
  kwargs <- try pkwargs <|> pure []
  -- TODO: preserve whitespace, for if text needs it 
  _ <- lexeme $ char '|'
  body <- many (try (Right <$> delimiters len)
                <|> (Left <$> codex))
    
  _ <- lexeme $ parsen (char ']') len
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

-- non_block_str :: Parser Text
-- non_block_str =  
--   pack <$> many1 (satisfy (\v -> (v /= ']') && (v /= '[')))

delimiters :: Int -> Parser Text
delimiters n = loop n ""   where 
    loop :: Int -> Text -> Parser Text
    loop n txt = do
      val  <- (try $ do
        cs <- many (satisfy (== '['))
        case cs of
          [] -> do
            cs' <- many (satisfy (== ']'))
            case cs' of
              [] -> Just . singleton <$> satisfy (const True)
              _ | length cs' < n -> pure $ Just $ pack cs'
                | otherwise -> fail "rbrace"
          _ | length cs < n -> pure $ Just $ pack cs
            | otherwise -> fail "lbrace")
             <|> pure Nothing
      case val of
        Just txt' -> loop n (txt <> txt')
        Nothing ->
          if Text.null txt
          then fail "delimiters"
          else pure (" " <> txt) -- this is a hack; we need to make it so
               -- whitespace-only is ignored as 'not text'

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
