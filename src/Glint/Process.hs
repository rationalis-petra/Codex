module Glint.Process
  ( process_doc
  , process
  ) where

import Prelude hiding (take, drop)
import Control.Monad.Except (MonadError, throwError)

import Data.Text (Text, uncons, take, drop)
import Data.Maybe
import Data.Foldable

import Prettyprinter.Render.Glint
import Prettyprinter
import Glint.Syntax

process_doc :: forall m. MonadError DocErr m => [GlnRaw] -> m GlintDocument
process_doc [] = pure $ GlintDocument "" []
process_doc (Node "title" _ _ body : ds) = do
  title_text <- get_text body
  (GlintDocument title_text . (Title title_text :)) <$> mapM process ds
  where 
    get_text :: [Either GlnRaw Text] -> m Text
    get_text body =
      fold <$> mapM (either (throwError . ("Exptected text, got:" <+>) . pretty) pure) body
process_doc ds = GlintDocument "" <$> mapM process ds


process :: forall m. MonadError DocErr m => GlnRaw -> m GlintDoc
process (Node typ args kwargs body) = 
  case typ of 
    -- structure
    "section" -> do
      name <- maybe (throwError "sections need a name") pure (listToMaybe args)
      nodes <- get_subnodes body 
      pure $ Section name nodes
    "p" -> Paragraph <$> get_subnodes body

    -- text formatting
    "b" -> Text Bold <$> get_text body
    "i" -> Text Italic <$> get_text body
    "mono" -> Text Monospace <$> get_text body
    "quote" -> flip Quote (listToMaybe args) <$> get_text body
    "br" -> pure $ Linebreak

    -- links
    "ref" -> Ref <$> (get_link <$> get_arg 0 args) <*> get_text body
    
    -- math
    "def" -> Definition <$> get_arg 0 args <*> get_subnodes body
    "ex" -> Example (listToMaybe args) (lookup_kwarg "for" kwargs) <$> get_subnodes body
    "prop" -> Proposition (listToMaybe args) <$> get_subnodes body
    "lemma" -> Lemma (listToMaybe args) <$> get_subnodes body
    "proof" -> Proof (lookup_kwarg "for" kwargs) <$> get_subnodes body
    "m" -> InlMath <$> get_text body
    "M" -> BlockMath <$> get_text body

    -- lists
    "ol" -> List True <$> get_list_els body
    "ul" -> List False <$> get_list_els body
    -- dl
    "bib" -> Bib <$> get_bib_els body
    "table" -> Table <$> get_table body
    "render" -> Render <$> get_arg 0 args <*> get_text body

    _ -> case uncons typ of 
      Just ('#', tl) -> Tag tl <$> get_subnodes body
      -- Tags
      _ -> throwError ("unsupported node type:" <+> pretty typ)

  where 
    get_text :: [Either GlnRaw Text] -> m Text
    get_text body =
      fold <$> mapM (either (throwError . ("Exptected text, got:" <+>) . pretty) pure) body
                            
    get_subnodes :: [Either GlnRaw Text] -> m [GlintDoc]
    get_subnodes =
      mapM (either process (pure . Text Regular))

    get_list_els :: [Either GlnRaw Text] -> m [(Maybe Text, [GlintDoc])]
    get_list_els = mapM (either get_el (throwError . ("Exptected list element, got text:" <+>) . pretty))
      where
        get_el :: GlnRaw -> m (Maybe Text, [GlintDoc])
        get_el node = case node of 
          (Node "li" args _ body) -> ((,) (listToMaybe args) <$> get_subnodes body)
          _ -> throwError ("expected list item, got:" <+> pretty node)

    get_bib_els :: [Either GlnRaw Text] -> m [Text]
    get_bib_els = mapM (either get_el (throwError . ("Exptected bibrfef element, got text:" <+>) . pretty))
      where
        get_el :: GlnRaw -> m Text
        get_el node = case node of 
          (Node "bibref" _ _ body) -> get_text body
          _ -> throwError ("expected bibref element, got:" <+> pretty node)

    get_arg :: Int -> [a] -> m a
    get_arg 0 (s:_) = pure s
    get_arg k (_:ss) = get_arg (k - 1) ss
    get_arg _ _ = throwError "not enough args!"

    get_link :: Text -> Link 
    get_link text = 
      if (take 4 text == "doc:") then 
        DocLink $ drop 4 text
      else 
        URLLink text

    get_table :: [Either GlnRaw Text] -> m [[[GlintDoc]]]
    get_table t = mapM (either get_rows (throwError . ("Exptected row element, got text:" <+>) . pretty)) t
      where
        get_rows (Node "row" _ _ body) = mapM (either get_val (throwError . ("Exptected val element, got text:" <+>) . pretty)) body
        get_rows n = throwError $ "Exptected row element, got:" <> pretty n

        get_val (Node "tv" _ _ body) = get_subnodes body 
        get_val n = throwError $ "Exptected tv element, got:" <> pretty n



lookup_kwarg :: Eq a => a -> [(a, b)] -> Maybe b
lookup_kwarg _ [] = Nothing
lookup_kwarg a ((a', b):bs)
  | a == a' = Just b
  | otherwise = lookup_kwarg a bs
