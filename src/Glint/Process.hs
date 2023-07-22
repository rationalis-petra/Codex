module Glint.Process (process) where

import Control.Monad.Except (MonadError, throwError)

import Data.Text (Text)
import Data.Maybe
import Data.Foldable

import Prettyprinter.Render.Glint
import Prettyprinter
import Glint.Syntax


process :: forall m. MonadError DocErr m => GlnRaw -> m GlintDoc
process (Node typ args _ body) = 
  case typ of 
    -- structure
    "section" -> do
      name <- maybe (throwError "sections need a name") pure (listToMaybe args)
      nodes <- get_subnodes body 
      pure $ Section name nodes
      
    -- text formatting
    "b" -> Text Bold <$> get_text body
    "i" -> Text Italic <$> get_text body
    "mono" -> Text Monospace <$> get_text body
    
    -- math
    "m" -> InlMath <$> get_text body
    "M" -> BlockMath <$> get_text body

    -- "M" -> 
    _ -> throwError ("unsupported node type:" <+> pretty typ)

  where 
    get_text :: [Either GlnRaw Text] -> m Text
    get_text body =
      fold <$> mapM (either (throwError . ("Exptected text, got:" <+>) . pretty) pure) body
                            
    get_subnodes :: [Either GlnRaw Text] -> m [GlintDoc]
    get_subnodes body =
      mapM (either process (pure . Text Regular)) body
