module Glint.Render.Html
  ( render
  , render_doc
  ) where

import Text.Blaze.Html5 (Html, html, toHtml)
import qualified Text.Blaze.Html5 as H
--import qualified Text.Blaze.Html5.Attributes as A

import Glint.Syntax

render :: GlintDocument -> Html
render (GlintDocument {..}) = html $ do
  H.head $ do 
    H.title $ toHtml title
  H.body $ render_doc body

render_doc :: GlintDoc -> Html
render_doc doc = case doc of 
  Text tp text -> case tp of 
      Regular -> H.span $ toHtml text
      Bold -> H.b $ toHtml text
      Italic -> H.i $ toHtml text
      Monospace -> H.code $ toHtml text
  _ -> H.p "unknown doc" 
  
