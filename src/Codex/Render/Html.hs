{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Codex.Render.Html
  ( render
  , render_doc
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Text.Blaze.Html5 (Html, html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Codex.Syntax

  -- TODO: only load the modules (mathjax/node/tikz) that are needed!
  -- TODO: graphviz based on render_https://github.com/magjac/d3-graphviz
  -- also see
  -- https://stackoverflow.com/questions/50432295/using-tikz-in-a-browser-like-mathjax
  -- 
render_doc :: Text -> CodexDocument -> Html
render_doc root (CodexDocument {..}) = html $ do
  H.head $ do 
    H.title $ toHtml title
    H.script $ do 
      "MathJax = {"
      " processClass : 'math',"
      " loader: {load: ['[tex]/bussproofs']},"
      " tex: {packages: {'[+]': ['bussproofs']}}"
      "};"
    H.script ! A.src "https://polyfill.io/v3/polyfill.min.js?features=es6" $ pure ()
    H.script
      ! A.id "MathJax-script"
      ! A.src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
      $ pure ()
    H.script
      ! A.src (H.textValue $ root <> "/node_modules/d3/dist/d3.js")
      $ pure ()
    H.script
      ! A.src (H.textValue $ root <> "/node_modules/@hpcc-js/wasm/dist/graphviz.umd.js")
      $ pure ()
    H.script
      ! A.src (H.textValue $ root <> "/node_modules/d3-graphviz/build/d3-graphviz.js")
      $ pure ()
    -- H.script 
    --   ! A.src "http://tikzjax.com/v1/tikzjax.js"
    --   $ pure ()
    H.link
      ! A.rel "stylesheet"
      ! A.type_ "text/css"
      ! A.href (H.textValue $ root <> "/style.css")
    H.script $ do 
      "function loadHook() {"
      "  for (node of document.getElementsByClassName('dot')) {"
      "    let txt = node.innerText; node.innerHTML = ''; d3.select(node).graphviz().renderDot(txt);"
      "  }"
      "}"
    -- H.style $ do
    --   "a {color: #83accc}"
    --   "body {position: relative; width: 50%; left: 25%; color: #a8a7ab; background-color: #212025}"
    --   "table {border-style: solid none; border-color: #83accc; border-width: 2px; border-collapse: collapse; margin: 20px}"
    --   "td {padding: 2px 10px}"
    --   ".quote {position: relative; left: 5%; color: #929096}"
    --   ".tag {color: #aa4040}"
    --   ".paragraph {margin: 1em 0em}"
  H.body ! A.onload "loadHook()" $ mapM_ (render 1 root) body


render :: Int -> Text -> CodexDoc -> Html
render depth root doc = case doc of 
  Text tp text -> case tp of 
      Regular -> H.span $ toHtml text
      Bold -> H.b $ toHtml text
      Italic -> H.i $ toHtml text
      Monospace -> H.code $ toHtml text

  Title text -> H.h1 $ toHtml text 

  Section title nodes ->
    H.div ! A.class_ "section" $ do 
      header_n depth (toHtml title)
      mapM_ (render (depth + 1) root) nodes
  Paragraph nodes -> H.p $ mapM_ (render depth root) nodes
  Quote text mauthor -> 
    H.div ! A.class_ "quote" $ do
      toHtml text
      maybe (pure ()) (\v -> H.br >> toHtml (" -" <> v)) mauthor
  Linebreak -> H.br

  List b children -> 
    (if b then H.ol else H.ul) $ do
      let render_child (mtitle, children) = 
            H.li $ do
              maybe (pure ()) (H.b . toHtml . (<> ": ")) mtitle  
              mapM_ (render depth root) children
      mapM_ render_child children

  Tag typ children ->  
    H.p $ do
      H.b ! A.class_ "tag" $ toHtml (typ <> ": ")
      mapM_ (render depth root) children

  Ref link text -> 
    H.a ! A.href (url link) $ toHtml text 
    where 
      url (DocLink txt) =
        if Text.take 2 txt == "./"
        then H.textValue $ txt <> ".html" 
        else H.textValue $ root <> "/" <> txt <> ".html" 
      url (URLLink txt) = H.textValue txt
      
  Definition name children ->
    H.div ! A.class_ "paragraph" $ do
      H.b "Definition"
      toHtml (" (" <> name <> "). ")
      mapM_ (render depth root) children
  Proposition mname children -> 
    H.div ! A.class_ "paragraph" $ do
      H.b "Proposition"
      toHtml $ maybe ". " (\name -> " (" <> name <> "). ") mname
      mapM_ (render depth root) children
  Lemma mname children -> 
    H.div ! A.class_ "paragraph" $ do 
      H.b "Lemma"
      toHtml $ maybe ". " (\name -> " (" <> name <> "). ") mname
      mapM_ (render depth root) children
  Corollary mname children -> 
    H.div ! A.class_ "paragraph" $ do 
      H.b "Corollary"
      toHtml $ maybe ". " (\name -> " (" <> name <> "). ") mname
      mapM_ (render depth root) children
  Example mname _ children  -> 
    H.div ! A.class_ "paragraph" $ do
      H.b "Example"
      toHtml $ maybe ". " (\name -> " (" <> name <> "). ") mname
      mapM_ (render depth root) children
  Proof _ children -> 
    H.div ! A.class_ "paragraph" $ do
      H.i "Proof: "
      mapM_ (render depth root) children
  InlMath text -> 
    H.span ! A.class_ "math" $ toHtml ("\\(" <> text <> "\\)")
  BlockMath text -> 
    H.p ! A.class_ "math" $ toHtml ("\\[" <> text <> "\\]")

  Render typ text -> 
      H.div ! A.class_ (H.textValue typ) $ toHtml text
  Code typ text -> 
      H.code ! A.class_ (H.textValue typ) $ toHtml text

  Table rows -> H.table $ mapM_ render_row rows
    where
      render_row = H.tr . mapM_ render_val
      render_val = H.td . mapM_ (render depth root)


  Bib refs -> 
    H.div $ do
      H.h2 "Bibliography"
      H.ul $ mapM_ (H.li . toHtml) refs


header_n :: Int -> Html -> Html 
header_n n = case n of 
  0 -> H.h1
  1 -> H.h2
  2 -> H.h3
  3 -> H.h4
  4 -> H.h5
  _ -> H.h6
