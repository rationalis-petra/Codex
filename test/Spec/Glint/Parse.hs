module Spec.Codex.Parse (parse_spec) where

import Data.Text (Text)

import Prettyprinter
import Prettyprinter.Render.Codex

import TestFramework

import Codex.Syntax
import Codex.Parse


parse_spec :: TestGroup
parse_spec = TestGroup "document-parsing" $ Right
    [ node_test "empty-block" "[m|]" (Node "m" [] [] [])
    , node_test "n-brackets" "[[m|]]" (Node "m" [] [] [])
    , node_test "text" "[m|ax + b]" (Node "m" [] [] [Right "ax + b"])
    , node_test "nested" "[m|[m|ax + b][m|cx + d]]"
      (Node "m" [] [] [ Left (Node "m" [] [] [Right "ax + b"])
                      , Left (Node "m" [] [] [Right "cx + d"])])
    ]
  where
    node_test :: Text -> Text -> GlnRaw -> Test
    node_test name text out =
      case runParser codex name text of  
        Right val ->
          if val == out then
            Test name Nothing
          else
            Test name $ Just $
              "got:" <+> annotate (fg_colour (dull white)) (pretty val) <+>
              "expected:" <+> annotate (fg_colour (dull white)) (pretty out)
        Left msg -> Test name $ Just msg
