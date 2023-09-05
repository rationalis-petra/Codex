module Spec.Codex.Process (process_spec) where

import Control.Monad.Except (runExcept)
import Data.Text (Text)

import Prettyprinter
import Prettyprinter.Render.Codex

import TestFramework

import Codex.Syntax
import Codex.Process


process_spec :: TestGroup
process_spec = TestGroup "document-processing" $ Right
    [ proc_test "empty-block" (Node "m" [] [] []) (InlMath "")
    , proc_test "math-text" (Node "m" [] [] [Right "ax + b"]) (InlMath "ax + b")
    , proc_test "nested"
        (Node "section" ["Head"] [] [Left (Node "m" [] [] [Right "ax + b"])])
        (Section "Head" [InlMath "ax + b"])
    -- , proc_test "nested" "[m|[m|ax + b][m|cx + d]]"
    --   (Node "m" [] [] [ Left (Node "m" [] [] [Right "ax + b"])
    --                   , Left (Node "m" [] [] [Right "cx + d"])])
    ]
  where
    proc_test :: Text -> GlnRaw -> CodexDoc -> Test
    proc_test name raw out =
      case runExcept (process raw) of  
        Right val ->
          if val == out then
            Test name Nothing
          else
            Test name $ Just $
              "got:" <+> annotate (fg_colour (dull white)) (pretty val) <+>
              "expected:" <+> annotate (fg_colour (dull white)) (pretty out)
        Left msg -> Test name $ Just msg
