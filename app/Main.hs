module Main (main) where

import Data.Text (pack)
import Control.Monad.Except(runExcept)

import Options.Applicative
import Text.Blaze.Html.Renderer.String

import Prettyprinter.Render.Glint
import Glint.Parse
import Glint.Process
import Glint.Render.Html
  

data Options = Options { input_file :: String
                       , output_file :: String }

opts :: Parser Options
opts = Options
  <$> strOption
    ( long "input-file"
    <> short 'i'
    <> help "Specify an (input) filename" )
  <*> strOption
    ( long "output-file"
    <> short 'o'
    <> help "Specify an (output) filename. Leave empty to deduce from input filename"
    <> value "" )

  
main :: IO ()
main = do
  options <- execParser $ info (opts <**> helper)
    ( fullDesc
    <> progDesc "Process Glint Documents"
    <> header "An implementation of the Glint Markup Langauge")
  infile <- readFile $ input_file options

  case runParser glint (pack $ input_file options) (pack infile) of 
    Right val ->
      case runExcept (process val) of 
        Right val -> 
          writeFile (output_file options) (renderHtml $ render_doc val)
        Left err -> putDocLn err
    Left err -> putDocLn err



