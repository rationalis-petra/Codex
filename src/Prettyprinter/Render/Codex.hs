module Prettyprinter.Render.Codex
  ( CodexStyle
  , DocErr
  , Colour
  , putDoc
  , putDocLn
  , bold
  , italicized
  , underlined
  , fg_colour
  , bg_colour
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , dull ) where


{-------------------------- Sigil. PRETTY PRINTER STYLE -------------------------}
{- This Module defines the CodexStyle type. This style is used throughout      -}
{- Codex in Prettyprinter docs. While not representative of a specific         -}
{- backedn, CodexStyle Documents can be converted to a variety of backends,    -}
{- including an ANSI terminal or HTML style.                                   -}
{-------------------------------------------------------------------------------}


import Prettyprinter
import qualified Prettyprinter.Render.Terminal as PPTerm


{------------------------------------ TYPES ------------------------------------}


newtype CodexStyle = CodexStyle PPTerm.AnsiStyle
type DocErr = Doc CodexStyle

type Colour = (Vividness, PPTerm.Color)
data Vividness = Normal | Dull


{---------------------------------- RENDERING ----------------------------------}


putDoc :: Doc CodexStyle -> IO ()  
putDoc doc = PPTerm.putDoc (reAnnotate (\(CodexStyle s) -> s) doc)

putDocLn :: Doc CodexStyle -> IO ()  
putDocLn doc = putDoc doc >> putStrLn ""


{-------------------------------- CREATE STYLES --------------------------------}


bold :: CodexStyle  
bold = CodexStyle PPTerm.bold

italicized :: CodexStyle  
italicized = CodexStyle PPTerm.italicized

underlined :: CodexStyle  
underlined = CodexStyle PPTerm.underlined

fg_colour :: Colour -> CodexStyle
fg_colour (v, c) = CodexStyle $ case v of 
  Normal -> PPTerm.color c
  Dull -> PPTerm.colorDull c

bg_colour :: Colour -> CodexStyle
bg_colour (v, c) = CodexStyle $ case v of 
  Normal -> PPTerm.bgColor c
  Dull -> PPTerm.bgColorDull c

{----------------------------------- COLOURS -----------------------------------}

black   :: Colour
black   = (Normal, PPTerm.Black)

red     :: Colour
red     = (Normal, PPTerm.Red)

green   :: Colour
green   = (Normal, PPTerm.Green)

yellow  :: Colour
yellow  = (Normal, PPTerm.Yellow)

blue    :: Colour
blue    = (Normal, PPTerm.Blue)

magenta :: Colour
magenta = (Normal, PPTerm.Magenta)

cyan    :: Colour
cyan    = (Normal, PPTerm.Cyan)

white   :: Colour
white   = (Normal, PPTerm.White)

dull :: Colour -> Colour
dull (_, c) = (Dull, c) 

        
