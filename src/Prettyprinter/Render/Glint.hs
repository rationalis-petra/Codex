module Prettyprinter.Render.Glint
  ( GlintStyle
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


{-------------------------- GLYPH PRETTY PRINTER STYLE -------------------------}
{- This Module defines the GlintStyle type. This style is used throughout      -}
{- Glint in Prettyprinter docs. While not representative of a specific         -}
{- backedn, GlintStyle Documents can be converted to a variety of backends,    -}
{- including an ANSI terminal or HTML style.                                   -}
{-------------------------------------------------------------------------------}


import Prettyprinter
import qualified Prettyprinter.Render.Terminal as PPTerm


{------------------------------------ TYPES ------------------------------------}


newtype GlintStyle = GlintStyle PPTerm.AnsiStyle
type DocErr = Doc GlintStyle

type Colour = (Vividness, PPTerm.Color)
data Vividness = Normal | Dull


{---------------------------------- RENDERING ----------------------------------}


putDoc :: Doc GlintStyle -> IO ()  
putDoc doc = PPTerm.putDoc (reAnnotate (\(GlintStyle s) -> s) doc)

putDocLn :: Doc GlintStyle -> IO ()  
putDocLn doc = putDoc doc >> putStrLn ""


{-------------------------------- CREATE STYLES --------------------------------}


bold :: GlintStyle  
bold = GlintStyle PPTerm.bold

italicized :: GlintStyle  
italicized = GlintStyle PPTerm.italicized

underlined :: GlintStyle  
underlined = GlintStyle PPTerm.underlined

fg_colour :: Colour -> GlintStyle
fg_colour (v, c) = GlintStyle $ case v of 
  Normal -> PPTerm.color c
  Dull -> PPTerm.colorDull c

bg_colour :: Colour -> GlintStyle
bg_colour (v, c) = GlintStyle $ case v of 
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

        
