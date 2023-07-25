module Glint.Syntax
  ( GlnRaw(..)
  , GlintDocument(..)
  , GlintDoc(..)
  , Lang(..)
  , TextProperty(..)
  , Link(..)
  ) where

import Data.Text (Text)

import Prettyprinter

data GlnRaw
  = Node Text [Text] [(Text, Text)] [Either GlnRaw Text]
  deriving (Show, Ord, Eq)

instance Pretty GlnRaw where
  pretty (Node name args kwargs body) =
    "[" <> pretty name <+> (sep . map pretty $ args)
    <> (if (not (null kwargs)) then "," <> (sep . map pretty $ kwargs) else "")
    <> "|" <> sep (map (either pretty pretty) body) <> "]"


data TextProperty = Regular | Bold | Italic | Monospace --  | Strikethrough
  deriving (Show, Ord, Eq)

instance Pretty TextProperty where
  pretty p = case p of 
    Regular -> "text"
    Bold -> "b"
    Italic -> "i"
    Monospace -> "mono"

data GlintDocument = GlintDocument  
  { title :: Text
  , body :: [GlintDoc] }

data Link = DocLink Text | URLLink Text
  deriving (Show, Ord, Eq)

data Lang = Graphviz
  deriving (Show, Eq, Ord)

data GlintDoc 
  -- structrue
  = Section Text [GlintDoc]
  | Title Text
  | Paragraph [GlintDoc]
  | Quote Text (Maybe Text)
  -- Basic eleents of text & document structure
  | Text TextProperty Text
  | Ref Link Text
  | Linebreak

  -- Math/Tex
  | InlMath Text
  | BlockMath Text

  -- Definitions, Examples
  | Definition Text [GlintDoc]
  | Example (Maybe Text) (Maybe Text) [GlintDoc]
  | Proposition (Maybe Text) [GlintDoc]
  | Lemma (Maybe Text) [GlintDoc]
  | Proof (Maybe Text) [GlintDoc]

  -- Lists, bool = ordered/unordered
  | List Bool [(Maybe Text, [GlintDoc])]

  -- Tags
  | Tag Text [GlintDoc]

  -- Diagramming langauges
  | Render Text Text
  | Table [[[GlintDoc]]]

  -- bibliography (move into document?)
  | Bib [Text]

  deriving (Eq, Ord, Show)

instance Pretty GlintDoc where
  pretty node = case node of
    (Text prop text) -> "[" <> pretty prop <> "|" <> pretty text <> "]"
    (InlMath text) -> "[m|" <> pretty text <> "]"
    _ -> ("pretty not implemented for:" <+> (pretty $ show $ node))

