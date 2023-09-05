module Codex.Syntax
  ( GlnRaw(..)
  , CodexDocument(..)
  , CodexDoc(..)
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

data CodexDocument = CodexDocument  
  { title :: Text
  , body :: [CodexDoc] }

data Link = DocLink Text | URLLink Text
  deriving (Show, Ord, Eq)

data Lang = Graphviz
  deriving (Show, Eq, Ord)

data CodexDoc 
  -- structrue
  = Section Text [CodexDoc]
  | Title Text
  | Paragraph [CodexDoc]
  | Quote Text (Maybe Text)
  -- Basic eleents of text & document structure
  | Text TextProperty Text
  | Ref Link Text
  | Linebreak

  -- Math/Tex
  | InlMath Text
  | BlockMath Text

  -- Definitions, Examples
  | Definition Text [CodexDoc]
  | Example (Maybe Text) (Maybe Text) [CodexDoc]
  | Proposition (Maybe Text) [CodexDoc]
  | Lemma (Maybe Text) [CodexDoc]
  | Proof (Maybe Text) [CodexDoc]
  | Corollary (Maybe Text) [CodexDoc]

  -- Lists, bool = ordered/unordered
  | List Bool [(Maybe Text, [CodexDoc])]

  -- Tags
  | Tag Text [CodexDoc]

  -- Diagramming and langauges
  | Render Text Text
  | Code Text Text
  | Table [[[CodexDoc]]]

  -- bibliography (move into document?)
  | Bib [Text]

  deriving (Eq, Ord, Show)

instance Pretty CodexDoc where
  pretty node = case node of
    (Text prop text) -> "[" <> pretty prop <> "|" <> pretty text <> "]"
    (InlMath text) -> "[m|" <> pretty text <> "]"
    _ -> ("pretty not implemented for:" <+> (pretty $ show $ node))

