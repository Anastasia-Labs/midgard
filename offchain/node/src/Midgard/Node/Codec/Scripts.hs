module Midgard.Node.Codec.Scripts (
  ScriptLanguageName (..),
  ScriptLanguageTag (..),
  SupportedScriptLanguageTag (..),
  migardSupportedScriptLanguages,
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Deriving.Aeson

-- | JSON encodes into "PlutusV3" or "MidgardV1".
data ScriptLanguageName = PlutusV3 | MidgardV1
  deriving stock (Eq, Ord, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[TagSingleConstructors] ScriptLanguageName

-- | The int tag used for a specific script type.
data ScriptLanguageTag = PlutusV3Tag | MidgardV1Tag
  deriving stock (Eq, Ord, Show, Generic)

-- | Follows the typescript node codec.
tagToInt :: ScriptLanguageTag -> Int
tagToInt PlutusV3Tag = 2
tagToInt MidgardV1Tag = 0x80

instance ToJSON ScriptLanguageTag where
  toJSON = toJSON . tagToInt
  toEncoding = toEncoding . tagToInt

instance FromJSON ScriptLanguageTag where
  parseJSON value = do
    tag <- parseJSON value :: Parser Int
    case tag of
      2 -> pure PlutusV3Tag
      0x80 -> pure MidgardV1Tag
      _ -> fail ("Unsupported script language tag: " <> show tag)

-- | Follows the typescript defined format.
data SupportedScriptLanguageTag = SupportedScriptLanguageTag
  { name :: ScriptLanguageName
  , tag :: ScriptLanguageTag
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Equivalent to MIDGARD_SUPPORTED_SCRIPT_LANGUAGES constant in typescript.
migardSupportedScriptLanguages :: [SupportedScriptLanguageTag]
migardSupportedScriptLanguages =
  [ SupportedScriptLanguageTag PlutusV3 PlutusV3Tag
  , SupportedScriptLanguageTag MidgardV1 MidgardV1Tag
  ]
