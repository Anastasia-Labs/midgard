module Midgard.Node.Server.Protocol (
  protocolInfoHandler,
) where

import Control.Monad.Reader (asks)
import Data.Text qualified as Text
import GHC.Natural (Natural)
import Midgard.Node.Codec.Scripts (migardSupportedScriptLanguages)
import Midgard.Node.Config (MidgardConfig (..), MidgardNodeConfig (..), ProtocolConfig (..))
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Server.JSON.Types (NaturalJSON (..))
import Midgard.Node.Server.Monad (ServerM (..))
import Midgard.Node.Server.Protocol.Types (
  Network (..),
  ProtocolFeeParameters (..),
  ProtocolInfoApiVersion (..),
  ProtocolInfoResponse (..),
  ProtocolSubmissionLimits (..),
  ProtocolValidation (..),
 )
import Text.Read (readMaybe)

protocolInfoHandler :: ServerM ProtocolInfoResponse
protocolInfoHandler = do
  env <- ServerM (asks id)
  let protocol = env.config.protocol
  pure
    ProtocolInfoResponse
      { apiVersion = ProtocolInfoApiV_1
      , network = parseNetwork env.config.midgard.network
      , -- TODO(protocol): source this from the Midgard native transaction codec
        -- once that module is wired into the node executable.
        midgardNativeTxVersion = 1
      , -- TODO(provider): the TypeScript node queries lucid.api.currentSlot().
        -- The Haskell node should use the Cardano provider/indexer here.
        currentSlot = NaturalJSON 0
      , -- These mirror the current OpenAPI/TypeScript protocol surface. Keep the
        -- shape stable even while implementation details move behind providers.
        supportedScriptLanguages = migardSupportedScriptLanguages
      , protocolFeeParameters =
          ProtocolFeeParameters
            { minFeeA = parseNaturalConfig protocol.minFeeA
            , minFeeB = parseNaturalConfig protocol.minFeeB
            }
      , submissionLimits =
          ProtocolSubmissionLimits
            { maxSubmitTxCborBytes = protocol.maxSubmitTxCborBytes
            }
      , validation =
          ProtocolValidation
            { strictnessProfile = protocol.validationStrictnessProfile
            , localValidationIsAuthoritative = False
            }
      }

parseNetwork :: Text.Text -> Network
parseNetwork network =
  case Text.toLower network of
    "mainnet" -> Mainnet
    "preview" -> Preview
    "preprod" -> Preprod
    "custom" -> Custom
    _ -> Custom

parseNaturalConfig :: Text.Text -> NaturalJSON
parseNaturalConfig value =
  -- TODO(config): make protocol fee fields typed at config parse time so bad
  -- values fail startup instead of falling back here.
  NaturalJSON (maybe 0 id (readMaybe (Text.unpack value) :: Maybe Natural))
