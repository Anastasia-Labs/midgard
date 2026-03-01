{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.RetiredOperators (Datum (..), MintRedeemer (..), rootKey, nodeKeyPrefix, nodeKeyPrefixLen) where

import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

import Ply (PlyArg)

rootKey :: C.AssetName
rootKey = C.UnsafeAssetName $ BS8.pack "MIDGARD_ACTIVE_OPERATORS"

nodeKeyPrefix :: C.AssetName
nodeKeyPrefix = C.UnsafeAssetName $ BS8.pack "MACT"

nodeKeyPrefixLen :: Int
nodeKeyPrefixLen = BS8.length $ C.serialiseToRawBytes nodeKeyPrefix

newtype Datum = Datum
  { bondUnlockTime :: Maybe POSIXTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''Datum
     [ ('Datum, 0)
     ]
 )

data MintRedeemer
  = Init {outputIndex :: Integer}
  | Deinit {inputIndex :: Integer}
  | RetireOperator
      { newRetiredOperatorKey :: PubKeyHash
      , bondUnlockTime :: Maybe POSIXTime
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorAnchorElementInputIndex :: Integer
      , retiredOperatorAnchorElementOutputIndex :: Integer
      , retiredOperatorInsertedNodeOutputIndex :: Integer
      , activeOperatorsRedeemerIndex :: Integer
      }
  | RecoverOperatorBond
      { retiredOperatorKey :: PubKeyHash
      , retiredOperatorAnchorElementInputIndex :: Integer
      , retiredOperatorRemovedNodeInputIndex :: Integer
      , retiredOperatorAnchorElementOutputIndex :: Integer
      }
  | RemoveOperatorBadState
      { slashedRetiredOperatorKey :: PubKeyHash
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorAnchorElementInputIndex :: Integer
      , retiredOperatorSlashedNodeInputIndex :: Integer
      , retiredOperatorAnchorElementOutputIndex :: Integer
      , stateQueueRedeemerIndex :: Integer
      }
  | RemoveOperatorBadSettlement
      { slashedRetiredOperatorKey :: PubKeyHash
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorAnchorElementInputIndex :: Integer
      , retiredOperatorSlashedNodeInputIndex :: Integer
      , retiredOperatorAnchorElementOutputIndex :: Integer
      , settlementInputIndex :: Integer
      , settlementRedeemerIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     , ('RetireOperator, 2)
     , ('RecoverOperatorBond, 3)
     , ('RemoveOperatorBadState, 4)
     , ('RemoveOperatorBadSettlement, 5)
     ]
 )

instance PlyArg MintRedeemer
