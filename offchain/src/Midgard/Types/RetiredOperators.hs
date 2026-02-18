{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.RetiredOperators (Datum (..), MintRedeemer (..), rootKey, nodeKeyPrefix, nodeKeyPrefixLen) where

import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (BuiltinByteString, POSIXTime)
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
  = Init
  | Deinit
  | RetireOperator
      { newRetiredOperatorKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorAppendedNodeOutputIndex :: Integer
      , retiredOperatorAnchorNodeOutputIndex :: Integer
      , activeOperatorsRedeemerIndex :: Integer
      }
  | RecoverOperatorBond
      { retiredOperatorKey :: BuiltinByteString
      , removedNodeInputIndex :: Integer
      , anchorNodeInputIndex :: Integer
      }
  | RemoveOperatorBadState
      { slashedRetiredOperatorKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorSlashedNodeInputIndex :: Integer
      , retiredOperatorAnchorNodeInputIndex :: Integer
      , stateQueueRedeemerIndex :: Integer
      }
  | RemoveOperatorBadSettlement
      { slashedRetiredOperatorKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorSlashedNodeInputIndex :: Integer
      , retiredOperatorAnchorNodeInputIndex :: Integer
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
