{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.RegisteredOperators (OperatorStatus, MintRedeemer (..), rootKey, nodeKeyPrefix, nodeKeyPrefixLen) where

import Data.ByteString.Char8 qualified as BS8
import GHC.Generics

import Cardano.Api qualified as C
import PlutusLedgerApi.V3
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import Ply (PlyArg)

rootKey :: C.AssetName
rootKey = C.UnsafeAssetName $ BS8.pack "MIDGARD_REGISTERED_OPERATORS"

nodeKeyPrefix :: C.AssetName
nodeKeyPrefix = C.UnsafeAssetName $ BS8.pack "MREG"

nodeKeyPrefixLen :: Int
nodeKeyPrefixLen = BS8.length $ C.serialiseToRawBytes nodeKeyPrefix

data OperatorStatus
  = Registered
  | Active
  | Retired
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$(makeIsDataSchemaIndexed ''OperatorStatus [('Registered, 0), ('Active, 1), ('Retired, 2)])

data MintRedeemer
  = Init
  | Deinit
  | Register
      { keyToPrepend :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , activeOperatorRefInputIndex :: Integer
      , activeOperatorAssetName :: BuiltinByteString
      , retiredOperatorRefInputIndex :: Integer
      , retiredOperatorAssetName :: BuiltinByteString
      , prependedNodeOutputIndex :: Integer
      , anchorNodeOutputIndex :: Integer
      }
  | Activate
      { nodeToActivateKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorRefInputIndex :: Integer
      , retiredOperatorAssetName :: BuiltinByteString
      , removedNodeInputIndex :: Integer
      , anchorNodeInputIndex :: Integer
      , activeOperatorsInsertedNodeOutputIndex :: Integer
      , activeOperatorsAnchorNodeOutputIndex :: Integer
      }
  | Deregister
      { nodeToDeregisterKey :: BuiltinByteString
      , removedNodeInputIndex :: Integer
      , anchorNodeInputIndex :: Integer
      }
  | RemoveDuplicateSlashBond
      { duplicateNodeKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , duplicateNodeRefInputIndex :: Integer
      , duplicateNodeRefInputAssetName :: BuiltinByteString
      , removedNodeInputIndex :: Integer
      , anchorNodeInputIndex :: Integer
      , witnessStatus :: OperatorStatus
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Make sure to use PlutusTx's tools for implementing data and blueprint instances for our custom type!
$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     , ('Register, 2)
     , ('Activate, 3)
     , ('Deregister, 4)
     , ('RemoveDuplicateSlashBond, 5)
     ]
 )

instance PlyArg MintRedeemer
