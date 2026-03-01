{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.RegisteredOperators (
  DuplicateOperatorStatus (..),
  MintRedeemer (..),
  rootKey,
  nodeKeyPrefix,
  nodeKeyPrefixLen,
) where

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

data DuplicateOperatorStatus
  = DuplicateIsRegistered
  | DuplicateIsActive {hubOracleRefInputIndex :: Integer}
  | DuplicateIsRetired
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''DuplicateOperatorStatus
     [ ('DuplicateIsRegistered, 0)
     , ('DuplicateIsActive, 1)
     , ('DuplicateIsRetired, 2)
     ]
 )

data MintRedeemer
  = Init {outputIndex :: Integer}
  | Deinit {inputIndex :: Integer}
  | RegisterOperator
      { registeringOperator :: PubKeyHash
      , rootInputIndex :: Integer
      , rootOutputIndex :: Integer
      , registeredNodeOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , activeOperatorsElementRefInputIndex :: Integer
      , retiredOperatorsElementRefInputIndex :: Integer
      }
  | ActivateOperator
      { activatingOperator :: PubKeyHash
      , anchorElementInputIndex :: Integer
      , removedNodeInputIndex :: Integer
      , anchorElementOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorsElementRefInputIndex :: Integer
      , activeOperatorsRedeemerIndex :: Integer
      }
  | DeregisterOperator
      { deregisteringOperator :: PubKeyHash
      , anchorElementInputIndex :: Integer
      , removedNodeInputIndex :: Integer
      , anchorElementOutputIndex :: Integer
      }
  | RemoveDuplicateSlashBond
      { duplicateOperator :: PubKeyHash
      , anchorElementInputIndex :: Integer
      , removedNodeInputIndex :: Integer
      , anchorElementOutputIndex :: Integer
      , duplicateNodeRefInputIndex :: Integer
      , duplicateOperatorStatus :: DuplicateOperatorStatus
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     , ('RegisterOperator, 2)
     , ('ActivateOperator, 3)
     , ('DeregisterOperator, 4)
     , ('RemoveDuplicateSlashBond, 5)
     ]
 )

instance PlyArg MintRedeemer
