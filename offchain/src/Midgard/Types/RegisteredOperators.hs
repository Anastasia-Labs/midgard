{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.RegisteredOperators (
  DuplicateOperatorStatus (..),
  MintRedeemer (..),
  NodeData (..),
  Datum,
  rootAssetName,
  nodeAssetNamePrefix,
  nodeAssetNamePrefixLen,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  PubKeyHash,
  TxOutRef,
 )
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import Ply (PlyArg)

import Midgard.Types.LinkedList qualified as LinkedList

rootAssetName :: C.AssetName
rootAssetName = C.UnsafeAssetName $ BS8.pack "MIDGARD_REGISTERED_OPERATORS"

nodeAssetNamePrefix :: ByteString
nodeAssetNamePrefix = BS8.pack "MREG"

nodeAssetNamePrefixLen :: Int
nodeAssetNamePrefixLen = BS8.length nodeAssetNamePrefix

newtype NodeData = NodeData
  { operator :: PubKeyHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''NodeData
     [ ('NodeData, 0)
     ]
 )

type Datum = LinkedList.Element BuiltinByteString NodeData

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
  | Deinit
  | RegisterOperator
      { registeringOperator :: PubKeyHash
      , rootOutputIndex :: Integer
      , registeredNodeOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , activeOperatorsElementRefInputIndex :: Integer
      , retiredOperatorsElementRefInputIndex :: Integer
      }
  | ActivateOperator
      { activatingOperator :: PubKeyHash
      , anchorElementInputOutRef :: TxOutRef
      , anchorElementOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , retiredOperatorsElementRefInputIndex :: Integer
      , activeOperatorsRedeemerIndex :: Integer
      }
  | DeregisterOperator
      { deregisteringOperator :: PubKeyHash
      , anchorElementInputOutRef :: TxOutRef
      , anchorElementOutputIndex :: Integer
      }
  | SlashDuplicateOperator
      { duplicateOperator :: PubKeyHash
      , anchorElementInputOutRef :: TxOutRef
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
     , ('SlashDuplicateOperator, 5)
     ]
 )

instance PlyArg MintRedeemer
