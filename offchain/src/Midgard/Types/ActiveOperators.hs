{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.ActiveOperators (
  NodeData (..),
  Datum,
  SpendRedeemer (..),
  MintRedeemer (..),
  rootKey,
  nodeKeyPrefix,
  nodeKeyPrefixLen,
) where

import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (BuiltinByteString, POSIXTime, PubKeyHash)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

import Ply (PlyArg)

import Midgard.Types.LinkedList qualified as LinkedList

rootKey :: C.AssetName
rootKey = C.UnsafeAssetName $ BS8.pack "MIDGARD_ACTIVE_OPERATORS"

nodeKeyPrefix :: C.AssetName
nodeKeyPrefix = C.UnsafeAssetName $ BS8.pack "MACT"

nodeKeyPrefixLen :: Int
nodeKeyPrefixLen = BS8.length $ C.serialiseToRawBytes nodeKeyPrefix

newtype NodeData = NodeData
  { bondUnlockTime :: Maybe POSIXTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''NodeData
     [ ('NodeData, 0)
     ]
 )

type Datum = LinkedList.Element BuiltinByteString NodeData

data SpendRedeemer
  = ListStateTransition
  | UpdateBondHoldNewState
      { activeNodeInputIndex :: Integer
      , activeNodeOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , stateQueueInputIndex :: Integer
      , stateQueueRedeemerIndex :: Integer
      }
  | UpdateBondHoldNewSettlement
      { activeNodeInputIndex :: Integer
      , activeNodeOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , settlementInputIndex :: Integer
      , settlementRedeemerIndex :: Integer
      , newBondUnlockTime :: POSIXTime
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SpendRedeemer
     [ ('ListStateTransition, 0)
     , ('UpdateBondHoldNewState, 1)
     , ('UpdateBondHoldNewSettlement, 2)
     ]
 )

instance PlyArg SpendRedeemer

-- Mint redeemer

data MintRedeemer
  = Init {outputIndex :: Integer}
  | Deinit {inputIndex :: Integer}
  | ActivateOperator
      { newActiveOperatorKey :: PubKeyHash
      , activeOperatorAnchorElementInputIndex :: Integer
      , activeOperatorAnchorElementOutputIndex :: Integer
      , activeOperatorInsertedNodeOutputIndex :: Integer
      , registeredOperatorsRedeemerIndex :: Integer
      }
  | RemoveOperatorBadState
      { slashedActiveOperatorKey :: PubKeyHash
      , hubOracleRefInputIndex :: Integer
      , activeOperatorAnchorElementInputIndex :: Integer
      , activeOperatorSlashedNodeInputIndex :: Integer
      , activeOperatorAnchorElementOutputIndex :: Integer
      , stateQueueRedeemerIndex :: Integer
      }
  | RemoveOperatorBadSettlement
      { slashedActiveOperatorKey :: PubKeyHash
      , hubOracleRefInputIndex :: Integer
      , activeOperatorAnchorElementInputIndex :: Integer
      , activeOperatorSlashedNodeInputIndex :: Integer
      , activeOperatorAnchorElementOutputIndex :: Integer
      , settlementInputIndex :: Integer
      , settlementRedeemerIndex :: Integer
      }
  | RetireOperator
      { activeOperatorKey :: PubKeyHash
      , activeOperatorAnchorElementInputIndex :: Integer
      , activeOperatorRemovedNodeInputIndex :: Integer
      , activeOperatorAnchorElementOutputIndex :: Integer
      , retiredOperatorsRedeemerIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     , ('ActivateOperator, 2)
     , ('RemoveOperatorBadState, 3)
     , ('RemoveOperatorBadSettlement, 4)
     , ('RetireOperator, 5)
     ]
 )

instance PlyArg MintRedeemer
