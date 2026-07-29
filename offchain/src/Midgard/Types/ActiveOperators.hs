{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.ActiveOperators (
  NodeData (..),
  OperatorRemovalSchedulerSync (..),
  Datum,
  SpendRedeemer (..),
  MintRedeemer (..),
  rootAssetName,
  nodeAssetNamePrefix,
  nodeAssetNamePrefixLen,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (BuiltinByteString, POSIXTime, PubKeyHash, TxOutRef)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

import Ply (PlyArg)

import Midgard.Types.LinkedList qualified as LinkedList
import Midgard.Types.OperatorDirectory (SlashingArguments)

rootAssetName :: C.AssetName
rootAssetName = C.UnsafeAssetName $ BS8.pack "MIDGARD_ACTIVE_OPERATORS"

nodeAssetNamePrefix :: ByteString
nodeAssetNamePrefix = BS8.pack "MACT"

nodeAssetNamePrefixLen :: Int
nodeAssetNamePrefixLen = BS8.length nodeAssetNamePrefix

data NodeData = NodeData
  { bondUnlockTime :: Maybe POSIXTime
  , inactivityStrikes :: Integer
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
      { activeOperator :: PubKeyHash
      , activeNodeInputIndex :: Integer
      , activeNodeOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , stateQueueMintRedeemerIndex :: Integer
      }
  | UpdateBondHoldNewSettlement
      { activeOperator :: PubKeyHash
      , activeNodeInputIndex :: Integer
      , activeNodeOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , settlementInputIndex :: Integer
      , settlementRedeemerIndex :: Integer
      , resolutionTime :: POSIXTime
      }
  | StrikeForInactivity
      { activeNodeInputIndex :: Integer
      , activeNodeOutputIndex :: Integer
      , operator :: PubKeyHash
      , activeNodeLink :: Maybe LinkedList.NodeKey
      , schedulerInputIndex :: Integer
      , schedulerRedeemerIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SpendRedeemer
     [ ('ListStateTransition, 0)
     , ('UpdateBondHoldNewState, 1)
     , ('UpdateBondHoldNewSettlement, 2)
     , ('StrikeForInactivity, 3)
     ]
 )

instance PlyArg SpendRedeemer

data OperatorRemovalSchedulerSync
  = ShowOperatorIsInactive
      { schedulerRefInputIndex :: Integer
      }
  | ShowSchedulerIsAdvancing
      { schedulerInputIndex :: Integer
      , schedulerRedeemerIndex :: Integer
      , removingOperatorsAnchorElementKey :: Maybe LinkedList.NodeKey
      , removingOperatorIsTheLastMember :: Bool
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''OperatorRemovalSchedulerSync
     [ ('ShowOperatorIsInactive, 0)
     , ('ShowSchedulerIsAdvancing, 1)
     ]
 )

-- Mint redeemer

data MintRedeemer
  = Init {outputIndex :: Integer}
  | Deinit
  | ActivateOperator
      { newActiveOperatorKey :: PubKeyHash
      , activeOperatorAnchorElementOutputIndex :: Integer
      , activeOperatorInsertedNodeOutputIndex :: Integer
      , registeredOperatorsRedeemerIndex :: Integer
      , activeOperatorsSetWasEmpty :: Bool
      }
  | RetireOperator
      { activeOperatorKey :: PubKeyHash
      , hubOracleRefInputIndex :: Integer
      , activeOperatorAnchorElementInputOutRef :: TxOutRef
      , activeOperatorAnchorElementOutputIndex :: Integer
      , retiredOperatorsRedeemerIndex :: Integer
      , penalizeForInactivity :: Bool
      , operatorRemovalSchedulerSync :: OperatorRemovalSchedulerSync
      }
  | SlashOperator
      { slashingArguments :: SlashingArguments
      , operatorRemovalSchedulerSync :: OperatorRemovalSchedulerSync
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     , ('ActivateOperator, 2)
     , ('RetireOperator, 3)
     , ('SlashOperator, 4)
     ]
 )

instance PlyArg MintRedeemer
