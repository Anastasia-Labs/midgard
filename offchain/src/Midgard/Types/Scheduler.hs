{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.Scheduler (
  AdvancingApproach (..),
  Datum (..),
  MintRedeemer (..),
  NeglectedUserEvent (..),
  OperatorRemovalReason (..),
  SpendRedeemer (..),
  assetName,
) where

import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

import Ply (PlyArg)

assetName :: C.AssetName
assetName = C.UnsafeAssetName $ BS8.pack "MIDGARD_SCHEDULER"

data Datum
  = NoActiveOperators
  | ActiveOperator
      { operator :: PubKeyHash
      , startTime :: POSIXTime
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''Datum
     [ ('NoActiveOperators, 0)
     , ('ActiveOperator, 1)
     ]
 )

data MintRedeemer
  = Init
  | Deinit
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     ]
 )

instance PlyArg MintRedeemer

data OperatorRemovalReason
  = OperatorRetirement
  | OperatorSlashing
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''OperatorRemovalReason
     [ ('OperatorRetirement, 0)
     , ('OperatorSlashing, 1)
     ]
 )

data NeglectedUserEvent
  = NoNeglectedUserEvent
  | NeglectedDeposit
      { depositRefInputIndex :: Integer
      }
  | NeglectedWithdrawal
      { withdrawalRefInputIndex :: Integer
      }
  | NeglectedTxOrder
      { txOrderRefInputIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''NeglectedUserEvent
     [ ('NoNeglectedUserEvent, 0)
     , ('NeglectedDeposit, 1)
     , ('NeglectedWithdrawal, 2)
     , ('NeglectedTxOrder, 3)
     ]
 )

data AdvancingApproach
  = GoToNextDueToEndOfShift
      { newShiftsOperatorNodeRefInputIndex :: Integer
      }
  | RewindDueToEndOfShift
      { activeOperatorsRootRefInputIndex :: Integer
      , activeOperatorsLastNodeRefInputIndex :: Integer
      , registeredElementRefInputIndex :: Integer
      }
  | GoToNextDueToSkippedOperator
      { newShiftsOperatorNodeRefInputIndex :: Integer
      , skippedOperatorNodeInputIndex :: Integer
      , activeOperatorsSpendRedeemerIndex :: Integer
      , stateQueueRefInputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , neglectedUserEvent :: NeglectedUserEvent
      }
  | RewindDueToSkippedOperator
      { activeOperatorsRootRefInputIndex :: Integer
      , skippedOperatorNodeInputIndex :: Integer
      , activeOperatorsSpendRedeemerIndex :: Integer
      , stateQueueRefInputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , mActiveOperatorsLastNodeRefInputIndex :: Maybe Integer
      , registeredElementRefInputIndex :: Integer
      , neglectedUserEvent :: NeglectedUserEvent
      }
  | GoToNextDueToOperatorRemoval
      { activeOperatorsMintRedeemerIndex :: Integer
      , removalReason :: OperatorRemovalReason
      }
  | RewindDueToOperatorRemoval
      { activeOperatorsMintRedeemerIndex :: Integer
      , mActiveOperatorsLastNodeRefInputIndex :: Maybe Integer
      , removalReason :: OperatorRemovalReason
      , registeredElementRefInputIndex :: Integer
      }
  | AppointFirstOperator
      { newShiftsOperatorNodeRefInputIndex :: Integer
      , registeredElementRefInputIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''AdvancingApproach
     [ ('GoToNextDueToEndOfShift, 0)
     , ('RewindDueToEndOfShift, 1)
     , ('GoToNextDueToSkippedOperator, 2)
     , ('RewindDueToSkippedOperator, 3)
     , ('GoToNextDueToOperatorRemoval, 4)
     , ('RewindDueToOperatorRemoval, 5)
     , ('AppointFirstOperator, 6)
     ]
 )

data SpendRedeemer = SpendRedeemer
  { schedulerInputIndex :: Integer
  , schedulerOutputIndex :: Integer
  , advancingApproach :: AdvancingApproach
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SpendRedeemer
     [ ('SpendRedeemer, 0)
     ]
 )

instance PlyArg SpendRedeemer
