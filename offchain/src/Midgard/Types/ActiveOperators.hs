{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.ActiveOperators () where

import GHC.Generics (Generic)

import PlutusLedgerApi.V3 (BuiltinByteString, POSIXTime)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

import Ply (PlyArg)

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

data SpendRedeemer
  = ListStateTransition
  | UpdateBondHoldNewState
      { activeNodeOutputIndex :: Integer
      , hubOracleRefInputIndex :: Integer
      , stateQueueRedeemerIndex :: Integer
      }
  | UpdateBondHoldNewSettlement
      { activeNodeOutputIndex :: Integer
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
  = Init
  | Deinit
  | ActivateOperator
      { newActiveOperatorKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , activeOperatorAppendedNodeOutputIndex :: Integer
      , activeOperatorAnchorNodeOutputIndex :: Integer
      , registeredOperatorsRedeemerIndex :: Integer
      }
  | RemoveOperatorBadState
      { slashedActiveOperatorKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , activeOperatorSlashedNodeInputIndex :: Integer
      , activeOperatorAnchorNodeInputIndex :: Integer
      , stateQueueRedeemerIndex :: Integer
      }
  | RemoveOperatorBadSettlement
      { slashedActiveOperatorKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , activeOperatorSlashedNodeInputIndex :: Integer
      , activeOperatorAnchorNodeInputIndex :: Integer
      , settlementInputIndex :: Integer
      , settlementRedeemerIndex :: Integer
      }
  | RetireOperator
      { activeOperatorKey :: BuiltinByteString
      , hubOracleRefInputIndex :: Integer
      , activeOperatorRemovedNodeInputIndex :: Integer
      , activeOperatorAnchorNodeInputIndex :: Integer
      , retiredOperatorInsertedNodeOutputIndex :: Integer
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
