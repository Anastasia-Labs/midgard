{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.RetiredOperators () where

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
