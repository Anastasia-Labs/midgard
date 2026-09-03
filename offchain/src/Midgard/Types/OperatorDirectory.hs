{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.OperatorDirectory (
  SlashingReason (..),
  SlashingArguments (..),
) where

import GHC.Generics (Generic)

import PlutusLedgerApi.V3 (PubKeyHash, TxOutRef)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

data SlashingReason
  = SlashOperatorForBadState
      { stateQueueRedeemerIndex :: Integer
      }
  | SlashOperatorForBadSettlement
      { settlementInputIndex :: Integer
      , settlementRedeemerIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SlashingReason
     [ ('SlashOperatorForBadState, 0)
     , ('SlashOperatorForBadSettlement, 1)
     ]
 )

data SlashingArguments = SlashingArguments
  { slashedOperator :: PubKeyHash
  , hubOracleRefInputIndex :: Integer
  , slashedOperatorAnchorElementInputOutRef :: TxOutRef
  , slashedOperatorAnchorElementOutputIndex :: Integer
  , slashingReason :: SlashingReason
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SlashingArguments
     [ ('SlashingArguments, 0)
     ]
 )
