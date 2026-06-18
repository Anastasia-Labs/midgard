{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.OperatorDirectory (
  SlashingReason (..),
  SlashingArguments (..),
) where

import GHC.Generics (Generic)

import PlutusLedgerApi.V3 (PubKeyHash)
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
  , slashedOperatorAnchorElementInputIndex :: Integer
  , slashedOperatorNodeInputIndex :: Integer
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
