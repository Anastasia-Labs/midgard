{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.HubOracle (Datum (..)) where

import GHC.Generics (Generic)

import PlutusLedgerApi.V3 (CurrencySymbol)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

data Datum = Datum
  { registeredOperators :: CurrencySymbol
  , activeOperators :: CurrencySymbol
  , retiredOperators :: CurrencySymbol
  --   , scheduler                  :: CurrencySymbol
  --   , stateQueue                 :: CurrencySymbol
  --   , fraudProofCatalogue        :: CurrencySymbol
  --   , fraudProof                 :: CurrencySymbol
  --   , deposit                    :: CurrencySymbol
  --   , withdrawal                 :: CurrencySymbol
  --   , txOrder                    :: CurrencySymbol
  --   , settlement                 :: CurrencySymbol
  --   , payout                     :: CurrencySymbol
  --   , registeredOperatorsAddr    :: Address
  --   , activeOperatorsAddr        :: Address
  --   , retiredOperatorsAddr       :: Address
  --   , schedulerAddr              :: Address
  --   , stateQueueAddr             :: Address
  --   , fraudProofCatalogueAddr    :: Address
  --   , fraudProofAddr             :: Address
  --   , depositAddr                :: Address
  --   , withdrawalAddr             :: Address
  --   , txOrderAddr                :: Address
  --   , settlementAddr             :: Address
  --   , reserveAddr                :: Address
  --   , payoutAddr                 :: Address
  --   , reserveObserver            :: ScriptHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''Datum
     [ ('Datum, 0)
     ]
 )
