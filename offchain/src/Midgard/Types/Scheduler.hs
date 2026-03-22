{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.Scheduler (
  Datum (..),
  MintRedeemer (..),
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

data Datum = Datum
  { operator :: PubKeyHash
  , startTime :: POSIXTime
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     ]
 )

instance PlyArg MintRedeemer

data SpendRedeemer
  = Advance
      { schedulerInputIndex :: Integer
      , schedulerOutputIndex :: Integer
      , activeNodeRefInputIndex :: Integer
      }
  | Rewind
      { schedulerInputIndex :: Integer
      , schedulerOutputIndex :: Integer
      , activeNodeRefInputIndex :: Integer
      , activeRootRefInputIndex :: Integer
      , registeredElementRefInputIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SpendRedeemer
     [ ('Advance, 0)
     , ('Rewind, 1)
     ]
 )

instance PlyArg SpendRedeemer
