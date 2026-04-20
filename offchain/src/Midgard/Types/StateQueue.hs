{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.StateQueue (
  SlashingApproach (..),
  BlockRemovalApproach (..),
  MintRedeemer (..),
  Datum,
  confirmedStateAssetName,
  blockAssetNamePrefix,
  blockAssetNamePrefixLen,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (BuiltinByteString, PubKeyHash)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

import Ply (PlyArg)

import Midgard.Types.LedgerState qualified as LedgerState
import Midgard.Types.LinkedList qualified as LinkedList

confirmedStateAssetName :: C.AssetName
confirmedStateAssetName = C.UnsafeAssetName $ BS8.pack "MIDGARD_CONFIRMED_STATE"

blockAssetNamePrefix :: ByteString
blockAssetNamePrefix = BS8.pack "MBLC"

blockAssetNamePrefixLen :: Int
blockAssetNamePrefixLen = BS8.length blockAssetNamePrefix

type Datum = LinkedList.Element LedgerState.ConfirmedState LedgerState.Header

data SlashingApproach
  = SlashActiveOperator {activeOperatorsRedeemerIndex :: Integer}
  | SlashRetiredOperator {retiredOperatorsRedeemerIndex :: Integer}
  | OperatorAlreadySlashed
      { activeOperatorsElementRefInputIndex :: Integer
      , retiredOperatorsElementRefInputIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SlashingApproach
     [ ('SlashActiveOperator, 0)
     , ('SlashRetiredOperator, 1)
     , ('OperatorAlreadySlashed, 2)
     ]
 )

data BlockRemovalApproach
  = RemoveLastFraudulentBlock
      { anchorElementInputIndex :: Integer
      , anchorElementOutputIndex :: Integer
      }
  | RemoveFraudulentBlocksLink
      { fraudulentNodeOutputIndex :: Integer
      , removedBlockInputIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''BlockRemovalApproach
     [ ('RemoveLastFraudulentBlock, 0)
     , ('RemoveFraudulentBlocksLink, 1)
     ]
 )

data MintRedeemer
  = Init {outputIndex :: Integer}
  | Deinit {inputIndex :: Integer}
  | CommitBlockHeader
      { latestBlockInputIndex :: Integer
      , newBlockOutputIndex :: Integer
      , continuedLatestBlockOutputIndex :: Integer
      , operator :: PubKeyHash
      , schedulerRefInputIndex :: Integer
      , activeOperatorsInputIndex :: Integer
      , activeOperatorsRedeemerIndex :: Integer
      }
  | MergeToConfirmedState
      { headerNodeKey :: BuiltinByteString
      , headerNodeInputIndex :: Integer
      , confirmedStateInputIndex :: Integer
      , confirmedStateOutputIndex :: Integer
      , mSettlementRedeemerIndex :: Maybe Integer
      }
  | RemoveFraudulentBlockHeader
      { fraudulentOperator :: PubKeyHash
      , fraudulentBlocksHeaderHash :: BuiltinByteString
      , slashingApproach :: SlashingApproach
      , fraudulentNodeInputIndex :: Integer
      , fraudProofRefInputIndex :: Integer
      , blockRemovalApproach :: BlockRemovalApproach
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''MintRedeemer
     [ ('Init, 0)
     , ('Deinit, 1)
     , ('CommitBlockHeader, 2)
     , ('MergeToConfirmedState, 3)
     , ('RemoveFraudulentBlockHeader, 4)
     ]
 )

instance PlyArg MintRedeemer
