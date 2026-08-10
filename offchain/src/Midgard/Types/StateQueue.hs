{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.StateQueue (
  StateQueueNode (..),
  SlashingApproach (..),
  BlockRemovalApproach (..),
  SpendRedeemer (..),
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
import PlutusLedgerApi.V3 (BuiltinByteString, PubKeyHash, TxOutRef)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

import Ply (PlyArg)

import Midgard.Types.LedgerState (
  ConfirmedState,
  DepositsRoot,
  EventToStepRoot,
  ForcedTransactionsRoot,
  Header,
  HeaderHash,
  MidgardTxsRoot,
  TransitionTraceRoot,
  WithdrawalsRoot,
 )
import Midgard.Types.LinkedList qualified as LinkedList

confirmedStateAssetName :: C.AssetName
confirmedStateAssetName = C.UnsafeAssetName $ BS8.pack "MIDGARD_CONFIRMED_STATE"

blockAssetNamePrefix :: ByteString
blockAssetNamePrefix = BS8.pack "MBLC"

blockAssetNamePrefixLen :: Int
blockAssetNamePrefixLen = BS8.length blockAssetNamePrefix

data StateQueueNode = StateQueueNode
  { header :: Header
  , daAttestation :: BuiltinByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''StateQueueNode
     [ ('StateQueueNode, 0)
     ]
 )

type Datum = LinkedList.Element ConfirmedState StateQueueNode

data SpendRedeemer
  = LinkedListMutation
  | AttachDaAttestation
      { stateQueueInputIndex :: Integer
      , daAttestationMintRedeemerIndex :: Integer
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''SpendRedeemer
     [ ('LinkedListMutation, 0)
     , ('AttachDaAttestation, 1)
     ]
 )

instance PlyArg SpendRedeemer

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
      { anchorElementInputOutRef :: TxOutRef
      , anchorElementOutputIndex :: Integer
      }
  | RemoveFraudulentBlocksLink
      { fraudulentNodeInputOutRef :: TxOutRef
      , fraudulentNodeOutputIndex :: Integer
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
  | Deinit
  | CommitBlockHeader
      { newBlockOutputIndex :: Integer
      , continuedLatestBlockOutputIndex :: Integer
      , operator :: PubKeyHash
      , schedulerRefInputIndex :: Integer
      , activeOperatorsInputIndex :: Integer
      , activeOperatorsRedeemerIndex :: Integer
      }
  | MergeToConfirmedState
      { headerNodeKey :: BuiltinByteString
      , confirmedStateInputOutref :: TxOutRef
      , confirmedStateOutputIndex :: Integer
      , mSettlementRedeemerIndex :: Maybe Integer
      , mergedBlockWithdrawalsRoot :: WithdrawalsRoot
      , mergedBlockForcedTransactionsRoot :: ForcedTransactionsRoot
      , mergedBlockTransactionsRoot :: MidgardTxsRoot
      , mergedBlockDepositsRoot :: DepositsRoot
      , mergedBlockTransitionTraceRoot :: TransitionTraceRoot
      , mergedBlockEventToStepRoot :: EventToStepRoot
      , mergedBlockWithdrawalCount :: Integer
      , mergedBlockForcedTransactionCount :: Integer
      , mergedBlockL2TransactionCount :: Integer
      , mergedBlockDepositCount :: Integer
      , mergedBlockTotalEventCount :: Integer
      , mergedBlockTransitionStepCount :: Integer
      }
  | RemoveFraudulentBlockHeader
      { fraudulentOperator :: PubKeyHash
      , fraudulentBlocksHeaderHash :: HeaderHash
      , slashingApproach :: SlashingApproach
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
