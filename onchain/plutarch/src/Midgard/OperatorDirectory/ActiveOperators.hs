{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.OperatorDirectory.ActiveOperators
Description : Partial Plutarch port of
              @lib/midgard/operator-directory/active-operators.ak@.

The active set is the middle of the three: an operator activates into it from
the registered set and retires out of it into the retired set. It is the only
one whose nodes carry mutable state — a bond unlock time and an inactivity
strike count, both updated in place by the spend path.
-}
module Midgard.OperatorDirectory.ActiveOperators (
  prootAssetName,
  pnodeAssetNamePrefix,
  pnodeAssetNamePrefixLength,
  PNodeData (..),
  POperatorRemovalSchedulerSync (..),
  PSpendRedeemer (..),
  PMintRedeemer (..),
  pfinalizeLinkedList,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PPubKeyHash, PTokenName (..), PTxOutRef)
import Plutarch.Prelude

import LinkedList.Types (PLink, PNodeKey, PRootKey)
import Midgard.Common.Types (PPosixTime)
import Midgard.OperatorDirectory (PSlashingArguments)

-- | Aiken @active_operators.root_asset_name@.
prootAssetName :: forall (s :: S). Term s (PAsData PRootKey)
prootAssetName = pdata (pcon (PTokenName (pconstant "MIDGARD_ACTIVE_OPERATORS")))

{- | Aiken @active_operators.node_asset_name_prefix@ — "Midgard Active".

Four bytes, followed by the operator's 28-byte key hash, which exactly fills
Cardano's 32-byte asset-name limit.
-}
pnodeAssetNamePrefix :: forall (s :: S). Term s PByteString
pnodeAssetNamePrefix = pconstant "MACT"

-- | Aiken @active_operators.node_asset_name_prefix_length@.
pnodeAssetNamePrefixLength :: forall (s :: S). Term s PInteger
pnodeAssetNamePrefixLength = 4

{- | Aiken @active_operators.NodeData@.

The node's key is the operator, so the data is free to carry state. Both fields
are mutable: @bond_unlock_time@ moves forward monotonically as the operator
commits blocks or attaches resolution claims, and @inactivity_strikes@ counts up
when the scheduler skips the operator.
-}
data PNodeData (s :: S) = PNodeData
  { pactiveNode'bondUnlockTime :: Term s (PMaybeData PPosixTime)
  , pactiveNode'inactivityStrikes :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNodeData)

{- | Aiken @active_operators.OperatorRemovalSchedulerSync@.

Removing an operator from the active set has to account for the scheduler, which
may currently point at it. Two ways to discharge that: show the scheduler is not
pointing at this operator (@ShowOperatorIsInactive@), or show the scheduler is
being advanced past it in the same transaction (@ShowSchedulerIsAdvancing@).

Tags: @ShowOperatorIsInactive@ 0, @ShowSchedulerIsAdvancing@ 1.
-}
data POperatorRemovalSchedulerSync (s :: S)
  = PShowOperatorIsInactive
      {pshowInactive'schedulerRefInputIndex :: Term s (PAsData PInteger)}
  | PShowSchedulerIsAdvancing
      { pshowAdvancing'schedulerInputIndex :: Term s (PAsData PInteger)
      , pshowAdvancing'schedulerRedeemerIndex :: Term s (PAsData PInteger)
      , pshowAdvancing'removingOperatorsAnchorElementKey :: Term s (PMaybeData PNodeKey)
      , pshowAdvancing'removingOperatorIsTheLastMember :: Term s (PAsData PBool)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POperatorRemovalSchedulerSync)

{- | Aiken @active_operators.SpendRedeemer@.

Tags: @ListStateTransition@ 0, @UpdateBondHoldNewState@ 1,
@UpdateBondHoldNewSettlement@ 2, @StrikeForInactivity@ 3.

@ListStateTransition@ is the plain gate deferring to the minting policy; the
other three spend and reproduce a single node in place, which the linked list
permits without any mint.
-}
data PSpendRedeemer (s :: S)
  = PListStateTransition
  | PUpdateBondHoldNewState
      { pupdateState'activeOperator :: Term s (PAsData PPubKeyHash)
      , pupdateState'activeNodeInputIndex :: Term s (PAsData PInteger)
      , pupdateState'activeNodeOutputIndex :: Term s (PAsData PInteger)
      , pupdateState'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pupdateState'stateQueueRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PUpdateBondHoldNewSettlement
      { pupdateSettlement'activeOperator :: Term s (PAsData PPubKeyHash)
      , pupdateSettlement'activeNodeInputIndex :: Term s (PAsData PInteger)
      , pupdateSettlement'activeNodeOutputIndex :: Term s (PAsData PInteger)
      , pupdateSettlement'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pupdateSettlement'settlementInputIndex :: Term s (PAsData PInteger)
      , pupdateSettlement'settlementRedeemerIndex :: Term s (PAsData PInteger)
      , pupdateSettlement'resolutionTime :: Term s (PAsData PPosixTime)
      }
  | PStrikeForInactivity
      { pstrike'activeNodeInputIndex :: Term s (PAsData PInteger)
      , pstrike'activeNodeOutputIndex :: Term s (PAsData PInteger)
      , pstrike'operator :: Term s (PAsData PPubKeyHash)
      , pstrike'activeNodeLink :: Term s PLink
      , pstrike'schedulerInputIndex :: Term s (PAsData PInteger)
      , pstrike'schedulerRedeemerIndex :: Term s (PAsData PInteger)
      , pstrike'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @active_operators.MintRedeemer@.

Tags: @Init@ 0, @Deinit@ 1, @ActivateOperator@ 2, @RetireOperator@ 3,
@SlashOperator@ 4. The registered set matches on tag 2, the retired set on tag 3.

Every field now carries its real type — this set's own validator reads all of
them, so the placeholder 'PData's that stood in while only other sets consumed
this redeemer are gone.
-}
data PMintRedeemer (s :: S)
  = PInit {pactiveInit'outputIndex :: Term s (PAsData PInteger)}
  | PDeinit
  | PActivateOperator
      { pactivate'newActiveOperatorKey :: Term s (PAsData PPubKeyHash)
      , pactivate'anchorElementOutputIndex :: Term s (PAsData PInteger)
      , pactivate'insertedNodeOutputIndex :: Term s (PAsData PInteger)
      , pactivate'registeredOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      , pactivate'activeOperatorsSetWasEmpty :: Term s (PAsData PBool)
      }
  | PRetireOperator
      { pactiveRetire'activeOperatorKey :: Term s (PAsData PPubKeyHash)
      , pactiveRetire'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pactiveRetire'anchorElementInputOutref :: Term s (PAsData PTxOutRef)
      , pactiveRetire'anchorElementOutputIndex :: Term s (PAsData PInteger)
      , pactiveRetire'retiredOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      , pactiveRetire'penalizeForInactivity :: Term s (PAsData PBool)
      , pactiveRetire'operatorRemovalSchedulerSync ::
          Term s (PAsData POperatorRemovalSchedulerSync)
      }
  | PSlashOperator
      { pactiveSlash'slashingArguments :: Term s (PAsData PSlashingArguments)
      , pactiveSlash'operatorRemovalSchedulerSync ::
          Term s (PAsData POperatorRemovalSchedulerSync)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @active_operators.finalize_linked_list@.

Supplies the active set's namespace to a linked-list operation. Callers outside
the active set use this to authenticate an element they only /reference/ — a
non-membership proof, or a duplicate-operator witness.
-}
pfinalizeLinkedList ::
  forall (s :: S) (a :: S -> Type).
  ( Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PRootKey) ->
    Term s PByteString ->
    Term s PInteger ->
    Term s a
  ) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s a
pfinalizeLinkedList eval nftPolicyId =
  eval nftPolicyId prootAssetName pnodeAssetNamePrefix pnodeAssetNamePrefixLength
