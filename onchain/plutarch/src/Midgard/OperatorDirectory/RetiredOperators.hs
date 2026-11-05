{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.OperatorDirectory.RetiredOperators
Description : Plutarch port of @lib/midgard/operator-directory/retired-operators.ak@.

The retired set is the third and last operator list. An operator lands here from
the active set, waits out its bond unlock time, and then recovers its bond.
-}
module Midgard.OperatorDirectory.RetiredOperators (
  prootAssetName,
  pnodeAssetNamePrefix,
  pnodeAssetNamePrefixLength,
  PNodeData (..),
  PMintRedeemer (..),
  pfinalizeLinkedList,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PPubKeyHash, PTokenName (..), PTxOutRef)
import Plutarch.Prelude

import LinkedList.Types (PRootKey)
import Midgard.Common.Types (PPosixTime)
import Midgard.OperatorDirectory (PSlashingArguments)

-- | Aiken @retired_operators.root_asset_name@.
prootAssetName :: forall (s :: S). Term s (PAsData PRootKey)
prootAssetName = pdata (pcon (PTokenName (pconstant "MIDGARD_RETIRED_OPERATORS")))

{- | Aiken @retired_operators.node_asset_name_prefix@ — "Midgard Retired".

Must stay at four bytes: a node's asset name is this prefix followed by its
operator's 28-byte key hash, and Cardano caps asset names at 32.
-}
pnodeAssetNamePrefix :: forall (s :: S). Term s PByteString
pnodeAssetNamePrefix = pconstant "MRET"

-- | Aiken @retired_operators.node_asset_name_prefix_length@.
pnodeAssetNamePrefixLength :: forall (s :: S). Term s PInteger
pnodeAssetNamePrefixLength = 4

{- | Aiken @retired_operators.NodeData@.

'PDNothing' means the bond is unlocked immediately — a voluntary retirement with
no pending obligations.
-}
newtype PNodeData (s :: S) = PNodeData
  {pretiredNode'bondUnlockTime :: Term s (PMaybeData PPosixTime)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNodeData)

{- | Aiken @retired_operators.MintRedeemer@.

Tags: @Init@ 0, @Deinit@ 1, @RetireOperator@ 2, @RecoverOperatorBond@ 3,
@SlashOperator@ 4.
-}
data PMintRedeemer (s :: S)
  = PInit {pretiredInit'outputIndex :: Term s (PAsData PInteger)}
  | PDeinit
  | PRetireOperator
      { pretire'newRetiredOperatorKey :: Term s (PAsData PPubKeyHash)
      , pretire'bondUnlockTime :: Term s (PMaybeData PPosixTime)
      , pretire'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pretire'anchorElementOutputIndex :: Term s (PAsData PInteger)
      , pretire'insertedNodeOutputIndex :: Term s (PAsData PInteger)
      , pretire'activeOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PRecoverOperatorBond
      { precover'retiredOperatorKey :: Term s (PAsData PPubKeyHash)
      , precover'anchorElementInputOutref :: Term s (PAsData PTxOutRef)
      , precover'anchorElementOutputIndex :: Term s (PAsData PInteger)
      }
  | PSlashOperator {pretiredSlash'slashingArguments :: Term s (PAsData PSlashingArguments)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @retired_operators.finalize_linked_list@.

Supplies this set's namespace constants to a linked-list operation. Every mint
branch ends by piping through here, which is what binds the generic list logic
to the retired set's own root key and node prefix.
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
