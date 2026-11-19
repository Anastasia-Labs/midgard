{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.OperatorDirectory.RegisteredOperators
Description : Plutarch port of
              @lib/midgard/operator-directory/registered-operators.ak@.

The registered set is where an operator first appears. It is the one operator
list keyed by /time/ rather than by operator: a node's key is the big-endian
encoding of the operator's activation time, and the list is sorted descending,
so the earliest-activating operator sits at the tail.
-}
module Midgard.OperatorDirectory.RegisteredOperators (
  prootAssetName,
  pnodeAssetNamePrefix,
  pnodeAssetNamePrefixLength,
  PNodeData (..),
  PDuplicateOperatorStatus (..),
  PMintRedeemer (..),
  pactivationTimeToNodeKey,
  pnodeKeyToActivationTime,
  pfinalizeLinkedList,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (
  pbyteStringToInteger,
  pintegerToByteString,
  pmostSignificantFirst,
 )
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PPubKeyHash, PTokenName (..), PTxOutRef)
import Plutarch.Prelude

import LinkedList.Types (PRootKey)
import Midgard.Common.Types (PPosixTime)

-- | Aiken @registered_operators.root_asset_name@.
prootAssetName :: forall (s :: S). Term s (PAsData PRootKey)
prootAssetName = pdata (pcon (PTokenName (pconstant "MIDGARD_REGISTERED_OPERATORS")))

{- | Aiken @registered_operators.node_asset_name_prefix@ — "Midgard Registered".

Four bytes, followed by the node's activation-time key.
-}
pnodeAssetNamePrefix :: forall (s :: S). Term s PByteString
pnodeAssetNamePrefix = pconstant "MREG"

-- | Aiken @registered_operators.node_asset_name_prefix_length@.
pnodeAssetNamePrefixLength :: forall (s :: S). Term s PInteger
pnodeAssetNamePrefixLength = 4

{- | Aiken @registered_operators.NodeData@.

The operator itself lives in the /data/ here, not in the key — the key is the
activation time. That inversion is why this set needs a node-data check where
the other two sets can just compare keys.
-}
newtype PNodeData (s :: S) = PNodeData
  {pregisteredNode'operator :: Term s (PAsData PPubKeyHash)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNodeData)

{- | Aiken @registered_operators.DuplicateOperatorStatus@.

Which set the pre-existing copy of a duplicate operator sits in. Tags:
@DuplicateIsRegistered@ 0, @DuplicateIsActive@ 1, @DuplicateIsRetired@ 2.

Only the active variant carries a field: the active operators policy id is not a
parameter of this validator, so it has to be read from the hub oracle.
-}
data PDuplicateOperatorStatus (s :: S)
  = PDuplicateIsRegistered
  | PDuplicateIsActive
      {pduplicateActive'hubOracleRefInputIndex :: Term s (PAsData PInteger)}
  | PDuplicateIsRetired
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDuplicateOperatorStatus)

{- | Aiken @registered_operators.MintRedeemer@.

Tags: @Init@ 0, @Deinit@ 1, @RegisterOperator@ 2, @ActivateOperator@ 3,
@DeregisterOperator@ 4, @SlashDuplicateOperator@ 5.
-}
data PMintRedeemer (s :: S)
  = PInit {pregisteredInit'outputIndex :: Term s (PAsData PInteger)}
  | PDeinit
  | PRegisterOperator
      { pregister'registeringOperator :: Term s (PAsData PPubKeyHash)
      , pregister'rootOutputIndex :: Term s (PAsData PInteger)
      , pregister'registeredNodeOutputIndex :: Term s (PAsData PInteger)
      , pregister'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pregister'activeOperatorsElementRefInputIndex :: Term s (PAsData PInteger)
      , pregister'retiredOperatorsElementRefInputIndex :: Term s (PAsData PInteger)
      }
  | PActivateOperator
      { pactivate'activatingOperator :: Term s (PAsData PPubKeyHash)
      , pactivate'anchorElementInputOutref :: Term s (PAsData PTxOutRef)
      , pactivate'anchorElementOutputIndex :: Term s (PAsData PInteger)
      , pactivate'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pactivate'retiredOperatorsElementRefInputIndex :: Term s (PAsData PInteger)
      , pactivate'activeOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PDeregisterOperator
      { pderegister'deregisteringOperator :: Term s (PAsData PPubKeyHash)
      , pderegister'anchorElementInputOutref :: Term s (PAsData PTxOutRef)
      , pderegister'anchorElementOutputIndex :: Term s (PAsData PInteger)
      }
  | PSlashDuplicateOperator
      { pslashDuplicate'duplicateOperator :: Term s (PAsData PPubKeyHash)
      , pslashDuplicate'anchorElementInputOutref :: Term s (PAsData PTxOutRef)
      , pslashDuplicate'anchorElementOutputIndex :: Term s (PAsData PInteger)
      , pslashDuplicate'duplicateNodeRefInputIndex :: Term s (PAsData PInteger)
      , pslashDuplicate'duplicateOperatorStatus :: Term s (PAsData PDuplicateOperatorStatus)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @registered_operators.activation_time_to_node_key@.

@builtin.integer_to_bytearray(True, 0, posix_time)@ — big-endian, width zero
meaning "as few bytes as possible".

The minimal width is what makes the list's descending order agree with numeric
order on the encoded keys, since the list compares keys as bytestrings. Note
this is a total function only for non-negative times; a negative activation time
fails in the builtin, which is the Aiken behaviour too.
-}
pactivationTimeToNodeKey :: forall (s :: S). Term s (PPosixTime :--> PByteString)
pactivationTimeToNodeKey = phoistAcyclic $
  plam $ \posixTime -> pintegerToByteString # pmostSignificantFirst # 0 # posixTime

{- | Aiken @registered_operators.node_key_to_activation_time@.

@builtin.bytearray_to_integer(True, node_key)@ — the inverse of
'pactivationTimeToNodeKey'.
-}
pnodeKeyToActivationTime :: forall (s :: S). Term s (PByteString :--> PPosixTime)
pnodeKeyToActivationTime = phoistAcyclic $
  plam $ \nodeKey -> pbyteStringToInteger # pmostSignificantFirst # nodeKey

{- | Aiken @registered_operators.finalize_linked_list@.

Supplies this set's namespace to a linked-list operation.
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
