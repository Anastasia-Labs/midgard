{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.StateQueue
Description : Partial Plutarch port of @lib/midgard/state-queue.ak@.

The whole library module is ported: the namespace constants, the node and datum
types, both redeemers, the protocol-version gate, the commit-time binding, and
the four readers the rest of the protocol uses to look a block or the confirmed
state up out of a reference input. The state queue's own validator (1,070 lines)
is a separate slice.
-}
module Midgard.StateQueue (
  PMintRedeemer (..),
  PYieldRedeemer (..),
  PSpendRedeemer (..),
  PSlashingApproach (..),
  PBlockRemovalApproach (..),
  PAttestationTimeoutRemovalApproach (..),
  PDatum,
  pconfirmedStateAssetName,
  pblockAssetNamePrefix,
  pblockAssetNamePrefixLength,
  pnoDaAttestation,
  pdaAttestationTimeoutV1,
  PStateQueueNode (..),
  pdecodeHeaderView,
  pcommitBoundHeaderTimeIsValid,
  pgetConfirmedState,
  pgetConfirmedStateRoot,
  pgetStateQueueNode,
  pgetBlockDatumV1,
  pgetPrevHeaderHashOfNodeV1,
  pvalidateDaAttestationAttachment,
  pvalidateDaAvailabilityStatusTransition,
  pfinalizeLinkedList,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PPosixTime,
  PPubKeyHash,
  PTokenName (..),
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList (pgetElementInfo)
import LinkedList.Types (PElement, PLink, PRootKey)
import Midgard.Common.Utils (pgetInclusiveBoundsOfAShortValidityRange)
import Midgard.AvailabilityChallenge (PStateQueueStatusV1 (..))
import Midgard.LedgerState (
  PConfirmedState,
  PHeaderHash,
  PHeaderV1 (..),
  pprotocolVersionV1,
 )

{- | Aiken @state_queue.Datum = linked_list.Element<ConfirmedState, StateQueueNode>@.

The queue is a linked list whose root payload is the confirmed state and whose
node payloads are blocks. 'PElement' keeps both payloads as raw 'PData' — the
readers below are what give them their types, and which type you get depends on
whether the element authenticated as the root or as a node.
-}
type PDatum = PElement

{- | Aiken @state_queue.SpendRedeemer@.

@LinkedListMutation@ is 0 and @AttachDaAttestation@ is 1. Structural changes to
the queue go through the first and are decided by the minting policy; the second
is the only spend that edits a node without minting or burning anything.
-}
data PSpendRedeemer (s :: S)
  = PLinkedListMutation
  | PAttachDaAttestation
      { psqAttach'stateQueueInputIndex :: Term s (PAsData PInteger)
      , psqAttach'daAttestationMintRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PAvailabilityStatusUpdate
      { psqAvailabilityUpdate'stateQueueInputIndex :: Term s (PAsData PInteger)
      , psqAvailabilityUpdate'stateQueueOutputIndex :: Term s (PAsData PInteger)
      , psqAvailabilityUpdate'availabilityMintRedeemerIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @state_queue.MintRedeemer@.

Constructor order fixes the on-chain tag: @InitV1@ 0, @Deinit@ 1,
@CommitBlockHeader@ 2, @RemoveFraudulentBlockHeader@ 3,
@RemoveUnattestedBlockAfterTimeout@ 4,
@RemoveUnavailableBlockAfterTimeout@ 5, and @MergeToConfirmedStateV1@ 6. The
directory's slashing path matches on tag 3, so this ordering is load-bearing.

Every field has its real wire type. This matters to consumers such as the
correction lock: Aiken's typed redeemer decode validates the whole value, even
when a consumer only reads a subset of its fields.
-}
data PSlashingApproach (s :: S)
  = PSlashActiveOperator
      { pslashActive'activeOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      , pslashActive'mFraudProverRewardOutputIndex :: Term s (PAsData (PMaybeData PInteger))
      }
  | PSlashRetiredOperator
      { pslashRetired'retiredOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      , pslashRetired'mFraudProverRewardOutputIndex :: Term s (PAsData (PMaybeData PInteger))
      }
  | POperatorAlreadySlashed
      { palreadySlashed'activeElementRefInputIndex :: Term s (PAsData PInteger)
      , palreadySlashed'retiredElementRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSlashingApproach)

data PBlockRemovalApproach (s :: S)
  = PRemoveLastFraudulentBlock
      { premoveLast'anchorElementInputOutref :: Term s (PAsData PTxOutRef)
      , premoveLast'anchorElementOutputIndex :: Term s (PAsData PInteger)
      }
  | PRemoveFraudulentBlocksLink
      { premoveLink'fraudulentNodeInputOutref :: Term s (PAsData PTxOutRef)
      , premoveLink'fraudulentNodeOutputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBlockRemovalApproach)

data PAttestationTimeoutRemovalApproach (s :: S)
  = PPruneTimedOutBlockDescendant
      { ppruneTimedOut'confirmedStateRefInputIndex :: Term s (PAsData PInteger)
      , ppruneTimedOut'timedOutNodeInputOutref :: Term s (PAsData PTxOutRef)
      , ppruneTimedOut'timedOutNodeOutputIndex :: Term s (PAsData PInteger)
      }
  | PRemoveTimedOutHead
      { premoveTimedOutHead'confirmedStateInputOutref :: Term s (PAsData PTxOutRef)
      , premoveTimedOutHead'confirmedStateOutputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAttestationTimeoutRemovalApproach)

data PMintRedeemer (s :: S)
  = PInitV1 {psqInit'outputIndex :: Term s (PAsData PInteger)}
  | PDeinit
  | PCommitBlockHeader
      { psqCommit'yieldToRefInputIndex :: Term s (PAsData PInteger)
      , psqCommit'newBlockOutputIndex :: Term s (PAsData PInteger)
      , psqCommit'continuedLatestBlockOutputIndex :: Term s (PAsData PInteger)
      , psqCommit'operator :: Term s (PAsData PPubKeyHash)
      , psqCommit'schedulerRefInputIndex :: Term s (PAsData PInteger)
      , psqCommit'activeOperatorsInputIndex :: Term s (PAsData PInteger)
      , psqCommit'activeOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      , psqCommit'mConfirmedStateRefInputIndex :: Term s (PAsData (PMaybeData PInteger))
      , psqCommit'mHeadStateQueueNodeRefInputIndex :: Term s (PAsData (PMaybeData PInteger))
      }
  | PRemoveFraudulentBlockHeader
      { psqRemove'yieldToRefInputIndex :: Term s (PAsData PInteger)
      , psqRemove'fraudulentOperator :: Term s (PAsData PPubKeyHash)
      , psqRemove'fraudulentBlocksHeaderHash :: Term s (PAsData PHeaderHash)
      , psqRemove'slashingApproach :: Term s (PAsData PSlashingApproach)
      , psqRemove'fraudProofRefInputIndex :: Term s (PAsData PInteger)
      , psqRemove'blockRemovalApproach :: Term s (PAsData PBlockRemovalApproach)
      }
  | PRemoveUnattestedBlockAfterTimeout
      { psqRemoveUnattested'yieldToRefInputIndex :: Term s (PAsData PInteger)
      , psqRemoveUnattested'timedOutHeaderHash :: Term s (PAsData PHeaderHash)
      , psqRemoveUnattested'removalApproach :: Term s (PAsData PAttestationTimeoutRemovalApproach)
      }
  | PRemoveUnavailableBlockAfterTimeout
      { psqRemoveUnavailable'yieldToRefInputIndex :: Term s (PAsData PInteger)
      , psqRemoveUnavailable'unavailableHeaderHash :: Term s (PAsData PHeaderHash)
      , psqRemoveUnavailable'challengeAssetName :: Term s (PAsData PTokenName)
      , psqRemoveUnavailable'removalApproach :: Term s (PAsData PAttestationTimeoutRemovalApproach)
      }
  | PMergeToConfirmedStateV1
      { psqMerge'yieldToRefInputIndex :: Term s (PAsData PInteger)
      , psqMerge'headerNodeKey :: Term s (PAsData PByteString)
      , psqMerge'confirmedStateInputOutref :: Term s (PAsData PTxOutRef)
      , psqMerge'confirmedStateOutputIndex :: Term s (PAsData PInteger)
      , psqMerge'mSettlementRedeemerIndex :: Term s (PAsData (PMaybeData PInteger))
      , psqMerge'withdrawalsRoot :: Term s (PAsData PByteString)
      , psqMerge'forcedTransactionsRoot :: Term s (PAsData PByteString)
      , psqMerge'transactionsRoot :: Term s (PAsData PByteString)
      , psqMerge'depositsRoot :: Term s (PAsData PByteString)
      , psqMerge'transitionTraceRoot :: Term s (PAsData PByteString)
      , psqMerge'eventToStepRoot :: Term s (PAsData PByteString)
      , psqMerge'validationTracesRoot :: Term s (PAsData PByteString)
      , psqMerge'withdrawalCount :: Term s (PAsData PInteger)
      , psqMerge'forcedTransactionCount :: Term s (PAsData PInteger)
      , psqMerge'l2TransactionCount :: Term s (PAsData PInteger)
      , psqMerge'depositCount :: Term s (PAsData PInteger)
      , psqMerge'totalEventCount :: Term s (PAsData PInteger)
      , psqMerge'transitionStepCount :: Term s (PAsData PInteger)
      , psqMerge'validationTraceCount :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

-- | Fieldless withdrawal payload; the operation comes from the unique mint redeemer.
data PYieldRedeemer (s :: S) = PYieldStateQueue
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PYieldRedeemer)

{- | Aiken @state_queue.confirmed_state_asset_name@.

The state queue is a linked list whose /root/ is the confirmed state, so this is
the root key rather than an ordinary node name.
-}
pconfirmedStateAssetName :: forall (s :: S). Term s (PAsData PRootKey)
pconfirmedStateAssetName = pdata (pcon (PTokenName (pconstant "MIDGARD_CONFIRMED_STATE")))

{- | Aiken @state_queue.block_asset_name_prefix@ — "Midgard Block".

Four bytes, followed by a 28-byte header hash, which exactly fills Cardano's
32-byte asset-name limit.
-}
pblockAssetNamePrefix :: forall (s :: S). Term s PByteString
pblockAssetNamePrefix = pconstant "MBLC"

-- | Aiken @state_queue.block_asset_name_prefix_length@.
pblockAssetNamePrefixLength :: forall (s :: S). Term s PInteger
pblockAssetNamePrefixLength = 4

{- | Aiken @state_queue.StateQueueNode@.

One committed block: its header, and the data-availability attestation attached
to it later. @da_attestation@ is the empty bytestring until one is attached —
Aiken names that @no_da_attestation@.
-}
data PStateQueueNode (s :: S) = PStateQueueNode
  { pstateQueueNode'header :: Term s (PAsData PHeaderV1)
  , pstateQueueNode'daAttestation :: Term s (PAsData PStateQueueStatusV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStateQueueNode)

{- | Aiken @state_queue.decode_header_view@.

@
expect header.protocol_version == protocol_version_v1
header
@

A one-field gate rather than a conversion: it refuses a header whose protocol
version is not v1, which is what stops a genesis sentinel (version zero) or a
future version being read as a v1 block.
-}
pdecodeHeaderView :: forall (s :: S). Term s (PAsData PHeaderV1 :--> PAsData PHeaderV1)
pdecodeHeaderView = phoistAcyclic $
  plam $ \header ->
    pif
      ( pmatch (pfromData header) $ \h ->
          pfromData (pheader'protocolVersion h) #== pprotocolVersionV1
      )
      header
      perror

{- | Aiken @state_queue.commit_bound_header_time_is_valid@.

Ties a block's event interval to the transaction that commits it: the interval
must be non-empty, and its end must be exactly the commit transaction's
inclusive upper bound.

The start is /not/ checked here — it is the preceding header's end, which the
state queue checks when linking the block in. What this pins is that an operator
cannot claim an interval extending past the transaction it is committing in, and
the short-range cap on the validity interval bounds how much it can claim at
once.
-}
pcommitBoundHeaderTimeIsValid ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s (PInterval PPosixTime) ->
  Term s PBool
pcommitBoundHeaderTimeIsValid headerStartTime headerEndTime commitValidityRange =
  let (_, inclusiveUpperBound) =
        pgetInclusiveBoundsOfAShortValidityRange commitValidityRange
   in pand'List
        [ headerStartTime #< headerEndTime
        , headerEndTime #== inclusiveUpperBound
        ]

{- | Aiken @state_queue.no_da_attestation@ — the empty bytestring.

A node carries this until a data-availability attestation is attached. It is
what @validate_da_attestation_attachment@ requires of the input side, so an
attestation can be attached exactly once.
-}
pnoDaAttestation :: forall (s :: S). Term s (PAsData PStateQueueStatusV1)
pnoDaAttestation = pdata (pcon PUnattested)

-- | Aiken @state_queue.da_attestation_timeout_v1@.
pdaAttestationTimeoutV1 :: forall (s :: S). Term s PInteger
pdaAttestationTimeoutV1 = 3_600_000

{- | Aiken @state_queue.get_confirmed_state@.

The list's /root/ payload: the last block merged into confirmed state.

@expect None = m_state_queue_element_key@ is the load-bearing line — it is what
makes this the root reader rather than a node reader. A node's payload is a
'PStateQueueNode', not a 'PConfirmedState', and the two would decode into one
another's field positions without it.
-}
pgetConfirmedState ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PConfirmedState
pgetConfirmedState referenceInputs stateQueuePolicy refInputIndex =
  pfinalizeLinkedList
    ( pgetElementInfo (presolvedOutputAt referenceInputs refInputIndex) $
        \_address _lovelace mKey elementData _link ->
          pmatch mKey $ \case
            PDNothing -> pfromData (punsafeCoerce @(PAsData PConfirmedState) elementData)
            PDJust _ -> perror
    )
    stateQueuePolicy

{- | Aiken @state_queue.get_confirmed_state_root@.

Authenticate the singleton root and expose both its confirmed-state payload and
current head link. Append and timeout-correction paths use this reader to agree
on one queue head without consuming the root during an ordinary append.
-}
pgetConfirmedStateRoot ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  (Term s PConfirmedState -> Term s PLink -> Term s r) ->
  Term s r
pgetConfirmedStateRoot referenceInputs stateQueuePolicy refInputIndex k =
  pfinalizeLinkedList
    ( pgetElementInfo (presolvedOutputAt referenceInputs refInputIndex) $
        \_address _lovelace mKey elementData headLink ->
          pmatch mKey $ \case
            PDNothing ->
              k
                (pfromData (punsafeCoerce @(PAsData PConfirmedState) elementData))
                headLink
            PDJust _ -> perror
    )
    stateQueuePolicy

{- | Aiken @state_queue.get_state_queue_node@.

The mirror of 'pgetConfirmedState': a node, and the header hash that keys it.

The key is not decoration. It is the node's NFT asset name minus the @MBLC@
prefix, so it is minted rather than stated in the datum, and a caller that
checks it against an expected hash has authenticated /which/ block it read —
which is what 'pgetPrevHeaderHashOfNodeV1' does with it.
-}
pgetStateQueueNode ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  (Term s PStateQueueNode -> Term s PByteString -> Term s r) ->
  Term s r
pgetStateQueueNode referenceInputs stateQueuePolicy refInputIndex k =
  pfinalizeLinkedList
    ( pgetElementInfo (presolvedOutputAt referenceInputs refInputIndex) $
        \_address _lovelace mKey elementData _link ->
          pmatch mKey $ \case
            PDNothing -> perror
            PDJust headerHash ->
              k
                (pfromData (punsafeCoerce @(PAsData PStateQueueNode) elementData))
                (pfromData headerHash)
    )
    stateQueuePolicy

{- | Aiken @state_queue.get_block_datum_v1@.

'pgetStateQueueNode' with the protocol-version gate applied to the header, so a
caller that wants a v1 block cannot accidentally read a node at another version.

The 'plet' is load-bearing. Aiken evaluates
@let header = decode_header_view(...)@ strictly, so the gate runs whether or not
the continuation ever looks at the header; handing the continuation an unforced
term instead would silently let a caller that ignores the header — the
@get_state_queue_node@ callers do exist — accept a block at any version. 'plet'
compiles to a lambda application, and UPLC application is call-by-value.
-}
pgetBlockDatumV1 ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  (Term s (PAsData PHeaderV1) -> Term s PByteString -> Term s r) ->
  Term s r
pgetBlockDatumV1 referenceInputs stateQueuePolicy refInputIndex k =
  pgetStateQueueNode referenceInputs stateQueuePolicy refInputIndex $
    \node headerHash ->
      pmatch node $ \PStateQueueNode {pstateQueueNode'header} ->
        plet (pdecodeHeaderView # pstateQueueNode'header) $ \header ->
          k header headerHash

{- | Aiken @state_queue.get_prev_header_hash_of_node_v1@.

Walks one link back up the chain of blocks. The @expect@ on the retrieved hash
is what makes the answer meaningful: without it a caller would learn the
predecessor of /whichever/ block sat at that reference-input index, not of the
block it meant to ask about.
-}
pgetPrevHeaderHashOfNodeV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PByteString ->
  Term s (PAsData PHeaderHash)
pgetPrevHeaderHashOfNodeV1 referenceInputs stateQueuePolicy refInputIndex expectedHeaderHash =
  pgetBlockDatumV1 referenceInputs stateQueuePolicy refInputIndex $
    \header retrievedHeaderHash ->
      pif
        (retrievedHeaderHash #== expectedHeaderHash)
        (pmatch (pfromData header) $ \h -> pheader'prevHeaderHash h)
        perror

{- | Aiken @state_queue.validate_da_attestation_attachment@.

Attaching a data-availability attestation to an already-committed block. The
block itself must not change: this is the one state-queue spend that mutates a
node in place, so every part of the node other than the attestation field is
pinned — same address, same key on both sides, same link, same header, and
Lovelace that may only grow.

@input_block.da_attestation == no_da_attestation@ is what makes attachment
one-shot: a node that already carries an attestation cannot be re-attached, so
the attestation for a block cannot be swapped after the fact.
-}
pvalidateDaAttestationAttachment ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s (PAsData PTokenName) ->
  Term s (PInterval PPosixTime) ->
  Term s PBool
pvalidateDaAttestationAttachment
  inputs
  outputs
  stateQueuePolicy
  stateQueueInputIndex
  stateQueueOutputIndex
  expectedHeaderHash
  daBondAssetName
  validityRange = P.do
    _ <-
      plet $
        pif
          ( pvalidateDaAvailabilityStatusTransition
              inputs
              outputs
              stateQueuePolicy
              stateQueueInputIndex
              stateQueueOutputIndex
              expectedHeaderHash
              (pcon PUnattested)
              (pcon $ PAttested daBondAssetName)
          )
          (pconstant @PUnit ())
          perror
    inputOutput <- plet $ presolvedOutputAt inputs stateQueueInputIndex
    pgetNodeInfo inputOutput stateQueuePolicy $ \_ _ inputBlock _ ->
      pmatch inputBlock $ \PStateQueueNode {pstateQueueNode'header} ->
        pmatch (pfromData pstateQueueNode'header) $ \PHeaderV1 {pheader'endTime} ->
          let (_, inclusiveUpperBound) = pgetInclusiveBoundsOfAShortValidityRange validityRange
           in inclusiveUpperBound #<= pfromData pheader'endTime + pdaAttestationTimeoutV1

pvalidateDaAvailabilityStatusTransition ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PStateQueueStatusV1 ->
  Term s PStateQueueStatusV1 ->
  Term s PBool
pvalidateDaAvailabilityStatusTransition
  inputs
  outputs
  stateQueuePolicy
  stateQueueInputIndex
  stateQueueOutputIndex
  expectedHeaderHash
  expectedInputStatus
  expectedOutputStatus = P.do
    inputOutput <- plet $ presolvedOutputAt inputs stateQueueInputIndex
    output <- plet $ pfromData (pelemAt # stateQueueOutputIndex # outputs)
    PTxOut {ptxOut'address = inputAddress} <- pmatch inputOutput
    PTxOut {ptxOut'address = outputAddress} <- pmatch output
    pif
      (pnot #$ inputAddress #== outputAddress)
      perror
      $ pgetNodeInfo inputOutput stateQueuePolicy
      $ \inputLovelace inputHeaderHash inputBlock inputLink ->
        pgetNodeInfo output stateQueuePolicy $
          \outputLovelace outputHeaderHash outputBlock outputLink -> P.do
            PStateQueueNode
              { pstateQueueNode'header = inputHeader
              , pstateQueueNode'daAttestation = inputAttestation
              } <-
              pmatch inputBlock
            PStateQueueNode
              { pstateQueueNode'header = outputHeader
              , pstateQueueNode'daAttestation = outputAttestation
              } <-
              pmatch outputBlock
            -- Every conjunct here is an Aiken `expect`, so a failure errors
            -- rather than returning False. `pand'List` is strict but none of
            -- these can error on their own, so strictness costs only work.
            pif
              ( pand'List
                  [ inputHeaderHash #== expectedHeaderHash
                  , outputHeaderHash #== expectedHeaderHash
                  , inputHeaderHash #== pblake2b_224 # (pserialiseData # pforgetData inputHeader)
                  , inputLovelace #<= outputLovelace
                  , outputLink #== inputLink
                  , inputAttestation #== pdata expectedInputStatus
                  , outputHeader #== inputHeader
                  , outputAttestation #== pdata expectedOutputStatus
                  ]
              )
              (pconstant True)
              perror

{- | Aiken @state_queue.get_node_info@ — private there, private here.

Node-only, and unlike 'pgetStateQueueNode' it reads an 'PTxOut' the caller
already has rather than indexing into the reference inputs, because
@validate_da_attestation_attachment@ needs it on both an input and an output.
-}
pgetNodeInfo ::
  forall (s :: S) (r :: S -> Type).
  Term s PTxOut ->
  Term s (PAsData PCurrencySymbol) ->
  ( Term s PInteger ->
    Term s PByteString ->
    Term s PStateQueueNode ->
    Term s PLink ->
    Term s r
  ) ->
  Term s r
pgetNodeInfo output stateQueuePolicy k =
  pfinalizeLinkedList
    ( pgetElementInfo output $ \_address lovelace mKey elementData link ->
        pmatch mKey $ \case
          PDNothing -> perror
          PDJust headerHash ->
            k
              lovelace
              (pfromData headerHash)
              (pfromData (punsafeCoerce @(PAsData PStateQueueNode) elementData))
              link
    )
    stateQueuePolicy

{- | @expect Some(Input { output, .. }) = list.at(inputs, index)@.

'pelemAt' errors past the end, which is what the @expect@ does.
-}
presolvedOutputAt ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PInteger ->
  Term s PTxOut
presolvedOutputAt inputs index =
  pmatch (pfromData (pelemAt # index # inputs)) $
    \PTxInInfo {ptxInInfo'resolved} -> ptxInInfo'resolved

{- | Aiken @state_queue.finalize_linked_list@.

Supplies the state queue's namespace to a linked-list operation.
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
pfinalizeLinkedList eval stateQueuePolicy =
  eval
    stateQueuePolicy
    pconfirmedStateAssetName
    pblockAssetNamePrefix
    pblockAssetNamePrefixLength
