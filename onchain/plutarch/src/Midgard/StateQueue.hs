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
  PSpendRedeemer (..),
  PDatum,
  pconfirmedStateAssetName,
  pblockAssetNamePrefix,
  pblockAssetNamePrefixLength,
  pnoDaAttestation,
  PStateQueueNode (..),
  pdecodeHeaderView,
  pcommitBoundHeaderTimeIsValid,
  pgetConfirmedState,
  pgetStateQueueNode,
  pgetBlockDatumV1,
  pgetPrevHeaderHashOfNodeV1,
  pvalidateDaAttestationAttachment,
  pfinalizeLinkedList,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
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
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList (pgetElementInfo)
import LinkedList.Types (PElement, PLink, PRootKey)
import Midgard.Common.Utils (pgetInclusiveBoundsOfAShortValidityRange)
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
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @state_queue.MintRedeemer@.

Constructor order fixes the on-chain tag: @InitV1@ 0, @Deinit@ 1,
@CommitBlockHeader@ 2, @RemoveFraudulentBlockHeader@ 3,
@MergeToConfirmedStateV1@ 4. The directory's slashing path matches on tag 3, so
this ordering is load-bearing.

Fields this consumer does not read are typed 'PData' rather than being given
their real types, which would drag in @SlashingApproach@,
@BlockRemovalApproach@ and the seven root/count types of the merge redeemer.

That is a deliberate, and small, departure from Aiken. Aiken's
@expect RemoveFraudulentBlockHeader { fraudulent_operator, .. } = data@
structurally validates the /whole/ redeemer; reading one field positionally out
of a 'DeriveAsDataStruct' does not. The lost check is redundant in practice: the
state queue's own minting policy runs in the same transaction against the same
redeemer and validates it properly. Revisit this if that ever stops being true.
-}
data PMintRedeemer (s :: S)
  = PInitV1 {psqInit'outputIndex :: Term s (PAsData PInteger)}
  | PDeinit
  | PCommitBlockHeader
      { psqCommit'newBlockOutputIndex :: Term s (PAsData PInteger)
      , psqCommit'continuedLatestBlockOutputIndex :: Term s (PAsData PInteger)
      , psqCommit'operator :: Term s (PAsData PPubKeyHash)
      , psqCommit'schedulerRefInputIndex :: Term s (PAsData PInteger)
      , psqCommit'activeOperatorsInputIndex :: Term s (PAsData PInteger)
      , psqCommit'activeOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PRemoveFraudulentBlockHeader
      { psqRemove'fraudulentOperator :: Term s (PAsData PPubKeyHash)
      , psqRemove'fraudulentBlocksHeaderHash :: Term s PData
      , psqRemove'slashingApproach :: Term s PData
      , psqRemove'fraudProofRefInputIndex :: Term s PData
      , psqRemove'blockRemovalApproach :: Term s PData
      }
  | PMergeToConfirmedStateV1
      { psqMerge'headerNodeKey :: Term s PData
      , psqMerge'confirmedStateInputOutref :: Term s PData
      , psqMerge'confirmedStateOutputIndex :: Term s PData
      , psqMerge'mSettlementRedeemerIndex :: Term s PData
      , psqMerge'withdrawalsRoot :: Term s PData
      , psqMerge'forcedTransactionsRoot :: Term s PData
      , psqMerge'transactionsRoot :: Term s PData
      , psqMerge'depositsRoot :: Term s PData
      , psqMerge'transitionTraceRoot :: Term s PData
      , psqMerge'eventToStepRoot :: Term s PData
      , psqMerge'validationTracesRoot :: Term s PData
      , psqMerge'withdrawalCount :: Term s PData
      , psqMerge'forcedTransactionCount :: Term s PData
      , psqMerge'l2TransactionCount :: Term s PData
      , psqMerge'depositCount :: Term s PData
      , psqMerge'totalEventCount :: Term s PData
      , psqMerge'transitionStepCount :: Term s PData
      , psqMerge'validationTraceCount :: Term s PData
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

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
  , pstateQueueNode'daAttestation :: Term s (PAsData PByteString)
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
pnoDaAttestation :: forall (s :: S). Term s (PAsData PByteString)
pnoDaAttestation = pdata (pconstant "")

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
  Term s (PAsData PCurrencySymbol) ->
  Term s PBool
pvalidateDaAttestationAttachment
  inputs
  outputs
  stateQueuePolicy
  stateQueueInputIndex
  stateQueueOutputIndex
  expectedHeaderHash
  daAttestationPolicyId = P.do
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
                  , inputLovelace #<= outputLovelace
                  , outputLink #== inputLink
                  , inputAttestation #== pnoDaAttestation
                  , outputHeader #== inputHeader
                  , outputAttestation #== pdata (pto (pfromData daAttestationPolicyId))
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
