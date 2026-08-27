{- |
Module      : Midgard.Validators.StateQueue
Description : Plutarch port of @validators/state-queue.ak@.

The state queue is Midgard's chain of blocks: a linked list whose root is the
confirmed state and whose nodes are committed block headers, each keyed by its
own hash. Every other L1 script reads it; this is the only script that writes it.

Its five mint branches are the whole lifecycle of a block. @InitV1@ and @Deinit@
create and destroy the queue alongside the hub oracle. @CommitBlockHeader@
appends a block. @RemoveFraudulentBlockHeader@ tears one out once fraud has been
proved. @MergeToConfirmedStateV1@ retires the oldest block into the confirmed
state after it matures.

Three things are worth knowing before reading it.

/A block's key is its own hash./ The node's NFT asset name is
@blake2b_224(serialise(header))@, minted by this policy, so the key cannot
disagree with the header it names. Everything downstream that identifies a block
by hash — fraud proofs, settlements, the merge — relies on that.

/Appending has two routes, and they are not symmetric./ A block appended after
another block carries over that block's fields directly. A block appended after
the /confirmed state/ must go through
@confirmed_state_next_header_protocol_version_v1@, which authenticates the state
and answers with the version the new header must carry. That indirection is what
keeps the genesis sentinel's protocol version zero from ever reaching a block.

/Removal and merge both walk the list, in opposite directions./ Removal takes a
node out from behind an anchor; merge folds the oldest node into the root. Both
go through the linked-list library, so the structural bookkeeping is shared and
only the payload conditions live here.
-}
module Midgard.Validators.StateQueue (
  stateQueueSpendValidator,
  stateQueueMintValidator,
  pcommitBlockHeaderOutputIsValidV1,
  pcommitBlockHeaderOperatorIsLegitimateV1,
  pcommitBlockHeaderCarriesPreviousBlockV1,
  pcommitBlockHeaderCarriesConfirmedStateV1,
  pmergeCommitmentsMatchHeader,
  pmergeSettlementBindingMatchesHeader,
  pmergeSettlementIdAtRoute,
  pheaderCarriesL2Material,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PMintValue,
  PPubKeyHash,
  PRedeemer,
  PScriptContext (..),
  PScriptHash (..),
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList (
  pappendUnordered,
  pdeinit,
  pfoldFromRoot,
  pinit,
  premove,
  pspendForAddingOrRemovingAnElement,
 )
import Midgard.Common.Utils (
  pgetInclusiveBoundsOfAShortValidityRange,
  pgetInclusiveLowerBoundOfInterval,
  pgetRedeemerAt,
  pgetSpendingRedeemerDataAt,
  phasSigned,
  pquantityOfMint,
 )
import Midgard.DaAttestation qualified as Da
import Midgard.Env qualified as Env
import Midgard.FraudProof (pgetProvenFraudulentBlocksHeaderHash)
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (
  PConfirmedState (..),
  PHeaderV1 (..),
  pblockMaturityDurationV1,
  pconfirmedStateNextHeaderProtocolVersionV1,
  pgenesisConfirmedStateV1,
  pheaderV1IsValid,
  pprotocolVersionV1,
 )
import Midgard.OperatorDirectory (PSlashingReason (..))
import Midgard.OperatorDirectory qualified as Dir
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RetiredOperators qualified as Retired
import Midgard.Scheduler (PSchedDatum (..))
import Midgard.Scheduler qualified as Scheduler
import Midgard.Settlement (pdecodeMintRedeemer)
import Midgard.Settlement qualified as Settlement
import Midgard.StateQueue (
  PMintRedeemer (..),
  PSpendRedeemer (..),
  PStateQueueNode (..),
  pconfirmedStateAssetName,
  pdecodeHeaderView,
  pfinalizeLinkedList,
  pnoDaAttestation,
 )

--------------------------------------------------------------------------------
-- Small shared helpers
--------------------------------------------------------------------------------

pscriptHashOf ::
  forall (s :: S). Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pscriptHashOf cs = pdata (pcon (PScriptHash (pto (pfromData cs))))

punsafeCoerceRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer r = punsafeCoerce (pto (pfromData r))

punsafeCoerceOwnRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer r = punsafeCoerce (pto r)

punsafeCoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

--------------------------------------------------------------------------------
-- The pure seams
--------------------------------------------------------------------------------

{- | Aiken @state_queue.commit_block_header_output_is_valid_v1@.

A committed header must cover a non-empty interval and satisfy
'pheaderV1IsValid'. The interval check lives here rather than in
'pheaderV1IsValid' because that predicate is deliberately non-relational; this
is the one thing about a header's times that depends on nothing else.
-}
pcommitBlockHeaderOutputIsValidV1 :: forall (s :: S). Term s (PHeaderV1 :--> PBool)
pcommitBlockHeaderOutputIsValidV1 = phoistAcyclic $
  plam $ \header ->
    pmatch header $ \PHeaderV1 {pheader'startTime, pheader'endTime} ->
      (pfromData pheader'startTime #< pfromData pheader'endTime)
        #&& (pheaderV1IsValid # header)

{- | Aiken @state_queue.commit_block_header_operator_is_legitimate_v1@.

Four names of an operator must coincide: the one written into the header, the
one the redeemer claims, the one the scheduler says is on shift, and the one
whose active-set node is being spent.

None of the four is trusted on its own — each is authenticated by whichever
script owns it, and this is only the place they are made to agree. That is what
ties "who signed" to "whose turn it is" to "whose bond is being held".
-}
pcommitBlockHeaderOperatorIsLegitimateV1 ::
  forall (s :: S).
  Term s (PAsData PPubKeyHash) ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PAsData PPubKeyHash) ->
  Term s PBool
pcommitBlockHeaderOperatorIsLegitimateV1 headerOp redeemerOp schedulerOp activeOp =
  pand'List
    [ headerOp #== redeemerOp
    , schedulerOp #== redeemerOp
    , activeOp #== redeemerOp
    ]

{- | Aiken @state_queue.commit_block_header_carries_previous_block_v1@.

Appending after another block. The new header's @prev_utxos_root@ must be the
predecessor's @utxos_root@ and its @start_time@ the predecessor's @end_time@,
which is what makes the queue a chain rather than a set: the ledger state and
the covered interval are continuous across the join.
-}
pcommitBlockHeaderCarriesPreviousBlockV1 ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PByteString ->
  Term s PHeaderV1 ->
  Term s PBool
pcommitBlockHeaderCarriesPreviousBlockV1 outputHeader previousHeaderHash previousHeader =
  pmatch outputHeader $
    \PHeaderV1
      { pheader'prevHeaderHash = outPrevHash
      , pheader'prevUtxosRoot = outPrevUtxos
      , pheader'startTime = outStart
      , pheader'protocolVersion = outVersion
      } ->
        pmatch previousHeader $
          \PHeaderV1
            { pheader'utxosRoot = prevUtxos
            , pheader'endTime = prevEnd
            , pheader'protocolVersion = prevVersion
            } ->
              pand'List
                [ pfromData outPrevHash #== previousHeaderHash
                , outPrevUtxos #== prevUtxos
                , outStart #== prevEnd
                , outVersion #== prevVersion
                ]

{- | Aiken @state_queue.commit_block_header_carries_confirmed_state_v1@.

The same continuity, but appending after the /root/. The difference that matters
is the protocol version: it is not copied from the confirmed state but taken
from 'pconfirmedStateNextHeaderProtocolVersionV1', which authenticates the state
first and answers with the version the /next/ header must carry.

That is what stops the genesis sentinel's version zero reaching a block, and it
is why this returns @False@ rather than erroring when the state fails to
authenticate: an unauthenticated state simply cannot answer.
-}
pcommitBlockHeaderCarriesConfirmedStateV1 ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PConfirmedState ->
  Term s PBool
pcommitBlockHeaderCarriesConfirmedStateV1 outputHeader confirmedState =
  pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedState) $ \case
    PNothing -> pconstant False
    PJust nextProtocolVersion ->
      pmatch outputHeader $
        \PHeaderV1
          { pheader'prevHeaderHash = outPrevHash
          , pheader'prevUtxosRoot = outPrevUtxos
          , pheader'startTime = outStart
          , pheader'protocolVersion = outVersion
          } ->
            pmatch confirmedState $
              \PConfirmedState
                { pconfirmed'headerHash
                , pconfirmed'utxoRoot
                , pconfirmed'endTime
                } ->
                  pand'List
                    [ outPrevHash #== pconfirmed'headerHash
                    , outPrevUtxos #== pconfirmed'utxoRoot
                    , outStart #== pconfirmed'endTime
                    , pfromData outVersion #== nextProtocolVersion
                    ]

{- | Aiken @state_queue.merge_commitments_match_header@.

The merge redeemer restates all seven roots and all seven counts, and this
requires every one to equal the header's. The restatement is not redundancy for
its own sake: the settlement that these commitments spawn reads them from the
redeemer, so if they could drift from the header a settlement could be spawned
against commitments no block ever made.
-}
pmergeCommitmentsMatchHeader ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PMintRedeemer ->
  Term s PBool
pmergeCommitmentsMatchHeader header redeemer =
  pmatch header $
    \PHeaderV1
      { pheader'withdrawalsRoot
      , pheader'forcedTransactionsRoot
      , pheader'transactionsRoot
      , pheader'depositsRoot
      , pheader'transitionTraceRoot
      , pheader'eventToStepRoot
      , pheader'validationTracesRoot
      , pheader'withdrawalCount
      , pheader'forcedTransactionCount
      , pheader'l2TransactionCount
      , pheader'depositCount
      , pheader'totalEventCount
      , pheader'transitionStepCount
      , pheader'validationTraceCount
      } ->
        pmatch redeemer $ \case
          PMergeToConfirmedStateV1
            { psqMerge'withdrawalsRoot
            , psqMerge'forcedTransactionsRoot
            , psqMerge'transactionsRoot
            , psqMerge'depositsRoot
            , psqMerge'transitionTraceRoot
            , psqMerge'eventToStepRoot
            , psqMerge'validationTracesRoot
            , psqMerge'withdrawalCount
            , psqMerge'forcedTransactionCount
            , psqMerge'l2TransactionCount
            , psqMerge'depositCount
            , psqMerge'totalEventCount
            , psqMerge'transitionStepCount
            , psqMerge'validationTraceCount
            } ->
              pand'List
                [ psqMerge'withdrawalsRoot #== pforgetData pheader'withdrawalsRoot
                , psqMerge'forcedTransactionsRoot #== pforgetData pheader'forcedTransactionsRoot
                , psqMerge'transactionsRoot #== pforgetData pheader'transactionsRoot
                , psqMerge'depositsRoot #== pforgetData pheader'depositsRoot
                , psqMerge'transitionTraceRoot #== pforgetData pheader'transitionTraceRoot
                , psqMerge'eventToStepRoot #== pforgetData pheader'eventToStepRoot
                , psqMerge'validationTracesRoot #== pforgetData pheader'validationTracesRoot
                , psqMerge'withdrawalCount #== pforgetData pheader'withdrawalCount
                , psqMerge'forcedTransactionCount #== pforgetData pheader'forcedTransactionCount
                , psqMerge'l2TransactionCount #== pforgetData pheader'l2TransactionCount
                , psqMerge'depositCount #== pforgetData pheader'depositCount
                , psqMerge'totalEventCount #== pforgetData pheader'totalEventCount
                , psqMerge'transitionStepCount #== pforgetData pheader'transitionStepCount
                , psqMerge'validationTraceCount #== pforgetData pheader'validationTraceCount
                ]
          _ -> perror

{- | Aiken @state_queue.header_carries_l2_material@.

Whether a block did anything an L2 user could dispute: any of the four event
roots being non-empty. This is the switch that decides whether merging the block
must also spawn a settlement.
-}
pheaderCarriesL2Material :: forall (s :: S). Term s (PHeaderV1 :--> PBool)
pheaderCarriesL2Material = phoistAcyclic $
  plam $ \header -> pmatch header $
    \PHeaderV1
      { pheader'transactionsRoot
      , pheader'depositsRoot
      , pheader'withdrawalsRoot
      , pheader'forcedTransactionsRoot
      } ->
        plet Env.pemptyMerkleTreeRoot $ \emptyRoot ->
          pnot
            #$ pand'List
              [ pfromData pheader'transactionsRoot #== emptyRoot
              , pfromData pheader'depositsRoot #== emptyRoot
              , pfromData pheader'withdrawalsRoot #== emptyRoot
              , pfromData pheader'forcedTransactionsRoot #== emptyRoot
              ]

{- | Aiken @state_queue.merge_settlement_binding_matches_header@.

A block carrying L2 material must spawn a settlement whose id is the block's own
hash; a block carrying none must spawn no settlement at all.

Both directions matter. The first is what gives users something to dispute
against. The second stops a settlement being spawned for an empty block, which
would let an operator's bond be tied up — or a payout claimed — against a block
that moved nothing.
-}
pmergeSettlementBindingMatchesHeader ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PByteString ->
  Term s (PMaybe PByteString) ->
  Term s PBool
pmergeSettlementBindingMatchesHeader header headerNodeKey mSettlementId =
  pif
    (pheaderCarriesL2Material # header)
    ( pmatch mSettlementId $ \case
        PNothing -> pconstant False
        PJust settlementId -> settlementId #== headerNodeKey
    )
    ( pmatch mSettlementId $ \case
        PNothing -> pconstant True
        PJust _ -> pconstant False
    )

{- | Aiken @state_queue.merge_settlement_id_at_route@.

Reads the settlement id out of the settlement policy's own @Spawn@ redeemer, at
the index the merge redeemer names. @None@ means no settlement is being spawned;
it is not an error, because that is the correct shape for an empty block.
-}
pmergeSettlementIdAtRoute ::
  forall (s :: S).
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PInteger) ->
  Term s (PMaybe PByteString)
pmergeSettlementIdAtRoute redeemers settlementScriptHash mSettlementRedeemerIndex =
  pmatch mSettlementRedeemerIndex $ \case
    PDNothing -> pcon PNothing
    PDJust settlementRedeemerIndex ->
      pmatch
        ( pdecodeMintRedeemer
            #$ pto
            $ pfromData
              ( pgetRedeemerAt
                  # redeemers
                  # pdata (pcon (PMinting settlementScriptHash))
                  # pfromData settlementRedeemerIndex
              )
        )
        $ \case
          Settlement.PSpawn {Settlement.pspawn'settlementId} ->
            pcon (PJust (pto (pfromData pspawn'settlementId)))
          _ -> perror

--------------------------------------------------------------------------------
-- Spend
--------------------------------------------------------------------------------

{- | Aiken @validators/state-queue.ak@ — @spend@.

Two ways to spend a queue UTxO. @LinkedListMutation@ is the usual gate: any
structural change is permitted whenever the queue's own minting policy runs,
which is where the real decisions are made.

@AttachDaAttestation@ is the exception — the one spend that edits a node without
minting anything. It defers to the DA attestation policy's redeemer, and the
only thing checked here is that both scripts name the same state-queue input.
Without that agreement one attestation could be attached to a different block
than the one the DA policy validated.
-}
stateQueueSpendValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- state queue mint script hash
        :--> PAsData PCurrencySymbol -- DA attestation policy id
        :--> PScriptContext
        :--> PUnit
    )
stateQueueSpendValidator = plam $ \stateQueueMintScriptHash daAttestationPolicyId ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
  redeemer <-
    plet $ pfromData (punsafeCoerceOwnRedeemer @PSpendRedeemer pscriptContext'redeemer)
  pif
    ( pmatch redeemer $ \case
        PLinkedListMutation ->
          pspendForAddingOrRemovingAnElement
            # stateQueueMintScriptHash
            # pfromData ptxInfo'mint
        PAttachDaAttestation
          { psqAttach'stateQueueInputIndex
          , psqAttach'daAttestationMintRedeemerIndex
          } -> P.do
            stateQueueInputIndex <- plet $ pfromData psqAttach'stateQueueInputIndex
            PTxInInfo {ptxInInfo'outRef} <-
              pmatch $
                pfromData (pelemAt # stateQueueInputIndex # pfromData ptxInfo'inputs)
            pif
              (pnot # (ptxInInfo'outRef #== ownRef))
              perror
              $ pmatch
                ( pfromData
                    ( punsafeCoerceRedeemer @Da.PMintRedeemer $
                        pgetRedeemerAt
                          # pto (pto (pfromData ptxInfo'redeemers))
                          # pdata (pcon (PMinting daAttestationPolicyId))
                          # pfromData psqAttach'daAttestationMintRedeemerIndex
                    )
                )
              $ \case
                Da.PApplyToStateQueue {Da.papply'stateQueueInputIndex} ->
                  pfromData papply'stateQueueInputIndex #== stateQueueInputIndex
                _ -> perror
    )
    (pconstant ())
    perror

--------------------------------------------------------------------------------
-- Mint
--------------------------------------------------------------------------------

{- | Aiken @validators/state-queue.ak@ — @mint@.

Eight parameters, all fixed at deployment: the hub oracle, the two operator sets
that can hold a fraudulent operator, the active set's address (for reading its
spend redeemer), the scheduler, the fraud proof policy, the settlement policy,
and the DA attestation policy.
-}
stateQueueMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- hub oracle script hash
        :--> PAsData PCurrencySymbol -- active operators script hash
        :--> PAsData PAddress -- active operators address
        :--> PAsData PCurrencySymbol -- retired operators script hash
        :--> PAsData PCurrencySymbol -- scheduler script hash
        :--> PAsData PCurrencySymbol -- fraud proof script hash
        :--> PAsData PCurrencySymbol -- settlement script hash
        :--> PAsData PCurrencySymbol -- DA attestation policy id
        :--> PScriptContext
        :--> PUnit
    )
stateQueueMintValidator = plam $
  \hubOracleScriptHash
   activeOperatorsScriptHash
   activeOperatorsAddr
   retiredOperatorsScriptHash
   schedulerScriptHash
   fraudProofScriptHash
   settlementScriptHash
   daAttestationPolicyId
   ctx -> P.do
      PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
        pmatch ctx
      ownPolicyId <-
        plet $ pmatch pscriptContext'scriptInfo $ \case
          PMintingScript cs -> cs
          _ -> perror
      PTxInfo
        { ptxInfo'inputs
        , ptxInfo'outputs
        , ptxInfo'referenceInputs
        , ptxInfo'mint
        , ptxInfo'signatories
        , ptxInfo'redeemers
        , ptxInfo'validRange
        } <-
        pmatch pscriptContext'txInfo
      inputs <- plet $ pfromData ptxInfo'inputs
      outputs <- plet $ pfromData ptxInfo'outputs
      referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
      mint <- plet $ pfromData ptxInfo'mint
      redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
      redeemer <-
        plet $ pfromData (punsafeCoerceOwnRedeemer @PMintRedeemer pscriptContext'redeemer)

      pif
        ( pmatch redeemer $ \case
            ------------------------------------------------------------------
            PInitV1 {psqInit'outputIndex} ->
              pvalidateInit
                ownPolicyId
                hubOracleScriptHash
                outputs
                mint
                ptxInfo'validRange
                (pfromData psqInit'outputIndex)
            ------------------------------------------------------------------
            PDeinit ->
              pvalidateDeinit ownPolicyId hubOracleScriptHash inputs mint
            ------------------------------------------------------------------
            PCommitBlockHeader
              { psqCommit'newBlockOutputIndex
              , psqCommit'continuedLatestBlockOutputIndex
              , psqCommit'operator
              , psqCommit'schedulerRefInputIndex
              , psqCommit'activeOperatorsInputIndex
              , psqCommit'activeOperatorsRedeemerIndex
              } ->
                pvalidateCommitBlockHeader
                  ownPolicyId
                  activeOperatorsAddr
                  schedulerScriptHash
                  inputs
                  outputs
                  referenceInputs
                  mint
                  redeemers
                  (pfromData ptxInfo'signatories)
                  ptxInfo'validRange
                  (pfromData psqCommit'newBlockOutputIndex)
                  (pfromData psqCommit'continuedLatestBlockOutputIndex)
                  psqCommit'operator
                  (pfromData psqCommit'schedulerRefInputIndex)
                  (pfromData psqCommit'activeOperatorsInputIndex)
                  (pfromData psqCommit'activeOperatorsRedeemerIndex)
            ------------------------------------------------------------------
            PRemoveFraudulentBlockHeader
              { psqRemove'fraudulentOperator
              , psqRemove'fraudulentBlocksHeaderHash
              , psqRemove'slashingApproach
              , psqRemove'fraudProofRefInputIndex
              , psqRemove'blockRemovalApproach
              } ->
                pvalidateRemoveFraudulentBlockHeader
                  ownPolicyId
                  activeOperatorsScriptHash
                  retiredOperatorsScriptHash
                  fraudProofScriptHash
                  inputs
                  outputs
                  referenceInputs
                  mint
                  redeemers
                  psqRemove'fraudulentOperator
                  (punsafeCoerceData @PByteString psqRemove'fraudulentBlocksHeaderHash)
                  (punsafeCoerceData @PSlashingApproach psqRemove'slashingApproach)
                  (punsafeCoerceData @PInteger psqRemove'fraudProofRefInputIndex)
                  (punsafeCoerceData @PBlockRemovalApproach psqRemove'blockRemovalApproach)
            ------------------------------------------------------------------
            PMergeToConfirmedStateV1 {} ->
              pvalidateMergeToConfirmedState
                ownPolicyId
                daAttestationPolicyId
                settlementScriptHash
                inputs
                outputs
                mint
                redeemers
                ptxInfo'validRange
                redeemer
        )
        (pconstant ())
        perror

--------------------------------------------------------------------------------
-- InitV1 / Deinit
--------------------------------------------------------------------------------

{- | Aiken @InitV1@.

Creates the queue with the genesis sentinel as its root, at the transaction's
inclusive upper bound. The hub oracle NFT must be minted in the same
transaction, which is what makes the queue and the protocol instance the same
object.

The root's data must equal the sentinel exactly — 'pgenesisConfirmedStateV1'
builds it here and the output is compared against that, rather than the output
being inspected field by field.
-}
pvalidateInit ::
  forall (s :: S) (a :: S -> Type).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PMintValue ->
  Term s a ->
  Term s PInteger ->
  Term s PBool
pvalidateInit ownPolicyId hubOracleScriptHash outputs mint validityRange outputIndex = P.do
  rootOutput <- plet $ pfromData (pelemAt # outputIndex # outputs)
  let (_, currentTimeUpper) =
        pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce validityRange)
  expectedRoot <-
    plet $
      pmatch (pgenesisConfirmedStateV1 # currentTimeUpper) $ \case
        PNothing -> perror
        PJust st -> pforgetData (pdata st)
  pinit
    ( pquantityOfMint
        # mint
        # hubOracleScriptHash
        # Hub.passetName
        #== 1
    )
    rootOutput
    mint
    (\_address _lovelace rootData -> rootData #== expectedRoot)
    ownPolicyId
    pconfirmedStateAssetName

{- | Aiken @Deinit@.

Tears the queue down. The linked list only permits this when the root is the
sole remaining element, so there is nothing here beyond requiring the hub's NFT
to burn alongside — a queue holding blocks cannot be discarded.
-}
pvalidateDeinit ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  Term s PBool
pvalidateDeinit ownPolicyId hubOracleScriptHash inputs mint =
  pif
    (pquantityOfMint # mint # hubOracleScriptHash # Hub.passetName #== (-1))
    ( pdeinit
        inputs
        mint
        (\_input _lovelace _rootData -> pconstant True)
        ownPolicyId
        pconfirmedStateAssetName
    )
    perror

--------------------------------------------------------------------------------
-- CommitBlockHeader
--------------------------------------------------------------------------------

{- | Aiken @CommitBlockHeader@.

Appends a block. The order of the checks is the order of the argument it makes:
the operator signed, the append is a valid list operation, the node's key is the
header's own hash, the header is internally valid and its interval is bound to
this transaction, the scheduler says it is this operator's turn, the active set
is holding this operator's bond, and finally the header carries over correctly
from whatever it was appended to.

The key-is-the-hash step is the one everything else leans on. The asset name is
minted by this policy as @blake2b_224(serialise(header))@, so nothing downstream
that identifies a block by hash can be pointed at a different header.
-}
pvalidateCommitBlockHeader ::
  forall (s :: S) (a :: S -> Type).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PAddress) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s a ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pvalidateCommitBlockHeader
  ownPolicyId
  activeOperatorsAddr
  schedulerScriptHash
  inputs
  outputs
  referenceInputs
  mint
  redeemers
  signatories
  validityRange
  newBlockOutputIndex
  continuedLatestBlockOutputIndex
  operator
  schedulerRefInputIndex
  activeOperatorsInputIndex
  activeOperatorsRedeemerIndex = P.do
    pif
      (pnot #$ phasSigned # operator # signatories)
      perror
      $ P.do
        contAnchorOutput <-
          plet $ pfromData (pelemAt # continuedLatestBlockOutputIndex # outputs)
        newBlockOutput <- plet $ pfromData (pelemAt # newBlockOutputIndex # outputs)
        pfinalizeLinkedList
          ( pappendUnordered contAnchorOutput newBlockOutput inputs mint $
              \_anchorInput _anchorLovelaceChange mAnchorKey anchorData _newLovelace newNodeKey newNodeData -> P.do
                PStateQueueNode
                  { pstateQueueNode'header = outputHeaderData
                  , pstateQueueNode'daAttestation
                  } <-
                  pmatch (punsafeCoerceData @PStateQueueNode newNodeData)
                -- A block is committed without an attestation; attaching one is
                -- a later, separate spend.
                _ <-
                  plet $
                    pif
                      (pstateQueueNode'daAttestation #== pnoDaAttestation)
                      (pconstant @PUnit ())
                      perror
                outputHeader <- plet $ pfromData (pdecodeHeaderView # outputHeaderData)
                PHeaderV1 {pheader'operatorVkey, pheader'startTime, pheader'endTime} <-
                  pmatch outputHeader
                schedulerOperator <-
                  plet $
                    pmatch
                      ( Scheduler.pgetDatum
                          # referenceInputs
                          # schedulerScriptHash
                          # schedulerRefInputIndex
                      )
                      $ \case
                        PActiveOperator {pschedActive'operator} -> pschedActive'operator
                        PNoActiveOperators -> perror
                activeOperator <-
                  plet $
                    pmatch
                      ( pfromData
                          ( punsafeCoerceRedeemer @Active.PSpendRedeemer $
                              pgetSpendingRedeemerDataAt
                                # pfromData activeOperatorsAddr
                                # activeOperatorsInputIndex
                                # activeOperatorsRedeemerIndex
                                # inputs
                                # redeemers
                          )
                      )
                      $ \case
                        Active.PUpdateBondHoldNewState {Active.pupdateState'activeOperator} ->
                          pupdateState'activeOperator
                        _ -> perror
                pand'List
                  [ -- The node's key is the header's own hash.
                    newNodeKey
                      #== (pblake2b_224 #$ pserialiseData # pforgetData outputHeaderData)
                  , pcommitBlockHeaderOutputIsValidV1 # outputHeader
                  , pcommitBoundHeaderTime
                      (pfromData pheader'startTime)
                      (pfromData pheader'endTime)
                      validityRange
                  , pcommitBlockHeaderOperatorIsLegitimateV1
                      pheader'operatorVkey
                      operator
                      schedulerOperator
                      activeOperator
                  , pmatch mAnchorKey $ \case
                      PDJust anchorHeaderHash ->
                        pmatch (punsafeCoerceData @PStateQueueNode anchorData) $
                          \PStateQueueNode {pstateQueueNode'header = anchorHeaderData} ->
                            pcommitBlockHeaderCarriesPreviousBlockV1
                              outputHeader
                              (pfromData anchorHeaderHash)
                              (pfromData (pdecodeHeaderView # anchorHeaderData))
                      PDNothing ->
                        pcommitBlockHeaderCarriesConfirmedStateV1
                          outputHeader
                          (punsafeCoerceData @PConfirmedState anchorData)
                  ]
          )
          ownPolicyId
    where
      pcommitBoundHeaderTime st en vr =
        let (_, upper) = pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce vr)
         in (st #< en) #&& (en #== upper)

--------------------------------------------------------------------------------
-- RemoveFraudulentBlockHeader
--------------------------------------------------------------------------------

{- | Aiken @state_queue.SlashingApproach@ — where the fraudulent operator is.

Tags: @SlashActiveOperator@ 0, @SlashRetiredOperator@ 1,
@OperatorAlreadySlashed@ 2.
-}
data PSlashingApproach (s :: S)
  = PSlashActiveOperator
      {pslashActive'activeOperatorsRedeemerIndex :: Term s (PAsData PInteger)}
  | PSlashRetiredOperator
      {pslashRetired'retiredOperatorsRedeemerIndex :: Term s (PAsData PInteger)}
  | POperatorAlreadySlashed
      { palreadySlashed'activeElementRefInputIndex :: Term s (PAsData PInteger)
      , palreadySlashed'retiredElementRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSlashingApproach)

{- | Aiken @state_queue.BlockRemovalApproach@.

Tags: @RemoveLastFraudulentBlock@ 0, @RemoveFraudulentBlocksLink@ 1.

A fraudulent block cannot be taken out while anything is appended after it, so
removal proceeds from the tail inwards: @RemoveFraudulentBlocksLink@ strips a
successor, @RemoveLastFraudulentBlock@ takes the block itself once it is last.
-}
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

{- | Aiken @RemoveFraudulentBlockHeader@.

Three independent obligations, all required.

The operator must be losing its bond — either slashed out of the active set,
slashed out of the retired set, or shown by non-membership proofs to be in
neither, having been slashed already. In the first two cases the reason is read
back out of the operator set's own redeemer and must be @SlashOperatorForBadState@;
an operator being slashed for something else does not license removing its
blocks.

The block must actually leave the queue, and because a fraudulent block's
successors inherit its fraud, removal walks in from the tail: a successor can be
stripped without its own fraud proof, and only the last block is removed against
the proof itself.

And fraud must have been proved: a reference input carries a fraud-proof token
whose name is the hash of the block being removed.
-}
pvalidateRemoveFraudulentBlockHeader ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s (PAsData PPubKeyHash) ->
  Term s PByteString ->
  Term s PSlashingApproach ->
  Term s PInteger ->
  Term s PBlockRemovalApproach ->
  Term s PBool
pvalidateRemoveFraudulentBlockHeader
  ownPolicyId
  activeOperatorsScriptHash
  retiredOperatorsScriptHash
  fraudProofScriptHash
  inputs
  outputs
  referenceInputs
  mint
  redeemers
  fraudulentOperator
  fraudulentBlocksHeaderHash
  slashingApproach
  fraudProofRefInputIndex
  blockRemovalApproach = P.do
    slashed <-
      plet $
        pmatch slashingApproach $ \case
          PSlashActiveOperator {pslashActive'activeOperatorsRedeemerIndex} ->
            pisBadStateSlashing $
              Dir.pcrossValidateSlashingReason
                fraudulentOperator
                activeOperatorsScriptHash
                (pfromData pslashActive'activeOperatorsRedeemerIndex)
                ( \redeemerData ->
                    pmatch (pfromData (punsafeCoerceRedeemer @Active.PMintRedeemer redeemerData)) $
                      \case
                        Active.PSlashOperator {Active.pactiveSlash'slashingArguments} ->
                          pfromData pactiveSlash'slashingArguments
                        _ -> perror
                )
                redeemers
          PSlashRetiredOperator {pslashRetired'retiredOperatorsRedeemerIndex} ->
            pisBadStateSlashing $
              Dir.pcrossValidateSlashingReason
                fraudulentOperator
                retiredOperatorsScriptHash
                (pfromData pslashRetired'retiredOperatorsRedeemerIndex)
                ( \redeemerData ->
                    pmatch (pfromData (punsafeCoerceRedeemer @Retired.PMintRedeemer redeemerData)) $
                      \case
                        Retired.PSlashOperator {Retired.pretiredSlash'slashingArguments} ->
                          pfromData pretiredSlash'slashingArguments
                        _ -> perror
                )
                redeemers
          POperatorAlreadySlashed
            { palreadySlashed'activeElementRefInputIndex
            , palreadySlashed'retiredElementRefInputIndex
            } ->
              -- Neither set holds the operator, so it was slashed out of both
              -- already; two non-membership proofs, one per set.
              Active.pfinalizeLinkedList
                ( Dir.poperatorIsNotAMember
                    fraudulentOperator
                    referenceInputs
                    (pfromData palreadySlashed'activeElementRefInputIndex)
                )
                activeOperatorsScriptHash
                #&& Retired.pfinalizeLinkedList
                  ( Dir.poperatorIsNotAMember
                      fraudulentOperator
                      referenceInputs
                      (pfromData palreadySlashed'retiredElementRefInputIndex)
                  )
                  retiredOperatorsScriptHash

    removed <-
      plet $
        pfinalizeLinkedList
          ( \policy rootKey prefix prefixLen -> pmatch blockRemovalApproach $ \case
              PRemoveFraudulentBlocksLink
                { premoveLink'fraudulentNodeInputOutref
                , premoveLink'fraudulentNodeOutputIndex
                } ->
                    premove
                      (pfromData premoveLink'fraudulentNodeInputOutref)
                      (pfromData (pelemAt # pfromData premoveLink'fraudulentNodeOutputIndex # outputs))
                      inputs
                      mint
                      ( \_anchorInput _anchorLovelace mAnchorKey _anchorData _removedInput _removedLovelace _removedKey removedData _removedLink ->
                          -- The anchor is the fraudulent block itself; what is
                          -- being stripped is its successor, which inherits the
                          -- fraud and so needs no proof of its own.
                          (mAnchorKey #== pcon (PDJust (pdata fraudulentBlocksHeaderHash)))
                            #&& (premovedOperator removedData #== fraudulentOperator)
                      )
                      policy
                      rootKey
                      prefix
                      prefixLen
              PRemoveLastFraudulentBlock
                { premoveLast'anchorElementInputOutref
                , premoveLast'anchorElementOutputIndex
                } ->
                    premove
                      (pfromData premoveLast'anchorElementInputOutref)
                      (pfromData (pelemAt # pfromData premoveLast'anchorElementOutputIndex # outputs))
                      inputs
                      mint
                      ( \_anchorInput _anchorLovelace _mAnchorKey _anchorData _removedInput _removedLovelace removedKey removedData removedLink ->
                          pand'List
                            [ fraudulentBlocksHeaderHash #== removedKey
                            , premovedOperator removedData #== fraudulentOperator
                            , removedLink #== pcon PDNothing
                            ]
                      )
                      policy
                      rootKey
                      prefix
                      prefixLen
          )
          ownPolicyId

    pand'List
      [ slashed
      , removed
      , pgetProvenFraudulentBlocksHeaderHash
          # referenceInputs
          # fraudProofScriptHash
          # fraudProofRefInputIndex
          #== fraudulentBlocksHeaderHash
      ]
    where
      pisBadStateSlashing reason =
        pmatch (pfromData reason) $ \case
          PSlashOperatorForBadState _ -> pconstant True
          _ -> perror
      premovedOperator removedData =
        pmatch (punsafeCoerceData @PStateQueueNode removedData) $
          \PStateQueueNode {pstateQueueNode'header} ->
            pmatch (pfromData (pdecodeHeaderView # pstateQueueNode'header)) $
              \PHeaderV1 {pheader'operatorVkey} -> pheader'operatorVkey

--------------------------------------------------------------------------------
-- MergeToConfirmedStateV1
--------------------------------------------------------------------------------

{- | Aiken @state_queue.merge_to_confirmed_state@.

Retires the oldest block into the confirmed state — the point at which a block
stops being disputable and becomes Midgard's settled history.

The conditions are, in order: the block is the one immediately after the root
(the linked-list fold enforces that), it is the block the redeemer names, it
carries a DA attestation, it is a valid v1 header, the redeemer's restated
commitments match it, it has matured, the confirmed state authenticates, the new
confirmed state is exactly the expected one, and the settlement binding is
right.

Two of those deserve attention.

/Maturity/ is @block_maturity_duration_v1@ — seven days — measured from the
block's own @end_time@ to the transaction's inclusive /lower/ bound. Using the
lower bound is what makes it a real wait: a transaction cannot claim maturity it
has not reached by widening its validity range downwards.

/The new confirmed state is constructed here and compared/, rather than being
checked field by field. It keeps the old state's @start_time@ — the confirmed
state covers everything since genesis, not just the merged block — while taking
the block's hash, utxo root, end time and protocol version. Building the
expected value and comparing whole leaves no field unchecked by omission.
-}
pvalidateMergeToConfirmedState ::
  forall (s :: S) (a :: S -> Type).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PMintValue ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s a ->
  Term s PMintRedeemer ->
  Term s PBool
pvalidateMergeToConfirmedState
  ownPolicyId
  daAttestationPolicyId
  settlementScriptHash
  inputs
  outputs
  mint
  redeemers
  validityRange
  redeemer =
    pmatch redeemer $ \case
      PMergeToConfirmedStateV1
        { psqMerge'headerNodeKey
        , psqMerge'confirmedStateInputOutref
        , psqMerge'confirmedStateOutputIndex
        , psqMerge'mSettlementRedeemerIndex
        } -> P.do
          headerNodeKey <- plet $ punsafeCoerceData @PByteString psqMerge'headerNodeKey
          contConfirmedStateOutput <-
            plet $
              pfromData
                ( pelemAt
                    # punsafeCoerceData @PInteger psqMerge'confirmedStateOutputIndex
                    # outputs
                )
          pfinalizeLinkedList
            ( pfoldFromRoot
                (punsafeCoerceData @PTxOutRef psqMerge'confirmedStateInputOutref)
                contConfirmedStateOutput
                inputs
                mint
                $ \_rootInput _rootLovelaceChange inputConfirmedStateData _headerInput _headerLovelace inputHeaderNodeKey inputHeaderNodeData _inputHeaderLink outputConfirmedStateData -> P.do
                  PStateQueueNode
                    { pstateQueueNode'header = inputHeaderData
                    , pstateQueueNode'daAttestation
                    } <-
                    pmatch (punsafeCoerceData @PStateQueueNode inputHeaderNodeData)
                  inputHeader <- plet $ pfromData (pdecodeHeaderView # inputHeaderData)
                  PHeaderV1
                    { pheader'utxosRoot
                    , pheader'endTime
                    , pheader'protocolVersion
                    } <-
                    pmatch inputHeader
                  inputConfirmedState <-
                    plet $ punsafeCoerceData @PConfirmedState inputConfirmedStateData
                  PConfirmedState
                    { pconfirmed'headerHash = inputHeaderHash
                    , pconfirmed'startTime = inputStartTime
                    } <-
                    pmatch inputConfirmedState
                  expectedOutputConfirmedState <-
                    plet . pforgetData . pdata . pcon $
                      PConfirmedState
                        { pconfirmed'headerHash = pdata headerNodeKey
                        , pconfirmed'prevHeaderHash = inputHeaderHash
                        , pconfirmed'utxoRoot = pheader'utxosRoot
                        , pconfirmed'startTime = inputStartTime
                        , pconfirmed'endTime = pheader'endTime
                        , pconfirmed'protocolVersion = pheader'protocolVersion
                        }
                  settlementBinding <-
                    plet $
                      pif
                        (pheaderCarriesL2Material # inputHeader)
                        ( pmergeSettlementBindingMatchesHeader
                            inputHeader
                            headerNodeKey
                            ( pmergeSettlementIdAtRoute
                                redeemers
                                settlementScriptHash
                                (punsafeCoerceData @(PMaybeData PInteger) psqMerge'mSettlementRedeemerIndex)
                            )
                        )
                        -- An empty block must spawn no settlement at all, and
                        -- must not even name a redeemer index.
                        ( ( punsafeCoerceData @(PMaybeData PInteger) psqMerge'mSettlementRedeemerIndex
                              #== pcon PDNothing
                          )
                            #&& pmergeSettlementBindingMatchesHeader
                              inputHeader
                              headerNodeKey
                              (pcon PNothing)
                        )
                  pand'List
                    [ inputHeaderNodeKey #== headerNodeKey
                    , pstateQueueNode'daAttestation
                        #== pdata (pto (pfromData daAttestationPolicyId))
                    , pfromData pheader'protocolVersion #== pprotocolVersionV1
                    , pheaderV1IsValid # inputHeader
                    , pmergeCommitmentsMatchHeader inputHeader redeemer
                    , (pfromData pheader'endTime + pblockMaturityDurationV1)
                        #<= pgetInclusiveLowerBoundOfInterval # punsafeCoerce validityRange
                    , pmatch
                        (pconfirmedStateNextHeaderProtocolVersionV1 # inputConfirmedState)
                        $ \case
                          PNothing -> pconstant False
                          PJust v -> v #== pprotocolVersionV1
                    , outputConfirmedStateData #== expectedOutputConfirmedState
                    , settlementBinding
                    ]
            )
            ownPolicyId
      _ -> perror
