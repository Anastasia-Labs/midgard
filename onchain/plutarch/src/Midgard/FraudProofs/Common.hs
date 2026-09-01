{- |
Module      : Midgard.FraudProofs.Common
Description : Plutarch port of @lib/midgard/fraud-proofs/common.ak@.

The scaffolding every fraud-proof family is built on. A fraud proof is a
computation thread: a chain of UTxOs, each holding the thread's NFT and a
working datum, each spent to produce the next. This module owns the part of that
chain which is the same for every family, so that a family's own script contains
only the step it actually proves.

Three things live here.

__Step transitions.__ 'pcontinue' carries a thread one step forward and
'pfinalize' ends it in a conviction; 'pcancel' abandons it. What they enforce is
the thread's identity: the input carries exactly one thread token and no other
tokens, the output carries the very same token, and the fraud prover named in
the datum does not change. The token being singular is what prevents double
satisfaction — two threads cannot be advanced by one output — and it is checked
on both sides of the transition rather than only on the output.

__Evidence.__ 'pverifyNativeTxInStateQueueNodeWith' is the seam through which a
step reaches a transaction inside a committed block. It is worth reading closely,
because everything that makes a family's evidence trustworthy is here and
nowhere else:

  * the prover's @native_tx_compact_cbor@ must decode canonically and hash to
    the @native_tx_id@ it claims (the codec precondition);
  * the hub oracle names the state queue policy, so the block being challenged
    cannot be read from an impostor UTxO;
  * the queue node's key must equal the thread token's asset name with the
    catalogue id dropped, so a thread opened against block A cannot be advanced
    with evidence from block B;
  * the prover-supplied /raw/ transactions MPF root must re-commit, under this
    block's @l2_transaction_count@, to the header's counted
    @transactions_root@ — this is what turns an arbitrary byte string into the
    header's own commitment; and only then
  * the membership opening runs, against the now-authenticated root.

The opening itself is delegated to a @membership_check@ callback. That is the
whole point of the @_with@ split: a carriage chooses /where the proof's bytes
travelled/, never /what they prove/.

__Carriage.__ 'PNativeTxInclusionCarriage' and 'PNonMembershipCarriage' are the
prover's choice between putting a proof in this transaction's redeemers and
naming chunk UTxOs published beforehand (issue #545). Both arms end at the same
verified opening against the same authenticated root, so the choice is a cost
decision and not a trust one.

=== Rejection mode

Aiken's @expect@ aborts; it does not return @False@. Every check ported here
that was an @expect@ aborts too, including the final @expect validation(...)@
in the @pass_*@ helpers. A family that returns @False@ where the Aiken original
aborted is a divergence even though both fail the transaction, because the two
compose differently inside a caller's @and@ block.

=== One deliberate weakening

Aiken's @expect ... : ct.StepDatum<Data> = output_datum_data@ type-checks the
produced datum's shape before reading it. Plutarch coerces instead, so a
malformed datum fails when a field is read rather than at the coercion. Every
field is read on every path here, so the two agree on which transactions pass;
they can differ in which error is reported. This matches how the rest of the
port reads datums.
-}
module Midgard.FraudProofs.Common (
  -- * Carriage
  PNativeTxInclusionArgs (..),
  PPublishedChunkInclusionArgs (..),
  PNativeTxInclusionCarriage (..),
  PNonMembershipCarriage (..),
  pcarriageTransactionsPhasRoot,
  pverifyNonMembershipCarried,

  -- * First steps
  ppassNativeTxToNextStepCarried,
  ppassNativeTxToNextStep,
  ppassCommittedTransactionsLeafToNextStep,

  -- * Step transitions
  pcancel,
  pcontinue,
  pfinalize,

  -- * Evidence
  pverifyCommittedTransactionsLeafInStateQueueNode,
  pverifyNativeTxInStateQueueNode,
  pverifyNativeTxInStateQueueNodeWith,

  -- * Payout
  pvalidateOutputToFraudProver,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  POutputDatum (..),
  PPubKeyHash,
  PRedeemer,
  PScriptHash,
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.SingularUtxoIndexer (poneToOne)
import Midgard.Common.Types (PProof)
import Midgard.Common.Utils (
  PAssetTriplet (..),
  pgetRedeemerAt,
  pgetSingleAssetFromValueApartFromAda,
  phasSigned,
  pplutarchPexcludesRaw,
  pplutarchPhasRaw,
 )
import Midgard.ComputationThread (PMintRedeemer (..), PStepDatum (..))
import Midgard.Env qualified as Env
import Midgard.FraudProof qualified as FraudProof
import Midgard.FraudProofs.ChunkedInclusion (
  PPublishedProofCarriage (..),
  pdelegatedChunkMembership,
  pdelegatedChunkNonMembership,
 )
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxCompactCborV1)
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.TransitionTrace (PRootDomain (..), pcommitCountedRoot)

--------------------------------------------------------------------------------
-- Carriage types
--------------------------------------------------------------------------------

{- | Aiken @fraud_proofs/common.NativeTxInclusionArgs@.

Everything a step needs to reach one transaction of one committed block, with
the membership proof travelling in this transaction's redeemers.

@transactions_phas_root@ is the /raw/ MPF root the prover supplies. It is not
trusted on arrival: 'pverifyNativeTxInStateQueueNodeWith' authenticates it
against the header's counted @transactions_root@ before any opening runs.
-}
data PNativeTxInclusionArgs (s :: S) = PNativeTxInclusionArgs
  { pinclusionArgs'inputIndex :: Term s (PAsData PInteger)
  , pinclusionArgs'outputIndex :: Term s (PAsData PInteger)
  , pinclusionArgs'hubRefInputIndex :: Term s (PAsData PInteger)
  , pinclusionArgs'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , pinclusionArgs'nativeTxId :: Term s (PAsData PByteString)
  , pinclusionArgs'nativeTxCompactCbor :: Term s (PAsData PByteString)
  , pinclusionArgs'transactionsPhasRoot :: Term s (PAsData PByteString)
  , pinclusionArgs'txMembershipProof :: Term s (PAsData PProof)
  , pinclusionArgs'inclusionProofScriptWithdrawRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeTxInclusionArgs)

{- | Aiken @fraud_proofs/common.PublishedChunkInclusionArgs@.

The same evidence as 'PNativeTxInclusionArgs' with the proof taken out of the
transaction: its steps live in published chunk UTxOs and only their order
reaches the redeemer, as indices into the transaction's reference inputs
(issue #545). Field-for-field identical up to the proof, because the two
carriages authenticate the very same commitment.
-}
data PPublishedChunkInclusionArgs (s :: S) = PPublishedChunkInclusionArgs
  { ppublishedArgs'inputIndex :: Term s (PAsData PInteger)
  , ppublishedArgs'outputIndex :: Term s (PAsData PInteger)
  , ppublishedArgs'hubRefInputIndex :: Term s (PAsData PInteger)
  , ppublishedArgs'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , ppublishedArgs'nativeTxId :: Term s (PAsData PByteString)
  , ppublishedArgs'nativeTxCompactCbor :: Term s (PAsData PByteString)
  , ppublishedArgs'transactionsPhasRoot :: Term s (PAsData PByteString)
  , ppublishedArgs'orderedChunkReferenceInputIndices ::
      Term s (PAsData (PBuiltinList (PAsData PInteger)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPublishedChunkInclusionArgs)

{- | Aiken @fraud_proofs/common.NativeTxInclusionCarriage@.

How a step's transactions-root membership opening reaches L1. The prover
chooses: a small proof belongs in the redeemer, a proof an adversary has grinded
past the envelope belongs on the published-chunk route.
-}
data PNativeTxInclusionCarriage (s :: S)
  = -- | The proof travels in this transaction's redeemers, verified by the
    -- merkelized @phas@ validator. About 276 signed bytes per proof level.
    PRedeemerCarriedInclusion
      {predeemerCarried'args :: Term s (PAsData PNativeTxInclusionArgs)}
  | -- | The proof was published beforehand; this transaction names its chunks.
    -- One integer and one reference input per chunk.
    PPublishedChunkInclusion
      {ppublishedChunk'args :: Term s (PAsData PPublishedChunkInclusionArgs)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeTxInclusionCarriage)

{- | Aiken @fraud_proofs/common.NonMembershipCarriage@.

The absence twin of 'PNativeTxInclusionCarriage'; the same reasoning applies
unchanged.
-}
data PNonMembershipCarriage (s :: S)
  = -- | The proof travels in this transaction's redeemers, verified by the
    -- merkelized @pexcludes@ validator.
    PRedeemerCarriedNonMembership
      { pnonMembership'proof :: Term s (PAsData PProof)
      , pnonMembership'scriptRedeemerIndex :: Term s (PAsData PInteger)
      }
  | -- | The proof was published beforehand; this transaction names its chunks.
    PPublishedChunkNonMembership
      {ppublishedNonMembership'carriage :: Term s (PAsData PPublishedProofCarriage)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNonMembershipCarriage)

--------------------------------------------------------------------------------
-- Carriage accessors
--------------------------------------------------------------------------------

{- | Aiken @fraud_proofs/common.carriage_transactions_phas_root@.

The raw transactions MPF root a carriage names, whichever arm it is. Safe for a
step to thread forward only /after/ the carriage has been verified, because that
is where the root is authenticated against the challenged header's counted
@transactions_root@.
-}
pcarriageTransactionsPhasRoot ::
  forall (s :: S). Term s (PNativeTxInclusionCarriage :--> PByteString)
pcarriageTransactionsPhasRoot = phoistAcyclic $
  plam $ \carriage -> pmatch carriage $ \case
    PRedeemerCarriedInclusion {predeemerCarried'args} ->
      pmatch (pfromData predeemerCarried'args) $
        \PNativeTxInclusionArgs {pinclusionArgs'transactionsPhasRoot} ->
          pfromData pinclusionArgs'transactionsPhasRoot
    PPublishedChunkInclusion {ppublishedChunk'args} ->
      pmatch (pfromData ppublishedChunk'args) $
        \PPublishedChunkInclusionArgs {ppublishedArgs'transactionsPhasRoot} ->
          pfromData ppublishedArgs'transactionsPhasRoot

{- | Aiken @fraud_proofs/common.verify_non_membership_carried@.

Verifies the absence of @key_bytes@ under an already-authenticated
@merkle_root@, by whichever route the prover chose.

The published-chunk arm delegates to the merkelized verifier, which reads the
chunks from the very same reference inputs this step sees. Keeping the walk out
of the step's own script is what stops the chunked route from enlarging the
direct one.

The Aiken original takes @reference_inputs@ and never reads it — the delegated
check works entirely from the redeemer — and the port keeps the parameter so the
call sites match.
-}
pverifyNonMembershipCarried ::
  forall (s :: S).
  Term s PNonMembershipCarriage ->
  Term s PByteString ->
  Term s PByteString ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pverifyNonMembershipCarried carriage merkleRoot keyBytes _referenceInputs redeemers =
  pmatch carriage $ \case
    PRedeemerCarriedNonMembership {pnonMembership'proof} ->
      pplutarchPexcludesRaw
        merkleRoot
        keyBytes
        (pforgetData pnonMembership'proof)
        redeemers
    PPublishedChunkNonMembership {ppublishedNonMembership'carriage} ->
      pdelegatedChunkNonMembership
        # Env.pmpfChunkedVerifyValidatorHash
        # redeemers
        # pfromData ppublishedNonMembership'carriage
        # merkleRoot
        # keyBytes

--------------------------------------------------------------------------------
-- Evidence
--------------------------------------------------------------------------------

{- | The callback a family's step receives once a native transaction has been
authenticated: the thread's identity, the step's state, the challenged header,
the transaction id and the decoded compact transaction.

Named because three of the four @pass_*@ helpers take it and its nine arguments
are otherwise unreadable at the call site.
-}
type PNativeTxStepValidation (s :: S) =
  Term s (PAsData PScriptHash) -> -- own (input) script hash
  Term s (PAsData PTokenName) -> -- computation thread token asset name
  Term s (PAsData PPubKeyHash) -> -- fraud prover
  Term s (PMaybeData PData) -> -- input state data
  Term s (PAsData PScriptHash) -> -- output script hash
  Term s PData -> -- output state data
  Term s (PAsData PHeaderV1) -> -- challenged header
  Term s PByteString -> -- native transaction id
  Term s PVerifiedMidgardNativeTxCompact ->
  Term s PBool

{- | Aiken @fraud_proofs/common.verify_native_tx_in_state_queue_node_with@.

The carriage-agnostic core of 'pverifyNativeTxInStateQueueNode'. Everything that
makes the evidence trustworthy lives here — the canonical codec precondition,
hub identity, state-queue node identity, and the counted-root authentication
that turns a prover-supplied raw root into the header's own commitment — and the
opening itself is delegated to @membershipCheck@, which receives the
authenticated root, the key and the value.

The first conjunct of the guard re-states the codec precondition. It is a
tautology given 'pverifyNativeTxCompactCborV1', which either returns a record
whose @txId@ is the argument or aborts; it is there because Aiken evaluates that
call strictly and the port must abort on a bad codec even for a caller that
never looks at the decoded view.
-}
pverifyNativeTxInStateQueueNodeWith ::
  forall (s :: S) (r :: S -> Type).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s (PAsData PTokenName) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  (Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s PBool) ->
  ( Term s PByteString ->
    Term s (PAsData PHeaderV1) ->
    Term s PVerifiedMidgardNativeTxCompact ->
    Term s r
  ) ->
  Term s r
pverifyNativeTxInStateQueueNodeWith
  nativeTxId
  nativeTxCompactCbor
  transactionsPhasRoot
  computationThreadTokenAssetName
  hubOracle
  hubRefInputIndex
  stateQueueNodeRefInputIndex
  referenceInputs
  membershipCheck
  k = P.do
    nativeTxView <-
      plet $ pverifyNativeTxCompactCborV1 # nativeTxId # nativeTxCompactCbor
    PVerifiedMidgardNativeTxCompact {pverified'txId} <- pmatch nativeTxView

    PHubOracleDatum {phubOracle'stateQueue} <-
      pmatch (Hub.pgetDatum # referenceInputs # hubOracle # hubRefInputIndex)

    pgetBlockDatumV1 referenceInputs phubOracle'stateQueue stateQueueNodeRefInputIndex $
      \header stateQueueNodeKey -> P.do
        PHeaderV1 {pheader'transactionsRoot, pheader'l2TransactionCount} <-
          pmatch (pfromData header)
        pif
          ( pverified'txId
              #== nativeTxId
              #&& stateQueueNodeKey
              #== (FraudProof.passetNameToHeaderHash # computationThreadTokenAssetName)
              #&& pcountedRootMatches
                pheader'transactionsRoot
                pheader'l2TransactionCount
                transactionsPhasRoot
              #&& membershipCheck transactionsPhasRoot nativeTxId nativeTxCompactCbor
          )
          (k nativeTxId header nativeTxView)
          perror

{- | The counted-root authentication both evidence helpers perform.

Only the genuine raw root re-hashes to the header's committed value under this
block's @l2_transaction_count@, so after this the raw root is exactly as
trustworthy as the header commitment.
-}
pcountedRootMatches ::
  forall (s :: S).
  Term s (PAsData PByteString) ->
  Term s (PAsData PInteger) ->
  Term s PByteString ->
  Term s PBool
pcountedRootMatches committedRoot l2TransactionCount phasRoot =
  pcommitCountedRoot
    (pdata (pcon PTransactionsV1RootDomain))
    phasRoot
    (pfromData l2TransactionCount)
    #== pfromData committedRoot

{- | Aiken @fraud_proofs/common.verify_native_tx_in_state_queue_node@.

'pverifyNativeTxInStateQueueNodeWith' with the opening carried in this
transaction's redeemers, checked by delegation to the merkelized @phas@
validator.
-}
pverifyNativeTxInStateQueueNode ::
  forall (s :: S) (r :: S -> Type).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PData ->
  Term s (PAsData PTokenName) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  ( Term s PByteString ->
    Term s (PAsData PHeaderV1) ->
    Term s PVerifiedMidgardNativeTxCompact ->
    Term s r
  ) ->
  Term s r
pverifyNativeTxInStateQueueNode
  nativeTxId
  nativeTxCompactCbor
  transactionsPhasRoot
  txMembershipProof
  computationThreadTokenAssetName
  hubOracle
  hubRefInputIndex
  stateQueueNodeRefInputIndex
  inclusionProofScriptWithdrawRedeemerIndex
  referenceInputs
  redeemers
  k =
    pverifyNativeTxInStateQueueNodeWith
      nativeTxId
      nativeTxCompactCbor
      transactionsPhasRoot
      computationThreadTokenAssetName
      hubOracle
      hubRefInputIndex
      stateQueueNodeRefInputIndex
      referenceInputs
      ( \authenticatedRoot keyBytes valueBytes ->
          -- The withdraw redeemer index is accepted and ignored, exactly as in
          -- Aiken: the entry is found by script hash instead.
          plet inclusionProofScriptWithdrawRedeemerIndex $ \_ ->
            pplutarchPhasRaw
              authenticatedRoot
              keyBytes
              valueBytes
              txMembershipProof
              redeemers
      )
      k

{- | Aiken @fraud_proofs/common.verify_committed_transactions_leaf_in_state_queue_node@.

Authenticates one raw @(key, value)@ leaf of a committed block's
@transactions_root@ __without__ requiring the value to be a well-formed
canonical native-V1 transaction and __without__ requiring the key to be that
value's transaction id.

This is the evidence primitive of the @da-hash-preimage@ family (@GOAL_SPEC.md@
Q44): the very fault it proves is a committed leaf whose key is not the
hash-preimage commitment of its value, so the codec check that
'pverifyNativeTxInStateQueueNodeWith' performs must not be a precondition here.
Everything else is identical, so the returned leaf is exactly as trustworthy as
the header commitment. No other family may use it.
-}
pverifyCommittedTransactionsLeafInStateQueueNode ::
  forall (s :: S) (r :: S -> Type).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PData ->
  Term s (PAsData PTokenName) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  (Term s (PAsData PHeaderV1) -> Term s r) ->
  Term s r
pverifyCommittedTransactionsLeafInStateQueueNode
  committedTxId
  committedLeafValue
  transactionsPhasRoot
  txMembershipProof
  computationThreadTokenAssetName
  hubOracle
  hubRefInputIndex
  stateQueueNodeRefInputIndex
  inclusionProofScriptWithdrawRedeemerIndex
  referenceInputs
  redeemers
  k = P.do
    PHubOracleDatum {phubOracle'stateQueue} <-
      pmatch (Hub.pgetDatum # referenceInputs # hubOracle # hubRefInputIndex)

    pgetBlockDatumV1 referenceInputs phubOracle'stateQueue stateQueueNodeRefInputIndex $
      \header stateQueueNodeKey -> P.do
        PHeaderV1 {pheader'transactionsRoot, pheader'l2TransactionCount} <-
          pmatch (pfromData header)
        pif
          ( stateQueueNodeKey
              #== (FraudProof.passetNameToHeaderHash # computationThreadTokenAssetName)
              #&& pcountedRootMatches
                pheader'transactionsRoot
                pheader'l2TransactionCount
                transactionsPhasRoot
              #&& plet inclusionProofScriptWithdrawRedeemerIndex
                ( \_ ->
                    pplutarchPhasRaw
                      transactionsPhasRoot
                      committedTxId
                      committedLeafValue
                      txMembershipProof
                      redeemers
                )
          )
          (k header)
          perror

--------------------------------------------------------------------------------
-- First steps
--------------------------------------------------------------------------------

{- | Aiken @fraud_proofs/common.pass_native_tx_to_next_step@.

The first step of a family whose evidence is committed by the node's native
transaction root rather than by PlutusData @MidgardTxCompact@ values: advance
the thread, authenticate the transaction, then hand both to the family's own
@validation@.
-}
ppassNativeTxToNextStep ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PNativeTxInclusionArgs ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  PNativeTxStepValidation s ->
  Term s PBool
ppassNativeTxToNextStep
  computationThreadTokenPolicyId
  hubOracle
  datum
  args
  ownOutRef
  inputs
  referenceInputs
  outputs
  redeemers
  validation = P.do
    PNativeTxInclusionArgs
      { pinclusionArgs'inputIndex
      , pinclusionArgs'outputIndex
      , pinclusionArgs'hubRefInputIndex
      , pinclusionArgs'stateQueueNodeRefInputIndex
      , pinclusionArgs'nativeTxId
      , pinclusionArgs'nativeTxCompactCbor
      , pinclusionArgs'transactionsPhasRoot
      , pinclusionArgs'txMembershipProof
      , pinclusionArgs'inclusionProofScriptWithdrawRedeemerIndex
      } <-
      pmatch args
    nativeTxId <- plet $ pfromData pinclusionArgs'nativeTxId

    pcontinue
      computationThreadTokenPolicyId
      (pexpectStepDatum datum)
      (pfromData pinclusionArgs'inputIndex)
      (pfromData pinclusionArgs'outputIndex)
      ownOutRef
      inputs
      outputs
      $ \ownScriptHash ctAssetName fraudProver mInputState outputScriptHash outputStateData ->
        pverifyNativeTxInStateQueueNode
          nativeTxId
          (pfromData pinclusionArgs'nativeTxCompactCbor)
          (pfromData pinclusionArgs'transactionsPhasRoot)
          (pforgetData pinclusionArgs'txMembershipProof)
          ctAssetName
          hubOracle
          (pfromData pinclusionArgs'hubRefInputIndex)
          (pfromData pinclusionArgs'stateQueueNodeRefInputIndex)
          (pfromData pinclusionArgs'inclusionProofScriptWithdrawRedeemerIndex)
          referenceInputs
          redeemers
          $ \_verifiedTxId header nativeTxView ->
            pif
              ( validation
                  ownScriptHash
                  ctAssetName
                  fraudProver
                  mInputState
                  outputScriptHash
                  outputStateData
                  header
                  nativeTxId
                  nativeTxView
              )
              (pconstant True)
              perror

{- | Aiken @fraud_proofs/common.pass_native_tx_to_next_step_carried@.

The single seam through which all four foundational families reach
published-chunk carriage. The per-family semantics that follow the opening are
untouched by it: both arms end at the same @validation@ call with the same
authenticated evidence.
-}
ppassNativeTxToNextStepCarried ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PNativeTxInclusionCarriage ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  PNativeTxStepValidation s ->
  Term s PBool
ppassNativeTxToNextStepCarried
  computationThreadTokenPolicyId
  hubOracle
  datum
  carriage
  ownOutRef
  inputs
  referenceInputs
  outputs
  redeemers
  validation =
    pmatch carriage $ \case
      PRedeemerCarriedInclusion {predeemerCarried'args} ->
        ppassNativeTxToNextStep
          computationThreadTokenPolicyId
          hubOracle
          datum
          (pfromData predeemerCarried'args)
          ownOutRef
          inputs
          referenceInputs
          outputs
          redeemers
          validation
      PPublishedChunkInclusion {ppublishedChunk'args} -> P.do
        PPublishedChunkInclusionArgs
          { ppublishedArgs'inputIndex
          , ppublishedArgs'outputIndex
          , ppublishedArgs'hubRefInputIndex
          , ppublishedArgs'stateQueueNodeRefInputIndex
          , ppublishedArgs'nativeTxId
          , ppublishedArgs'nativeTxCompactCbor
          , ppublishedArgs'transactionsPhasRoot
          , ppublishedArgs'orderedChunkReferenceInputIndices
          } <-
          pmatch (pfromData ppublishedChunk'args)
        nativeTxId <- plet $ pfromData ppublishedArgs'nativeTxId
        chunkCarriage <-
          plet $
            pcon
              ( PPublishedProofCarriage
                  { pcarriage'orderedChunkReferenceInputIndices =
                      ppublishedArgs'orderedChunkReferenceInputIndices
                  }
              )

        pcontinue
          computationThreadTokenPolicyId
          (pexpectStepDatum datum)
          (pfromData ppublishedArgs'inputIndex)
          (pfromData ppublishedArgs'outputIndex)
          ownOutRef
          inputs
          outputs
          $ \ownScriptHash ctAssetName fraudProver mInputState outputScriptHash outputStateData ->
            pverifyNativeTxInStateQueueNodeWith
              nativeTxId
              (pfromData ppublishedArgs'nativeTxCompactCbor)
              (pfromData ppublishedArgs'transactionsPhasRoot)
              ctAssetName
              hubOracle
              (pfromData ppublishedArgs'hubRefInputIndex)
              (pfromData ppublishedArgs'stateQueueNodeRefInputIndex)
              referenceInputs
              ( \authenticatedRoot keyBytes valueBytes ->
                  pdelegatedChunkMembership
                    # Env.pmpfChunkedVerifyValidatorHash
                    # redeemers
                    # chunkCarriage
                    # authenticatedRoot
                    # keyBytes
                    # valueBytes
              )
              $ \_verifiedTxId header nativeTxView ->
                pif
                  ( validation
                      ownScriptHash
                      ctAssetName
                      fraudProver
                      mInputState
                      outputScriptHash
                      outputStateData
                      header
                      nativeTxId
                      nativeTxView
                  )
                  (pconstant True)
                  perror

{- | Aiken @fraud_proofs/common.pass_committed_transactions_leaf_to_next_step@.

The codec-free twin of 'ppassNativeTxToNextStep': identical computation-thread,
hub, state-queue, counted-root and MPF-membership binding, but the leaf key and
leaf value are handed to @validation@ verbatim. Only the @da-hash-preimage@
family may use it; every other family must keep the codec precondition.

It reads its indices out of a 'PNativeTxInclusionArgs' too, where @native_tx_id@
is the committed leaf's key and @native_tx_compact_cbor@ its value.
-}
ppassCommittedTransactionsLeafToNextStep ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PNativeTxInclusionArgs ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  ( Term s (PAsData PScriptHash) ->
    Term s (PAsData PTokenName) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PMaybeData PData) ->
    Term s (PAsData PScriptHash) ->
    Term s PData ->
    Term s (PAsData PHeaderV1) ->
    Term s PByteString ->
    Term s PByteString ->
    Term s PBool
  ) ->
  Term s PBool
ppassCommittedTransactionsLeafToNextStep
  computationThreadTokenPolicyId
  hubOracle
  datum
  args
  ownOutRef
  inputs
  referenceInputs
  outputs
  redeemers
  validation = P.do
    PNativeTxInclusionArgs
      { pinclusionArgs'inputIndex
      , pinclusionArgs'outputIndex
      , pinclusionArgs'hubRefInputIndex
      , pinclusionArgs'stateQueueNodeRefInputIndex
      , pinclusionArgs'nativeTxId
      , pinclusionArgs'nativeTxCompactCbor
      , pinclusionArgs'transactionsPhasRoot
      , pinclusionArgs'txMembershipProof
      , pinclusionArgs'inclusionProofScriptWithdrawRedeemerIndex
      } <-
      pmatch args
    committedTxId <- plet $ pfromData pinclusionArgs'nativeTxId
    committedLeafValue <- plet $ pfromData pinclusionArgs'nativeTxCompactCbor

    pcontinue
      computationThreadTokenPolicyId
      (pexpectStepDatum datum)
      (pfromData pinclusionArgs'inputIndex)
      (pfromData pinclusionArgs'outputIndex)
      ownOutRef
      inputs
      outputs
      $ \ownScriptHash ctAssetName fraudProver mInputState outputScriptHash outputStateData ->
        pverifyCommittedTransactionsLeafInStateQueueNode
          committedTxId
          committedLeafValue
          (pfromData pinclusionArgs'transactionsPhasRoot)
          (pforgetData pinclusionArgs'txMembershipProof)
          ctAssetName
          hubOracle
          (pfromData pinclusionArgs'hubRefInputIndex)
          (pfromData pinclusionArgs'stateQueueNodeRefInputIndex)
          (pfromData pinclusionArgs'inclusionProofScriptWithdrawRedeemerIndex)
          referenceInputs
          redeemers
          $ \header ->
            pif
              ( validation
                  ownScriptHash
                  ctAssetName
                  fraudProver
                  mInputState
                  outputScriptHash
                  outputStateData
                  header
                  committedTxId
                  committedLeafValue
              )
              (pconstant True)
              perror

--------------------------------------------------------------------------------
-- Step transitions
--------------------------------------------------------------------------------

{- | @expect Some(step_datum) = datum@.

Every @pass_*@ helper opens with this, and a first step that arrives without a
datum has nothing to advance.
-}
pexpectStepDatum ::
  forall (s :: S). Term s (PMaybeData PStepDatum) -> Term s PStepDatum
pexpectStepDatum datum = pmatch datum $ \case
  PDJust d -> pfromData d
  PDNothing -> perror

{- | Aiken @fraud_proofs/common.cancel@.

Abandons a thread. The prover gets nothing, so the only questions are whether
the thread is real and whether the person abandoning it is the person who opened
it: the computation thread policy must be running its @BurnForCancellation@
branch for this exact token, the spent input must be the authentic thread UTxO
carrying that token, and the prover named in its datum must have signed.
-}
pcancel ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PBool
pcancel
  computationThreadTokenPolicyId
  mDatum
  inputIndex
  ownOutRef
  mintRedeemerIndex
  inputs
  redeemers
  extraSignatories = P.do
    -- 1. The computation thread policy's `BurnForCancellation` redeemer must be
    --    invoked, which is what actually burns the token.
    mintRedeemerData <-
      plet $
        pgetRedeemerAt
          # redeemers
          # pdata (pcon (PMinting computationThreadTokenPolicyId))
          # mintRedeemerIndex
    burningAssetName <-
      plet $
        pmatch (punsafeCoerceData @PMintRedeemer (pto (pfromData mintRedeemerData))) $ \case
          PBurnForCancellation {pctBurnForCancellation'burningTokenAssetName} ->
            pctBurnForCancellation'burningTokenAssetName
          _ -> perror

    -- 2. The input UTxO must be the authentic thread UTxO.
    PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <-
      pmatch (pfromData (pelemAt # inputIndex # inputs))
    PTxOut {ptxOut'value} <- pmatch ptxInInfo'resolved
    PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
      pmatch (pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value)

    -- 3. The fraud prover must have signed.
    PStepDatum {pstep'fraudProver} <- pmatch (pexpectStepDatum mDatum)

    pif
      ( pand'List
          [ ptxInInfo'outRef #== ownOutRef
          , passetTriplet'policy #== computationThreadTokenPolicyId
          , passetTriplet'name #== burningAssetName
          , pfromData passetTriplet'amount #== 1
          , phasSigned # pstep'fraudProver # extraSignatories
          ]
      )
      (pconstant True)
      perror

{- | Aiken @fraud_proofs/common.continue@.

Carries a thread one step forward, and hands the family whatever it needs to
decide whether that step was earned:

  * the input's script hash — the step script being spent;
  * the thread token's asset name, which names both the fraud category and the
    block under challenge;
  * the fraud prover;
  * the input's state data, as an @Option@ so that /first/ steps, whose data is
    always @None@, can use this too;
  * the output's script hash — the next step script; and
  * the output's state data, as raw @Data@.

The last of those is why 'pfinalize' exists separately: the output state is
required to be @Some@ here, so a final step, which produces none, cannot be
built on this function.
-}
pcontinue ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PStepDatum ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  ( Term s (PAsData PScriptHash) ->
    Term s (PAsData PTokenName) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PMaybeData PData) ->
    Term s (PAsData PScriptHash) ->
    Term s PData ->
    Term s PBool
  ) ->
  Term s PBool
pcontinue
  computationThreadTokenPolicyId
  stepDatum
  inputIndex
  outputIndex
  ownOutRef
  inputs
  outputs
  validationFn =
    pvalidateInputAndProvideOutput
      computationThreadTokenPolicyId
      stepDatum
      inputIndex
      outputIndex
      ownOutRef
      inputs
      outputs
      $ \inputScriptHash ctAssetName fraudProver mInputState output -> P.do
        -- 1. The output must sit at a script address, carry the same NFT as the
        --    input and nothing else (which is what prevents double
        --    satisfaction), have an inline datum, and no reference script.
        PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
          pmatch output
        PAddress {paddress'credential} <- pmatch ptxOut'address
        outputScriptHash <-
          plet $ pmatch paddress'credential $ \case
            PScriptCredential h -> h
            PPubKeyCredential _ -> perror
        outputDatumData <-
          plet $ pmatch ptxOut'datum $ \case
            POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
            _ -> perror
        _noReferenceScript <-
          plet $ pmatch ptxOut'referenceScript $ \case
            PDNothing -> pconstant @PUnit ()
            PDJust _ -> perror
        PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
          pmatch (pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value)

        -- 2. The fraud prover must not change during the transition, and the
        --    output must carry state.
        PStepDatum {pstep'fraudProver = outputFraudProver, pstep'data = outputData} <-
          pmatch (punsafeCoerceData @PStepDatum outputDatumData)
        outputStateData <-
          plet $ pmatch outputData $ \case
            PDJust d -> pfromData d
            PDNothing -> perror

        pif
          ( pand'List
              [ passetTriplet'policy #== computationThreadTokenPolicyId
              , passetTriplet'name #== ctAssetName
              , pfromData passetTriplet'amount #== 1
              , outputFraudProver #== fraudProver
              ]
          )
          -- 3. Custom validation for each fraud proof must pass.
          ( validationFn
              inputScriptHash
              ctAssetName
              fraudProver
              mInputState
              outputScriptHash
              outputStateData
          )
          perror

{- | Aiken @fraud_proofs/common.finalize@.

Ends a thread in a conviction. The thread token is burned and a fraud proof
token of the same asset name is minted in its place, at the fraud proof spending
script — an always-fails address, because a conviction is a permanent record and
not a spendable UTxO. The produced datum carries the prover and nothing else,
which is checked by rebuilding it and comparing.

The burn itself is the fraud proof policy's business; what is checked here is
that the policy ran and that it named this thread's asset name.
-}
pfinalize ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PAddress) ->
  Term s PStepDatum ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  ( Term s (PAsData PScriptHash) ->
    Term s (PAsData PTokenName) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PMaybeData PData) ->
    Term s PBool
  ) ->
  Term s PBool
pfinalize
  computationThreadTokenPolicyId
  fraudProofTokenPolicyId
  fraudProofAddress
  stepDatum
  inputIndex
  outputIndex
  fraudProofMintRedeemerIndex
  ownOutRef
  inputs
  outputs
  redeemers
  validationFn =
    pvalidateInputAndProvideOutput
      computationThreadTokenPolicyId
      stepDatum
      inputIndex
      outputIndex
      ownOutRef
      inputs
      outputs
      $ \inputScriptHash ctAssetName fraudProver mInputState output -> P.do
        -- 1. The output must have an inline datum and no reference script.
        PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
          pmatch output
        outputDatumData <-
          plet $ pmatch ptxOut'datum $ \case
            POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
            _ -> perror
        _noReferenceScript <-
          plet $ pmatch ptxOut'referenceScript $ \case
            PDNothing -> pconstant @PUnit ()
            PDJust _ -> perror
        PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
          pmatch (pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value)

        -- 5, 6. The fraud proof policy must have run, naming this thread.
        fraudProofRedeemerAssetName <-
          plet $
            pmatch
              ( punsafeCoerceData @FraudProof.PMintRedeemer
                  ( pto
                      ( pfromData
                          ( pgetRedeemerAt
                              # redeemers
                              # pdata (pcon (PMinting fraudProofTokenPolicyId))
                              # fraudProofMintRedeemerIndex
                          )
                      )
                  )
              )
              $ \FraudProof.PMintRedeemer {FraudProof.pfpMint'computationThreadTokenAssetName} ->
                pfpMint'computationThreadTokenAssetName

        -- 4. The prover is preserved, with no additional data.
        expectedOutputDatumData <-
          plet $
            pforgetData
              ( pdata
                  ( pcon
                      ( FraudProof.PFraudProofDatum
                          {FraudProof.pfraudProof'fraudProver = punsafeCoerce fraudProver}
                      )
                  )
              )

        pif
          ( pand'List
              [ -- 2. Convictions are parked at the always-fails script.
                ptxOut'address #== pfromData fraudProofAddress
              , -- 3. Exactly one fraud proof NFT, same asset name as the thread's.
                passetTriplet'policy #== fraudProofTokenPolicyId
              , passetTriplet'name #== ctAssetName
              , pfromData passetTriplet'amount #== 1
              , outputDatumData #== expectedOutputDatumData
              , fraudProofRedeemerAssetName #== ctAssetName
              ]
          )
          -- 7. Custom validation for each fraud proof must pass.
          (validationFn inputScriptHash ctAssetName fraudProver mInputState)
          perror

--------------------------------------------------------------------------------
-- Payout
--------------------------------------------------------------------------------

{- | Aiken @fraud_proofs/common.validate_output_to_fraud_prover@.

Whether an output pays a key-hash address belonging to the named prover. A
script address is rejected outright rather than erroring — this is the one
predicate here whose Aiken original returns @False@ on the miss.
-}
pvalidateOutputToFraudProver ::
  forall (s :: S). Term s (PTxOut :--> PByteString :--> PBool)
pvalidateOutputToFraudProver = phoistAcyclic $
  plam $ \output proverHash ->
    pmatch output $ \PTxOut {ptxOut'address} ->
      pmatch ptxOut'address $ \PAddress {paddress'credential} ->
        pmatch paddress'credential $ \case
          PPubKeyCredential h -> pto (pfromData h) #== proverHash
          PScriptCredential _ -> pconstant False

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

{- | Aiken @fraud_proofs/common.validate_input_and_provide_output@ (private).

Validates that the input at @input_index@ is the thread UTxO this script is
being run for — a script address carrying exactly one computation thread token
and no other tokens — and hands the caller the output at @output_index@ with no
validation performed on it.

Not exported, for the reason the Aiken original gives: double satisfaction is
prevented by checking that the /output/ carries the same token, which happens in
'pcontinue' and 'pfinalize' rather than here. Handing this out on its own would
hand out half a check.
-}
pvalidateInputAndProvideOutput ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PStepDatum ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  ( Term s (PAsData PScriptHash) ->
    Term s (PAsData PTokenName) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PMaybeData PData) ->
    Term s PTxOut ->
    Term s PBool
  ) ->
  Term s PBool
pvalidateInputAndProvideOutput
  computationThreadTokenPolicyId
  stepDatum
  inputIndex
  outputIndex
  ownOutRef
  inputs
  outputs
  validationFn =
    poneToOne inputIndex outputIndex ownOutRef inputs outputs (pconstant True) $
      \input output -> P.do
        PTxInInfo {ptxInInfo'resolved} <- pmatch (pfromData input)
        PTxOut {ptxOut'address = inputAddress, ptxOut'value = inputValue} <-
          pmatch ptxInInfo'resolved
        PAddress {paddress'credential} <- pmatch inputAddress
        inputScriptHash <-
          plet $ pmatch paddress'credential $ \case
            PScriptCredential h -> h
            PPubKeyCredential _ -> perror
        PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
          pmatch (pgetSingleAssetFromValueApartFromAda # pfromData inputValue)
        PStepDatum {pstep'fraudProver, pstep'data} <- pmatch stepDatum
        pif
          ( passetTriplet'policy
              #== computationThreadTokenPolicyId
              #&& pfromData passetTriplet'amount
              #== 1
          )
          ( validationFn
              inputScriptHash
              passetTriplet'name
              pstep'fraudProver
              pstep'data
              output
          )
          perror

-- | @punsafeCoerce@ into a data-encoded Plutarch type, as the rest of the port does.
punsafeCoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)
