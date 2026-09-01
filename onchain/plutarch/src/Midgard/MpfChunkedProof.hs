{- |
Module      : Midgard.MpfChunkedProof
Description : Plutarch port of @lib/midgard/mpf-chunked-proof-v1.ak@.

Published proof chunks with atomic final verification.

=== Why this exists

A fault-proof step that carries its MPF membership proof inside the final
transaction's redeemer pays roughly 276 signed-transaction bytes per proof
level. Against the preserved 16,384-byte envelope that exhausts around branch
level 21–23, so an adversary grinding sibling keys at 2^4 work per level can
push a proof past the envelope for as little as 2^84 work — the honest prover is
censored.

This module takes the proof out of the final transaction. A large proof is
/published/ across bounded UTxOs whose inline datum carries nothing but a list
of steps; the finalizing transaction references them, names their order as
indices into @tx.reference_inputs@, concatenates and verifies atomically. Its
marginal cost per proof level falls to one small integer and one reference
input.

=== Why publication needs no validator

A chunk UTxO is inert data at an arbitrary address. It holds no protocol token,
authorises nothing and is never mutated. The finalizing validator trusts a
chunk's /content/ only, never its provenance: a chunk whose steps do not
reconstruct the challenged root simply fails. With no mutable intermediate proof
state there is nothing to poison between publication and finalization — the
whole proof is checked in one transaction or not at all. Publishing and
finalizing are therefore both permissionless, while the reward stays bound to
the challenge's @proof_owner@.

=== One thing to read carefully

An /empty/ chunk order is allowed. The zero-step proof is the legitimate shape
for a one-leaf trie, and like every other shape it still has to reconstruct the
challenged root. So 'pchunkIndicesAreWellFormed' bounds the count from above
only.
-}
module Midgard.MpfChunkedProof (
  -- * Bounds
  pdigestByteCount,
  pheaderHashByteCount,
  pmaximumChunkProofStepCount,
  pmaximumChunkDatumByteCount,
  pmaximumTotalProofStepCount,
  pmaximumChunkCount,

  -- * Types
  PProofMode (..),
  PProofChallengeDatum (..),
  PProofChunkDatum (..),
  PFinalizeProofRedeemer (..),
  PChallengeMintRedeemer (..),

  -- * Header binding
  pheaderCountedCommitment,
  pchallengeMatchesHeader,

  -- * Named invariants
  pchallengeDatumIsWellFormed,
  pchunkDatumIsWellFormed,
  pchunkIndicesAreWellFormed,

  -- * Reassembly and verification
  pchunkDatumAt,
  pconcatenatePublishedSteps,
  pproofReachesChallengedTerminal,
  pverifyPublishedProof,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Data (pserialiseData)
import Plutarch.LedgerApi.V3 (
  PDatum (..),
  POutputDatum (..),
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PH28, PH32)
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.MpfProof (
  pdoesNotHave,
  phasValueHash,
  pmaximumProofStepCount,
  pproofHasAtMostSteps,
 )
import Midgard.MpfProof.Types (PProof (..), PProofStep)
import Midgard.TransitionTrace (
  PRootCountProof (..),
  PRootDomain (..),
  pverifyRootCountProof,
 )

--------------------------------------------------------------------------------
-- Bounds
--------------------------------------------------------------------------------

-- | Aiken @mpf_chunked_proof_v1.digest_byte_count@ — every Blake2b-256 field.
pdigestByteCount :: forall (s :: S). Term s PInteger
pdigestByteCount = 32

{- | Aiken @mpf_chunked_proof_v1.header_hash_byte_count@ — @28@.

The specification called for 32 bytes on every hash field. The header hash is
the one field whose width is fixed by the existing state queue, so it is bound
to the real 28-byte width rather than to a value no honest prover could supply.
-}
pheaderHashByteCount :: forall (s :: S). Term s PInteger
pheaderHashByteCount = 28

-- | Aiken @mpf_chunked_proof_v1.maximum_chunk_proof_step_count@ — @16@.
pmaximumChunkProofStepCount :: forall (s :: S). Term s PInteger
pmaximumChunkProofStepCount = 16

{- | Aiken @mpf_chunked_proof_v1.maximum_chunk_datum_byte_count@ — @2304@.

Derived, not chosen: a @Branch@ step is the largest shape at 139–140 bytes of
Plutus data, so sixteen of them plus the wrapper is 2,246 bytes, rounded up.
That is about 14% of the usable inline-datum payload, so one publication
transaction can carry six full chunks and still fit.
-}
pmaximumChunkDatumByteCount :: forall (s :: S). Term s PInteger
pmaximumChunkDatumByteCount = 2304

{- | Aiken @mpf_chunked_proof_v1.maximum_total_proof_step_count@.

The protocol-wide 64, shared with 'Midgard.MpfProof.pmaximumProofStepCount' —
an MPF path is 64 nibbles, so no well-formed proof can be deeper.
-}
pmaximumTotalProofStepCount :: forall (s :: S). Term s PInteger
pmaximumTotalProofStepCount = pmaximumProofStepCount

{- | Aiken @mpf_chunked_proof_v1.maximum_chunk_count@ — @8@.

Sixty-four steps need at most four chunks; eight keeps the finalizing
transaction's reference-input list small while tolerating partially packed ones.
-}
pmaximumChunkCount :: forall (s :: S). Term s PInteger
pmaximumChunkCount = 8

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Aiken @mpf_chunked_proof_v1.ProofMode@.
data PProofMode (s :: S)
  = PMembership
  | PNonMembership
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProofMode)

{- | Aiken @mpf_chunked_proof_v1.ProofChallengeDatum@.

The challenge thread's state, authenticated once at initialization against the
challenged block header and never mutated afterwards.

@challenged_root_domain@ is carried in the /datum/ and not only in the
initialization redeemer, deliberately: a redeemer is not retained by the ledger,
so a reader of a finalized challenge could not otherwise recover which of the
header's seven roots was challenged. It is not advisory —
'pchallengeMatchesHeader' requires it to be the domain the counted-root
commitment was reproduced under.
-}
data PProofChallengeDatum (s :: S) = PProofChallengeDatum
  { pchallenge'proofOwner :: Term s (PAsData PH28)
  , pchallenge'challengedHeaderHash :: Term s (PAsData PH28)
  , pchallenge'challengedRootDomain :: Term s (PAsData PRootDomain)
  , pchallenge'targetKey :: Term s (PAsData PH32)
  , pchallenge'targetValueHash :: Term s (PAsData PH32)
  , pchallenge'expectedRoot :: Term s (PAsData PH32)
  , pchallenge'expectedLeafCount :: Term s (PAsData PInteger)
  , pchallenge'mode :: Term s (PAsData PProofMode)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProofChallengeDatum)

{- | Aiken @mpf_chunked_proof_v1.ProofChunkDatum@.

Nothing but steps: no owner, no ordering claim, no link to a challenge. Order
comes from the finalizing redeemer, which is what lets publication be
permissionless.
-}
data PProofChunkDatum (s :: S) = PProofChunkDatum
  { pchunk'proofSteps :: Term s (PAsData (PBuiltinList (PAsData PProofStep)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProofChunkDatum)

-- | Aiken @mpf_chunked_proof_v1.FinalizeProofRedeemer@.
data PFinalizeProofRedeemer (s :: S) = PFinalizeProofRedeemer
  { pfinalize'orderedChunkReferenceInputIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFinalizeProofRedeemer)

{- | Aiken @mpf_chunked_proof_v1.ChallengeMintRedeemer@.

@BurnChallenge@ is only reachable while the thread's own UTxO is being spent, so
the spending handler carries the real conditions.
-}
data PChallengeMintRedeemer (s :: S)
  = PInitChallenge
      { pinit'nonceInputIndex :: Term s (PAsData PInteger)
      , pinit'challengeOutputIndex :: Term s (PAsData PInteger)
      , pinit'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pinit'challengedBlockRefInputIndex :: Term s (PAsData PInteger)
      , pinit'challengedRootDomain :: Term s (PAsData PRootDomain)
      }
  | PBurnChallenge
      { pburn'challengeInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PChallengeMintRedeemer)

--------------------------------------------------------------------------------
-- Header binding
--------------------------------------------------------------------------------

{- | Aiken @mpf_chunked_proof_v1.header_counted_commitment@.

The @(counted root, leaf count)@ pair a challenged header commits for one root
domain. @commit_counted_root@ already binds a domain's raw MPF root to its leaf
count, so a challenge's @expected_root@ / @expected_leaf_count@ pair is
authenticated against the header by reproducing that commitment.
-}
pheaderCountedCommitment ::
  forall (s :: S).
  Term s (PHeaderV1 :--> PRootDomain :--> PPair PByteString PInteger)
pheaderCountedCommitment = phoistAcyclic $
  plam $ \header domain -> P.do
    PHeaderV1
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
      , pheader'transitionStepCount
      , pheader'totalEventCount
      , pheader'validationTraceCount
      } <-
      pmatch header
    let commitment root count = pcon (PPair (pfromData root) (pfromData count))
    pmatch domain $ \case
      PWithdrawalsRootDomain -> commitment pheader'withdrawalsRoot pheader'withdrawalCount
      PForcedTransactionsV1RootDomain ->
        commitment pheader'forcedTransactionsRoot pheader'forcedTransactionCount
      PTransactionsV1RootDomain -> commitment pheader'transactionsRoot pheader'l2TransactionCount
      PDepositsRootDomain -> commitment pheader'depositsRoot pheader'depositCount
      PTransitionTraceRootDomain ->
        commitment pheader'transitionTraceRoot pheader'transitionStepCount
      PEventToStepRootDomain -> commitment pheader'eventToStepRoot pheader'totalEventCount
      PValidationTracesRootDomain ->
        commitment pheader'validationTracesRoot pheader'validationTraceCount

{- | Aiken @mpf_chunked_proof_v1.challenge_matches_header@.

The challenge's target commitment is exactly what the challenged header commits
for the named domain, and the datum names that same domain.
-}
pchallengeMatchesHeader ::
  forall (s :: S).
  Term
    s
    ( PProofChallengeDatum
        :--> PHeaderV1
        :--> PByteString
        :--> PAsData PRootDomain
        :--> PBool
    )
pchallengeMatchesHeader = phoistAcyclic $
  plam $ \challenge header headerHash domain -> P.do
    PProofChallengeDatum
      { pchallenge'challengedHeaderHash
      , pchallenge'challengedRootDomain
      , pchallenge'expectedRoot
      , pchallenge'expectedLeafCount
      } <-
      pmatch challenge
    PPair countedRoot leafCount <-
      pmatch (pheaderCountedCommitment # header #$ pfromData domain)
    pfromData pchallenge'challengedHeaderHash
      #== headerHash
      -- The datum's own record of which root was challenged is bound to the
      -- domain this commitment was reproduced under, so it survives the
      -- redeemer.
      #&& pchallenge'challengedRootDomain
      #== domain
      #&& pverifyRootCountProof
        ( pcon
            ( PRootCountProof
                { prootCount'domain = domain
                , prootCount'root = pdata countedRoot
                , prootCount'phasRoot = pchallenge'expectedRoot
                , prootCount'count = pdata leafCount
                }
            )
        )
        domain
        countedRoot
        leafCount

--------------------------------------------------------------------------------
-- Named invariants
--------------------------------------------------------------------------------

{- | Aiken @mpf_chunked_proof_v1.challenge_datum_is_well_formed@.

Every hash field at its exact protocol width, and a membership challenge naming
a non-empty trie — nothing is a member of an empty trie.
-}
pchallengeDatumIsWellFormed ::
  forall (s :: S). Term s (PProofChallengeDatum :--> PBool)
pchallengeDatumIsWellFormed = phoistAcyclic $
  plam $ \challenge -> P.do
    PProofChallengeDatum
      { pchallenge'proofOwner
      , pchallenge'challengedHeaderHash
      , pchallenge'targetKey
      , pchallenge'targetValueHash
      , pchallenge'expectedRoot
      , pchallenge'expectedLeafCount
      , pchallenge'mode
      } <-
      pmatch challenge
    leafCount <- plet (pfromData pchallenge'expectedLeafCount)
    plengthBS
      # pfromData pchallenge'proofOwner
      #== pheaderHashByteCount
      #&& plengthBS
      # pfromData pchallenge'challengedHeaderHash
      #== pheaderHashByteCount
      #&& plengthBS
      # pfromData pchallenge'targetKey
      #== pdigestByteCount
      #&& plengthBS
      # pfromData pchallenge'targetValueHash
      #== pdigestByteCount
      #&& plengthBS
      # pfromData pchallenge'expectedRoot
      #== pdigestByteCount
      #&& 0
      #<= leafCount
      #&& pmatch
        (pfromData pchallenge'mode)
        ( \case
            PMembership -> 0 #< leafCount
            PNonMembership -> pconstant True
        )

{- | Aiken @mpf_chunked_proof_v1.chunk_datum_is_well_formed@.

At least one step, no more than the per-chunk step bound, and within the
per-chunk byte bound. The byte bound is measured on the datum's own
serialisation, so a chunk cannot smuggle size in through step shapes the count
does not see.
-}
pchunkDatumIsWellFormed :: forall (s :: S). Term s (PProofChunkDatum :--> PBool)
pchunkDatumIsWellFormed = phoistAcyclic $
  plam $ \chunk -> P.do
    PProofChunkDatum {pchunk'proofSteps} <- pmatch chunk
    stepCount <- plet (plength # pfromData pchunk'proofSteps)
    1
      #<= stepCount
      #&& stepCount
      #<= pmaximumChunkProofStepCount
      #&& plengthBS
      # (pserialiseData #$ pforgetData (pdata chunk))
      #<= pmaximumChunkDatumByteCount

-- | Aiken @mpf_chunked_proof_v1.indices_are_distinct@.
pindicesAreDistinct ::
  forall (s :: S). Term s (PBuiltinList (PAsData PInteger) :--> PBool)
pindicesAreDistinct = phoistAcyclic $
  pfix $ \self -> plam $ \indices ->
    pelimList
      (\index rest -> pnot # (pelem # index # rest) #&& self # rest)
      (pconstant True)
      indices

{- | Aiken @mpf_chunked_proof_v1.chunk_indices_are_well_formed@.

Bounded, in range and duplicate-free. Duplicates are rejected by name rather
than left to fail the proof walk.

/An empty order is allowed./ The zero-step proof is the legitimate shape for a
one-leaf trie, and like every other shape it still has to reconstruct the
challenged root.
-}
pchunkIndicesAreWellFormed ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PInteger) :--> PInteger :--> PBool)
pchunkIndicesAreWellFormed = phoistAcyclic $
  plam $ \indices referenceInputCount ->
    plength
      # indices
      #<= pmaximumChunkCount
      #&& pallInRange
      # indices
      # referenceInputCount
      #&& pindicesAreDistinct
      # indices

-- | @list.all(indices, fn(i) { i >= 0 && i < reference_input_count })@.
pallInRange ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PInteger) :--> PInteger :--> PBool)
pallInRange = phoistAcyclic $
  pfix $ \self -> plam $ \indices referenceInputCount ->
    pelimList
      ( \index rest ->
          plet (pfromData index) $ \i ->
            0 #<= i #&& i #< referenceInputCount #&& self # rest # referenceInputCount
      )
      (pconstant True)
      indices

--------------------------------------------------------------------------------
-- Reassembly and verification
--------------------------------------------------------------------------------

{- | Aiken @mpf_chunked_proof_v1.chunk_datum_at@.

Fails closed on an out-of-range index and on any datum that is not inline.

A datum that /is/ inline but is not a @ProofChunkDatum@ aborts the script rather
than returning @Nothing@ — Aiken's strict @expect@ does the same, and the module
header there calls the two the same rejection. The port reaches the abort by a
different route: the coerced value is read through @pmatch@, whose @unConstrData@
errors on anything that is not a constructor.
-}
pchunkDatumAt ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PMaybe PProofChunkDatum)
pchunkDatumAt = phoistAcyclic $
  plam $ \referenceInputs index ->
    pmatch (pelemAtMaybe # index # referenceInputs) $ \case
      PNothing -> pcon PNothing
      PJust referenceInput -> P.do
        PTxInInfo {ptxInInfo'resolved} <- pmatch (pfromData referenceInput)
        PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
        pmatch ptxOut'datum $ \case
          POutputDatum datum -> P.do
            PDatum chunkData <- pmatch datum
            chunk <- plet (pfromData (punsafeCoerce @(PAsData PProofChunkDatum) (pdata chunkData)))
            pif (pchunkDatumIsWellFormed # chunk) (pcon (PJust chunk)) (pcon PNothing)
          _ -> pcon PNothing

{- | Aiken's @list.at@ — the element at an index, or @None@ past the end.

Written here because a negative index must also give @None@ rather than walking
off the front.
-}
pelemAtMaybe ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PInteger :--> PBuiltinList a :--> PMaybe a)
pelemAtMaybe = phoistAcyclic $
  pfix $ \self -> plam $ \index items ->
    pif (index #< 0) (pcon PNothing) $
      pelimList
        (\item rest -> pif (index #== 0) (pcon (PJust item)) (self # (index - 1) # rest))
        (pcon PNothing)
        items

{- | Aiken @mpf_chunked_proof_v1.concatenate_published_steps@.

Selects the named publication UTxOs and concatenates their steps in the order
the redeemer gave. Any missing or malformed chunk collapses the whole result to
@Nothing@ rather than yielding a shorter proof.
-}
pconcatenatePublishedSteps ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PBuiltinList (PAsData PInteger)
        :--> PMaybe PProof
    )
pconcatenatePublishedSteps = phoistAcyclic $
  plam $ \referenceInputs indices ->
    pmatch (pconcatSteps # referenceInputs # indices) $ \case
      PNothing -> pcon PNothing
      PJust steps -> pcon (PJust (pcon (PProof steps)))

-- | The step run behind 'pconcatenatePublishedSteps'.
pconcatSteps ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PBuiltinList (PAsData PInteger)
        :--> PMaybe (PBuiltinList (PAsData PProofStep))
    )
pconcatSteps = phoistAcyclic $
  pfix $ \self -> plam $ \referenceInputs indices ->
    pelimList
      ( \index rest ->
          pmatch (pchunkDatumAt # referenceInputs # pfromData index) $ \case
            PNothing -> pcon PNothing
            PJust chunk -> P.do
              PProofChunkDatum {pchunk'proofSteps} <- pmatch chunk
              pmatch (self # referenceInputs # rest) $ \case
                PNothing -> pcon PNothing
                PJust remaining ->
                  pcon (PJust (pconcatList # pfromData pchunk'proofSteps # remaining))
      )
      (pcon (PJust pnil))
      indices

-- | Aiken @list.concat@.
pconcatList ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PBuiltinList a :--> PBuiltinList a :--> PBuiltinList a)
pconcatList = phoistAcyclic $
  pfix $ \self -> plam $ \left right ->
    pelimList (\item rest -> pcons # item # (self # rest # right)) right left

{- | Aiken @mpf_chunked_proof_v1.proof_reaches_challenged_terminal@.

Both arms are total and fail-closed: they reject unless every step is consumed
and the exact terminal is reached — the target key at the challenged value
digest, or the canonical absence witness. A missing, reordered, substituted,
duplicated or trailing step changes the reconstructed root and is rejected.
-}
pproofReachesChallengedTerminal ::
  forall (s :: S). Term s (PProofChallengeDatum :--> PProof :--> PBool)
pproofReachesChallengedTerminal = phoistAcyclic $
  plam $ \challenge proof -> P.do
    PProofChallengeDatum
      {pchallenge'targetKey, pchallenge'targetValueHash, pchallenge'expectedRoot, pchallenge'mode} <-
      pmatch challenge
    pmatch (pfromData pchallenge'mode) $ \case
      PMembership ->
        phasValueHash
          # pfromData pchallenge'expectedRoot
          # pfromData pchallenge'targetKey
          # pfromData pchallenge'targetValueHash
          # proof
      PNonMembership ->
        pdoesNotHave
          # pfromData pchallenge'expectedRoot
          # pfromData pchallenge'targetKey
          # proof

-- | Aiken @mpf_chunked_proof_v1.verify_published_proof@ — every named invariant, in order.
pverifyPublishedProof ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PProofChallengeDatum
        :--> PFinalizeProofRedeemer
        :--> PBool
    )
pverifyPublishedProof = phoistAcyclic $
  plam $ \referenceInputs challenge redeemer -> P.do
    PFinalizeProofRedeemer {pfinalize'orderedChunkReferenceInputIndices} <- pmatch redeemer
    indices <- plet (pfromData pfinalize'orderedChunkReferenceInputIndices)
    pchallengeDatumIsWellFormed
      # challenge
      #&& pchunkIndicesAreWellFormed
      # indices
      # (plength # referenceInputs)
      #&& pmatch
        (pconcatenatePublishedSteps # referenceInputs # indices)
        ( \case
            PNothing -> pconstant False
            PJust proof ->
              pproofHasAtMostSteps
                # proof
                # pmaximumTotalProofStepCount
                #&& pproofReachesChallengedTerminal
                # challenge
                # proof
        )
