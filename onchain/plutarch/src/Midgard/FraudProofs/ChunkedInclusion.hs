{- |
Module      : Midgard.FraudProofs.ChunkedInclusion
Description : Plutarch port of
              @lib/midgard/fraud-proofs/chunked-inclusion-v1.ak@.

Published-chunk carriage of a fault-proof step's MPF opening: the one place
where "Midgard.MpfChunkedProof"'s publication mechanism meets a fault-proof step.

=== Why this exists

The four foundational proof families — double-spend, no-input, invalid-range,
zero-input — open the challenged block's @transactions_root@ by carrying the MPF
proof inside the step transaction itself. That route pays about 276
complete-signed-transaction bytes per forced branch level, so the preserved
16,384-byte envelope is exhausted at branch level 21–23, and an adversary
grinding sibling keys at 2^4 work per level can censor the honest prover for as
little as 2^84 work.

=== What a step gains, and what it does not

The step's marginal cost per proof level falls to one small integer and one
reference input. /Nothing else about a step changes:/ the root is still the one
the challenged header commits, the key is still the step's own canonical
encoding, and the terminal is still the exact membership or canonical-absence
witness. A chunk whose steps do not reconstruct that root simply fails here,
which is why publication needs no validator.

=== Why the direct route stays

Small proofs — every non-adversarial block — are cheaper carried in the redeemer
than published: the direct route needs one transaction where this one needs two.
Both end in the same verified opening against the same authenticated root, so
which one a prover takes is the prover's business and neither weakens the other.

=== Two shapes of the same check

The @delegated_*@ pair hands the walk to the merkelized @mpf_chunked_verify@
withdraw validator and only requires that it was invoked, exactly once, on
exactly this claim. The @published_chunk_*@ pair does the walk here. The
division of labour in the first is the same one
@Midgard.Common.Utils.pplutarchPhasRaw@ already uses for the redeemer-carried
route: the step contributes the binding, the verifier contributes the walk, and
nothing in the claim is trusted by the verifier because the step is what refuses
a claim that is not its own.
-}
module Midgard.FraudProofs.ChunkedInclusion (
  -- * Types
  PPublishedProofCarriage (..),
  PProofTerminal (..),
  PChunkedProofClaim (..),
  pabsentValueHash,

  -- * Delegated verification
  pdelegatedChunkMembership,
  pdelegatedChunkNonMembership,

  -- * Direct verification
  ppublishedChunkMembership,
  ppublishedChunkMembershipByDigest,
  ppublishedChunkNonMembership,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (PRedeemer, PScriptPurpose, PTxInInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PH32)
import Midgard.Common.Utils (pgetUniqueWithdrawRedeemer)
import Midgard.MpfChunkedProof (
  pchunkIndicesAreWellFormed,
  pconcatenatePublishedSteps,
  pmaximumTotalProofStepCount,
 )
import Midgard.MpfProof (pdoesNotHave, phasValueHash, pproofHasAtMostSteps)
import Midgard.MpfProof.Types (PProof)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Aiken @chunked_inclusion_v1.PublishedProofCarriage@.

Names the publication UTxOs carrying one opening, and their proof order, as
indices into @tx.reference_inputs@.

This is @mpf_chunked_proof_v1.FinalizeProofRedeemer@ in a step redeemer's
clothing — the same single field, so a carriage's wire encoding is the one the
challenge thread already uses.
-}
data PPublishedProofCarriage (s :: S) = PPublishedProofCarriage
  { pcarriage'orderedChunkReferenceInputIndices ::
      Term s (PAsData (PBuiltinList (PAsData PInteger)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPublishedProofCarriage)

-- | Aiken @chunked_inclusion_v1.ProofTerminal@ — which terminal a claim demands.
data PProofTerminal (s :: S)
  = PMembership
  | PNonMembership
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProofTerminal)

{- | Aiken @chunked_inclusion_v1.ChunkedProofClaim@.

The redeemer of the merkelized @mpf_chunked_verify@ withdraw validator, and the
exact value a delegating step requires to be present in it.

Nothing here is trusted by the verifier: the root, key and terminal are the
step's own authenticated evidence, and the step is what refuses a claim that is
not its own.
-}
data PChunkedProofClaim (s :: S) = PChunkedProofClaim
  { pclaim'mode :: Term s (PAsData PProofTerminal)
  , pclaim'merkleRoot :: Term s (PAsData PH32)
  , pclaim'keyBytes :: Term s (PAsData PByteString)
  , pclaim'valueHash :: Term s (PAsData PH32)
  , pclaim'orderedChunkReferenceInputIndices ::
      Term s (PAsData (PBuiltinList (PAsData PInteger)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PChunkedProofClaim)

{- | Aiken @chunked_inclusion_v1.absent_value_hash@.

The 32-byte digest a non-membership claim carries in the @value_hash@ slot,
which the terminal does not use. Fixed so the claim has exactly one encoding —
otherwise a delegating step and its verifier could agree on the walk and
disagree on the claim.
-}
pabsentValueHash :: forall (s :: S). Term s PByteString
pabsentValueHash = pconstant (mconcat (replicate 32 "\x00"))

--------------------------------------------------------------------------------
-- Delegated verification
--------------------------------------------------------------------------------

{- | Aiken @chunked_inclusion_v1.delegated_chunk_membership@.

The step requires the merkelized verifier to have been invoked in this
transaction, exactly once, on exactly this claim. The equality is on the whole
claim, so a verifier running on a /different/ root, key, digest or chunk order
does not satisfy the step.
-}
pdelegatedChunkMembership ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))
        :--> PPublishedProofCarriage
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
pdelegatedChunkMembership = phoistAcyclic $
  plam $ \verifierScriptHash redeemers carriage merkleRoot keyBytes valueBytes ->
    pclaimIs
      verifierScriptHash
      redeemers
      carriage
      (pcon PMembership)
      merkleRoot
      keyBytes
      (pblake2b_256 # valueBytes)

{- | Aiken @chunked_inclusion_v1.delegated_chunk_non_membership@.

The @value_hash@ slot is the fixed 'pabsentValueHash', because an absence claim
has no value.
-}
pdelegatedChunkNonMembership ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))
        :--> PPublishedProofCarriage
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
pdelegatedChunkNonMembership = phoistAcyclic $
  plam $ \verifierScriptHash redeemers carriage merkleRoot keyBytes ->
    pclaimIs
      verifierScriptHash
      redeemers
      carriage
      (pcon PNonMembership)
      merkleRoot
      keyBytes
      pabsentValueHash

{- | The shape both delegated checks share: read the one withdraw redeemer for
the verifier script, and require it to equal the claim this step means.
-}
pclaimIs ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PPublishedProofCarriage ->
  Term s PProofTerminal ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PBool
pclaimIs verifierScriptHash redeemers carriage mode merkleRoot keyBytes valueHash = P.do
  PPublishedProofCarriage {pcarriage'orderedChunkReferenceInputIndices} <- pmatch carriage
  claim <-
    plet $
      pfromData $
        punsafeCoerce @(PAsData PChunkedProofClaim) $
          pto (pfromData (pgetUniqueWithdrawRedeemer # redeemers # verifierScriptHash))
  claim
    #== pcon
      ( PChunkedProofClaim
          { pclaim'mode = pdata mode
          , pclaim'merkleRoot = pdata merkleRoot
          , pclaim'keyBytes = pdata keyBytes
          , pclaim'valueHash = pdata valueHash
          , pclaim'orderedChunkReferenceInputIndices =
              pcarriage'orderedChunkReferenceInputIndices
          }
      )

--------------------------------------------------------------------------------
-- Direct verification
--------------------------------------------------------------------------------

{- | Aiken @chunked_inclusion_v1.published_proof@.

Reassembles the published proof, or fails closed.

Every bound the publication mechanism names is applied here and nowhere else:
bounded, in-range, duplicate-free indices; a strictly decoded, well-formed chunk
datum per index; and the protocol-wide 64-step ceiling on the concatenation.
-}
ppublishedProof ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PPublishedProofCarriage
        :--> PMaybe PProof
    )
ppublishedProof = phoistAcyclic $
  plam $ \referenceInputs carriage -> P.do
    PPublishedProofCarriage {pcarriage'orderedChunkReferenceInputIndices} <- pmatch carriage
    indices <- plet (pfromData pcarriage'orderedChunkReferenceInputIndices)
    pif
      (pchunkIndicesAreWellFormed # indices # (plength # referenceInputs))
      ( pmatch (pconcatenatePublishedSteps # referenceInputs # indices) $ \case
          PNothing -> pcon PNothing
          PJust proof ->
            pif
              (pproofHasAtMostSteps # proof # pmaximumTotalProofStepCount)
              (pcon (PJust proof))
              (pcon PNothing)
      )
      (pcon PNothing)

{- | Aiken @chunked_inclusion_v1.published_chunk_membership@.

The value's preimage is already in the step transaction — a step cannot act on a
transaction it has not decoded — so it is hashed here and the walk runs against
the digest.
-}
ppublishedChunkMembership ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PPublishedProofCarriage
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
ppublishedChunkMembership = phoistAcyclic $
  plam $ \referenceInputs carriage merkleRoot keyBytes valueBytes ->
    ppublishedChunkMembershipByDigest
      # referenceInputs
      # carriage
      # merkleRoot
      # keyBytes
      # (pblake2b_256 # valueBytes)

{- | Aiken @chunked_inclusion_v1.published_chunk_membership_by_digest@.

The same check taking the value's digest, which is what a delegated claim
carries: the merkelized verifier never sees the preimage.
-}
ppublishedChunkMembershipByDigest ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PPublishedProofCarriage
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
ppublishedChunkMembershipByDigest = phoistAcyclic $
  plam $ \referenceInputs carriage merkleRoot keyBytes valueHash ->
    pmatch (ppublishedProof # referenceInputs # carriage) $ \case
      PNothing -> pconstant False
      PJust proof -> phasValueHash # merkleRoot # keyBytes # valueHash # proof

{- | Aiken @chunked_inclusion_v1.published_chunk_non_membership@.

Absence under @merkle_root@, proved by the published chunks the carriage names.
The canonical absence terminal is required, exactly as on the direct route.
-}
ppublishedChunkNonMembership ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PPublishedProofCarriage
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
ppublishedChunkNonMembership = phoistAcyclic $
  plam $ \referenceInputs carriage merkleRoot keyBytes ->
    pmatch (ppublishedProof # referenceInputs # carriage) $ \case
      PNothing -> pconstant False
      PJust proof -> pdoesNotHave # merkleRoot # keyBytes # proof
