{-# LANGUAGE OverloadedStrings #-}

{- | Plutarch port of Aiken @lib/midgard/validation-claim-v1.ak@.

A /committed claim/ is the bridge between a block header and a validation-machine
run. The machine's own fraud proofs argue about one step of a run; before any of
them can be opened, somebody has to establish that the run being argued about is
the one the block actually committed to, applied to the transaction the block
actually committed to, in the transition step the block actually committed to.
That is this module's whole job, and it does it without running the machine.

=== Three roots, and why all three

A claim opens four block-committed trees and cross-ties them:

* @validation_traces_root@ yields the __descriptor__ — the trace's root, step
  count, endpoints and verdict — keyed by the event.
* @transition_trace_root@ yields the __step__ — where in the block's ledger
  evolution the event was applied, with the ledger root on either side.
* @event_to_step_root@ yields the __location__ — the step index and phase the
  event claims to occupy.
* @forced_transactions_root@ or @transactions_root@ yields the __source__ — the
  compact triple the transaction is reconstructed from.

Opening any one of them proves an isolated fact. The claim is the conjunction:
the step the event says it is at must be the step whose key is that event, whose
phase agrees with the event's own class, and whose pre-root is what the machine
started from. Drop any tie and a prover can pair a genuine trace with somebody
else's transition.

=== Structure, source and endpoints are three functions, not one

Aiken splits the check across
'pcommittedClaimStructureIsValid', 'pcommittedClaimSourceIsAuthenticated' and
'pcommittedClaimEndpointsAndSourceAreValid', and the split is load-bearing rather
than cosmetic. The structural half authenticates the roots and the descriptor's
endpoints /without/ asserting the endpoints are normatively correct, so a block
that commits a malformed initial or terminal state is itself convictable — by the
source step — instead of making every honest challenge against that block
impossible to open. A single fused predicate would make the malformed-endpoint
case unprovable in both directions.

=== The validation context

'pvalidationContextIsExact' departs from Aiken's shape, and the departure is
worth reading before changing it.

Aiken deserialises @validation_context_cbor@ to @Data@, destructures it as a
seven-element list, checks each element against the header, and separately checks
that re-serialising the decoded data reproduces the supplied bytes. There is no
CBOR /decoder/ builtin on-chain, so Aiken's @cbor.deserialise@ is a hand-written
byte walk — and the canonicality check afterwards is what makes the whole thing
equivalent to a much simpler statement.

Those checks hold together exactly when @context_cbor@ equals the canonical
serialisation of the list the header determines. The header is the only source of
every field: version and profile id are constants, and the other five are read
off the header. So this port builds that list from the header, serialises it once
with @serialiseData@, and compares. The two formulations accept and reject
exactly the same byte strings; this one needs no decoder.

The range checks (@expected_network_id@ in @{0,1}@, the two fee coefficients and
the block slot non-negative) survive the rewrite unchanged. They are checks on the
/header/ in both versions — Aiken applies them to the decoded values only after
having required those values to equal the header's — so they still catch a header
that is itself out of range rather than being made vacuous by the substitution.

=== The initial work root

A claim constrains its initial state by construction rather than by execution:
phase @CanonicalDecode@, program counter zero, no budget spent, verdict pending.
The @work_root@ is the last degree of freedom, and it is pinned to the hash of the
only witness that phase can begin from — a transaction field scan positioned at
field 0, item 0, chunk 0, with no item count established and nothing encoded yet.
See "Midgard.ValidationMachine" for that encoder.

=== A note on the source membership's two arms

@ValidationSourceMembershipV1@'s two constructors carry the same thing — a root
membership proof — and differ only in which tree it is against. In Aiken they are
distinguished by type parameters that erase on-chain; here the ported
@RootMembershipProof@ carries its key and value as @Data@, so the two arms are
structurally identical.

That makes a @pmatch@ over them the exact shape Plutarch mis-compiles when two
arms share a body, which 'pinitialWorkRootIsExact' would otherwise be. Every
consumer here therefore reads the tag with 'pconstrOf' and takes the proof out of
field 0 — which for 'pinitialWorkRootIsExact' means it never has to look at the
tag at all, since both arms want the same field.
-}
module Midgard.ValidationClaim (
  -- * Constants
  pclaimVersion,
  pvalidationContextVersionV1,
  pvalidationContextProfileIdV1,

  -- * Types
  PValidationSourceMembershipV1 (..),
  PValidationClaimWitnessV1 (..),

  -- * Source membership accessors
  pvalidationSourceMembershipFromData,
  psourceIsForced,
  psourceMembershipProof,

  -- * Component checks
  pvalidationContextIsExact,
  pinitialWorkRootIsExact,
  pimmutableContextMatches,
  pforcedVerdictMatches,
  pphaseTagForEventKeyTag,
  psourceProofCommitment,

  -- * The claim
  pcommittedClaimStructureIsValid,
  pcommittedClaimSourceIsAuthenticated,
  pcommittedClaimEndpointsAndSourceAreValid,
  pcommittedClaimIsValid,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (pasByteStr, pasConstr, pasInt, plistData, pserialiseData)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pconstrOf)
import Midgard.FraudProofs.NativeTx.Compact (
  pnativeTxProofCommitmentV1,
  pverifyNativeTxProofSourceV1,
 )
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.LedgerState (
  PEventToStepValue (..),
  PForcedInclusionTxV1 (..),
  PHeaderV1 (..),
  PL2TransactionSourceV1 (..),
  PMidgardTxValidity (..),
  PNativeTxProofSourceV1 (..),
  PTransitionStep (..),
  pprotocolVersionV1,
  ptransitionStepSchemaVersionV1,
 )
import Midgard.TransitionTrace (
  PRootDomain (..),
  PRootMembershipProof (..),
  pverifyRootMembershipWithBytes,
 )
import Midgard.ValidationMachine (pencodeTransactionFieldScanWitness)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationSourceKind (..),
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),
  PValidationVerdict (..),
  phashMachineState,
  phashValidationContext,
  phashWorkWitness,
  pmachineStateIsWellFormed,
  pverifyTraceProof,
 )

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Aiken @validation_claim_v1.claim_version@.
pclaimVersion :: forall (s :: S). Term s PInteger
pclaimVersion = 1

-- | Aiken @validation_claim_v1.validation_context_version_v1@.
pvalidationContextVersionV1 :: forall (s :: S). Term s PInteger
pvalidationContextVersionV1 = 1

{- | Aiken @validation_claim_v1.validation_context_profile_id_v1@ —
@"midgard-consensus-v1"@ in ASCII.
-}
pvalidationContextProfileIdV1 :: forall (s :: S). Term s PByteString
pvalidationContextProfileIdV1 = phexByteStr "6d6964676172642d636f6e73656e7375732d7631"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Aiken @validation_claim_v1.ValidationSourceMembershipV1@.

Which tree the transaction came out of. The payload is the same either way — see
this module's header for why the arms are read by tag rather than matched.
-}
data PValidationSourceMembershipV1 (s :: S)
  = PForcedValidationSource
      {pforcedSource'membership :: Term s (PAsData PRootMembershipProof)}
  | PNormalValidationSource
      {pnormalSource'membership :: Term s (PAsData PRootMembershipProof)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationSourceMembershipV1)

-- | Strict decoder for the source arm and its erased membership payload.
pvalidationSourceMembershipFromData :: forall s.
  Term s (PData :--> PValidationSourceMembershipV1)
pvalidationSourceMembershipFromData = phoistAcyclic $ plam $ \dat ->
  pif
    (pvalidationSourceMembershipDataIsValid # dat)
    (punsafeCoerce dat)
    perror

pvalidationSourceMembershipDataIsValid :: forall s. Term s (PData :--> PBool)
pvalidationSourceMembershipDataIsValid = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair sourceTag sourceFields) ->
    (sourceTag #== 0 #|| sourceTag #== 1)
      #&& plength # sourceFields #== 1
      #&& psourceMembershipDataIsValid
        # sourceTag # (phead # sourceFields)

psourceMembershipDataIsValid :: forall s.
  Term s (PInteger :--> PData :--> PBool)
psourceMembershipDataIsValid = phoistAcyclic $ plam $ \sourceTag membershipData ->
  pmatch (pasConstr # membershipData) $ \(PBuiltinPair membershipTag fields) ->
    membershipTag #== 0
      #&& plength # fields #== 7
      #&& pnullaryConstructorIs # (sourceTag + 1) # (pelemAt # 0 # fields)
      #&& pdataIsBytes # (pelemAt # 1 # fields)
      #&& pdataIsBytes # (pelemAt # 2 # fields)
      #&& pdataIsInteger # (pelemAt # 3 # fields)
      #&& pif
        (sourceTag #== 0)
        ( pforcedSourceKeyIsValid # (pelemAt # 4 # fields)
            #&& pforcedSourceValueIsValid # (pelemAt # 5 # fields)
        )
        ( pdataIsBytes # (pelemAt # 4 # fields)
            #&& pnormalSourceValueIsValid # (pelemAt # 5 # fields)
        )
      #&& pproofDataIsValid # (pelemAt # 6 # fields)

pforcedSourceKeyIsValid :: forall s. Term s (PData :--> PBool)
pforcedSourceKeyIsValid = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    tag #== 0
      #&& plength # fields #== 2
      #&& pdataIsBytes # (pelemAt # 0 # fields)
      #&& pdataIsInteger # (pelemAt # 1 # fields)

pforcedSourceValueIsValid :: forall s. Term s (PData :--> PBool)
pforcedSourceValueIsValid = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    tag #== 0
      #&& plength # fields #== 3
      #&& pdataIsBytes # (pelemAt # 0 # fields)
      #&& pnativeTxProofSourceDataIsValid # (pelemAt # 1 # fields)
      #&& pmatch (pasConstr # (pelemAt # 2 # fields)) (\(PBuiltinPair validityTag validityFields) ->
            validityTag #>= 0 #&& validityTag #<= 5 #&& pnull # validityFields)

pnormalSourceValueIsValid :: forall s. Term s (PData :--> PBool)
pnormalSourceValueIsValid = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    tag #== 0
      #&& plength # fields #== 2
      #&& pdataIsBytes # (pelemAt # 0 # fields)
      #&& pnativeTxProofSourceDataIsValid # (pelemAt # 1 # fields)

pnativeTxProofSourceDataIsValid :: forall s. Term s (PData :--> PBool)
pnativeTxProofSourceDataIsValid = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    tag #== 0
      #&& plength # fields #== 3
      #&& pdataIsBytes # (pelemAt # 0 # fields)
      #&& pdataIsBytes # (pelemAt # 1 # fields)
      #&& pdataIsBytes # (pelemAt # 2 # fields)

pnullaryConstructorIs :: forall s. Term s (PInteger :--> PData :--> PBool)
pnullaryConstructorIs = phoistAcyclic $ plam $ \expectedTag dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    tag #== expectedTag #&& pnull # fields

pdataIsBytes :: forall s. Term s (PData :--> PBool)
pdataIsBytes = phoistAcyclic $ plam $ \dat ->
  plengthBS # (pasByteStr # dat) #>= 0

pdataIsInteger :: forall s. Term s (PData :--> PBool)
pdataIsInteger = phoistAcyclic $ plam $ \dat ->
  pabs # (pasInt # dat) #>= 0

pproofDataIsValid :: forall s. Term s (PData :--> PBool)
pproofDataIsValid = phoistAcyclic $ plam $ \dat ->
  pproofStepsAreValid # (pasList # dat)

pproofStepsAreValid :: forall s. Term s (PBuiltinList PData :--> PBool)
pproofStepsAreValid = phoistAcyclic $ pfix $ \self -> plam $ \steps ->
  pelimList
    (\step rest -> pproofStepDataIsValid # step #&& self # rest)
    (pconstant True)
    steps

pproofStepDataIsValid :: forall s. Term s (PData :--> PBool)
pproofStepDataIsValid = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif
      (tag #== 0)
      ( plength # fields #== 2
          #&& pdataIsInteger # (pelemAt # 0 # fields)
          #&& pdataIsBytes # (pelemAt # 1 # fields)
      )
      ( pif
          (tag #== 1)
          ( plength # fields #== 2
              #&& pdataIsInteger # (pelemAt # 0 # fields)
              #&& pneighborDataIsValid # (pelemAt # 1 # fields)
          )
          ( tag #== 2
              #&& plength # fields #== 3
              #&& pdataIsInteger # (pelemAt # 0 # fields)
              #&& pdataIsBytes # (pelemAt # 1 # fields)
              #&& pdataIsBytes # (pelemAt # 2 # fields)
          )
      )

pneighborDataIsValid :: forall s. Term s (PData :--> PBool)
pneighborDataIsValid = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    tag #== 0
      #&& plength # fields #== 3
      #&& pdataIsInteger # (pelemAt # 0 # fields)
      #&& pdataIsBytes # (pelemAt # 1 # fields)
      #&& pdataIsBytes # (pelemAt # 2 # fields)

{- | Aiken @validation_claim_v1.ValidationClaimWitnessV1@.

Everything a claim has to supply: four membership proofs, the validation context
the run was parameterised by, and the run's two endpoint states with their trace
paths.
-}
data PValidationClaimWitnessV1 (s :: S) = PValidationClaimWitnessV1
  { pclaim'version :: Term s (PAsData PInteger)
  , pclaim'descriptorMembership :: Term s (PAsData PRootMembershipProof)
  , pclaim'transitionStepMembership :: Term s (PAsData PRootMembershipProof)
  , pclaim'eventToStepMembership :: Term s (PAsData PRootMembershipProof)
  , pclaim'sourceMembership :: Term s (PAsData PValidationSourceMembershipV1)
  , pclaim'validationContextCbor :: Term s (PAsData PByteString)
  , pclaim'initialState :: Term s (PAsData PValidationMachineStateV1)
  , pclaim'terminalState :: Term s (PAsData PValidationMachineStateV1)
  , pclaim'initialStateProof :: Term s (PAsData PValidationTraceProof)
  , pclaim'terminalStateProof :: Term s (PAsData PValidationTraceProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationClaimWitnessV1)

--------------------------------------------------------------------------------
-- Source membership accessors
--------------------------------------------------------------------------------

-- | Reinterpret raw @Data@ as a known data-encoded type.
pcoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
pcoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

-- | 'pconstrOf' for a value already erased to @Data@.
pconstrOfData ::
  forall (s :: S).
  Term s PData ->
  (Term s PInteger, Term s (PBuiltinList PData))
pconstrOfData d =
  let pair = pasConstr # d
   in (pfstBuiltin # pair, psndBuiltin # pair)

-- | Whether a source membership is the forced-inclusion arm (tag 0).
psourceIsForced ::
  forall (s :: S). Term s (PAsData PValidationSourceMembershipV1) -> Term s PBool
psourceIsForced source =
  pif
    (pvalidationSourceMembershipDataIsValid # pforgetData source)
    (fst (pconstrOf source) #== 0)
    perror

{- | The membership proof out of either arm.

Field 0 of both constructors, so no tag read is needed — which is the point: the
one caller that wants the proof regardless of arm gets it without writing a
two-armed match whose bodies would be identical.
-}
psourceMembershipProof ::
  forall (s :: S).
  Term s (PAsData PValidationSourceMembershipV1) ->
  Term s PRootMembershipProof
psourceMembershipProof source =
  pif
    (pvalidationSourceMembershipDataIsValid # pforgetData source)
    (pcoerceData (phead # snd (pconstrOf source)))
    perror

--------------------------------------------------------------------------------
-- Component checks
--------------------------------------------------------------------------------

{- | Aiken @validation_claim_v1.validation_context_is_exact@.

See this module's header for why this builds and serialises rather than
deserialises and destructures.
-}
pvalidationContextIsExact ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PByteString ->
  Term s PValidationMachineStateV1 ->
  Term s PBool
pvalidationContextIsExact header contextCbor initial = P.do
  PHeaderV1
    { pheader'endTime
    , pheader'expectedNetworkId
    , pheader'minFeeA
    , pheader'minFeeB
    , pheader'blockSlot
    } <-
    pmatch header
  PValidationMachineStateV1 {pmachineState'validationContextHash} <- pmatch initial
  expectedNetworkId <- plet (pfromData pheader'expectedNetworkId)
  minFeeA <- plet (pfromData pheader'minFeeA)
  minFeeB <- plet (pfromData pheader'minFeeB)
  blockSlot <- plet (pfromData pheader'blockSlot)
  canonical <-
    plet $
      pserialiseData
        #$ plistData
        #$ pcons
        # pforgetData (pdata pvalidationContextVersionV1)
        #$ pcons
        # pforgetData (pdata pvalidationContextProfileIdV1)
        #$ pcons
        # pforgetData pheader'endTime
        #$ pcons
        # pforgetData pheader'expectedNetworkId
        #$ pcons
        # pforgetData pheader'minFeeA
        #$ pcons
        # pforgetData pheader'minFeeB
        #$ pcons
        # pforgetData pheader'blockSlot
        # pcon PNil
  canonical
    #== contextCbor
    #&& (expectedNetworkId #== 0 #|| expectedNetworkId #== 1)
    #&& minFeeA
    #>= 0
    #&& minFeeB
    #>= 0
    #&& blockSlot
    #>= 0
    #&& (phashValidationContext # contextCbor)
    #== pfromData pmachineState'validationContextHash

{- | Aiken @validation_claim_v1.initial_work_root_is_exact@.

Both of Aiken's arms compute the same thing from the same field, so this reads
the field once and never looks at the tag.
-}
pinitialWorkRootIsExact ::
  forall (s :: S).
  Term s PValidationMachineStateV1 ->
  Term s PByteString ->
  Term s (PAsData PValidationSourceMembershipV1) ->
  Term s PBool
pinitialWorkRootIsExact initial contextCbor source = P.do
  PValidationMachineStateV1 {pmachineState'workRoot} <- pmatch initial
  PRootMembershipProof {prootMembership'value} <-
    pmatch (psourceMembershipProof source)
  -- Both leaf types carry the proof source at field 1; only the forced arm has a
  -- third field, and nothing here reads it.
  PNativeTxProofSourceV1
    { pnativeSource'compactCbor
    , pnativeSource'witnessSetCompactCbor
    , pnativeSource'fieldPreimageLengthsCbor
    } <-
    pmatch (pcoerceData (phead #$ ptail # snd (pconstrOfData prootMembership'value)))
  firstWorkWitness <-
    plet $
      pencodeTransactionFieldScanWitness
        # pfromData pnativeSource'compactCbor
        # pfromData pnativeSource'witnessSetCompactCbor
        # pfromData pnativeSource'fieldPreimageLengthsCbor
        # contextCbor
        # 0
        # 0
        # 0
        # (-1)
        # 0
  pfromData pmachineState'workRoot
    #== (phashWorkWitness # pcon PCanonicalDecode # 0 # firstWorkWitness)

{- | Aiken @validation_claim_v1.immutable_context_matches@.

The eight fields a machine step may never alter. A run that changed any of them
would be a run about a different transaction, event or ledger.
-}
pimmutableContextMatches ::
  forall (s :: S).
  Term s PValidationMachineStateV1 ->
  Term s PValidationMachineStateV1 ->
  Term s PBool
pimmutableContextMatches initial terminal = P.do
  PValidationMachineStateV1
    { pmachineState'machineVersion = iVersion
    , pmachineState'eventKeyHash = iEventKeyHash
    , pmachineState'transactionId = iTxId
    , pmachineState'transactionCommitment = iCommitment
    , pmachineState'validationContextHash = iContextHash
    , pmachineState'sourceKind = iSourceKind
    , pmachineState'priorLedgerRoot = iPriorRoot
    , pmachineState'ledgerDeltaRoot = iDeltaRoot
    } <-
    pmatch initial
  PValidationMachineStateV1
    { pmachineState'machineVersion = tVersion
    , pmachineState'eventKeyHash = tEventKeyHash
    , pmachineState'transactionId = tTxId
    , pmachineState'transactionCommitment = tCommitment
    , pmachineState'validationContextHash = tContextHash
    , pmachineState'sourceKind = tSourceKind
    , pmachineState'priorLedgerRoot = tPriorRoot
    , pmachineState'ledgerDeltaRoot = tDeltaRoot
    } <-
    pmatch terminal
  iVersion
    #== tVersion
    #&& iEventKeyHash
    #== tEventKeyHash
    #&& iTxId
    #== tTxId
    #&& iCommitment
    #== tCommitment
    #&& iContextHash
    #== tContextHash
    #&& iSourceKind
    #== tSourceKind
    #&& iPriorRoot
    #== tPriorRoot
    #&& iDeltaRoot
    #== tDeltaRoot

{- | Aiken @validation_claim_v1.forced_verdict_matches@.

A forced-inclusion transaction's descriptor verdict must reproduce the verdict
the operator already recorded for it in the block. Anything the operator called
invalid — for any of the five reasons — must have been rejected.
-}
pforcedVerdictMatches ::
  forall (s :: S).
  Term s (PAsData PMidgardTxValidity) ->
  Term s (PAsData PValidationVerdict) ->
  Term s PBool
pforcedVerdictMatches validity verdict =
  pif
    (validity #== pdata (pcon PTxIsValid))
    (verdict #== pdata (pcon PAccepted))
    (verdict #== pdata (pcon PRejected))

{- | Aiken @validation_claim_v1.phase_for_event_key@, as a tag map.

The four @EventKey@ constructors and the four @TransitionPhase@ constructors are
declared in the same order, so the mapping is the identity on tags. It is written
out arm by arm anyway: the identity is a fact about two declaration orders that
nothing enforces, and a table that has to be edited deliberately is cheaper than
a coincidence that has to be remembered.
-}
pphaseTagForEventKeyTag :: forall (s :: S). Term s PInteger -> Term s PInteger
pphaseTagForEventKeyTag tag =
  pif (tag #== 0) 0 $
    pif (tag #== 1) 1 $
      pif (tag #== 2) 2 $
        pif (tag #== 3) 3 perror

{- | Aiken @validation_claim_v1.source_proof_commitment@.

The §7 proof-source commitment of the membership's own compact triple. Since §4
retired the carried @transaction_commitment@ from both leaf types, this is what
the machine's @transaction_commitment@ is anchored to — derived at the one place
that reads it rather than carried on every leaf of two block-committed roots.
-}
psourceProofCommitment ::
  forall (s :: S). Term s PNativeTxProofSourceV1 -> Term s PByteString
psourceProofCommitment source = pmatch source $
  \( PNativeTxProofSourceV1
      { pnativeSource'compactCbor
      , pnativeSource'witnessSetCompactCbor
      , pnativeSource'fieldPreimageLengthsCbor
      }
    ) ->
      pnativeTxProofCommitmentV1
        # pfromData pnativeSource'compactCbor
        # pfromData pnativeSource'witnessSetCompactCbor
        # pfromData pnativeSource'fieldPreimageLengthsCbor

--------------------------------------------------------------------------------
-- Membership helpers
--------------------------------------------------------------------------------

-- | Aiken @validation_claim_v1.verify_descriptor_membership@.
pverifyDescriptorMembership ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PRootMembershipProof ->
  Term s PBool
pverifyDescriptorMembership header membership = P.do
  PHeaderV1 {pheader'validationTracesRoot, pheader'validationTraceCount} <-
    pmatch header
  PRootMembershipProof {prootMembership'key, prootMembership'value} <-
    pmatch membership
  pverifyRootMembershipWithBytes
    membership
    (pdata (pcon PValidationTracesRootDomain))
    (pfromData pheader'validationTracesRoot)
    (pfromData pheader'validationTraceCount)
    (pserialiseData # prootMembership'key)
    (pserialiseData # prootMembership'value)

{- | Aiken @validation_claim_v1.verify_transition_step_membership@.

The extra clauses beyond membership are what make the step /addressable/: its key
is its own index, that index is inside the block's step range, and the schema is
the one this claim knows how to read.
-}
pverifyTransitionStepMembership ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PRootMembershipProof ->
  Term s PBool
pverifyTransitionStepMembership header membership = P.do
  PHeaderV1 {pheader'transitionTraceRoot, pheader'transitionStepCount} <- pmatch header
  PRootMembershipProof
    { prootMembership'key
    , prootMembership'value
    , prootMembership'count
    } <-
    pmatch membership
  key <- plet (pasInt # prootMembership'key)
  PTransitionStep {ptransitionStep'schemaVersion, ptransitionStep'stepIndex} <-
    pmatch (pcoerceData prootMembership'value)
  pverifyRootMembershipWithBytes
    membership
    (pdata (pcon PTransitionTraceRootDomain))
    (pfromData pheader'transitionTraceRoot)
    (pfromData pheader'transitionStepCount)
    (pserialiseData # prootMembership'key)
    (pserialiseData # prootMembership'value)
    #&& key
    #== pfromData ptransitionStep'stepIndex
    #&& key
    #>= 0
    #&& key
    #< pfromData prootMembership'count
    #&& pfromData ptransitionStep'schemaVersion
    #== ptransitionStepSchemaVersionV1

-- | Aiken @validation_claim_v1.verify_event_to_step_membership@.
pverifyEventToStepMembership ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PRootMembershipProof ->
  Term s PBool
pverifyEventToStepMembership header membership = P.do
  PHeaderV1 {pheader'eventToStepRoot, pheader'totalEventCount} <- pmatch header
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
  pverifyRootMembershipWithBytes
    membership
    (pdata (pcon PEventToStepRootDomain))
    (pfromData pheader'eventToStepRoot)
    (pfromData pheader'totalEventCount)
    (pserialiseData # prootMembership'key)
    (pserialiseData # prootMembership'value)

{- | Aiken @validation_claim_v1.verify_source_authentication@.

The forced and normal arms differ in three ways that all matter: which root the
proof is against, how the key is serialised, and where the transaction id being
reconstructed comes from.

The key serialisation is the subtle one. A forced transaction is keyed by its
/order id/ — a structured @Data@ value, so the key bytes are its CBOR — while an
L2 transaction is keyed by its id, a bare byte string that goes into the tree as
itself. Serialising the latter would add a two-byte header and address a
different tree slot.
-}
pverifySourceAuthentication ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PData ->
  Term s (PAsData PValidationSourceMembershipV1) ->
  Term s PBool
pverifySourceAuthentication header eventKey source = P.do
  PHeaderV1
    { pheader'forcedTransactionsRoot
    , pheader'forcedTransactionCount
    , pheader'transactionsRoot
    , pheader'l2TransactionCount
    } <-
    pmatch header
  membership <- plet (psourceMembershipProof source)
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
  eventKeyTag <- plet (fst (pconstrOfData eventKey))
  eventKeyFields <- plet (snd (pconstrOfData eventKey))
  pif
    (psourceIsForced source)
    ( P.do
        -- @expect ForcedTransactionEventKey { tx_order_id } = event_key@.
        txOrderId <- plet (pexpectTag eventKeyTag 1 (phead # eventKeyFields))
        PForcedInclusionTxV1 {pforcedTx'txId, pforcedTx'source} <-
          pmatch (pcoerceData prootMembership'value)
        verified <-
          plet $
            pverifiedSource
              (pfromData pforcedTx'txId)
              (pfromData pforcedTx'source)
        pverifyRootMembershipWithBytes
          membership
          (pdata (pcon PForcedTransactionsV1RootDomain))
          (pfromData pheader'forcedTransactionsRoot)
          (pfromData pheader'forcedTransactionCount)
          (pserialiseData # prootMembership'key)
          (pserialiseData # prootMembership'value)
          #&& prootMembership'key
          #== txOrderId
          #&& verified
    )
    ( P.do
        -- @expect L2TransactionEventKey { tx_id } = event_key@.
        txId <- plet (pasByteStr #$ pexpectTag eventKeyTag 2 (phead # eventKeyFields))
        PL2TransactionSourceV1 {pl2Source'txId, pl2Source'source} <-
          pmatch (pcoerceData prootMembership'value)
        leafTxId <- plet (pfromData pl2Source'txId)
        verified <- plet $ pverifiedSource txId (pfromData pl2Source'source)
        pverifyRootMembershipWithBytes
          membership
          (pdata (pcon PTransactionsV1RootDomain))
          (pfromData pheader'transactionsRoot)
          (pfromData pheader'l2TransactionCount)
          (pasByteStr # prootMembership'key)
          (pserialiseData # prootMembership'value)
          #&& (pasByteStr # prootMembership'key)
          #== leafTxId
          #&& leafTxId
          #== txId
          #&& verified
    )
  where
    -- Aiken's @expect Ctor { .. } = value@: a wrong constructor aborts.
    pexpectTag tag expected value = pif (tag #== pconstant expected) value perror
    -- @verify_native_tx_proof_source_v1@ reconstructs and aborts on any
    -- mismatch; all that is left to check is the version it reports.
    pverifiedSource txId source' = pmatch source' $
      \( PNativeTxProofSourceV1
          { pnativeSource'compactCbor
          , pnativeSource'witnessSetCompactCbor
          , pnativeSource'fieldPreimageLengthsCbor
          }
        ) -> pmatch
          ( pverifyNativeTxProofSourceV1
              # txId
              # pfromData pnativeSource'compactCbor
              # pfromData pnativeSource'witnessSetCompactCbor
              # pfromData pnativeSource'fieldPreimageLengthsCbor
          )
          $ \(PPair compact _witnessSet) ->
            pmatch compact $ \PVerifiedMidgardNativeTxCompact {pverified'version} ->
              pverified'version #== 1

{- | Aiken @validation_claim_v1.source_binding_is_exact@.

Ties the machine's opening state to the leaf the source came out of: same
transaction id, same derived commitment, matching source kind — and, for a forced
transaction, a descriptor verdict that reproduces the operator's recorded one. A
normal transaction has no recorded verdict to reproduce, so its descriptor must
say @Accepted@: a committed L2 transaction that the machine rejects is a fault to
be proven elsewhere, not a claim to be opened here.
-}
psourceBindingIsExact ::
  forall (s :: S).
  Term s PValidationTraceDescriptorV1 ->
  Term s PValidationMachineStateV1 ->
  Term s (PAsData PValidationSourceMembershipV1) ->
  Term s PBool
psourceBindingIsExact descriptor initialState source = P.do
  PValidationTraceDescriptorV1 {pdescriptor'verdict} <- pmatch descriptor
  PValidationMachineStateV1
    { pmachineState'transactionId
    , pmachineState'transactionCommitment
    , pmachineState'sourceKind
    } <-
    pmatch initialState
  PRootMembershipProof {prootMembership'value} <- pmatch (psourceMembershipProof source)
  pif
    (psourceIsForced source)
    ( P.do
        PForcedInclusionTxV1
          { pforcedTx'txId
          , pforcedTx'source
          , pforcedTx'operatorValidity
          } <-
          pmatch (pcoerceData prootMembership'value)
        pforcedTx'txId
          #== pmachineState'transactionId
          #&& psourceProofCommitment (pfromData pforcedTx'source)
          #== pfromData pmachineState'transactionCommitment
          #&& pmachineState'sourceKind
          #== pdata (pcon PForced)
          #&& pforcedVerdictMatches pforcedTx'operatorValidity pdescriptor'verdict
    )
    ( P.do
        PL2TransactionSourceV1 {pl2Source'txId, pl2Source'source} <-
          pmatch (pcoerceData prootMembership'value)
        pl2Source'txId
          #== pmachineState'transactionId
          #&& psourceProofCommitment (pfromData pl2Source'source)
          #== pfromData pmachineState'transactionCommitment
          #&& pmachineState'sourceKind
          #== pdata (pcon PNormal)
          #&& pdescriptor'verdict
          #== pdata (pcon PAccepted)
    )

--------------------------------------------------------------------------------
-- The claim
--------------------------------------------------------------------------------

{- | Aiken @validation_claim_v1.committed_claim_structure_is_valid@.

Authenticates the block-owned roots, the descriptor's endpoints, and where in the
block the transition sits — and deliberately stops there. Normative endpoint and
source checks belong to 'pcommittedClaimEndpointsAndSourceAreValid', so a block
committing a malformed endpoint is convicted rather than rendered unchallengeable.
-}
pcommittedClaimStructureIsValid ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PValidationClaimWitnessV1 ->
  Term s PBool
pcommittedClaimStructureIsValid header witness = P.do
  PValidationClaimWitnessV1
    { pclaim'version
    , pclaim'descriptorMembership
    , pclaim'transitionStepMembership
    , pclaim'eventToStepMembership
    , pclaim'initialState
    , pclaim'terminalState
    , pclaim'initialStateProof
    , pclaim'terminalStateProof
    } <-
    pmatch witness
  PHeaderV1 {pheader'protocolVersion} <- pmatch header
  descriptorMembership <- plet (pfromData pclaim'descriptorMembership)
  stepMembership <- plet (pfromData pclaim'transitionStepMembership)
  eventMembership <- plet (pfromData pclaim'eventToStepMembership)
  PRootMembershipProof
    { prootMembership'key = descriptorKey
    , prootMembership'value = descriptorValue
    } <-
    pmatch descriptorMembership
  PRootMembershipProof
    { prootMembership'key = stepKey
    , prootMembership'value = stepValue
    } <-
    pmatch stepMembership
  PRootMembershipProof
    { prootMembership'key = eventKeyOfEventToStep
    , prootMembership'value = eventToStepValue
    } <-
    pmatch eventMembership
  descriptor <- plet (pcoerceData @PValidationTraceDescriptorV1 descriptorValue)
  PValidationTraceDescriptorV1
    { pdescriptor'stepCount
    , pdescriptor'initialStateHash
    , pdescriptor'terminalStateHash
    } <-
    pmatch descriptor
  PTransitionStep {ptransitionStep'eventKey, ptransitionStep'phase} <-
    pmatch (pcoerceData stepValue)
  PEventToStepValue {peventToStepValue'stepIndex, peventToStepValue'phase} <-
    pmatch (pcoerceData eventToStepValue)
  PValidationTraceProof
    { ptraceProof'stateIndex = initialIndex
    , ptraceProof'stateHash = initialHash
    } <-
    pmatch (pfromData pclaim'initialStateProof)
  PValidationTraceProof
    { ptraceProof'stateIndex = terminalIndex
    , ptraceProof'stateHash = terminalHash
    } <-
    pmatch (pfromData pclaim'terminalStateProof)
  stepPhaseTag <- plet (fst (pconstrOf ptransitionStep'phase))
  pfromData pclaim'version
    #== pclaimVersion
    #&& pfromData pheader'protocolVersion
    #== pprotocolVersionV1
    #&& pverifyDescriptorMembership header descriptorMembership
    #&& pverifyTransitionStepMembership header stepMembership
    #&& pverifyEventToStepMembership header eventMembership
    #&& (pasInt # stepKey)
    #== pfromData peventToStepValue'stepIndex
    #&& eventKeyOfEventToStep
    #== descriptorKey
    #&& pforgetData ptransitionStep'eventKey
    #== descriptorKey
    #&& stepPhaseTag
    #== pphaseTagForEventKeyTag (fst (pconstrOfData descriptorKey))
    #&& ptransitionStep'phase
    #== peventToStepValue'phase
    #&& pfromData initialIndex
    #== 0
    #&& pfromData initialHash
    #== (phashMachineState # pfromData pclaim'initialState)
    #&& (pverifyTraceProof # descriptor # pfromData pclaim'initialStateProof)
    #&& terminalIndex
    #== pdescriptor'stepCount
    #&& pfromData terminalHash
    #== (phashMachineState # pfromData pclaim'terminalState)
    #&& (pverifyTraceProof # descriptor # pfromData pclaim'terminalStateProof)
    #&& pdescriptor'initialStateHash
    #== initialHash
    #&& pdescriptor'terminalStateHash
    #== terminalHash

-- | Aiken @validation_claim_v1.committed_claim_source_is_authenticated@.
pcommittedClaimSourceIsAuthenticated ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PValidationClaimWitnessV1 ->
  Term s PBool
pcommittedClaimSourceIsAuthenticated header witness = pmatch witness $
  \( PValidationClaimWitnessV1
      {pclaim'descriptorMembership, pclaim'sourceMembership}
    ) ->
      pmatch (pfromData pclaim'descriptorMembership) $
        \PRootMembershipProof {prootMembership'key} ->
          pverifySourceAuthentication header prootMembership'key pclaim'sourceMembership

{- | Aiken @validation_claim_v1.committed_claim_endpoints_and_source_are_valid@.

Everything normative about the two endpoints. The initial state is pinned in full
— its event-key hash, its prior ledger root, its phase, counter, budget and
verdict, its context and its work root — because a machine run is only meaningful
relative to a starting point nobody got to choose. The terminal state is pinned to
the descriptor it is summarised by.

The last clause is the one that is easy to miss: a rejected transaction must leave
the ledger untouched, so its step's pre- and post-roots must be the same. Without
it, a rejection could still be committed alongside a ledger mutation.
-}
pcommittedClaimEndpointsAndSourceAreValid ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PValidationClaimWitnessV1 ->
  Term s PBool
pcommittedClaimEndpointsAndSourceAreValid header witness = P.do
  PValidationClaimWitnessV1
    { pclaim'descriptorMembership
    , pclaim'transitionStepMembership
    , pclaim'sourceMembership
    , pclaim'validationContextCbor
    , pclaim'initialState
    , pclaim'terminalState
    } <-
    pmatch witness
  PRootMembershipProof
    { prootMembership'key = eventKey
    , prootMembership'value = descriptorValue
    } <-
    pmatch (pfromData pclaim'descriptorMembership)
  PRootMembershipProof {prootMembership'value = stepValue} <-
    pmatch (pfromData pclaim'transitionStepMembership)
  descriptor <- plet (pcoerceData @PValidationTraceDescriptorV1 descriptorValue)
  PValidationTraceDescriptorV1
    { pdescriptor'stepCount
    , pdescriptor'verdict
    , pdescriptor'rejectionCodeHash
    } <-
    pmatch descriptor
  PTransitionStep {ptransitionStep'preUtxosRoot, ptransitionStep'postUtxosRoot} <-
    pmatch (pcoerceData stepValue)
  initial <- plet (pfromData pclaim'initialState)
  terminal <- plet (pfromData pclaim'terminalState)
  contextCbor <- plet (pfromData pclaim'validationContextCbor)
  PValidationMachineStateV1
    { pmachineState'eventKeyHash
    , pmachineState'priorLedgerRoot
    , pmachineState'phase = initialPhase
    , pmachineState'programCounter = initialCounter
    , pmachineState'executionCpu
    , pmachineState'executionMemory
    , pmachineState'verdict = initialVerdict
    } <-
    pmatch initial
  PValidationMachineStateV1
    { pmachineState'phase = terminalPhase
    , pmachineState'programCounter = terminalCounter
    , pmachineState'verdict = terminalVerdict
    , pmachineState'rejectionCodeHash = terminalRejection
    } <-
    pmatch terminal
  (pmachineStateIsWellFormed # initial)
    #&& (pmachineStateIsWellFormed # terminal)
    #&& pfromData pmachineState'eventKeyHash
    #== (pblake2b_256 #$ pserialiseData # eventKey)
    #&& pfromData pmachineState'priorLedgerRoot
    #== pfromData ptransitionStep'preUtxosRoot
    #&& initialPhase
    #== pdata (pcon PCanonicalDecode)
    #&& pfromData initialCounter
    #== 0
    #&& pfromData pmachineState'executionCpu
    #== 0
    #&& pfromData pmachineState'executionMemory
    #== 0
    #&& initialVerdict
    #== pdata (pcon PPending)
    #&& pvalidationContextIsExact header contextCbor initial
    #&& pinitialWorkRootIsExact initial contextCbor pclaim'sourceMembership
    #&& terminalPhase
    #== pdata (pcon PTerminal)
    #&& terminalCounter
    #== pdescriptor'stepCount
    #&& terminalVerdict
    #== pdescriptor'verdict
    #&& terminalRejection
    #== pdescriptor'rejectionCodeHash
    #&& pimmutableContextMatches initial terminal
    #&& psourceBindingIsExact descriptor initial pclaim'sourceMembership
    #&& pif
      (pdescriptor'verdict #== pdata (pcon PRejected))
      (ptransitionStep'preUtxosRoot #== ptransitionStep'postUtxosRoot)
      (pconstant True)

-- | Aiken @validation_claim_v1.committed_claim_is_valid@.
pcommittedClaimIsValid ::
  forall (s :: S).
  Term s (PHeaderV1 :--> PValidationClaimWitnessV1 :--> PBool)
pcommittedClaimIsValid = phoistAcyclic $
  plam $ \header witness ->
    pcommittedClaimStructureIsValid header witness
      #&& pcommittedClaimSourceIsAuthenticated header witness
      #&& pcommittedClaimEndpointsAndSourceAreValid header witness
