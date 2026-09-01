{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.ValidationTrace
Description : Plutarch port of @lib/midgard/validation-trace-v1.ak@.

The commitment scheme for a validation machine's execution trace: the machine
state, the descriptor that summarises a whole run, and the binary Merkle tree the
states are committed in.

Nothing here interprets a transaction. The module is a /codec and a tree/, which
is why it ports cleanly without the 18,000-line machine above it — a fault proof
about a validation run needs to authenticate states and steps, not re-execute
them.

=== Seven domain separators, and why they are not one

Every hash here is domain-separated: states, trace leaves, trace branches, work
witnesses, rejection codes, validation contexts and ledger deltas each get their
own prefix. Without that, a 32-byte value committed as one kind could be
presented as another — a leaf hash passed off as a state hash, say — and the
whole scheme is about pinning /which/ commitment a value is.

The two tree domains are the load-bearing pair: a Merkle tree whose leaves and
branches share a domain admits a second-preimage attack, where an internal node
is presented as a leaf.

=== The verdict and the rejection code are bound to each other

@Rejected@ must carry a non-zero rejection-code hash and every other verdict must
carry the zero one. A rejection with no code says nothing about why, and an
acceptance carrying a code is a contradiction — so both are refused rather than
normalised, and both the state and the descriptor check it.

@Pending@ is a legitimate /state/ and never a legitimate /descriptor/: a
descriptor summarises a finished run.
-}
module Midgard.ValidationTrace (
  -- * Constants
  pmachineVersion,
  pdescriptorVersion,
  pmaxMachineStepCount,

  -- * Types
  PValidationPhase (..),
  PValidationVerdict (..),
  PValidationSourceKind (..),
  PValidationMachineStateV1 (..),
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),

  -- * Well-formedness
  pmachineStateIsWellFormed,
  pdescriptorIsWellFormed,

  -- * Encoding and hashing
  pcborInt,
  pencodeMachineState,
  phashMachineState,
  phashWorkWitness,
  phashRejectionCode,
  phashValidationContext,
  phashLedgerDelta,
  pencodeDescriptor,

  -- * The trace tree
  ptraceLeafHash,
  ptraceBranchHash,
  ptraceDepth,
  pverifyTraceProof,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (pserialiseData)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Aiken @validation_trace_v1.machine_version@.
pmachineVersion :: forall (s :: S). Term s PInteger
pmachineVersion = 1

-- | Aiken @validation_trace_v1.descriptor_version@.
pdescriptorVersion :: forall (s :: S). Term s PInteger
pdescriptorVersion = 1

{- | Aiken @validation_trace_v1.max_machine_step_count@ — @2^32 - 1@.

A bound rather than a limit anyone expects to reach: it is what makes
'ptraceDepth' terminate and what stops a descriptor claiming a trace too deep to
walk.
-}
pmaxMachineStepCount :: forall (s :: S). Term s PInteger
pmaxMachineStepCount = 4_294_967_295

-- | @"MidgardValidationMachineStateV1"@.
pstateHashDomain :: forall (s :: S). Term s PByteString
pstateHashDomain = phexByteStr "4d69646761726456616c69646174696f6e4d616368696e6553746174655631"

-- | @"MidgardValidationTraceLeafV1"@.
ptraceLeafDomain :: forall (s :: S). Term s PByteString
ptraceLeafDomain = phexByteStr "4d69646761726456616c69646174696f6e54726163654c6561665631"

-- | @"MidgardValidationTraceBranchV1"@.
ptraceBranchDomain :: forall (s :: S). Term s PByteString
ptraceBranchDomain = phexByteStr "4d69646761726456616c69646174696f6e54726163654272616e63685631"

-- | @"MidgardValidationWorkWitnessV1"@.
pworkWitnessDomain :: forall (s :: S). Term s PByteString
pworkWitnessDomain = phexByteStr "4d69646761726456616c69646174696f6e576f726b5769746e6573735631"

-- | @"MidgardValidationRejectCodeV1"@.
prejectionCodeDomain :: forall (s :: S). Term s PByteString
prejectionCodeDomain = phexByteStr "4d69646761726456616c69646174696f6e52656a656374436f64655631"

-- | @"MidgardValidationContextV1"@.
pvalidationContextDomain :: forall (s :: S). Term s PByteString
pvalidationContextDomain = phexByteStr "4d69646761726456616c69646174696f6e436f6e746578745631"

-- | @"MidgardValidationLedgerDeltaV1"@.
pledgerDeltaDomain :: forall (s :: S). Term s PByteString
pledgerDeltaDomain = phexByteStr "4d69646761726456616c69646174696f6e4c656467657244656c74615631"

-- | The rejection-code hash a non-rejected verdict must carry: thirty-two zeros.
pnoRejectionCodeHash :: forall (s :: S). Term s PByteString
pnoRejectionCodeHash =
  phexByteStr "0000000000000000000000000000000000000000000000000000000000000000"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Aiken @validation_trace_v1.ValidationPhase@.

The fifteen phases a transaction's validation passes through, in order.
Constructor tags and 'pphaseCode' coincide here — both are @0..14@ — but the
encoder goes through 'pphaseCode' anyway, because the two are independent
decisions and only one of them is wire format.
-}
data PValidationPhase (s :: S)
  = PCanonicalDecode
  | PCompactBinding
  | PStaticLedgerRules
  | PInputSets
  | PSignatures
  | PPhaseANativeScripts
  | PPhaseAScriptPreconditions
  | PResolveInputs
  | PScriptSources
  | PNativeScripts
  | PScriptIntegrity
  | PCek
  | PValueAndMint
  | PLedgerDelta
  | PTerminal
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationPhase)

-- | Aiken @validation_trace_v1.ValidationVerdict@.
data PValidationVerdict (s :: S)
  = PPending
  | PAccepted
  | PRejected
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationVerdict)

{- | Aiken @validation_trace_v1.ValidationSourceKind@.

Whether the transaction reached the block through the ordinary queue or through
forced inclusion. It is committed into the machine state because the two are
validated under different rules, so a trace proving one says nothing about the
other.
-}
data PValidationSourceKind (s :: S)
  = PNormal
  | PForced
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationSourceKind)

{- | Aiken @validation_trace_v1.ValidationMachineStateV1@ — fifteen fields.

One frame of the machine: what it is validating, how far it has got, what it has
spent, and what it has concluded. Everything a dispute needs in order to say
"the run was here and then it was there".
-}
data PValidationMachineStateV1 (s :: S) = PValidationMachineStateV1
  { pmachineState'machineVersion :: Term s (PAsData PInteger)
  , pmachineState'eventKeyHash :: Term s (PAsData PByteString)
  , pmachineState'transactionId :: Term s (PAsData PByteString)
  , pmachineState'transactionCommitment :: Term s (PAsData PByteString)
  , pmachineState'validationContextHash :: Term s (PAsData PByteString)
  , pmachineState'sourceKind :: Term s (PAsData PValidationSourceKind)
  , pmachineState'priorLedgerRoot :: Term s (PAsData PByteString)
  , pmachineState'phase :: Term s (PAsData PValidationPhase)
  , pmachineState'programCounter :: Term s (PAsData PInteger)
  , pmachineState'workRoot :: Term s (PAsData PByteString)
  , pmachineState'executionCpu :: Term s (PAsData PInteger)
  , pmachineState'executionMemory :: Term s (PAsData PInteger)
  , pmachineState'verdict :: Term s (PAsData PValidationVerdict)
  , pmachineState'rejectionCodeHash :: Term s (PAsData PByteString)
  , pmachineState'ledgerDeltaRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationMachineStateV1)

{- | Aiken @validation_trace_v1.ValidationTraceDescriptorV1@ — eight fields.

A whole run, summarised: the tree its states are committed in, how many steps it
took, its two endpoints and its verdict. This is what a block commits per event,
and what a fault proof opens.
-}
data PValidationTraceDescriptorV1 (s :: S) = PValidationTraceDescriptorV1
  { pdescriptor'schemaVersion :: Term s (PAsData PInteger)
  , pdescriptor'machineVersion :: Term s (PAsData PInteger)
  , pdescriptor'traceRoot :: Term s (PAsData PByteString)
  , pdescriptor'stepCount :: Term s (PAsData PInteger)
  , pdescriptor'initialStateHash :: Term s (PAsData PByteString)
  , pdescriptor'terminalStateHash :: Term s (PAsData PByteString)
  , pdescriptor'verdict :: Term s (PAsData PValidationVerdict)
  , pdescriptor'rejectionCodeHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationTraceDescriptorV1)

{- | Aiken @validation_trace_v1.ValidationTraceProof@.

A Merkle path to one state of a trace. The index is carried alongside the
siblings because the path's /shape/ depends on it: at each level the sibling goes
left or right according to the index's low bit.
-}
data PValidationTraceProof (s :: S) = PValidationTraceProof
  { ptraceProof'stateIndex :: Term s (PAsData PInteger)
  , ptraceProof'stateHash :: Term s (PAsData PByteString)
  , ptraceProof'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationTraceProof)

--------------------------------------------------------------------------------
-- Codes
--------------------------------------------------------------------------------

-- | Aiken @validation_trace_v1.phase_code@.
pphaseCode :: forall (s :: S). Term s (PValidationPhase :--> PInteger)
pphaseCode = phoistAcyclic $
  plam $ \phase -> pmatch phase $ \case
    PCanonicalDecode -> 0
    PCompactBinding -> 1
    PStaticLedgerRules -> 2
    PInputSets -> 3
    PSignatures -> 4
    PPhaseANativeScripts -> 5
    PPhaseAScriptPreconditions -> 6
    PResolveInputs -> 7
    PScriptSources -> 8
    PNativeScripts -> 9
    PScriptIntegrity -> 10
    PCek -> 11
    PValueAndMint -> 12
    PLedgerDelta -> 13
    PTerminal -> 14

-- | Aiken @validation_trace_v1.verdict_code@.
pverdictCode :: forall (s :: S). Term s (PValidationVerdict :--> PInteger)
pverdictCode = phoistAcyclic $
  plam $ \verdict -> pmatch verdict $ \case
    PPending -> 0
    PAccepted -> 1
    PRejected -> 2

-- | Aiken @validation_trace_v1.source_kind_code@.
psourceKindCode :: forall (s :: S). Term s (PValidationSourceKind :--> PInteger)
psourceKindCode = phoistAcyclic $
  plam $ \sourceKind -> pmatch sourceKind $ \case
    PNormal -> 0
    PForced -> 1

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

{- | Aiken @validation_trace_v1.verdict_rejection_binding_is_valid@.

A rejection must say why and an acceptance must not pretend to. The zero hash is
the sentinel for "no code", so the check is an exact-32-bytes test plus an
equality in one direction and a disequality in the other.
-}
pverdictRejectionBindingIsValid ::
  forall (s :: S). Term s (PAsData PValidationVerdict) -> Term s PByteString -> Term s PBool
pverdictRejectionBindingIsValid verdict rejectionCodeHash =
  plengthBS
    # rejectionCodeHash
    #== 32
    #&& pif
      (verdict #== pdata (pcon PRejected))
      (pnot # (rejectionCodeHash #== pnoRejectionCodeHash))
      (rejectionCodeHash #== pnoRejectionCodeHash)

-- | Aiken @validation_trace_v1.machine_state_is_well_formed@.
pmachineStateIsWellFormed ::
  forall (s :: S). Term s (PValidationMachineStateV1 :--> PBool)
pmachineStateIsWellFormed = phoistAcyclic $
  plam $ \state -> pmatch state $
    \( PValidationMachineStateV1
        { pmachineState'machineVersion
        , pmachineState'eventKeyHash
        , pmachineState'transactionId
        , pmachineState'transactionCommitment
        , pmachineState'validationContextHash
        , pmachineState'priorLedgerRoot
        , pmachineState'programCounter
        , pmachineState'workRoot
        , pmachineState'executionCpu
        , pmachineState'executionMemory
        , pmachineState'verdict
        , pmachineState'rejectionCodeHash
        , pmachineState'ledgerDeltaRoot
        }
      ) ->
        plet (pfromData pmachineState'programCounter) $ \programCounter ->
          pand'List
            [ pfromData pmachineState'machineVersion #== pmachineVersion
            , pis32 pmachineState'eventKeyHash
            , pis32 pmachineState'transactionId
            , pis32 pmachineState'transactionCommitment
            , pis32 pmachineState'validationContextHash
            , pis32 pmachineState'priorLedgerRoot
            , programCounter #>= 0
            , programCounter #<= pmaxMachineStepCount
            , pis32 pmachineState'workRoot
            , pfromData pmachineState'executionCpu #>= 0
            , pfromData pmachineState'executionMemory #>= 0
            , pverdictRejectionBindingIsValid
                pmachineState'verdict
                (pfromData pmachineState'rejectionCodeHash)
            , pis32 pmachineState'ledgerDeltaRoot
            ]

{- | Aiken @validation_trace_v1.descriptor_is_well_formed@.

The state's checks plus one more: a descriptor's verdict may not be @Pending@.
A descriptor summarises a /finished/ run, so an unfinished one has no descriptor
to commit.
-}
pdescriptorIsWellFormed ::
  forall (s :: S). Term s (PValidationTraceDescriptorV1 :--> PBool)
pdescriptorIsWellFormed = phoistAcyclic $
  plam $ \descriptor -> pmatch descriptor $
    \( PValidationTraceDescriptorV1
        { pdescriptor'schemaVersion
        , pdescriptor'machineVersion
        , pdescriptor'traceRoot
        , pdescriptor'stepCount
        , pdescriptor'initialStateHash
        , pdescriptor'terminalStateHash
        , pdescriptor'verdict
        , pdescriptor'rejectionCodeHash
        }
      ) ->
        plet (pfromData pdescriptor'stepCount) $ \stepCount ->
          pand'List
            [ pfromData pdescriptor'schemaVersion #== pdescriptorVersion
            , pfromData pdescriptor'machineVersion #== pmachineVersion
            , stepCount #>= 0
            , stepCount #<= pmaxMachineStepCount
            , pnot # (pdescriptor'verdict #== pdata (pcon PPending))
            , pverdictRejectionBindingIsValid
                pdescriptor'verdict
                (pfromData pdescriptor'rejectionCodeHash)
            , pis32 pdescriptor'traceRoot
            , pis32 pdescriptor'initialStateHash
            , pis32 pdescriptor'terminalStateHash
            ]

pis32 :: forall (s :: S). Term s (PAsData PByteString) -> Term s PBool
pis32 bytes = plengthBS # pfromData bytes #== 32

--------------------------------------------------------------------------------
-- Encoding and hashing
--------------------------------------------------------------------------------

-- | Aiken @validation_trace_v1.encode_h32@ — @58 20@ then the digest.
pencodeH32 :: forall (s :: S). Term s PByteString -> Term s PByteString
pencodeH32 bytes =
  pif (plengthBS # bytes #== 32) (pconstant "\x58\x20" <> bytes) perror

-- | Aiken's @cbor.serialise@ over an integer.
pcborInt :: forall (s :: S). Term s PInteger -> Term s PByteString
pcborInt n = pserialiseData # pforgetData (pdata n)

{- | Aiken @validation_trace_v1.encode_machine_state@.

A definite fifteen-element array, @8f@, in field order. The three enums encode as
their /codes/, not their constructor tags: 'pphaseCode', 'pverdictCode' and
'psourceKindCode' are the wire format, and only the accident that the phase codes
match its tags hides how independent the two are.

The preconditions are re-asserted here rather than assumed, because this is the
function whose output gets hashed and committed — an encoder that quietly
serialised a malformed state would put a commitment on chain to something no
consumer can validate.
-}
pencodeMachineState ::
  forall (s :: S). Term s (PValidationMachineStateV1 :--> PByteString)
pencodeMachineState = phoistAcyclic $
  plam $ \state -> pmatch state $
    \( PValidationMachineStateV1
        { pmachineState'machineVersion
        , pmachineState'eventKeyHash
        , pmachineState'transactionId
        , pmachineState'transactionCommitment
        , pmachineState'validationContextHash
        , pmachineState'sourceKind
        , pmachineState'priorLedgerRoot
        , pmachineState'phase
        , pmachineState'programCounter
        , pmachineState'workRoot
        , pmachineState'executionCpu
        , pmachineState'executionMemory
        , pmachineState'verdict
        , pmachineState'rejectionCodeHash
        , pmachineState'ledgerDeltaRoot
        }
      ) ->
        plet (pfromData pmachineState'programCounter) $ \programCounter ->
          pif
            ( pfromData pmachineState'machineVersion
                #== pmachineVersion
                #&& programCounter
                #>= 0
                #&& programCounter
                #<= pmaxMachineStepCount
                #&& pfromData pmachineState'executionCpu
                #>= 0
                #&& pfromData pmachineState'executionMemory
                #>= 0
                #&& pverdictRejectionBindingIsValid
                  pmachineState'verdict
                  (pfromData pmachineState'rejectionCodeHash)
            )
            ( pconstant "\x8f"
                <> pcborInt (pfromData pmachineState'machineVersion)
                <> pencodeH32 (pfromData pmachineState'eventKeyHash)
                <> pencodeH32 (pfromData pmachineState'transactionId)
                <> pencodeH32 (pfromData pmachineState'transactionCommitment)
                <> pencodeH32 (pfromData pmachineState'validationContextHash)
                <> pcborInt (psourceKindCode # pfromData pmachineState'sourceKind)
                <> pencodeH32 (pfromData pmachineState'priorLedgerRoot)
                <> pcborInt (pphaseCode # pfromData pmachineState'phase)
                <> pcborInt programCounter
                <> pencodeH32 (pfromData pmachineState'workRoot)
                <> pcborInt (pfromData pmachineState'executionCpu)
                <> pcborInt (pfromData pmachineState'executionMemory)
                <> pcborInt (pverdictCode # pfromData pmachineState'verdict)
                <> pencodeH32 (pfromData pmachineState'rejectionCodeHash)
                <> pencodeH32 (pfromData pmachineState'ledgerDeltaRoot)
            )
            perror

-- | Aiken @validation_trace_v1.hash_machine_state@.
phashMachineState ::
  forall (s :: S). Term s (PValidationMachineStateV1 :--> PByteString)
phashMachineState = phoistAcyclic $
  plam $ \state -> pblake2b_256 #$ pstateHashDomain <> (pencodeMachineState # state)

{- | Aiken @validation_trace_v1.hash_work_witness@.

Note the witness bytes are @cbor.serialise@d rather than concatenated raw, so the
hash commits to a length-delimited byte string. Concatenating them would let a
short witness plus a long program counter collide with a long witness plus a
short one.
-}
phashWorkWitness ::
  forall (s :: S).
  Term s (PValidationPhase :--> PInteger :--> PByteString :--> PByteString)
phashWorkWitness = phoistAcyclic $
  plam $ \phase programCounter witnessCbor ->
    pif
      (programCounter #>= 0 #&& programCounter #<= pmaxMachineStepCount)
      ( pblake2b_256
          #$ pworkWitnessDomain
          <> pconstant "\x83"
          <> pcborInt (pphaseCode # phase)
          <> pcborInt programCounter
          <> (pserialiseData # pforgetData (pdata witnessCbor))
      )
      perror

-- | Aiken @validation_trace_v1.hash_rejection_code@.
phashRejectionCode :: forall (s :: S). Term s (PByteString :--> PByteString)
phashRejectionCode = phoistAcyclic $
  plam $ \rejectionCode -> pblake2b_256 #$ prejectionCodeDomain <> rejectionCode

-- | Aiken @validation_trace_v1.hash_validation_context@.
phashValidationContext :: forall (s :: S). Term s (PByteString :--> PByteString)
phashValidationContext = phoistAcyclic $
  plam $ \contextCbor -> pblake2b_256 #$ pvalidationContextDomain <> contextCbor

-- | Aiken @validation_trace_v1.hash_ledger_delta@.
phashLedgerDelta :: forall (s :: S). Term s (PByteString :--> PByteString)
phashLedgerDelta = phoistAcyclic $
  plam $ \deltaCbor -> pblake2b_256 #$ pledgerDeltaDomain <> deltaCbor

-- | Aiken @validation_trace_v1.encode_descriptor@ — a definite eight-element array.
pencodeDescriptor ::
  forall (s :: S). Term s (PValidationTraceDescriptorV1 :--> PByteString)
pencodeDescriptor = phoistAcyclic $
  plam $ \descriptor -> pmatch descriptor $
    \( PValidationTraceDescriptorV1
        { pdescriptor'schemaVersion
        , pdescriptor'machineVersion
        , pdescriptor'traceRoot
        , pdescriptor'stepCount
        , pdescriptor'initialStateHash
        , pdescriptor'terminalStateHash
        , pdescriptor'verdict
        , pdescriptor'rejectionCodeHash
        }
      ) ->
        plet (pfromData pdescriptor'stepCount) $ \stepCount ->
          pif
            ( pfromData pdescriptor'schemaVersion
                #== pdescriptorVersion
                #&& pfromData pdescriptor'machineVersion
                #== pmachineVersion
                #&& stepCount
                #>= 0
                #&& stepCount
                #<= pmaxMachineStepCount
                #&& (pnot # (pdescriptor'verdict #== pdata (pcon PPending)))
                #&& pverdictRejectionBindingIsValid
                  pdescriptor'verdict
                  (pfromData pdescriptor'rejectionCodeHash)
            )
            ( pconstant "\x88"
                <> pcborInt (pfromData pdescriptor'schemaVersion)
                <> pcborInt (pfromData pdescriptor'machineVersion)
                <> pencodeH32 (pfromData pdescriptor'traceRoot)
                <> pcborInt stepCount
                <> pencodeH32 (pfromData pdescriptor'initialStateHash)
                <> pencodeH32 (pfromData pdescriptor'terminalStateHash)
                <> pcborInt (pverdictCode # pfromData pdescriptor'verdict)
                <> pencodeH32 (pfromData pdescriptor'rejectionCodeHash)
            )
            perror

--------------------------------------------------------------------------------
-- The trace tree
--------------------------------------------------------------------------------

{- | Aiken @validation_trace_v1.trace_leaf_hash@.

Leaves and branches use __different__ domains, which is what stops an internal
node being presented as a leaf — the classic second-preimage attack on a Merkle
tree whose two node kinds hash alike.
-}
ptraceLeafHash :: forall (s :: S). Term s (PByteString :--> PByteString)
ptraceLeafHash = phoistAcyclic $
  plam $ \stateHash ->
    pif
      (plengthBS # stateHash #== 32)
      (pblake2b_256 #$ ptraceLeafDomain <> stateHash)
      perror

-- | Aiken @validation_trace_v1.trace_branch_hash@.
ptraceBranchHash ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
ptraceBranchHash = phoistAcyclic $
  plam $ \left right ->
    pif
      (plengthBS # left #== 32 #&& plengthBS # right #== 32)
      (pblake2b_256 #$ ptraceBranchDomain <> left <> right)
      perror

{- | Aiken @validation_trace_v1.trace_depth@.

The depth of a balanced tree holding @step_count + 1@ states — one more than the
steps, because a run of @n@ steps passes through @n + 1@ states.

Computed by doubling rather than by a logarithm, which is also what makes the
'pmaxMachineStepCount' bound necessary: without it the loop has no termination
argument a reader can check.
-}
ptraceDepth :: forall (s :: S). Term s (PInteger :--> PInteger)
ptraceDepth = phoistAcyclic $
  plam $ \stepCount ->
    pif
      (stepCount #>= 0 #&& stepCount #<= pmaxMachineStepCount)
      (ptraceDepthLoop # (stepCount + 1) # 1 # 0)
      perror

ptraceDepthLoop ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
ptraceDepthLoop = phoistAcyclic $
  pfix $ \self -> plam $ \stateCount capacity depth ->
    pif (capacity #>= stateCount) depth (self # stateCount # (capacity * 2) # (depth + 1))

{- | Aiken @validation_trace_v1.fold_trace_path@.

Walks a Merkle path upward, taking the index's low bit at each level to decide
which side the sibling goes on. The index is halved as it climbs, which is the
same walk read from the other end.
-}
pfoldTracePath ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PByteString)
        :--> PInteger
        :--> PByteString
        :--> PByteString
    )
pfoldTracePath = phoistAcyclic $
  pfix $ \self -> plam $ \siblings index hash ->
    pelimList
      ( \sibling rest ->
          plet (pfromData sibling) $ \siblingHash ->
            pif
              (plengthBS # siblingHash #== 32)
              ( plet
                  ( pif
                      (pmod # index # 2 #== 0)
                      (ptraceBranchHash # hash # siblingHash)
                      (ptraceBranchHash # siblingHash # hash)
                  )
                  $ \parent -> self # rest # (pdiv # index # 2) # parent
              )
              perror
      )
      hash
      siblings

{- | Aiken @validation_trace_v1.verify_trace_proof@.

A state is in a trace when the path from it reproduces the descriptor's root, and
when the descriptor itself is well formed, and when the index names a state the
trace has.

The sibling-count check is the one that is easy to leave out and expensive to
omit: a path shorter than the tree's depth would fold to a subtree root and
compare it against the whole tree's, which for a one-state trace is the same
value.
-}
pverifyTraceProof ::
  forall (s :: S).
  Term s (PValidationTraceDescriptorV1 :--> PValidationTraceProof :--> PBool)
pverifyTraceProof = phoistAcyclic $
  plam $ \descriptor proof -> pmatch descriptor $
    \(PValidationTraceDescriptorV1 {pdescriptor'traceRoot, pdescriptor'stepCount}) ->
      pmatch proof $
        \( PValidationTraceProof
            {ptraceProof'stateIndex, ptraceProof'stateHash, ptraceProof'siblings}
          ) ->
            plet (pfromData ptraceProof'stateIndex) $ \stateIndex ->
              plet (pfromData ptraceProof'siblings) $ \siblings ->
                pdescriptorIsWellFormed
                  # descriptor
                  #&& stateIndex
                  #>= 0
                  #&& stateIndex
                  #<= pfromData pdescriptor'stepCount
                  #&& plengthBS
                  # pfromData ptraceProof'stateHash
                  #== 32
                  #&& plength
                  # siblings
                  #== (ptraceDepth # pfromData pdescriptor'stepCount)
                  #&& ( pfoldTracePath
                          # siblings
                          # stateIndex
                          # (ptraceLeafHash # pfromData ptraceProof'stateHash)
                      )
                  #== pfromData pdescriptor'traceRoot
