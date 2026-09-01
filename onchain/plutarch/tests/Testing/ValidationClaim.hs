{-# LANGUAGE OverloadedStrings #-}

{- | Behavioural tests for "Midgard.ValidationClaim" and
"Midgard.ValidationMachine".

Everything the port is compared against here is built from the format, not from
the port: the nine-element scan witness, the fifteen-field state encoding, the
five hash domains, the counted roots and the two-leaf trace tree all have
independent reference implementations below.

=== The fixture is a whole claim, not a mock

A claim's job is to tie four block-committed trees together, so the only fixture
that exercises it is one where all four are real: a real native transaction whose
compact triple re-derives its own id, real single-entry MPF roots under real
counted commitments, a real depth-1 trace tree with real leaf and branch hashes,
and a header whose roots are /derived from/ the claim rather than asserted
alongside it.

That derivation is what makes the negative cases sharp. Perturbing a leaf
re-derives the root it sits under, so membership still holds and only the
cross-tie under test fails — which is the failure mode a fraud proof actually
faces, and the one a hand-written "wrong root" fixture cannot reach.

Three positives are carried: a normal L2 transaction, a forced transaction the
operator accepted, and a forced transaction the operator rejected. The third is
not redundant. It is the only one that exercises the verdict binding in its
non-trivial direction, the only one carrying a real rejection code hash, and the
only one where the transition step's two ledger roots have to agree.
-}
module Testing.ValidationClaim (tests, acceptedMismatchProofFixture) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Aiken.Cbor (pdeserialise)
import Midgard.LedgerState (PHeaderV1, PMidgardTxValidity, PNativeTxProofSourceV1)
import Midgard.FraudProofs.TransitionTrace.Proof (
  PTransitionFaultProof,
  pvalidateAcceptedTransactionFaultProof,
 )
import Midgard.ValidationClaim (
  PValidationClaimWitnessV1,
  PValidationSourceMembershipV1,
  pcommittedClaimEndpointsAndSourceAreValid,
  pcommittedClaimIsValid,
  pcommittedClaimSourceIsAuthenticated,
  pcommittedClaimStructureIsValid,
  pforcedVerdictMatches,
  pimmutableContextMatches,
  pinitialWorkRootIsExact,
  pphaseTagForEventKeyTag,
  psourceIsForced,
  psourceProofCommitment,
  pvalidationSourceMembershipFromData,
  pvalidationContextIsExact,
 )
import Midgard.ValidationMachine (pencodeTransactionFieldScanWitness)
import Midgard.ValidationTrace (PValidationMachineStateV1, PValidationVerdict)

import Testing.Eval (passertEval, pfails)
import Testing.FraudProofsFixture (
  blake2b256,
  blake2b224,
  cborInt,
  commitCountedRoot,
  compactOf,
  serialise,
  singleEntryPhasRoot,
  tx1,
  txIdOf,
  witnessSetCborOf,
 )

--------------------------------------------------------------------------------
-- Reference encoders
--------------------------------------------------------------------------------

-- | A CBOR byte string with a definite header.
definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes b
  | n <= 23 = BS.cons (0x40 + fromIntegral n) b
  | n <= 255 = BS.cons 0x58 (BS.cons (fromIntegral n) b)
  | otherwise =
      BS.pack [0x59, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)] <> b
  where
    n = BS.length b

-- | A 32-byte hash as a definite CBOR byte string.
h32 :: BS.ByteString -> BS.ByteString
h32 b = "\x58\x20" <> b

-- | The five hash domains this module needs, spelled out rather than imported.
stateDomain, leafDomain, branchDomain, workDomain, contextDomain :: BS.ByteString
stateDomain = "MidgardValidationMachineStateV1"
leafDomain = "MidgardValidationTraceLeafV1"
branchDomain = "MidgardValidationTraceBranchV1"
workDomain = "MidgardValidationWorkWitnessV1"
contextDomain = "MidgardValidationContextV1"

-- | Aiken @validation_machine_v1.encode_transaction_field_scan_witness@.
scanWitness ::
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  BS.ByteString
scanWitness compact ws lengths ctx fieldIx itemIx chunkIx itemCount encodedLen =
  BS.concat
    [ "\x89"
    , definiteBytes compact
    , definiteBytes ws
    , definiteBytes lengths
    , definiteBytes ctx
    , cborInt fieldIx
    , cborInt itemIx
    , cborInt chunkIx
    , cborInt itemCount
    , cborInt encodedLen
    ]

{- | Aiken @validation_trace_v1.hash_work_witness@.

The witness bytes go in under @cbor.serialise@, /not/ under the definite-byte
encoder the witness itself is built with. The two differ on anything longer than
64 bytes: @serialiseData@ chunks a long byte string into an indefinite-length
sequence of 64-byte segments, while @encode_definite_bytes@ emits one definite
header. Both appear in the same expression when a scan witness is hashed, and a
scan witness is always longer than 64 bytes, so getting them the same way round
is not optional.
-}
workWitnessHash :: Integer -> Integer -> BS.ByteString -> BS.ByteString
workWitnessHash phaseCode counter witnessCbor =
  blake2b256 $
    BS.concat
      [workDomain, "\x83", cborInt phaseCode, cborInt counter, serialise (PD.B witnessCbor)]

-- | Aiken @validation_trace_v1.hash_validation_context@.
contextHash :: BS.ByteString -> BS.ByteString
contextHash cbor = blake2b256 (contextDomain <> cbor)

-- | Aiken @validation_trace_v1.trace_leaf_hash@ and @trace_branch_hash@.
leafHash :: BS.ByteString -> BS.ByteString
leafHash h = blake2b256 (leafDomain <> h)

branchHash :: BS.ByteString -> BS.ByteString -> BS.ByteString
branchHash l r = blake2b256 (branchDomain <> l <> r)

{- | Aiken @validation_trace_v1.encode_machine_state@ and @hash_machine_state@.

Read positionally off the state's @Data@, which is how a claim's witness carries
it. Doing it this way rather than from a Haskell record keeps the encoder honest
about field order: a state whose fields are permuted encodes differently here
too, instead of being silently normalised by a record type.
-}
stateHash :: PD.Data -> BS.ByteString
stateHash (PD.Constr _ fs) =
  blake2b256 . (stateDomain <>) . BS.concat $
    [ "\x8f"
    , cborInt (int (fs !! 0))
    , h32 (bytes (fs !! 1))
    , h32 (bytes (fs !! 2))
    , h32 (bytes (fs !! 3))
    , h32 (bytes (fs !! 4))
    , cborInt (tag (fs !! 5))
    , h32 (bytes (fs !! 6))
    , cborInt (tag (fs !! 7))
    , cborInt (int (fs !! 8))
    , h32 (bytes (fs !! 9))
    , cborInt (int (fs !! 10))
    , cborInt (int (fs !! 11))
    , cborInt (tag (fs !! 12))
    , h32 (bytes (fs !! 13))
    , h32 (bytes (fs !! 14))
    ]
  where
    int (PD.I n) = n
    int d = error ("stateHash: expected an integer, got " <> show d)
    bytes (PD.B b) = b
    bytes d = error ("stateHash: expected bytes, got " <> show d)
    tag (PD.Constr t []) = t
    tag d = error ("stateHash: expected a nullary constructor, got " <> show d)
stateHash d = error ("stateHash: expected a constructor, got " <> show d)

--------------------------------------------------------------------------------
-- Terms from data
--------------------------------------------------------------------------------

asData :: forall s. PD.Data -> Term s PData
asData = pconstant @PData

fromData :: forall (a :: S -> Type) s. (PIsData a) => PD.Data -> Term s a
fromData d = pfromData (punsafeCoerce (asData d))

asDataTerm :: forall (a :: S -> Type) s. PD.Data -> Term s (PAsData a)
asDataTerm d = punsafeCoerce (asData d)

{- | A verifier that /refuses/ — returns False — as against one that aborts.

Reaching for 'pfails' where a check returns False would pass for the wrong
reason, and a claim's checks do both: an @expect@ in the Aiken aborts, a failed
conjunct returns False, and which one a given perturbation triggers is part of
what these tests pin.
-}
prefuses :: (forall s. Term s PBool) -> Assertion
prefuses p = passertEval (pnot # p)

--------------------------------------------------------------------------------
-- The transaction the claim is about
--------------------------------------------------------------------------------

txId, compactCbor, wsCbor, lengthsCbor :: BS.ByteString
txId = txIdOf tx1
compactCbor = compactOf tx1
wsCbor = witnessSetCborOf tx1

{- | Nine zero lengths. The proof-source verifier only requires the lengths to
re-encode to the bytes supplied; nothing in a claim reads their values.
-}
lengthsCbor = BS.concat ("\x89" : replicate 9 (cborInt 0))

sourceData :: PD.Data
sourceData = PD.Constr 0 [PD.B compactCbor, PD.B wsCbor, PD.B lengthsCbor]

-- | Aiken @compact.native_tx_proof_commitment_v1@.
proofCommitment :: BS.ByteString
proofCommitment =
  blake2b256 . BS.concat $
    [ "MidgardNativeTxProofSourceV1"
    , cborInt 1
    , "\x83"
    , definiteBytes compactCbor
    , definiteBytes wsCbor
    , definiteBytes lengthsCbor
    ]

--------------------------------------------------------------------------------
-- The validation context
--------------------------------------------------------------------------------

profileId :: BS.ByteString
profileId = "midgard-consensus-v1"

endTime, networkId, minFeeA, minFeeB, blockSlot :: Integer
endTime = 200
networkId = 0
minFeeA = 44
minFeeB = 155381
blockSlot = 77

contextCborOf :: [Integer] -> BS.ByteString
contextCborOf [e, n, a, b, s] =
  serialise (PD.List [PD.I 1, PD.B profileId, PD.I e, PD.I n, PD.I a, PD.I b, PD.I s])
contextCborOf _ = error "contextCborOf: expected five header parameters"

contextCbor :: BS.ByteString
contextCbor = contextCborOf [endTime, networkId, minFeeA, minFeeB, blockSlot]

--------------------------------------------------------------------------------
-- The claim
--------------------------------------------------------------------------------

preRoot, postRoot, deltaRoot, zeros32, rejectionHash :: BS.ByteString
preRoot = BS.replicate 32 0x51
postRoot = BS.replicate 32 0x52
deltaRoot = BS.replicate 32 0x53
zeros32 = BS.replicate 32 0x00
rejectionHash = blake2b256 "MidgardValidationRejectCodeV1fee-too-low"

{- | A claim, in the pieces a test wants to move independently.

The header is derived from these fields rather than carried beside them, so a
test that perturbs a leaf still gets a header committing to the perturbed leaf.
-}
data Claim = Claim
  { cVersion :: Integer
  , cEventKey :: PD.Data
  , cDescriptor :: PD.Data
  , cStepKey :: PD.Data
  , cStep :: PD.Data
  , cEventToStep :: PD.Data
  , cSourceTag :: Integer
  -- ^ 0 = forced, 1 = normal.
  , cSourceKey :: PD.Data
  , cSourceKeyBytes :: BS.ByteString
  -- ^ How the key enters the tree: raw for an L2 id, CBOR for an order id.
  , cSourceValue :: PD.Data
  , cContextCbor :: BS.ByteString
  , cInitial :: PD.Data
  , cTerminal :: PD.Data
  , cInitialProof :: PD.Data
  , cTerminalProof :: PD.Data
  , cHeaderParams :: [Integer]
  -- ^ end time, network id, min fee a, min fee b, block slot.
  , cProtocolVersion :: Integer
  , cHeaderUtxosRoot :: BS.ByteString
  }

--------------------------------------------------------------------------------
-- Claim builders
--------------------------------------------------------------------------------

{- | Build a claim from its event key, source arm and verdict.

The three positives differ only in these three inputs, which is the point: the
machinery being exercised is identical, and what changes is which tree the source
came out of and what the operator said about it.
-}
buildClaim :: Integer -> PD.Data -> PD.Data -> BS.ByteString -> PD.Data -> Integer -> Claim
buildClaim sourceTag eventKey sourceKey sourceKeyBytes sourceValue verdict =
  Claim
    { cVersion = 1
    , cEventKey = eventKey
    , cDescriptor = descriptor
    , cStepKey = PD.I 0
    , cStep = step
    , cEventToStep = eventToStep
    , cSourceTag = sourceTag
    , cSourceKey = sourceKey
    , cSourceKeyBytes = sourceKeyBytes
    , cSourceValue = sourceValue
    , cContextCbor = contextCbor
    , cInitial = initial
    , cTerminal = terminal
    , cInitialProof = PD.Constr 0 [PD.I 0, PD.B iHash, PD.List [PD.B (leafHash tHash)]]
    , cTerminalProof = PD.Constr 0 [PD.I 1, PD.B tHash, PD.List [PD.B (leafHash iHash)]]
    , cHeaderParams = [endTime, networkId, minFeeA, minFeeB, blockSlot]
    , cProtocolVersion = 1
    , cHeaderUtxosRoot = BS.replicate 32 0x02
    }
  where
    rejected = verdict == 2
    sourceKind = if sourceTag == 0 then 1 else 0 -- Forced 1, Normal 0
    phaseTag = eventKeyPhase eventKey
    eventKeyHash = blake2b256 (serialise eventKey)
    workRoot =
      workWitnessHash 0 0 $
        scanWitness compactCbor wsCbor lengthsCbor contextCbor 0 0 0 (-1) 0
    rejection = if rejected then rejectionHash else zeros32
    stateOf phase counter verdict' rejection' =
      PD.Constr
        0
        [ PD.I 1
        , PD.B eventKeyHash
        , PD.B txId
        , PD.B proofCommitment
        , PD.B (contextHash contextCbor)
        , PD.Constr sourceKind []
        , PD.B preRoot
        , PD.Constr phase []
        , PD.I counter
        , PD.B (if phase == 0 then workRoot else BS.replicate 32 0x60)
        , PD.I 0
        , PD.I 0
        , PD.Constr verdict' []
        , PD.B rejection'
        , PD.B deltaRoot
        ]
    initial = stateOf 0 0 0 zeros32
    terminal = stateOf 14 1 verdict rejection
    iHash = stateHash initial
    tHash = stateHash terminal
    descriptor =
      PD.Constr
        0
        [ PD.I 1
        , PD.I 1
        , PD.B (branchHash (leafHash iHash) (leafHash tHash))
        , PD.I 1
        , PD.B iHash
        , PD.B tHash
        , PD.Constr verdict []
        , PD.B rejection
        ]
    step =
      PD.Constr
        0
        [ PD.I 1
        , PD.I 0
        , eventKey
        , PD.Constr phaseTag []
        , PD.B preRoot
        , PD.B (if rejected then preRoot else postRoot)
        ]
    eventToStep = PD.Constr 0 [PD.I 0, PD.Constr phaseTag []]

-- | The phase an event key belongs to — the identity on tags, spelled out.
eventKeyPhase :: PD.Data -> Integer
eventKeyPhase (PD.Constr t _) = t
eventKeyPhase d = error ("eventKeyPhase: expected a constructor, got " <> show d)

txOrderId :: PD.Data
txOrderId = PD.Constr 0 [PD.B (BS.replicate 32 0x71), PD.I 3]

normalClaim :: Claim
normalClaim =
  buildClaim
    1
    (PD.Constr 2 [PD.B txId])
    (PD.B txId)
    txId
    (PD.Constr 0 [PD.B txId, sourceData])
    1

{- | A normal L2 transaction whose descriptor claims a rejection.

Rebuilt rather than patched: the verdict is committed in the descriptor, in the
terminal state and in the trace root that both are hashed into, so moving it by
hand would break membership instead of the binding under test.
-}
normalRejectingClaim :: Claim
normalRejectingClaim =
  buildClaim
    1
    (PD.Constr 2 [PD.B txId])
    (PD.B txId)
    txId
    (PD.Constr 0 [PD.B txId, sourceData])
    2

forcedClaim :: Integer -> Integer -> Claim
forcedClaim validity verdict =
  buildClaim
    0
    (PD.Constr 1 [txOrderId])
    txOrderId
    (serialise txOrderId)
    (PD.Constr 0 [PD.B txId, sourceData, PD.Constr validity []])
    verdict

--------------------------------------------------------------------------------
-- Header and witness
--------------------------------------------------------------------------------

membership :: Integer -> BS.ByteString -> Integer -> PD.Data -> PD.Data -> PD.Data
membership domain keyBytes count key value =
  PD.Constr
    0
    [ PD.Constr domain []
    , PD.B (commitCountedRoot domain phas count)
    , PD.B phas
    , PD.I count
    , key
    , value
    , PD.List []
    ]
  where
    phas = singleEntryPhasRoot keyBytes (serialise value)

countedRootOf :: Integer -> BS.ByteString -> PD.Data -> Integer -> BS.ByteString
countedRootOf domain keyBytes value count =
  commitCountedRoot domain (singleEntryPhasRoot keyBytes (serialise value)) count

{- | The header a claim commits under: nine roots, seven counts, the block's time
and fee parameters, and the protocol version.

Roots 0, 1 and 5 are unused by a claim and are left as distinguishable filler;
the four a claim opens are derived from the claim's own leaves.
-}
headerData :: Claim -> PD.Data
headerData c =
  PD.Constr
    0
    [ PD.B (BS.replicate 32 0x01)
    , PD.B (cHeaderUtxosRoot c)
    , PD.B (BS.replicate 32 0x03)
    , PD.B forcedRoot
    , PD.B transactionsRoot
    , PD.B (BS.replicate 32 0x06)
    , PD.B (countedRootOf 4 (serialise (cStepKey c)) (cStep c) 1)
    , PD.B (countedRootOf 5 (serialise (cEventKey c)) (cEventToStep c) 1)
    , PD.B (countedRootOf 6 (serialise (cEventKey c)) (cDescriptor c) 1)
    , PD.I 0
    , PD.I forcedCount
    , PD.I l2Count
    , PD.I 0
    , PD.I 1
    , PD.I 1
    , PD.I 1
    , PD.I 100
    , PD.I (params !! 0)
    , PD.I (params !! 4)
    , PD.I (params !! 1)
    , PD.I (params !! 2)
    , PD.I (params !! 3)
    , PD.B (BS.replicate 28 0xaa)
    , PD.B (BS.replicate 28 0xbb)
    , PD.I (cProtocolVersion c)
    ]
  where
    params = cHeaderParams c
    isForced = cSourceTag c == 0
    forcedCount = if isForced then 1 else 0
    l2Count = if isForced then 0 else 1
    sourceRoot =
      countedRootOf
        (if isForced then 1 else 2)
        (cSourceKeyBytes c)
        (cSourceValue c)
        1
    forcedRoot = if isForced then sourceRoot else BS.replicate 32 0x04
    transactionsRoot = if isForced then BS.replicate 32 0x05 else sourceRoot

witnessData :: Claim -> PD.Data
witnessData c =
  PD.Constr
    0
    [ PD.I (cVersion c)
    , membership 6 (serialise (cEventKey c)) 1 (cEventKey c) (cDescriptor c)
    , membership 4 (serialise (cStepKey c)) 1 (cStepKey c) (cStep c)
    , membership 5 (serialise (cEventKey c)) 1 (cEventKey c) (cEventToStep c)
    , PD.Constr
        (cSourceTag c)
        [ membership
            (if cSourceTag c == 0 then 1 else 2)
            (cSourceKeyBytes c)
            1
            (cSourceKey c)
            (cSourceValue c)
        ]
    , PD.B (cContextCbor c)
    , cInitial c
    , cTerminal c
    , cInitialProof c
    , cTerminalProof c
    ]

headerT :: forall s. Claim -> Term s PHeaderV1
headerT = fromData . headerData

witnessT :: forall s. Claim -> Term s PValidationClaimWitnessV1
witnessT = fromData . witnessData

claimHolds :: forall s. Claim -> Term s PBool
claimHolds c = pcommittedClaimIsValid # headerT c # witnessT c

terminalAcceptanceWitness :: BS.ByteString -> BS.ByteString
terminalAcceptanceWitness root =
  "\x84\x01\x40" <> definiteBytes root <> definiteBytes "\x82\x00\x80"

acceptedMismatchClaim :: Integer -> Integer -> BS.ByteString -> BS.ByteString -> Claim
acceptedMismatchClaim sourceValidity transitionPhase claimedPostRoot terminalPostRoot =
  base
    { cDescriptor = descriptor
    , cStep = transitionStep
    , cEventToStep = eventToStep
    , cTerminal = terminal
    , cInitialProof = PD.Constr 0 [PD.I 0, PD.B initialHash, PD.List [PD.B terminalLeaf]]
    , cTerminalProof = PD.Constr 0 [PD.I 1, PD.B terminalHash, PD.List [PD.B initialLeaf]]
    , cHeaderUtxosRoot = claimedPostRoot
    }
  where
    base = forcedClaim sourceValidity 1
    witnessCbor = terminalAcceptanceWitness terminalPostRoot
    transitionStep = replaceConstrField 5 (PD.B claimedPostRoot) $
      replaceConstrField 3 (PD.Constr transitionPhase []) (cStep base)
    eventToStep = replaceConstrField 1 (PD.Constr transitionPhase []) (cEventToStep base)
    terminal = replaceConstrField 9 (PD.B $ workWitnessHash 14 1 witnessCbor) (cTerminal base)
    initialHash = stateHash (cInitial base)
    terminalHash = stateHash terminal
    initialLeaf = leafHash initialHash
    terminalLeaf = leafHash terminalHash
    descriptor =
      PD.Constr
        0
        [ PD.I 1
        , PD.I 1
        , PD.B (branchHash initialLeaf terminalLeaf)
        , PD.I 1
        , PD.B initialHash
        , PD.B terminalHash
        , PD.Constr 1 []
        , PD.B zeros32
        ]

replaceConstrField :: Int -> PD.Data -> PD.Data -> PD.Data
replaceConstrField index replacement (PD.Constr constructor fields) =
  PD.Constr constructor (take index fields <> [replacement] <> drop (index + 1) fields)
replaceConstrField _ _ value = error ("replaceConstrField: expected a constructor, got " <> show value)

acceptedMismatchProof :: Claim -> BS.ByteString -> PD.Data
acceptedMismatchProof claim terminalPostRoot =
  PD.Constr 0 [PD.B headerHash, header, PD.Constr 9 [witness]]
  where
    header = headerData claim
    headerHash = blake2b224 (serialise header)
    witness = PD.Constr 0 [witnessData claim, PD.B (terminalAcceptanceWitness terminalPostRoot)]

acceptedMismatchHolds :: forall s. Claim -> BS.ByteString -> Term s PBool
acceptedMismatchHolds claim terminalPostRoot =
  pvalidateAcceptedTransactionFaultProof
    (fromData @PTransitionFaultProof $ acceptedMismatchProof claim terminalPostRoot)
    (asDataTerm $ PD.B $ "\x00\x00\x00\x04" <> headerHash)
  where
    headerHash = blake2b224 (serialise $ headerData claim)

acceptedMismatchProofFixture :: (BS.ByteString, PD.Data)
acceptedMismatchProofFixture =
  ("\x00\x00\x00\x04" <> headerHash, acceptedMismatchProof claim deltaRoot)
  where
    claim = acceptedMismatchClaim 0 1 postRoot deltaRoot
    headerHash = blake2b224 (serialise $ headerData claim)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Validation Claim Tests"
    [ testGroup "the field-scan witness" scanWitnessTests
    , testGroup "the validation context" contextTests
    , testGroup "the initial work root" workRootTests
    , testGroup "the immutable context" immutableTests
    , testGroup "the forced verdict" verdictTests
    , testGroup "the phase map" phaseTests
    , testGroup "the source commitment" commitmentTests
    , testGroup "the source membership ABI" sourceMembershipAbiTests
    , testGroup "a whole claim" claimTests
    , testGroup "the cross-ties" tieTests
    , testGroup "the accepted-transaction transition mismatch" acceptedMismatchTests
    ]

sourceMembershipAbiTests :: [TestTree]
sourceMembershipAbiTests =
  [ testCase "validation_source_membership_v1_typescript_vectors_are_exact" $
      passertEval sourceMembershipAbiVectors
  , testCase "validation_source_membership_v1_rejects_adjacent_tag" $
      pfails $ forceSourceMembershipData $ PD.Constr 2 []
  , testCase "validation_source_membership_v1_rejects_wrong_arity" $
      pfails $ forceSourceMembershipData $ PD.Constr 0 []
  , testCase "validation_source_membership_v1_rejects_wrong_nesting" $
      pfails $ forceSourceMembershipData $ PD.Constr 0 [PD.Constr 0 []]
  , testCase "validation_source_membership_v1_rejects_swapped_kind" $
      pfails malformedSwappedSourceMembership
  , testCase "validation_source_membership_v1_rejects_malformed_proof_step" $
      pfails malformedSourceMembershipProofStep
  , testCase "validation_source_membership_v1_accepts_all_canonical_proof_step_shapes" $
      passertEval acceptsCanonicalSourceMembershipProofSteps
  ]

forceSourceMembershipData :: forall s. PD.Data -> Term s PUnit
forceSourceMembershipData dat =
  pmatch
    (pvalidationSourceMembershipFromData # pconstant @PData dat)
    (const $ pconstant ())

malformedSwappedSourceMembership :: forall s. Term s PUnit
malformedSwappedSourceMembership =
  pmatch (pdeserialise # phexByteStr "d8799fd8799fd87a80582011111111111111111111111111111111111111111111111111111111111111115820121212121212121212121212121212121212121212121212121212121212121201d8799f5820131313131313131313131313131313131313131313131313131313131313131304ffd8799f58201414141414141414141414141414141414141414141414141414141414141414d8799f428101428102428103ffd87c80ff80ffff") $ \case
    PNothing -> perror
    PJust dat ->
      pmatch (pasConstr # dat) $ \(PBuiltinPair _ fields) ->
        pmatch
          ( pvalidationSourceMembershipFromData
              # pforgetData (pconstrBuiltin # 1 # fields)
          )
          (const $ pconstant ())

malformedSourceMembershipProofStep :: forall s. Term s PUnit
malformedSourceMembershipProofStep =
  pmatch
    ( pvalidationSourceMembershipFromData
        # pconstant @PData malformedSourceMembershipProofData
    )
    (const $ pconstant ())

malformedSourceMembershipProofData :: PD.Data
malformedSourceMembershipProofData =
  case sourceMembershipData normalClaim of
    PD.Constr sourceTag [membershipData] ->
      PD.Constr sourceTag [replaceField 6 (PD.List [PD.Constr 3 []]) membershipData]
    value -> error ("malformedSourceMembershipProofData: unexpected source " <> show value)

acceptsCanonicalSourceMembershipProofSteps :: forall s. Term s PBool
acceptsCanonicalSourceMembershipProofSteps =
  pmatch
    ( pvalidationSourceMembershipFromData
        # pconstant @PData canonicalSourceMembershipProofData
    )
    (const $ pconstant True)

canonicalSourceMembershipProofData :: PD.Data
canonicalSourceMembershipProofData =
  case sourceMembershipData normalClaim of
    PD.Constr sourceTag [membershipData] ->
      PD.Constr sourceTag [replaceField 6 (PD.List proofSteps) membershipData]
    value -> error ("canonicalSourceMembershipProofData: unexpected source " <> show value)
  where
    proofSteps =
      [ PD.Constr 0 [PD.I 0, PD.B ""]
      , PD.Constr 1 [PD.I 1, PD.Constr 0 [PD.I 2, PD.B "bb", PD.B "cc"]]
      , PD.Constr 2 [PD.I 3, PD.B "key", PD.B "value"]
      ]

sourceMembershipAbiVectors :: forall s. Term s PBool
sourceMembershipAbiVectors =
  plet (phexByteStr "d8799fd8799fd87a80582011111111111111111111111111111111111111111111111111111111111111115820121212121212121212121212121212121212121212121212121212121212121201d8799f5820131313131313131313131313131313131313131313131313131313131313131304ffd8799f58201414141414141414141414141414141414141414141414141414141414141414d8799f428101428102428103ffd87c80ff80ffff") $ \forcedCbor ->
  plet (phexByteStr "d87a9fd8799fd87b8058202121212121212121212121212121212121212121212121212121212121212121582022222222222222222222222222222222222222222222222222222222222222220158202323232323232323232323232323232323232323232323232323232323232323d8799f58202323232323232323232323232323232323232323232323232323232323232323d8799f43820102428104428105ffff80ffff") $ \normalCbor ->
  pmatch (pdeserialise # forcedCbor) $ \case
    PNothing -> pconstant False
    PJust forcedData ->
      pmatch (pdeserialise # normalCbor) $ \case
        PNothing -> pconstant False
        PJust normalData ->
          plet (pvalidationSourceMembershipFromData # forcedData) $ \forced ->
          plet (pvalidationSourceMembershipFromData # normalData) $ \normal ->
            pand'List
              [ psourceIsForced (pdata forced)
              , pnot # psourceIsForced (pdata normal)
              , pserialiseData # forcedData #== forcedCbor
              , pserialiseData # normalData #== normalCbor
              , pblake2b_256
                  # ( pserialiseData
                        # (plistData # (pcons # forcedData #$ pcons # normalData # pnil))
                    )
                  #== phexByteStr "5cba4fd71994ac1a802cd3a6edcda14f6acc7be43f7c1993839a65665b856f32"
              ]

acceptedMismatchTests :: [TestTree]
acceptedMismatchTests =
  [ testCase "accepts_valid_forced_transaction_wrong_accepted_post_root_fault" $
      passertEval $
        acceptedMismatchHolds
          (acceptedMismatchClaim 0 1 postRoot deltaRoot)
          deltaRoot
  , testCase "rejects_valid_forced_transaction_matching_accepted_post_root_fault" $
      prefuses $
        acceptedMismatchHolds
          (acceptedMismatchClaim 0 1 postRoot postRoot)
          postRoot
  , testCase "rejects_invalid_forced_source_with_accepted_validation_claim" $
      prefuses $
        acceptedMismatchHolds
          (acceptedMismatchClaim 4 1 postRoot deltaRoot)
          deltaRoot
  , testCase "rejects_forced_event_with_l2_transition_phase_in_accepted_claim" $
      prefuses $
        acceptedMismatchHolds
          (acceptedMismatchClaim 0 2 postRoot deltaRoot)
          deltaRoot
  ]

--------------------------------------------------------------------------------

scanWitnessTests :: [TestTree]
scanWitnessTests =
  [ testCase "encodes as four byte strings and five integers" $
      passertEval $
        encoded 0 0 0 (-1) 0
          #== pconstant (scanWitness compactCbor wsCbor lengthsCbor contextCbor 0 0 0 (-1) 0)
  , testCase "…and at a position no claim opens at" $
      passertEval $
        encoded 8 17 3 9 4096
          #== pconstant (scanWitness compactCbor wsCbor lengthsCbor contextCbor 8 17 3 9 4096)
  , testCase "the item count's -1 sentinel is a negative CBOR integer" $
      passertEval $
        pconstant @PByteString
          (scanWitness compactCbor wsCbor lengthsCbor contextCbor 0 0 0 (-1) 0)
          #== pconstant @PByteString
            ( BS.concat
                [ "\x89"
                , definiteBytes compactCbor
                , definiteBytes wsCbor
                , definiteBytes lengthsCbor
                , definiteBytes contextCbor
                , "\x00"
                , "\x00"
                , "\x00"
                , "\x20"
                , "\x00"
                ]
            )
  , testCase "moving a field index changes the encoding" $
      passertEval $
        pnot # (encoded 0 0 0 (-1) 0 #== encoded 1 0 0 (-1) 0)
  , testCase "aborts on a negative field index" $
      pfails (encoded (-1) 0 0 (-1) 0)
  , testCase "aborts on a field index past the ninth" $
      pfails (encoded 9 0 0 (-1) 0)
  , testCase "accepts the ninth field" $
      passertEval $
        encoded 8 0 0 (-1) 0 #== encoded 8 0 0 (-1) 0
  , testCase "aborts on a negative item index" $
      pfails (encoded 0 (-1) 0 (-1) 0)
  , testCase "aborts on a negative chunk index" $
      pfails (encoded 0 0 (-1) (-1) 0)
  , testCase "aborts on an item count below the sentinel" $
      pfails (encoded 0 0 0 (-2) 0)
  , testCase "aborts on a negative encoded length" $
      pfails (encoded 0 0 0 (-1) (-1))
  ]
  where
    encoded f i c ic el =
      pencodeTransactionFieldScanWitness
        # pconstant compactCbor
        # pconstant wsCbor
        # pconstant lengthsCbor
        # pconstant contextCbor
        # pconstant f
        # pconstant i
        # pconstant c
        # pconstant ic
        # pconstant el

--------------------------------------------------------------------------------

contextTests :: [TestTree]
contextTests =
  [ testCase "accepts the context the header determines" $
      passertEval (contextOf normalClaim contextCbor)
  , testCase "refuses a context naming another end time" $
      prefuses (contextOf normalClaim (contextCborOf [201, networkId, minFeeA, minFeeB, blockSlot]))
  , testCase "refuses a context naming another network" $
      prefuses (contextOf normalClaim (contextCborOf [endTime, 1, minFeeA, minFeeB, blockSlot]))
  , testCase "refuses a context naming another fee coefficient" $
      prefuses (contextOf normalClaim (contextCborOf [endTime, networkId, 45, minFeeB, blockSlot]))
  , testCase "refuses a context naming another block slot" $
      prefuses (contextOf normalClaim (contextCborOf [endTime, networkId, minFeeA, minFeeB, 78]))
  , testCase "refuses a context under another profile id" $
      prefuses $
        contextOf
          normalClaim
          ( serialise
              ( PD.List
                  [ PD.I 1
                  , PD.B "midgard-consensus-v2"
                  , PD.I endTime
                  , PD.I networkId
                  , PD.I minFeeA
                  , PD.I minFeeB
                  , PD.I blockSlot
                  ]
              )
          )
  , testCase "refuses a context under another version" $
      prefuses $
        contextOf
          normalClaim
          ( serialise
              ( PD.List
                  [ PD.I 2
                  , PD.B profileId
                  , PD.I endTime
                  , PD.I networkId
                  , PD.I minFeeA
                  , PD.I minFeeB
                  , PD.I blockSlot
                  ]
              )
          )
  , testCase "refuses a context with a field appended" $
      prefuses $
        contextOf
          normalClaim
          ( serialise
              ( PD.List
                  [ PD.I 1
                  , PD.B profileId
                  , PD.I endTime
                  , PD.I networkId
                  , PD.I minFeeA
                  , PD.I minFeeB
                  , PD.I blockSlot
                  , PD.I 0
                  ]
              )
          )
  , testCase "refuses a header naming a network that is neither 0 nor 1" $
      prefuses $
        contextOf
          normalClaim {cHeaderParams = [endTime, 2, minFeeA, minFeeB, blockSlot]}
          (contextCborOf [endTime, 2, minFeeA, minFeeB, blockSlot])
  , testCase "refuses a header naming a negative fee coefficient" $
      prefuses $
        contextOf
          normalClaim {cHeaderParams = [endTime, networkId, -1, minFeeB, blockSlot]}
          (contextCborOf [endTime, networkId, -1, minFeeB, blockSlot])
  , testCase "refuses a header naming a negative second fee coefficient" $
      prefuses $
        contextOf
          normalClaim {cHeaderParams = [endTime, networkId, minFeeA, -1, blockSlot]}
          (contextCborOf [endTime, networkId, minFeeA, -1, blockSlot])
  , testCase "refuses a header naming a negative block slot" $
      prefuses $
        contextOf
          normalClaim {cHeaderParams = [endTime, networkId, minFeeA, minFeeB, -1]}
          (contextCborOf [endTime, networkId, minFeeA, minFeeB, -1])
  , testCase "refuses a state committing to another context" $
      prefuses $
        pvalidationContextIsExact
          (headerT normalClaim)
          (pconstant contextCbor)
          (stateWithField 4 (PD.B (BS.replicate 32 0x99)))
  ]
  where
    -- The state's own @validation_context_hash@ is rebound to whatever context
    -- is under test, so a case fails on the clause it names rather than on the
    -- hash binding as well. The one test that /does/ target the hash binding
    -- leaves it alone.
    contextOf c cbor =
      pvalidationContextIsExact
        (headerT c)
        (pconstant cbor)
        (fromData (replaceField 4 (PD.B (contextHash cbor)) (cInitial normalClaim)))

-- | The claim's initial state with one field replaced.
stateWithField :: forall s. Int -> PD.Data -> Term s PValidationMachineStateV1
stateWithField i v = fromData (replaceField i v (cInitial normalClaim))

replaceField :: Int -> PD.Data -> PD.Data -> PD.Data
replaceField i v (PD.Constr t fs) = PD.Constr t (take i fs <> [v] <> drop (i + 1) fs)
replaceField _ _ d = error ("replaceField: expected a constructor, got " <> show d)

--------------------------------------------------------------------------------

workRootTests :: [TestTree]
workRootTests =
  [ testCase "accepts the scan witness the opening phase starts from" $
      passertEval (workRootOf normalClaim (cInitial normalClaim))
  , testCase "…for a forced source too" $
      passertEval (workRootOf (forcedClaim 0 1) (cInitial (forcedClaim 0 1)))
  , testCase "refuses a work root over another context" $
      prefuses $
        pinitialWorkRootIsExact
          (fromData (cInitial normalClaim))
          (pconstant (contextCborOf [201, networkId, minFeeA, minFeeB, blockSlot]))
          (sourceMembershipT normalClaim)
  , testCase "refuses a work root the state simply asserts" $
      prefuses $
        pinitialWorkRootIsExact
          (stateWithField 9 (PD.B (BS.replicate 32 0x77)))
          (pconstant contextCbor)
          (sourceMembershipT normalClaim)
  , testCase "refuses a work root positioned at the second field" $
      prefuses $
        pinitialWorkRootIsExact
          ( stateWithField
              9
              ( PD.B
                  ( workWitnessHash 0 0 $
                      scanWitness compactCbor wsCbor lengthsCbor contextCbor 1 0 0 (-1) 0
                  )
              )
          )
          (pconstant contextCbor)
          (sourceMembershipT normalClaim)
  , testCase "refuses a work root hashed under another phase" $
      prefuses $
        pinitialWorkRootIsExact
          ( stateWithField
              9
              ( PD.B
                  ( workWitnessHash 1 0 $
                      scanWitness compactCbor wsCbor lengthsCbor contextCbor 0 0 0 (-1) 0
                  )
              )
          )
          (pconstant contextCbor)
          (sourceMembershipT normalClaim)
  , testCase "reads the same field out of either arm" $
      passertEval $
        pinitialWorkRootIsExact
          (fromData (cInitial normalClaim))
          (pconstant contextCbor)
          (sourceMembershipT (forcedClaim 0 1))
  ]
  where
    workRootOf c initial =
      pinitialWorkRootIsExact
        (fromData initial)
        (pconstant (cContextCbor c))
        (sourceMembershipT c)

sourceMembershipT :: forall s. Claim -> Term s (PAsData PValidationSourceMembershipV1)
sourceMembershipT = asDataTerm . sourceMembershipData

sourceMembershipData :: Claim -> PD.Data
sourceMembershipData c =
  PD.Constr
    (cSourceTag c)
    [ membership
        (if cSourceTag c == 0 then 1 else 2)
        (cSourceKeyBytes c)
        1
        (cSourceKey c)
        (cSourceValue c)
    ]

--------------------------------------------------------------------------------

immutableTests :: [TestTree]
immutableTests =
  testCase "the claim's own endpoints agree" (passertEval (matches (cTerminal normalClaim)))
    : [ testCase ("a " <> name <> " that changed does not") $
        prefuses (matches (replaceField i v (cTerminal normalClaim)))
      | (i, name, v) <-
          [ (0, "machine version", PD.I 2)
          , (1, "event-key hash", PD.B (BS.replicate 32 0x91))
          , (2, "transaction id", PD.B (BS.replicate 32 0x92))
          , (3, "transaction commitment", PD.B (BS.replicate 32 0x93))
          , (4, "validation-context hash", PD.B (BS.replicate 32 0x94))
          , (5, "source kind", PD.Constr 1 [])
          , (6, "prior ledger root", PD.B (BS.replicate 32 0x96))
          , (14, "ledger-delta root", PD.B (BS.replicate 32 0x97))
          ]
      ]
    <> [ testCase "a work root that changed does not break it" $
          passertEval $
            matches (replaceField 9 (PD.B (BS.replicate 32 0x98)) (cTerminal normalClaim))
       , testCase "…nor does a verdict that was reached" $
          passertEval $
            matches
              ( replaceField
                  13
                  (PD.B rejectionHash)
                  (replaceField 12 (PD.Constr 2 []) (cTerminal normalClaim))
              )
       ]
  where
    matches terminal =
      pimmutableContextMatches (fromData (cInitial normalClaim)) (fromData terminal)

--------------------------------------------------------------------------------

verdictTests :: [TestTree]
verdictTests =
  [ testCase "a valid transaction must have been accepted" $
      passertEval (matches 0 1)
  , testCase "…and must not have been rejected" $
      prefuses (matches 0 2)
  , testCase "…and must not be left pending" $
      prefuses (matches 0 0)
  ]
    <> [ testCase ("an operator verdict of " <> name <> " must have been rejected") $
        passertEval (matches validity 2)
       | (validity, name) <-
          [ (1, "non-existent input")
          , (2, "invalid signature")
          , (3, "failed script")
          , (4, "fee too low")
          , (5, "unbalanced")
          ]
       ]
    <> [ testCase ("…and " <> name <> " must not have been accepted") $
        prefuses (matches validity 1)
       | (validity, name) <-
          [ (1, "non-existent input")
          , (4, "fee too low")
          ]
       ]
  where
    matches :: forall s. Integer -> Integer -> Term s PBool
    matches validity verdict =
      pforcedVerdictMatches
        (asDataTerm @PMidgardTxValidity (PD.Constr validity []))
        (asDataTerm @PValidationVerdict (PD.Constr verdict []))

--------------------------------------------------------------------------------

phaseTests :: [TestTree]
phaseTests =
  [ testCase "each event class maps to its own phase" $
      passertEval $
        foldr
          (#&&)
          (pconstant True)
          [pphaseTagForEventKeyTag (pconstant t) #== pconstant t | t <- [0 .. 3]]
  , testCase "aborts on a fifth event class" $
      pfails (pphaseTagForEventKeyTag 4)
  , testCase "aborts on a negative tag" $
      pfails (pphaseTagForEventKeyTag (-1))
  ]

--------------------------------------------------------------------------------

commitmentTests :: [TestTree]
commitmentTests =
  [ testCase "derives the §7 proof-source commitment of its triple" $
      passertEval $
        psourceProofCommitment (fromData @PNativeTxProofSourceV1 sourceData)
          #== pconstant proofCommitment
  , testCase "another witness set gives another commitment" $
      passertEval $
        pnot
          # ( psourceProofCommitment
                ( fromData @PNativeTxProofSourceV1
                    (PD.Constr 0 [PD.B compactCbor, PD.B (wsCbor <> "\x00"), PD.B lengthsCbor])
                )
                #== pconstant proofCommitment
            )
  , testCase "another length vector gives another commitment" $
      passertEval $
        pnot
          # ( psourceProofCommitment
                ( fromData @PNativeTxProofSourceV1
                    ( PD.Constr
                        0
                        [ PD.B compactCbor
                        , PD.B wsCbor
                        , PD.B (BS.concat ("\x89" : cborInt 1 : replicate 8 (cborInt 0)))
                        ]
                    )
                )
                #== pconstant proofCommitment
            )
  ]

--------------------------------------------------------------------------------

claimTests :: [TestTree]
claimTests =
  [ testCase "accepts a normal L2 transaction's claim" $
      passertEval (claimHolds normalClaim)
  , testCase "accepts a forced transaction the operator accepted" $
      passertEval (claimHolds (forcedClaim 0 1))
  , testCase "accepts a forced transaction the operator rejected" $
      passertEval (claimHolds (forcedClaim 4 2))
  , testCase "the structural half holds on its own" $
      passertEval $
        pcommittedClaimStructureIsValid (headerT normalClaim) (witnessT normalClaim)
  , testCase "the source half holds on its own" $
      passertEval $
        pcommittedClaimSourceIsAuthenticated (headerT normalClaim) (witnessT normalClaim)
  , testCase "the endpoint half holds on its own" $
      passertEval $
        pcommittedClaimEndpointsAndSourceAreValid (headerT normalClaim) (witnessT normalClaim)
  , testCase "refuses a claim under another claim version" $
      prefuses (claimHolds normalClaim {cVersion = 2})
  , testCase "refuses a claim under another protocol version" $
      prefuses (claimHolds normalClaim {cProtocolVersion = 2})
  , testCase "refuses a forced claim whose descriptor accepts a rejected transaction" $
      prefuses (claimHolds (forcedClaim 4 1))
  , testCase "refuses a forced claim whose descriptor rejects an accepted transaction" $
      prefuses (claimHolds (forcedClaim 0 2))
  , testCase "refuses a normal claim whose descriptor rejects" $
      prefuses (claimHolds normalRejectingClaim)
  ]

--------------------------------------------------------------------------------

tieTests :: [TestTree]
tieTests =
  [ testCase "refuses a step whose index is not the key it is filed under" $
      prefuses (claimHolds normalClaim {cStep = replaceField 1 (PD.I 1) (cStep normalClaim)})
  , testCase "refuses a step under an unknown schema version" $
      prefuses (claimHolds normalClaim {cStep = replaceField 0 (PD.I 2) (cStep normalClaim)})
  , testCase "refuses a step recording another event" $
      prefuses $
        claimHolds
          normalClaim
            { cStep = replaceField 2 (PD.Constr 3 [txOrderId]) (cStep normalClaim)
            }
  , testCase "refuses a step in a phase the event does not belong to" $
      prefuses $
        claimHolds
          normalClaim {cStep = replaceField 3 (PD.Constr 3 []) (cStep normalClaim)}
  , testCase "refuses an event-to-step entry naming another step" $
      prefuses $
        claimHolds
          normalClaim {cEventToStep = replaceField 0 (PD.I 1) (cEventToStep normalClaim)}
  , testCase "refuses an event-to-step entry in another phase" $
      prefuses $
        claimHolds
          normalClaim
            { cEventToStep = replaceField 1 (PD.Constr 3 []) (cEventToStep normalClaim)
            }
  , testCase "refuses a machine that started from another ledger root" $
      prefuses $
        claimHolds
          normalClaim {cStep = replaceField 4 (PD.B (BS.replicate 32 0x5a)) (cStep normalClaim)}
  , testCase "refuses a rejected claim whose step still moved the ledger" $
      prefuses $
        claimHolds
          ( let c = forcedClaim 4 2
             in c {cStep = replaceField 5 (PD.B postRoot) (cStep c)}
          )
  , testCase "refuses a source membership keyed under another transaction" $
      prefuses $
        claimHolds
          normalClaim
            { cSourceValue = PD.Constr 0 [PD.B (BS.replicate 32 0x7f), sourceData]
            }
  , testCase "refuses a descriptor keyed by an event the step does not record" $
      prefuses $
        claimHolds
          normalClaim {cEventKey = PD.Constr 2 [PD.B (BS.replicate 32 0x7e)]}
  , testCase "aborts on a forced claim presented against the transactions tree" $
      pfails (claimHolds (forcedClaim 0 1) {cSourceTag = 1})
  , testCase "aborts on a normal claim presented against the forced tree" $
      pfails (claimHolds normalClaim {cSourceTag = 0})
  , testCase "refuses an initial state at a non-zero program counter" $
      prefuses $
        claimHolds normalClaim {cInitial = replaceField 8 (PD.I 1) (cInitial normalClaim)}
  , testCase "refuses an initial state in a later phase" $
      prefuses $
        claimHolds
          normalClaim {cInitial = replaceField 7 (PD.Constr 1 []) (cInitial normalClaim)}
  , testCase "refuses an initial state that already spent budget" $
      prefuses $
        claimHolds normalClaim {cInitial = replaceField 10 (PD.I 1) (cInitial normalClaim)}
  , testCase "refuses an initial state that already reached a verdict" $
      prefuses $
        claimHolds
          normalClaim {cInitial = replaceField 12 (PD.Constr 1 []) (cInitial normalClaim)}
  , testCase "refuses a terminal state that is not terminal" $
      prefuses $
        claimHolds
          normalClaim {cTerminal = replaceField 7 (PD.Constr 13 []) (cTerminal normalClaim)}
  , testCase "refuses a terminal state whose counter is not the step count" $
      prefuses $
        claimHolds normalClaim {cTerminal = replaceField 8 (PD.I 2) (cTerminal normalClaim)}
  , testCase "refuses a trace proof folded at the wrong index" $
      prefuses $
        claimHolds
          normalClaim
            { cInitialProof = replaceField 0 (PD.I 1) (cInitialProof normalClaim)
            }
  , testCase "refuses a trace proof with no path at all" $
      prefuses $
        claimHolds
          normalClaim
            { cTerminalProof = replaceField 2 (PD.List []) (cTerminalProof normalClaim)
            }
  , testCase "refuses an event-key hash the initial state simply asserts" $
      prefuses $
        claimHolds
          normalClaim
            { cInitial = replaceField 1 (PD.B (BS.replicate 32 0x9f)) (cInitial normalClaim)
            }
  , testCase "refuses a transaction commitment the initial state simply asserts" $
      prefuses $
        claimHolds
          normalClaim
            { cInitial = replaceField 3 (PD.B (BS.replicate 32 0x9e)) (cInitial normalClaim)
            }
  , testCase "refuses a source kind that does not match the tree" $
      prefuses $
        claimHolds
          normalClaim
            { cInitial = replaceField 5 (PD.Constr 1 []) (cInitial normalClaim)
            }
  ]
