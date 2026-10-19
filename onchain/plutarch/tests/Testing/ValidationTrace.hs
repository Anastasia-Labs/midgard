{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ValidationTrace
Description : Behavioural tests for the Plutarch port of @lib/midgard/validation-trace-v1.ak@.

The commitment scheme a validation run is disputed through: the machine state,
the descriptor summarising a whole run, and the binary Merkle tree the states are
committed in.

Every encoder here is rebuilt from the format — the seven domain separators
spelled as ASCII rather than copied as hex, the CBOR written from the major-type
rules — so a change on either side fails a case instead of two copies of the same
mistake agreeing.

Three things the suite is built around:

* __The codes are not the constructor tags.__ @ValidationPhase@'s fifteen tags
  and its fifteen codes happen to coincide, which is exactly the coincidence that
  hides a mistake in the other two: @ValidationVerdict@ and
  @ValidationSourceKind@ are encoded through their code functions too, and the
  cases below pin every one of the twenty.

* __The verdict and the rejection code are bound to each other.__ @Rejected@ must
  carry a non-zero code hash and everything else must carry the zero one. Both
  directions are driven, on both the state and the descriptor, because a
  one-directional check would let an acceptance smuggle a rejection reason.

* __Leaves and branches hash under different domains.__ That is the whole
  second-preimage defence of the trace tree, and it is asserted directly rather
  than left implicit in a passing proof.
-}
module Testing.ValidationTrace (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ValidationTrace (
  PValidationPhase (..),
  PValidationSourceKind (..),
  PValidationVerdict (..),
  PValidationMachineStateV1,
  PValidationTraceDescriptorV1,
  PValidationTraceProof,
  pdescriptorIsWellFormed,
  pencodeDescriptor,
  pencodeMachineState,
  phashLedgerDelta,
  phashMachineState,
  phashRejectionCode,
  phashValidationContext,
  phashWorkWitness,
  pmachineStateIsWellFormed,
  ptraceBranchHash,
  ptraceDepth,
  ptraceLeafHash,
  pverifyTraceProof,
 )
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Validation Trace Tests"
    [ testGroup "Aiken conformance" aikenConformanceTests
    , testGroup "the machine state" stateTests
    , testGroup "the verdict/rejection binding" bindingTests
    , testGroup "the descriptor" descriptorTests
    , testGroup "the auxiliary hashes" auxiliaryTests
    , testGroup "the trace tree" treeTests
    , testGroup "verifyTraceProof" proofTests
    ]

aikenConformanceTests :: [TestTree]
aikenConformanceTests =
  [ testCase "matches_typescript_machine_state_encoding_and_hash" $
      passertEval aikenMachineStateEncodingAndHash
  , testCase "matches_typescript_descriptor_encoding" $
      passertEval aikenDescriptorEncoding
  , testCase "rejects_wrong_machine_state_version_and_malformed_hash" $
      passertEval aikenRejectsMalformedMachineStates
  , testCase "validation_phase_verdict_and_source_kind_tags_match_typescript" $
      passertEval aikenEnumTags
  , testCase "verifies_both_members_of_two_state_trace" $
      passertEval aikenVerifiesBothTraceMembers
  , testCase "rejects_wrong_state_or_wrong_path_depth" $
      passertEval aikenRejectsWrongStateOrDepth
  , testCase "depth_is_exact_for_padded_state_count" $
      passertEval aikenTraceDepths
  , testCase "work_context_and_delta_hashes_match_typescript" $
      passertEval aikenAuxiliaryHashVectors
  , testCase "long_work_witness_hash_matches_typescript_and_aiken_byte_chunking" $
      passertEval aikenLongWorkWitnessHash
  ]

aikenMachineStateEncodingAndHash :: forall s. Term s PBool
aikenMachineStateEncodingAndHash =
  plet (stateT $ state aikenStateFields) $ \fixture ->
    pand'List
      [ pmachineStateIsWellFormed # fixture
      , pencodeMachineState # fixture #== pconstant aikenExpectedStateCbor
      , phashMachineState # fixture #== pconstant aikenExpectedStateHash
      ]

aikenDescriptorEncoding :: forall s. Term s PBool
aikenDescriptorEncoding =
  plet (descriptorT $ descriptor aikenDescriptorFields) $ \fixture ->
    pdescriptorIsWellFormed # fixture
      #&& pencodeDescriptor # fixture #== pconstant aikenExpectedDescriptorCbor

aikenRejectsMalformedMachineStates :: forall s. Term s PBool
aikenRejectsMalformedMachineStates =
  pnot # (pmachineStateIsWellFormed # stateT (state aikenStateFields {dMachineVersion = 2}))
    #&& pnot
      # ( pmachineStateIsWellFormed
            # stateT (state aikenStateFields {dEventKey = BS.singleton 0})
        )

aikenEnumTags :: forall s. Term s PBool
aikenEnumTags =
  pand'List $
    [ serialisedDataIs phase (decodeHex expected)
    | (phase, expected) <-
        [ (pcon PCanonicalDecode, "d87980")
        , (pcon PCompactBinding, "d87a80")
        , (pcon PStaticLedgerRules, "d87b80")
        , (pcon PInputSets, "d87c80")
        , (pcon PSignatures, "d87d80")
        , (pcon PPhaseANativeScripts, "d87e80")
        , (pcon PPhaseAScriptPreconditions, "d87f80")
        , (pcon PResolveInputs, "d9050080")
        , (pcon PScriptSources, "d9050180")
        , (pcon PNativeScripts, "d9050280")
        , (pcon PScriptIntegrity, "d9050380")
        , (pcon PCek, "d9050480")
        , (pcon PValueAndMint, "d9050580")
        , (pcon PLedgerDelta, "d9050680")
        , (pcon PTerminal, "d9050780")
        ]
    ]
      <> [ serialisedDataIs verdict (decodeHex expected)
         | (verdict, expected) <-
            [ (pcon PPending, "d87980")
            , (pcon PAccepted, "d87a80")
            , (pcon PRejected, "d87b80")
            ]
         ]
      <> [ serialisedDataIs sourceKind (decodeHex expected)
         | (sourceKind, expected) <-
            [(pcon PNormal, "d87980"), (pcon PForced, "d87a80")]
         ]

aikenVerifiesBothTraceMembers :: forall s. Term s PBool
aikenVerifiesBothTraceMembers =
  runProof aikenDescriptorData
    (proof 0 aikenExpectedStateHash [leafHash aikenSecondStateHash])
    #&& runProof aikenDescriptorData
      (proof 1 aikenSecondStateHash [leafHash aikenExpectedStateHash])

aikenRejectsWrongStateOrDepth :: forall s. Term s PBool
aikenRejectsWrongStateOrDepth =
  pnot # runProof aikenDescriptorData
    (proof 0 aikenSecondStateHash [leafHash aikenSecondStateHash])
    #&& pnot # runProof aikenDescriptorData
      (proof 0 aikenExpectedStateHash [])

aikenTraceDepths :: forall s. Term s PBool
aikenTraceDepths =
  pand'List
    [ ptraceDepth # 0 #== 0
    , ptraceDepth # 1 #== 1
    , ptraceDepth # 4 #== 3
    , ptraceDepth # 4_294_967_295 #== 32
    ]

aikenAuxiliaryHashVectors :: forall s. Term s PBool
aikenAuxiliaryHashVectors =
  pand'List
    [ phashWorkWitness # pcon PCek # 17 # pconstant (decodeHex "820142abcd")
        #== pconstant (decodeHex "36d4e5e57f9cbcca2fc621a5f9411251be1c31a39378089dff8d44e2caa8e2bc")
    , phashValidationContext # pconstant (decodeHex "83010203")
        #== pconstant (decodeHex "e99abbeb7b56d9e4b72cbd1356b1dbacdc12f250dcda3ba4b86cea25ac3219bd")
    , phashLedgerDelta
        # pconstant (decodeHex "81820058201111111111111111111111111111111111111111111111111111111111111111")
        #== pconstant (decodeHex "0fe3b46f56f2eb3137c35fd2f51be4fdd96043a811d20b75edc011d56c27ef44")
    ]

aikenLongWorkWitnessHash :: forall s. Term s PBool
aikenLongWorkWitnessHash =
  phashWorkWitness # pcon PCanonicalDecode # 0 # pconstant (BS.replicate 200 0xab)
    #== pconstant (decodeHex "c0e76d5f16d1f18c9e27ce0744c2e02e84a00ef55af81ce392137b2fdc9b8a53")

serialisedDataIs :: forall a s. PIsData a => Term s a -> BS.ByteString -> Term s PBool
serialisedDataIs value expected =
  pserialiseData # pforgetData (pdata value) #== pconstant expected

--------------------------------------------------------------------------------
-- The machine state
--------------------------------------------------------------------------------

stateTests :: [TestTree]
stateTests =
  [ testCase "encodes as a definite fifteen-element array" $
      passertEval $ pencodeMachineState # stateT (state defaults) #== pconstant (encodeState defaults)
  , testCase "hashes under its own domain" $
      passertEval $
        phashMachineState
          # stateT (state defaults)
          #== pconstant (blake2b256 ("MidgardValidationMachineStateV1" <> encodeState defaults))
  , {- All fifteen phases, encoded through the port and compared against bytes
       written here. The phase's constructor tags and its codes coincide, so a
       port that encoded the tag would pass a single-phase test and still be
       wrong about the other two enums — hence all fifteen, and hence the
       verdict and source-kind cases below. -}
    testCase "every phase encodes as its code" $
      passertEval $
        pand'List
          [ pencodeMachineState # stateT (state defaults {dPhase = p})
            #== pconstant (encodeState defaults {dPhase = p})
          | p <- [0 .. 14]
          ]
  , testCase "every verdict encodes as its code" $
      passertEval $
        pand'List
          [ pencodeMachineState # stateT (state fields) #== pconstant (encodeState fields)
          | v <- [0 .. 2]
          , let fields = defaults {dVerdict = v, dRejection = codeFor v}
          ]
  , testCase "both source kinds encode as their codes" $
      passertEval $
        pand'List
          [ pencodeMachineState # stateT (state defaults {dSourceKind = k})
            #== pconstant (encodeState defaults {dSourceKind = k})
          | k <- [0, 1]
          ]
  , testCase "the fifteen phases encode distinctly" $
      length (distinct [encodeState defaults {dPhase = p} | p <- [0 .. 14]]) @?= 15
  , testCase "the three verdicts encode distinctly" $
      length (distinct [encodeState defaults {dVerdict = v, dRejection = codeFor v} | v <- [0 .. 2]])
        @?= 3
  , testCase "the two source kinds encode distinctly" $
      length (distinct [encodeState defaults {dSourceKind = k} | k <- [0, 1]]) @?= 2
  , -- Each of the six 32-byte fields is length-checked by the encoder.
    testCase "refuses a short event-key hash" $
      pfails $ pencodeMachineState # stateT (state defaults {dEventKey = BS.replicate 31 0x11})
  , testCase "refuses a machine version that is not 1" $
      pfails $ pencodeMachineState # stateT (state defaults {dMachineVersion = 2})
  , testCase "refuses a negative program counter" $
      pfails $ pencodeMachineState # stateT (state defaults {dCounter = -1})
  , testCase "refuses a program counter past the step bound" $
      pfails $ pencodeMachineState # stateT (state defaults {dCounter = 4_294_967_296})
  , testCase "accepts a program counter at the step bound" $
      passertEval $
        pencodeMachineState
          # stateT (state defaults {dCounter = 4_294_967_295})
          #== pconstant (encodeState defaults {dCounter = 4_294_967_295})
  , testCase "refuses negative execution cost" $
      pfails $ pencodeMachineState # stateT (state defaults {dCpu = -1})
  , -- Well-formedness is the total twin of the encoder's aborts.
    testCase "a well-formed state is well formed" $
      passertEval $ pmachineStateIsWellFormed # stateT (state defaults)
  , testCase "a short work root is not" $
      prefuses $ pmachineStateIsWellFormed # stateT (state defaults {dWorkRoot = BS.replicate 31 0x33})
  , testCase "a negative memory cost is not" $
      prefuses $ pmachineStateIsWellFormed # stateT (state defaults {dMemory = -1})
  ]

--------------------------------------------------------------------------------
-- The verdict/rejection binding
--------------------------------------------------------------------------------

{- | Both directions, on both carriers.

A rejection with no code says nothing about why; an acceptance carrying one is a
contradiction. Aiken refuses both rather than normalising either, and a port that
checked only the first would let a block commit an acceptance that looks like a
rejection to anything reading the code hash.
-}
bindingTests :: [TestTree]
bindingTests =
  [ testCase "a rejection carrying a code is well formed" $
      passertEval $
        pmachineStateIsWellFormed # stateT (state defaults {dVerdict = 2, dRejection = someCode})
  , testCase "a rejection carrying the zero code is not" $
      prefuses $
        pmachineStateIsWellFormed # stateT (state defaults {dVerdict = 2, dRejection = zeroCode})
  , testCase "an acceptance carrying the zero code is well formed" $
      passertEval $
        pmachineStateIsWellFormed # stateT (state defaults {dVerdict = 1, dRejection = zeroCode})
  , testCase "an acceptance carrying a code is not" $
      prefuses $
        pmachineStateIsWellFormed # stateT (state defaults {dVerdict = 1, dRejection = someCode})
  , -- Pending is a legitimate state and never a legitimate descriptor.
    testCase "a pending state is well formed" $
      passertEval $
        pmachineStateIsWellFormed # stateT (state defaults {dVerdict = 0, dRejection = zeroCode})
  , testCase "a pending descriptor is not" $
      prefuses $
        pdescriptorIsWellFormed # descriptorT (descriptor descDefaults {kVerdict = 0})
  , testCase "the same binding is enforced on a descriptor" $
      prefuses $
        pdescriptorIsWellFormed
          # descriptorT (descriptor descDefaults {kVerdict = 1, kRejection = someCode})
  , testCase "a code hash of the wrong width is refused whatever the verdict" $
      prefuses $
        pmachineStateIsWellFormed
          # stateT (state defaults {dVerdict = 2, dRejection = BS.replicate 31 0x77})
  ]

--------------------------------------------------------------------------------
-- The descriptor
--------------------------------------------------------------------------------

descriptorTests :: [TestTree]
descriptorTests =
  [ testCase "encodes as a definite eight-element array" $
      passertEval $
        pencodeDescriptor # descriptorT (descriptor descDefaults)
          #== pconstant (encodeDescriptor descDefaults)
  , testCase "a well-formed descriptor is well formed" $
      passertEval $ pdescriptorIsWellFormed # descriptorT (descriptor descDefaults)
  , testCase "refuses a schema version that is not 1" $
      pfails $ pencodeDescriptor # descriptorT (descriptor descDefaults {kSchema = 2})
  , testCase "refuses a machine version that is not 1" $
      pfails $ pencodeDescriptor # descriptorT (descriptor descDefaults {kMachine = 2})
  , testCase "refuses a pending verdict" $
      pfails $ pencodeDescriptor # descriptorT (descriptor descDefaults {kVerdict = 0})
  , testCase "refuses a negative step count" $
      pfails $ pencodeDescriptor # descriptorT (descriptor descDefaults {kStepCount = -1})
  , testCase "refuses a step count past the bound" $
      pfails $ pencodeDescriptor # descriptorT (descriptor descDefaults {kStepCount = 4_294_967_296})
  , testCase "refuses a short trace root" $
      pfails $ pencodeDescriptor # descriptorT (descriptor descDefaults {kRoot = BS.replicate 31 0x44})
  ]

--------------------------------------------------------------------------------
-- The auxiliary hashes
--------------------------------------------------------------------------------

auxiliaryTests :: [TestTree]
auxiliaryTests =
  [ {- The witness bytes are @cbor.serialise@d rather than concatenated raw, so
       the hash commits to a length-delimited string. Concatenating them would let
       a short witness with a long counter collide with the reverse. -}
    testCase "a work witness commits phase, counter and length-delimited bytes" $
      passertEval $
        phashWorkWitness # pcon PCanonicalDecode # 7 # pconstant witness
          #== pconstant
            ( blake2b256
                ( "MidgardValidationWorkWitnessV1"
                    <> "\x83"
                    <> cborInt 0
                    <> cborInt 7
                    <> cborBytes witness
                )
            )
  , testCase "refuses a program counter past the step bound" $
      pfails $ phashWorkWitness # pcon PCanonicalDecode # 4_294_967_296 # pconstant witness
  , testCase "a rejection code hashes under its own domain" $
      passertEval $
        phashRejectionCode # pconstant witness
          #== pconstant (blake2b256 ("MidgardValidationRejectCodeV1" <> witness))
  , testCase "a validation context hashes under its own domain" $
      passertEval $
        phashValidationContext # pconstant witness
          #== pconstant (blake2b256 ("MidgardValidationContextV1" <> witness))
  , testCase "a ledger delta hashes under its own domain" $
      passertEval $
        phashLedgerDelta # pconstant witness
          #== pconstant (blake2b256 ("MidgardValidationLedgerDeltaV1" <> witness))
  , {- The four auxiliary domains have to be pairwise distinct, or one committed
       value could be presented as another kind. Asserted over the bytes rather
       than over the hashes, so the case says what it means. -}
    testCase "the seven domains are pairwise distinct" $
      length (distinct allDomains) @?= 7
  ]

--------------------------------------------------------------------------------
-- The trace tree
--------------------------------------------------------------------------------

treeTests :: [TestTree]
treeTests =
  [ testCase "a leaf hashes under the leaf domain" $
      passertEval $
        ptraceLeafHash # pconstant hashA
          #== pconstant (blake2b256 ("MidgardValidationTraceLeafV1" <> hashA))
  , testCase "a branch hashes under the branch domain" $
      passertEval $
        ptraceBranchHash # pconstant hashA # pconstant hashB
          #== pconstant (blake2b256 ("MidgardValidationTraceBranchV1" <> hashA <> hashB))
  , {- The second-preimage defence: an internal node must not be presentable as a
       leaf. The two domains differ in length as well as content, so no padding
       trick recovers one from the other. -}
    testCase "a leaf and a branch of the same bytes hash differently" $
      assertBool "leaf and branch domains collide" $
        leafHash hashA /= branchHash hashA hashA
  , testCase "a branch is not symmetric in its children" $
      assertBool "branch hashing ignores order" $
        branchHash hashA hashB /= branchHash hashB hashA
  , testCase "a short leaf hash is refused" $
      pfails $ ptraceLeafHash # pconstant (BS.replicate 31 0x55)
  , testCase "a short branch child is refused" $
      pfails $ ptraceBranchHash # pconstant hashA # pconstant (BS.replicate 31 0x55)
  , {- Depth is over @step_count + 1@ states, not over the steps: a run of @n@
       steps passes through @n + 1@ states, so a one-step run needs a tree of
       depth one and not of depth zero. -}
    testCase "trace depths follow the state count, not the step count" $
      map depth [0, 1, 2, 3, 4, 7, 8] @?= [0, 1, 2, 2, 3, 3, 4]
  , testCase "refuses a negative step count" $
      pfails $ ptraceDepth # (-1)
  ]

--------------------------------------------------------------------------------
-- verifyTraceProof
--------------------------------------------------------------------------------

proofTests :: [TestTree]
proofTests =
  [ -- A run of no steps is one state, and its tree is that state's leaf.
    testCase "accepts the only state of a zero-step trace" $
      passertEval $ runProof (trace0 0) (proof 0 (stateHash 0) [])
  , testCase "accepts the lower state of a one-step trace" $
      passertEval $ runProof (trace1 1) (proof 0 (stateHash 0) [leafHash (stateHash 1)])
  , testCase "accepts the upper state of a one-step trace" $
      passertEval $ runProof (trace1 1) (proof 1 (stateHash 1) [leafHash (stateHash 0)])
  , -- Depth two, where the index's second bit starts to matter.
    testCase "accepts a state of a two-step trace" $
      passertEval $
        runProof
          (trace2 2)
          (proof 2 (stateHash 2) [leafHash (stateHash 3), branchHash (leafHash (stateHash 0)) (leafHash (stateHash 1))])
  , {- The index decides which side each sibling goes on, so the same siblings at
       the wrong index fold to a different root. -}
    testCase "refuses a path folded at the wrong index" $
      prefuses $ runProof (trace1 1) (proof 1 (stateHash 0) [leafHash (stateHash 1)])
  , testCase "refuses a state the trace does not hold" $
      prefuses $ runProof (trace1 1) (proof 0 (stateHash 9) [leafHash (stateHash 1)])
  , {- A path shorter than the tree's depth folds to a subtree root, which for a
       one-state trace is the whole root — so the sibling-count check is the only
       thing standing between a depth-one trace and a depth-zero proof of it. -}
    testCase "refuses a path shorter than the tree's depth" $
      prefuses $ runProof (trace1 1) (proof 0 (stateHash 0) [])
  , testCase "refuses a path longer than the tree's depth" $
      prefuses $
        runProof (trace1 1) (proof 0 (stateHash 0) [leafHash (stateHash 1), leafHash (stateHash 2)])
  , testCase "refuses an index past the trace's last state" $
      prefuses $ runProof (trace1 1) (proof 2 (stateHash 0) [leafHash (stateHash 1)])
  , testCase "refuses a negative index" $
      prefuses $ runProof (trace1 1) (proof (-1) (stateHash 0) [leafHash (stateHash 1)])
  , testCase "refuses a state hash of the wrong width" $
      prefuses $ runProof (trace1 1) (proof 0 (BS.replicate 31 0x66) [leafHash (stateHash 1)])
  , -- A malformed sibling aborts rather than refusing: it is not a claim that
    -- failed, it is a proof that cannot be read.
    testCase "aborts on a sibling of the wrong width" $
      pfails $ runProof (trace1 1) (proof 0 (stateHash 0) [BS.replicate 31 0x66])
  , testCase "refuses a proof against a malformed descriptor" $
      prefuses $
        runProofAgainst
          (descriptor descDefaults {kVerdict = 0, kStepCount = 0, kRoot = leafHash (stateHash 0)})
          (proof 0 (stateHash 0) [])
  ]

--------------------------------------------------------------------------------
-- Traces
--------------------------------------------------------------------------------

trace0, trace1, trace2 :: Integer -> PD.Data
trace0 n = descriptor descDefaults {kStepCount = n, kRoot = leafHash (stateHash 0)}
trace1 n =
  descriptor
    descDefaults
      {kStepCount = n, kRoot = branchHash (leafHash (stateHash 0)) (leafHash (stateHash 1))}
trace2 n = descriptor descDefaults {kStepCount = n, kRoot = root}
  where
    root =
      branchHash
        (branchHash (leafHash (stateHash 0)) (leafHash (stateHash 1)))
        (branchHash (leafHash (stateHash 2)) (leafHash (stateHash 3)))

-- | @ValidationTraceProof@ — index, state hash, siblings.
proof :: Integer -> BS.ByteString -> [BS.ByteString] -> PD.Data
proof index hash siblings =
  PD.Constr 0 [PD.I index, PD.B hash, PD.List (map PD.B siblings)]

stateHash :: Int -> BS.ByteString
stateHash n = blake2b256 (BS.pack [fromIntegral n])

runProof :: forall s. PD.Data -> PD.Data -> Term s PBool
runProof = runProofAgainst

runProofAgainst :: forall s. PD.Data -> PD.Data -> Term s PBool
runProofAgainst d p = pverifyTraceProof # descriptorT d # proofT p

--------------------------------------------------------------------------------
-- The machine state, as data and as bytes
--------------------------------------------------------------------------------

-- | Every field of a machine state, so a case can vary exactly one.
data StateFields = StateFields
  { dMachineVersion :: Integer
  , dEventKey :: BS.ByteString
  , dTxId :: BS.ByteString
  , dCommitment :: BS.ByteString
  , dContextHash :: BS.ByteString
  , dSourceKind :: Integer
  , dPriorRoot :: BS.ByteString
  , dPhase :: Integer
  , dCounter :: Integer
  , dWorkRoot :: BS.ByteString
  , dCpu :: Integer
  , dMemory :: Integer
  , dVerdict :: Integer
  , dRejection :: BS.ByteString
  , dDeltaRoot :: BS.ByteString
  }

defaults :: StateFields
defaults =
  StateFields
    { dMachineVersion = 1
    , dEventKey = BS.replicate 32 0x11
    , dTxId = BS.replicate 32 0x12
    , dCommitment = BS.replicate 32 0x13
    , dContextHash = BS.replicate 32 0x14
    , dSourceKind = 0
    , dPriorRoot = BS.replicate 32 0x15
    , dPhase = 3
    , dCounter = 42
    , dWorkRoot = BS.replicate 32 0x16
    , dCpu = 1000
    , dMemory = 2000
    , dVerdict = 0
    , dRejection = zeroCode
    , dDeltaRoot = BS.replicate 32 0x17
    }

state :: StateFields -> PD.Data
state d =
  PD.Constr
    0
    [ PD.I (dMachineVersion d)
    , PD.B (dEventKey d)
    , PD.B (dTxId d)
    , PD.B (dCommitment d)
    , PD.B (dContextHash d)
    , PD.Constr (dSourceKind d) []
    , PD.B (dPriorRoot d)
    , PD.Constr (dPhase d) []
    , PD.I (dCounter d)
    , PD.B (dWorkRoot d)
    , PD.I (dCpu d)
    , PD.I (dMemory d)
    , PD.Constr (dVerdict d) []
    , PD.B (dRejection d)
    , PD.B (dDeltaRoot d)
    ]

{- | @encode_machine_state@ — a definite fifteen-element array.

The three enums go in as their /codes/. For the phase the code equals the
constructor tag, which is why this reference spells the mapping out rather than
reusing the tag: the verdict's and the source kind's do too, and all three being
identity is a fact rather than a rule.
-}
encodeState :: StateFields -> BS.ByteString
encodeState d =
  BS.concat
    [ "\x8f"
    , cborInt (dMachineVersion d)
    , h32 (dEventKey d)
    , h32 (dTxId d)
    , h32 (dCommitment d)
    , h32 (dContextHash d)
    , cborInt (sourceKindCode (dSourceKind d))
    , h32 (dPriorRoot d)
    , cborInt (phaseCode (dPhase d))
    , cborInt (dCounter d)
    , h32 (dWorkRoot d)
    , cborInt (dCpu d)
    , cborInt (dMemory d)
    , cborInt (verdictCode (dVerdict d))
    , h32 (dRejection d)
    , h32 (dDeltaRoot d)
    ]

--------------------------------------------------------------------------------
-- The descriptor, as data and as bytes
--------------------------------------------------------------------------------

data DescriptorFields = DescriptorFields
  { kSchema :: Integer
  , kMachine :: Integer
  , kRoot :: BS.ByteString
  , kStepCount :: Integer
  , kInitial :: BS.ByteString
  , kTerminal :: BS.ByteString
  , kVerdict :: Integer
  , kRejection :: BS.ByteString
  }

descDefaults :: DescriptorFields
descDefaults =
  DescriptorFields
    { kSchema = 1
    , kMachine = 1
    , kRoot = BS.replicate 32 0x21
    , kStepCount = 5
    , kInitial = BS.replicate 32 0x22
    , kTerminal = BS.replicate 32 0x23
    , kVerdict = 1
    , kRejection = zeroCode
    }

aikenStateFields :: StateFields
aikenStateFields =
  StateFields
    { dMachineVersion = 1
    , dEventKey = BS.replicate 32 0x01
    , dTxId = BS.replicate 32 0x02
    , dCommitment = BS.replicate 32 0x03
    , dContextHash = BS.replicate 32 0x04
    , dSourceKind = 1
    , dPriorRoot = BS.replicate 32 0x05
    , dPhase = 11
    , dCounter = 17
    , dWorkRoot = BS.replicate 32 0x06
    , dCpu = 123
    , dMemory = 45
    , dVerdict = 0
    , dRejection = zeroCode
    , dDeltaRoot = BS.replicate 32 0x07
    }

aikenExpectedStateCbor, aikenExpectedStateHash, aikenSecondStateHash, aikenTraceRoot :: BS.ByteString
aikenExpectedStateCbor =
  decodeHex
    "8f015820010101010101010101010101010101010101010101010101010101010101010158200202020202020202020202020202020202020202020202020202020202020202582003030303030303030303030303030303030303030303030303030303030303035820040404040404040404040404040404040404040404040404040404040404040401582005050505050505050505050505050505050505050505050505050505050505050b1158200606060606060606060606060606060606060606060606060606060606060606187b182d005820000000000000000000000000000000000000000000000000000000000000000058200707070707070707070707070707070707070707070707070707070707070707"
aikenExpectedStateHash = decodeHex "fa9598fae21355bd529770b1c2c750ace65d721ada641bec6bd5f87a22c18088"
aikenSecondStateHash = BS.replicate 32 0x09
aikenTraceRoot = decodeHex "c6760a9266746c67578026b6d44e533ae8390264d227a9649e6558a3d70970eb"

aikenDescriptorFields :: DescriptorFields
aikenDescriptorFields =
  DescriptorFields
    { kSchema = 1
    , kMachine = 1
    , kRoot = aikenTraceRoot
    , kStepCount = 1
    , kInitial = aikenExpectedStateHash
    , kTerminal = aikenSecondStateHash
    , kVerdict = 2
    , kRejection = BS.replicate 32 0x08
    }

aikenDescriptorData :: PD.Data
aikenDescriptorData = descriptor aikenDescriptorFields

aikenExpectedDescriptorCbor :: BS.ByteString
aikenExpectedDescriptorCbor =
  decodeHex
    "8801015820c6760a9266746c67578026b6d44e533ae8390264d227a9649e6558a3d70970eb015820fa9598fae21355bd529770b1c2c750ace65d721ada641bec6bd5f87a22c18088582009090909090909090909090909090909090909090909090909090909090909090258200808080808080808080808080808080808080808080808080808080808080808"

descriptor :: DescriptorFields -> PD.Data
descriptor k =
  PD.Constr
    0
    [ PD.I (kSchema k)
    , PD.I (kMachine k)
    , PD.B (kRoot k)
    , PD.I (kStepCount k)
    , PD.B (kInitial k)
    , PD.B (kTerminal k)
    , PD.Constr (kVerdict k) []
    , PD.B (kRejection k)
    ]

encodeDescriptor :: DescriptorFields -> BS.ByteString
encodeDescriptor k =
  BS.concat
    [ "\x88"
    , cborInt (kSchema k)
    , cborInt (kMachine k)
    , h32 (kRoot k)
    , cborInt (kStepCount k)
    , h32 (kInitial k)
    , h32 (kTerminal k)
    , cborInt (verdictCode (kVerdict k))
    , h32 (kRejection k)
    ]

--------------------------------------------------------------------------------
-- The codes, written out
--------------------------------------------------------------------------------

phaseCode, verdictCode, sourceKindCode :: Integer -> Integer
phaseCode n
  | n >= 0 && n <= 14 = n
  | otherwise = error "no such phase"
verdictCode n
  | n >= 0 && n <= 2 = n
  | otherwise = error "no such verdict"
sourceKindCode n
  | n >= 0 && n <= 1 = n
  | otherwise = error "no such source kind"

codeFor :: Integer -> BS.ByteString
codeFor 2 = someCode
codeFor _ = zeroCode

zeroCode, someCode :: BS.ByteString
zeroCode = BS.replicate 32 0x00
someCode = BS.replicate 32 0xaa

--------------------------------------------------------------------------------
-- Reference encoders
--------------------------------------------------------------------------------

-- | @encode_h32@ — the two-byte definite header for a 32-byte string.
h32 :: BS.ByteString -> BS.ByteString
h32 bytes
  | BS.length bytes == 32 = "\x58\x20" <> bytes
  | otherwise = error "not a digest"

cborInt :: Integer -> BS.ByteString
cborInt n
  | n <= 23 = BS.pack [fromIntegral n]
  | n <= 255 = BS.pack [24, fromIntegral n]
  | n <= 65535 = BS.pack [25] <> be 2 n
  | n <= 4294967295 = BS.pack [26] <> be 4 n
  | otherwise = BS.pack [27] <> be 8 n
  where
    be w v = BS.pack [fromIntegral (v `div` (256 ^ i) `mod` 256) | i <- [w - 1, w - 2 .. 0 :: Integer]]

-- | Aiken's @cbor.serialise@ over a byte string: a definite-length major-2 head.
cborBytes :: BS.ByteString -> BS.ByteString
cborBytes bytes
  | n <= 23 = BS.cons (fromIntegral (0x40 + n)) bytes
  | n <= 255 = BS.pack [0x58, fromIntegral n] <> bytes
  | otherwise = BS.pack [0x59] <> BS.pack [fromIntegral (n `div` 256), fromIntegral (n `mod` 256)] <> bytes
  where
    n = BS.length bytes

leafHash :: BS.ByteString -> BS.ByteString
leafHash h = blake2b256 ("MidgardValidationTraceLeafV1" <> h)

branchHash :: BS.ByteString -> BS.ByteString -> BS.ByteString
branchHash l r = blake2b256 ("MidgardValidationTraceBranchV1" <> l <> r)

-- | @trace_depth@ — the smallest depth whose capacity covers @n + 1@ states.
depth :: Integer -> Integer
depth stepCount = go 1 0
  where
    go capacity d
      | capacity >= stepCount + 1 = d
      | otherwise = go (capacity * 2) (d + 1)

allDomains :: [BS.ByteString]
allDomains =
  [ "MidgardValidationMachineStateV1"
  , "MidgardValidationTraceLeafV1"
  , "MidgardValidationTraceBranchV1"
  , "MidgardValidationWorkWitnessV1"
  , "MidgardValidationRejectCodeV1"
  , "MidgardValidationContextV1"
  , "MidgardValidationLedgerDeltaV1"
  ]

witness :: BS.ByteString
witness = BS.replicate 40 0x5a

decodeHex :: BS.ByteString -> BS.ByteString
decodeHex = Base16.decodeLenient

hashA, hashB :: BS.ByteString
hashA = BS.replicate 32 0x31
hashB = BS.replicate 32 0x32

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

distinct :: [BS.ByteString] -> [BS.ByteString]
distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

--------------------------------------------------------------------------------
-- Coercions
--------------------------------------------------------------------------------

{- | A refusal, which for the well-formedness predicates and 'pverifyTraceProof'
is @False@ rather than an abort.
-}
prefuses :: (forall s. Term s PBool) -> Assertion
prefuses p = passertEval (pnot # p)

stateT :: forall s. PD.Data -> Term s PValidationMachineStateV1
stateT = punsafeFromData

descriptorT :: forall s. PD.Data -> Term s PValidationTraceDescriptorV1
descriptorT = punsafeFromData

proofT :: forall s. PD.Data -> Term s PValidationTraceProof
proofT = punsafeFromData

punsafeFromData :: forall a s. PD.Data -> Term s a
punsafeFromData d = punsafeCoerce (pconstant @PData d)
