{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CekProof
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/cek-proof-v1.ak@.

The content-addressed program format, checked against a Haskell reference
written here from @cek-proof-v1.ak@'s preimage layouts. The reference borrows
'blake2b256' and 'definiteBytes' from "Testing.BoundedItem" — the shared CBOR
primitives, already pinned there against a different port — and spells out
every domain string, tag and field order itself.

=== Nine domains, and the pairs that would otherwise collide

A sequence link and an environment link have byte-identical preimages; so do a
delay term and a lambda term but for one integer. The first group below asserts
that each domain produces a different root for the same bytes, because a port
that dropped or duplicated a domain string would pass every round-trip test and
still let a proof read an environment as a term list.

=== The completeness walk is tested on a program, not on a fixture

The last group builds a real eight-node program — @(λx. x) c@ over a @Data@
constant — in Haskell: every term, the constant's value node, its type blob, its
semantic @Data@ node and that node's blob leaf, each hashed, sorted, and packed
into a sidecar. Verification of that program is one test. The rest are
mutations of it, one field at a time, and each names the specific rule it
violates: an entry the walk never reaches, a node count that is off by one, a
zero-count sequence pointed at something other than the canonical empty root.

=== Where the port and the Aiken part company

Nowhere in this module — but 'Midgard.CekData.pinspectDataNodePreimageV1'
cannot inspect a @BytesData@ node at all (see "Testing.CekData"), so the
completeness walk below commits its @Data@ constant as an integer rather than a
byte string. A byte-string constant would fail at kind 5 for a reason that has
nothing to do with this module.
-}
module Testing.CekProof (tests) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.List (sortOn)
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.CekProof (
  PProgramEnvelopeV1 (..),
  PProgramTermMaterialV1 (..),
  pboundedBlobRootV1,
  pdecodeProgramEnvelopeV1,
  pemptyContinuationRootV1,
  pemptyEnvironmentRootV1,
  pemptySequenceRootV1,
  pencodeProgramEnvelopeV1,
  pmaxProgramEnvelopeCborBytes,
  pmaxProgramMaterialByteLength,
  pmaxProgramNodeCount,
  phashApplicationTermV1,
  phashApplyArgumentContinuationV1,
  phashApplyFunctionContinuationV1,
  phashApplyValueContinuationV1,
  phashBlobBranchV1,
  phashBlobChunkV1,
  phashBlsMillerLoopExpressionV1,
  phashBlsMillerLoopValueV1,
  phashBlsMultiplyExpressionV1,
  phashBuiltinTermV1,
  phashBuiltinValueV1,
  phashCaseApplyContinuationV1,
  phashCaseContinuationV1,
  phashCaseSelectContinuationV1,
  phashCaseTermV1,
  phashConstantTermV1,
  phashConstantValueV1,
  phashConstrContinuationV1,
  phashConstrTermV1,
  phashConstrValueV1,
  phashContextConstantTermV1,
  phashDelayTermV1,
  phashDelayValueV1,
  phashEnvironmentNodeV1,
  phashErrorTermV1,
  phashForceContinuationV1,
  phashForceTermV1,
  phashLambdaTermV1,
  phashLambdaValueV1,
  phashMachineStateV1,
  phashProgramEnvelopeV1,
  phashSequenceNodeV1,
  phashVariableTermV1,
  pinspectCompleteProgramMaterialSidecarV1,
  pinspectProgramBlobMaterialV1,
  pinspectProgramEnvelopeV1,
  pinspectProgramSequenceMaterialV1,
  pinspectProgramTermMaterialV1,
  pinspectProgramValueMaterialV1,
  pverifyCompleteProgramMaterialEntriesV1,
  pverifyCompleteProgramMaterialV1,
 )
import Midgard.LedgerState (PCekProgramMaterialDatumV1 (..))
import Testing.BoundedItem (blake2b256, definiteBytes)
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Midgard.CekProof"
    [ crossLanguageGoldenTests
    , domainTests
    , termHashTests
    , valueHashTests
    , linkHashTests
    , continuationHashTests
    , blobTests
    , machineStateTests
    , envelopeTests
    , termInspectionTests
    , valueInspectionTests
    , blobInspectionTests
    , sidecarTests
    , completenessTests
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

hA, hB, hC, hD :: BS.ByteString
hA = BS.replicate 32 0xa1
hB = BS.replicate 32 0xb2
hC = BS.replicate 32 0xc3
hD = BS.replicate 32 0xd4

short :: BS.ByteString
short = BS.replicate 31 0x11

--------------------------------------------------------------------------------
-- The cross-language vector in cek-proof-v1.test.ak
--------------------------------------------------------------------------------

crossLanguageGoldenTests :: TestTree
crossLanguageGoldenTests =
  testGroup
    "the Aiken cross-language golden vectors"
    [ testCase "cross_language_cek_commitment_vectors" $
        passertEval crossLanguageCekCommitmentVectors
    , testCase "canonical_program_envelope_decodes_with_exact_bounds" $
        passertEval canonicalProgramEnvelopeAtExactBounds
    , testCase "complete_program_material_entries_bind_root_counts_and_no_extras" $
        passertEval completeProgramMaterialEntriesBindRootCountsAndNoExtras
    ]

crossLanguageCekCommitmentVectors :: forall (s :: S). Term s PBool
crossLanguageCekCommitmentVectors =
  plet (phashApplicationTermV1 # h01 # h02) $ \term ->
    plet (phashLambdaValueV1 # h03 # pemptyEnvironmentRootV1) $ \value ->
      plet (phashSequenceNodeV1 # term # pemptySequenceRootV1 # 1) $ \sequence ->
        plet (phashEnvironmentNodeV1 # value # pemptyEnvironmentRootV1 # 1) $ \environment ->
          plet
            (phashApplyArgumentContinuationV1 # h04 # environment # pemptyContinuationRootV1)
            $ \continuation ->
              pand'List
                [ pemptySequenceRootV1
                    #== phexByteStr "8ab46e13655026ca6fe253b057ff678ebf9fa088097d6fa4c62276ba140f1743"
                , pemptyEnvironmentRootV1
                    #== phexByteStr "0b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7"
                , pemptyContinuationRootV1
                    #== phexByteStr "53163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b"
                , term
                    #== phexByteStr "2a37aa5b923cf90c6f3c8849e8fe2b28adcda97ccd736af6bf35b8312035f431"
                , value
                    #== phexByteStr "a103975a15b084afa3e69e3f71ff66b57d3ce83f992df1325d0958245d337941"
                , sequence
                    #== phexByteStr "854e2610e77c03a89283632923ddb99af0f276bedae48865036e85c2ed1f23cb"
                , environment
                    #== phexByteStr "4c2bb324a912cff2fc99c9056faa7d8bb72ab5dd512362fbd6a7383fe9c71a5c"
                , continuation
                    #== phexByteStr "7082fd0df1eb9680517dd87620f9fa94e5ea4598a56676e224f20a833bea3513"
                , phashMachineStateV1
                    # 0
                    # 2
                    # term
                    # environment
                    # continuation
                    # 0
                    # 16000
                    # 100
                    #== phexByteStr "8a475241923d49a38fa1d6376dd1c40ebb8d16adc87f51bc4a6dea7d954f88d1"
                , phashApplyValueContinuationV1 # h01 # h02
                    #== phexByteStr "facc375c9390b9503ccb80c0500bb3b88dc62c5da62c05599e6035938535fe61"
                , phashCaseSelectContinuationV1 # h01 # h02 # 3
                    #== phexByteStr "e71825335e324be2d099381365d5bc42c769bb334df509e5adae883842b9c1ea"
                , phashCaseApplyContinuationV1 # h01 # h02
                    #== phexByteStr "12aedf8df3ce9e44db4cc847cd52d0cff0b9b832ff4425954fa7c38c523e9888"
                , phashMachineStateV1
                    # 7
                    # 0
                    # h01
                    # h02
                    # pemptyContinuationRootV1
                    # 3
                    # 4
                    # 5
                    #== phexByteStr "31884c469ef240c7bad0a586ca242f45fa1728d13ab8385a97e5e0a18b806eb6"
                , phashMachineStateV1
                    # 8
                    # 0
                    # h01
                    # h02
                    # pemptyContinuationRootV1
                    # 3
                    # 4
                    # 5
                    #== phexByteStr "9d83a7edc397becce073970978dff4d6bae2c0334f0dc7c3525e35d901f47a13"
                , phashProgramEnvelopeV1 # 1 # 1 # 0 # term # 3 # 144
                    #== phexByteStr "e9d2696eff22d8078ae7bd71c83f6058e0db4d938a016314a2cd35feaccfdefa"
                , phashBlobChunkV1 # phexByteStr "010203"
                    #== phexByteStr "0bb4f6f24cd0080e59e98d57a13c6453e07991452b80f14ff1d9791c02db1fc9"
                , phashBlsMillerLoopExpressionV1 # h01 # h02
                    #== phexByteStr "6bd7d80222a87e5a09102534274099c501f82c5d898fe75f24fdc951c2fb3cc0"
                , phashBlsMultiplyExpressionV1 # h01 # h02
                    #== phexByteStr "be5de65e45ff867ca8394e227ee30e4f8cfaee0d4cb1d2834fe1af943b806082"
                , phashBlsMillerLoopValueV1
                    # (phashBlsMillerLoopExpressionV1 # h01 # h02)
                    #== phexByteStr "d93762ca25b0c585073f9e97f50873b76e13de1290182f6bb4744d29d9fb3fb7"
                ]

h01, h02, h03, h04 :: forall (s :: S). Term s PByteString
h01 = pconstant $ BS.replicate 32 0x01
h02 = pconstant $ BS.replicate 32 0x02
h03 = pconstant $ BS.replicate 32 0x03
h04 = pconstant $ BS.replicate 32 0x04

canonicalProgramEnvelopeAtExactBounds :: forall (s :: S). Term s PBool
canonicalProgramEnvelopeAtExactBounds =
  plet
    ( pencodeProgramEnvelopeV1
        # 1
        # 1
        # 0
        # h01
        # pmaxProgramNodeCount
        # pmaxProgramMaterialByteLength
    )
    $ \envelope ->
      pmatch (pinspectProgramEnvelopeV1 # envelope) $ \case
        PNothing -> pconstant False
        PJust decoded ->
          pmatch decoded $ \(PProgramEnvelopeV1 termRoot nodeCount materialByteLength) ->
            pand'List
              [ plengthBS # envelope #== pmaxProgramEnvelopeCborBytes
              , pfromData termRoot #== h01
              , pfromData nodeCount #== pmaxProgramNodeCount
              , pfromData materialByteLength #== pmaxProgramMaterialByteLength
              , phashProgramEnvelopeV1
                  # 1
                  # 1
                  # 0
                  # h01
                  # pmaxProgramNodeCount
                  # pmaxProgramMaterialByteLength
                  #== phexByteStr "5d6e5b270acedb1791dcb8a1cd7552efc37be56f8b2370e76f75729700cd9e17"
              ]

-- | The exact one-node completeness fixture from @cek-proof-v1.test.ak@.
completeProgramMaterialEntriesBindRootCountsAndNoExtras ::
  forall (s :: S). Term s PBool
completeProgramMaterialEntriesBindRootCountsAndNoExtras =
  plet aikenErrorMaterialEntry $ \entry ->
    plet aikenExtraMaterialEntry $ \extra ->
      pand'List
        [ isPresent (pinspectProgramEnvelopeV1 # pconstant aikenErrorEnvelope)
        , pinspectProgramTermMaterialV1 # pconstant errorPreimage
            #== pcon (PJust (pcon PErrorTerm))
        , isPresent (pinspectProgramValueMaterialV1 # pconstant aikenValuePreimage)
        , isAbsent (pinspectProgramValueMaterialV1 # pconstant (aikenValuePreimage <> "\x00"))
        , isPresent (pinspectProgramSequenceMaterialV1 # pconstant aikenSequencePreimage)
        , isAbsent
            (pinspectProgramSequenceMaterialV1 # pconstant (aikenSequencePreimage <> "\x00"))
        , isPresent (pinspectProgramBlobMaterialV1 # 3 # pconstant aikenBlobChunkPreimage)
        , isAbsent
            (pinspectProgramBlobMaterialV1 # 3 # pconstant (aikenBlobChunkPreimage <> "\x00"))
        , isPresent (pinspectProgramBlobMaterialV1 # 4 # pconstant aikenBlobBranchPreimage)
        , isAbsent
            (pinspectProgramBlobMaterialV1 # 4 # pconstant (aikenBlobBranchPreimage <> "\x00"))
        , pverifyCompleteProgramMaterialEntriesV1
            # pconstant aikenErrorEnvelope
            # (pcons # pdata entry # pnil)
        , pverifyCompleteProgramMaterialV1
            # pconstant aikenErrorEnvelope
            # pconstant aikenErrorSidecar
        , pnot
            #$ pverifyCompleteProgramMaterialV1
              # pconstant aikenErrorEnvelope
              # pconstant (aikenErrorSidecar <> "\x00")
        , pnot
            #$ pverifyCompleteProgramMaterialV1
              # pconstant aikenErrorEnvelope
              # pconstant "\x82\x01\x80"
        , pnot
            #$ pverifyCompleteProgramMaterialV1
              # pconstant aikenErrorEnvelope
              # pconstant aikenDuplicateSidecar
        , pnot
            #$ pverifyCompleteProgramMaterialV1
              # pconstant aikenErrorEnvelope
              # pconstant aikenSubstitutedSidecar
        , pnot
            #$ pverifyCompleteProgramMaterialV1
              # pconstant aikenErrorEnvelope
              # pconstant aikenNoncanonicalSidecar
        , pnot
            #$ pverifyCompleteProgramMaterialV1
              # pconstant aikenErrorEnvelope
              # pconstant aikenReorderedSidecar
        , pnot
            #$ pverifyCompleteProgramMaterialEntriesV1
              # pconstant aikenErrorEnvelope
              # pnil
        , pnot
            #$ pverifyCompleteProgramMaterialEntriesV1
              # pconstant aikenErrorEnvelope
              # (pcons # pdata entry #$ pcons # pdata extra # pnil)
        ]

isPresent :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s PBool
isPresent value = pmatch value $ \case
  PNothing -> pconstant False
  PJust _ -> pconstant True

isAbsent :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s PBool
isAbsent value = pnot # isPresent value

aikenErrorMaterialEntry :: forall (s :: S). Term s PCekProgramMaterialDatumV1
aikenErrorMaterialEntry =
  pcon $
    PCekProgramMaterialDatumV1
      (pdata 0)
      (pdata (pconstant hashError))
      (pdata (pconstant errorPreimage))

aikenExtraMaterialEntry :: forall (s :: S). Term s PCekProgramMaterialDatumV1
aikenExtraMaterialEntry =
  pcon $
    PCekProgramMaterialDatumV1
      (pdata 0)
      (pdata (pconstant (hashVariable 0)))
      (pdata (pconstant (variablePreimage 0)))

aikenH01, aikenH02, aikenH03 :: BS.ByteString
aikenH01 = BS.replicate 32 0x01
aikenH02 = BS.replicate 32 0x02
aikenH03 = BS.replicate 32 0x03

aikenValuePreimage :: BS.ByteString
aikenValuePreimage = valuePreimage aikenH01 aikenH02 1 aikenH03 0

aikenSequencePreimage :: BS.ByteString
aikenSequencePreimage = sequencePreimage aikenH01 emptySequenceRoot 1

aikenBlobChunkPreimage, aikenBlobBranchPreimage :: BS.ByteString
aikenBlobChunkPreimage = definiteBytes "\xca\xfe"
aikenBlobBranchPreimage = branchPreimage aikenH01 aikenH02 2

aikenErrorEntry, aikenExtraEntry :: Entry
aikenErrorEntry = Entry 0 hashError errorPreimage
aikenExtraEntry = Entry 0 (hashVariable 0) (variablePreimage 0)

aikenErrorEnvelope :: BS.ByteString
aikenErrorEnvelope = encodeEnvelope 1 1 0 hashError 1 2

aikenErrorSidecar, aikenDuplicateSidecar, aikenSubstitutedSidecar :: BS.ByteString
aikenErrorSidecar = encodeSidecar [aikenErrorEntry]
aikenDuplicateSidecar = encodeSidecar [aikenErrorEntry, aikenErrorEntry]
aikenSubstitutedSidecar = encodeSidecar [Entry 0 hashError (variablePreimage 0)]

aikenNoncanonicalSidecar, aikenReorderedSidecar :: BS.ByteString
aikenNoncanonicalSidecar = "\x82\x18\x01" <> BS.drop 2 aikenErrorSidecar
aikenReorderedSidecar = encodeSidecar [aikenExtraEntry, aikenErrorEntry]

--------------------------------------------------------------------------------
-- The domains
--------------------------------------------------------------------------------

domainTests :: TestTree
domainTests =
  testGroup
    "the domains keep the shapes apart"
    [ testCase "the empty sequence root is a node, not the hash of nothing" $
        passertEval $
          pemptySequenceRootV1 #== pconstant (blake2b256 (sequenceDomain <> "\x81\x00"))
    , testCase "the empty environment root" $
        passertEval $
          pemptyEnvironmentRootV1 #== pconstant (blake2b256 (environmentDomain <> "\x81\x00"))
    , testCase "the empty continuation root" $
        passertEval $
          pemptyContinuationRootV1 #== pconstant (blake2b256 (continuationDomain <> "\x81\x00"))
    , testCase "…and all three differ, which is the whole point of the domains" $
        passertEval $
          pand'List
            [ pemptySequenceRootV1 #/= pemptyEnvironmentRootV1
            , pemptySequenceRootV1 #/= pemptyContinuationRootV1
            , pemptyEnvironmentRootV1 #/= pemptyContinuationRootV1
            ]
    , testCase "a sequence link and an environment link share a preimage" $
        hashSequence hA hB 3 @?= blake2b256 (sequenceDomain <> linkPreimage hA hB 3)
    , testCase "…and are told apart only by the domain" $
        passertEval $
          phashSequenceNodeV1 # pconstant hA # pconstant hB # pconstant 3
            #/= phashEnvironmentNodeV1 # pconstant hA # pconstant hB # pconstant 3
    , testCase "a delay term and a lambda term differ by one tag byte" $
        passertEval $
          phashDelayTermV1 # pconstant hA #/= phashLambdaTermV1 # pconstant hA
    , testCase "a delay value and a lambda value likewise" $
        passertEval $
          phashDelayValueV1 # pconstant hA # pconstant hB
            #/= phashLambdaValueV1 # pconstant hA # pconstant hB
    , testCase "a term root and a value root over the same preimage differ" $
        passertEval $
          pconstant @PByteString (blake2b256 (termDomain <> unaryPreimage 1 hA))
            #/= pconstant (blake2b256 (valueDomain <> unaryPreimage 1 hA))
    ]

--------------------------------------------------------------------------------
-- Term roots
--------------------------------------------------------------------------------

termHashTests :: TestTree
termHashTests =
  testGroup
    "term roots"
    [ testCase "a variable" $
        passertEval $ phashVariableTermV1 # pconstant 7 #== pconstant (hashVariable 7)
    , testCase "a variable at index zero" $
        passertEval $ phashVariableTermV1 # pconstant 0 #== pconstant (hashVariable 0)
    , testCase "a variable at the top of uint32" $
        passertEval $
          phashVariableTermV1 # pconstant 4294967295 #== pconstant (hashVariable 4294967295)
    , testCase "a variable past uint32 aborts" $
        pfails $ phashVariableTermV1 # pconstant 4294967296
    , testCase "a negative variable aborts" $
        pfails $ phashVariableTermV1 # pconstant (-1)
    , testCase "a delay" $
        passertEval $ phashDelayTermV1 # pconstant hA #== pconstant (hashUnaryTerm 1 hA)
    , testCase "a lambda" $
        passertEval $ phashLambdaTermV1 # pconstant hA #== pconstant (hashUnaryTerm 2 hA)
    , testCase "a force" $
        passertEval $ phashForceTermV1 # pconstant hA #== pconstant (hashUnaryTerm 5 hA)
    , testCase "a constant" $
        passertEval $ phashConstantTermV1 # pconstant hA #== pconstant (hashUnaryTerm 4 hA)
    , testCase "the runtime-only context constant, at tag 10" $
        passertEval $
          phashContextConstantTermV1 # pconstant hA #== pconstant (hashUnaryTerm 10 hA)
    , testCase "…which is not the same root as an ordinary constant" $
        passertEval $
          phashContextConstantTermV1 # pconstant hA #/= phashConstantTermV1 # pconstant hA
    , testCase "a unary term over a short child aborts" $
        pfails $ phashDelayTermV1 # pconstant short
    , testCase "an application" $
        passertEval $
          phashApplicationTermV1 # pconstant hA # pconstant hB
            #== pconstant (hashApplication hA hB)
    , testCase "…and it is not symmetric" $
        passertEval $
          phashApplicationTermV1 # pconstant hA # pconstant hB
            #/= phashApplicationTermV1 # pconstant hB # pconstant hA
    , testCase "an error" $
        passertEval $ phashErrorTermV1 #== pconstant hashError
    , testCase "a builtin" $
        passertEval $ phashBuiltinTermV1 # pconstant 12 #== pconstant (hashBuiltinTerm 12)
    , testCase "a builtin at tag zero" $
        passertEval $ phashBuiltinTermV1 # pconstant 0 #== pconstant (hashBuiltinTerm 0)
    , testCase "a builtin at the highest tag, which is ripemd_160" $
        passertEval $ phashBuiltinTermV1 # pconstant 86 #== pconstant (hashBuiltinTerm 86)
    , testCase "a builtin one past the highest tag aborts" $
        pfails $ phashBuiltinTermV1 # pconstant 87
    , testCase "a negative builtin tag aborts" $
        pfails $ phashBuiltinTermV1 # pconstant (-1)
    , testCase "a constr" $
        passertEval $
          phashConstrTermV1 # pconstant 3 # pconstant 2 # pconstant hA
            #== pconstant (hashConstrTerm 3 2 hA)
    , testCase "a constr at a 64-bit tag" $
        passertEval $
          phashConstrTermV1 # pconstant 18446744073709551615 # pconstant 0 # pconstant hA
            #== pconstant (hashConstrTerm 18446744073709551615 0 hA)
    , testCase "a constr past a 64-bit tag aborts" $
        pfails $
          phashConstrTermV1 # pconstant 18446744073709551616 # pconstant 0 # pconstant hA
    , testCase "a constr with a count past uint32 aborts" $
        pfails $ phashConstrTermV1 # pconstant 0 # pconstant 4294967296 # pconstant hA
    , testCase "a case" $
        passertEval $
          phashCaseTermV1 # pconstant hA # pconstant 2 # pconstant hB
            #== pconstant (hashCaseTerm hA 2 hB)
    , testCase "a case with a short scrutinee aborts" $
        pfails $ phashCaseTermV1 # pconstant short # pconstant 2 # pconstant hB
    ]

--------------------------------------------------------------------------------
-- Value roots
--------------------------------------------------------------------------------

valueHashTests :: TestTree
valueHashTests =
  testGroup
    "value roots"
    [ testCase "a constant value" $
        passertEval $
          phashConstantValueV1
            # pconstant hA
            # pconstant hB
            # pconstant 40
            # pconstant hC
            # pconstant 12
            #== pconstant (hashConstantValue hA hB 40 hC 12)
    , testCase "…with the payload and semantic roots equal, as a program's must be" $
        passertEval $
          phashConstantValueV1
            # pconstant hA
            # pconstant hB
            # pconstant 40
            # pconstant hB
            # pconstant 12
            #== pconstant (hashConstantValue hA hB 40 hB 12)
    , testCase "a constant value with a short type root aborts" $
        pfails $
          phashConstantValueV1
            # pconstant short
            # pconstant hB
            # pconstant 40
            # pconstant hC
            # pconstant 12
    , testCase "a constant value with a negative payload length aborts" $
        pfails $
          phashConstantValueV1
            # pconstant hA
            # pconstant hB
            # pconstant (-1)
            # pconstant hC
            # pconstant 12
    , testCase "a lambda value" $
        passertEval $
          phashLambdaValueV1 # pconstant hA # pconstant hB
            #== pconstant (hashClosureValue 1 hA hB)
    , testCase "a delay value" $
        passertEval $
          phashDelayValueV1 # pconstant hA # pconstant hB
            #== pconstant (hashClosureValue 2 hA hB)
    , testCase "a constr value" $
        passertEval $
          phashConstrValueV1 # pconstant 3 # pconstant 2 # pconstant hA
            #== pconstant (hashConstrValue 3 2 hA)
    , testCase "a builtin value" $
        passertEval $
          phashBuiltinValueV1 # pconstant 12 # pconstant 1 # pconstant 2 # pconstant hA
            #== pconstant (hashBuiltinValue 12 1 2 hA)
    , testCase "a builtin value past the highest tag aborts" $
        pfails $
          phashBuiltinValueV1 # pconstant 87 # pconstant 0 # pconstant 0 # pconstant hA
    , testCase "a Miller-loop value, which travels as its expression's root" $
        passertEval $
          phashBlsMillerLoopValueV1 # pconstant hA #== pconstant (hashMillerLoopValue hA)
    , testCase "a Miller-loop expression" $
        passertEval $
          phashBlsMillerLoopExpressionV1 # pconstant hA # pconstant hB
            #== pconstant (hashBlsExpression 0 hA hB)
    , testCase "a multiply expression" $
        passertEval $
          phashBlsMultiplyExpressionV1 # pconstant hA # pconstant hB
            #== pconstant (hashBlsExpression 1 hA hB)
    , testCase "…and the two expressions differ, so a product is not a loop" $
        passertEval $
          phashBlsMillerLoopExpressionV1 # pconstant hA # pconstant hB
            #/= phashBlsMultiplyExpressionV1 # pconstant hA # pconstant hB
    ]

--------------------------------------------------------------------------------
-- Sequence and environment links
--------------------------------------------------------------------------------

linkHashTests :: TestTree
linkHashTests =
  testGroup
    "sequence and environment links"
    [ testCase "a sequence link" $
        passertEval $
          phashSequenceNodeV1 # pconstant hA # pconstant hB # pconstant 3
            #== pconstant (hashSequence hA hB 3)
    , testCase "an environment link" $
        passertEval $
          phashEnvironmentNodeV1 # pconstant hA # pconstant hB # pconstant 3
            #== pconstant (hashEnvironment hA hB 3)
    , testCase "a link of length one" $
        passertEval $
          phashSequenceNodeV1 # pconstant hA # pconstant hB # pconstant 1
            #== pconstant (hashSequence hA hB 1)
    , testCase "a link of length zero aborts: that is what the empty root is for" $
        pfails $ phashSequenceNodeV1 # pconstant hA # pconstant hB # pconstant 0
    , testCase "…and so does a negative length" $
        pfails $ phashEnvironmentNodeV1 # pconstant hA # pconstant hB # pconstant (-1)
    , testCase "a length past uint32 aborts" $
        pfails $ phashSequenceNodeV1 # pconstant hA # pconstant hB # pconstant 4294967296
    , testCase "a short tail aborts" $
        pfails $ phashSequenceNodeV1 # pconstant hA # pconstant short # pconstant 1
    ]

--------------------------------------------------------------------------------
-- Continuation frames
--------------------------------------------------------------------------------

continuationHashTests :: TestTree
continuationHashTests =
  testGroup
    "continuation frames"
    [ testCase "force" $
        passertEval $
          phashForceContinuationV1 # pconstant hA #== pconstant (hashForceCont hA)
    , testCase "apply argument" $
        passertEval $
          phashApplyArgumentContinuationV1 # pconstant hA # pconstant hB # pconstant hC
            #== pconstant (hashApplyArgumentCont hA hB hC)
    , testCase "apply function" $
        passertEval $
          phashApplyFunctionContinuationV1 # pconstant hA # pconstant hB
            #== pconstant (hashApplyFunctionCont hA hB)
    , testCase "constr, the nine-item frame" $
        passertEval $
          phashConstrContinuationV1
            # pconstant 3
            # pconstant 2
            # pconstant hA
            # pconstant 1
            # pconstant hB
            # pconstant hC
            # pconstant hD
            #== pconstant (hashConstrCont 3 2 hA 1 hB hC hD)
    , testCase "case" $
        passertEval $
          phashCaseContinuationV1 # pconstant 2 # pconstant hA # pconstant hB # pconstant hC
            #== pconstant (hashCaseCont 2 hA hB hC)
    , testCase "apply value" $
        passertEval $
          phashApplyValueContinuationV1 # pconstant hA # pconstant hB
            #== pconstant (hashApplyValueCont hA hB)
    , testCase "case select" $
        passertEval $
          phashCaseSelectContinuationV1 # pconstant hA # pconstant hB # pconstant 2
            #== pconstant (hashCaseSelectCont hA hB 2)
    , testCase "case apply" $
        passertEval $
          phashCaseApplyContinuationV1 # pconstant hA # pconstant hB
            #== pconstant (hashCaseApplyCont hA hB)
    , testCase "the eight frame tags are eight different roots" $
        passertEval $
          pand'List
            [ phashApplyFunctionContinuationV1 # pconstant hA # pconstant hB
                #/= phashApplyValueContinuationV1 # pconstant hA # pconstant hB
            , phashApplyFunctionContinuationV1 # pconstant hA # pconstant hB
                #/= phashCaseApplyContinuationV1 # pconstant hA # pconstant hB
            , phashApplyValueContinuationV1 # pconstant hA # pconstant hB
                #/= phashCaseApplyContinuationV1 # pconstant hA # pconstant hB
            ]
    , testCase "a constr frame with a short environment aborts" $
        pfails $
          phashConstrContinuationV1
            # pconstant 3
            # pconstant 2
            # pconstant hA
            # pconstant 1
            # pconstant hB
            # pconstant short
            # pconstant hD
    ]

--------------------------------------------------------------------------------
-- Blobs
--------------------------------------------------------------------------------

blobTests :: TestTree
blobTests =
  testGroup
    "the bounded blob root"
    [ testGroup
        "one chunk"
        [ testCase (show n <> " bytes") $
            passertEval $
              pboundedBlobRootV1 # pconstant (sample n) #== pconstant (blobRoot (sample n))
        | n <- [0, 1, 23, 24, 255, 256, 1000, 4094, 4095]
        ]
    , testGroup
        "two chunks"
        [ testCase (show n <> " bytes") $
            passertEval $
              pboundedBlobRootV1 # pconstant (sample n) #== pconstant (blobRoot (sample n))
        | n <- [4096, 5000, 8189, 8190]
        ]
    , testGroup
        "three chunks"
        [ testCase (show n <> " bytes") $
            passertEval $
              pboundedBlobRootV1 # pconstant (sample n) #== pconstant (blobRoot (sample n))
        | n <- [8191, 9000, 9215]
        ]
    , testCase "one byte past the bound aborts" $
        pfails $ pboundedBlobRootV1 # pconstant (sample 9216)
    , testCase "a chunk hash" $
        passertEval $
          phashBlobChunkV1 # pconstant (sample 100) #== pconstant (hashBlobChunk (sample 100))
    , testCase "a chunk past the chunk bound aborts" $
        pfails $ phashBlobChunkV1 # pconstant (sample 4096)
    , testCase "a branch hash" $
        passertEval $
          phashBlobBranchV1 # pconstant hA # pconstant hB # pconstant 4200
            #== pconstant (hashBlobBranch hA hB 4200)
    , testCase "a branch over a short left aborts" $
        pfails $ phashBlobBranchV1 # pconstant short # pconstant hB # pconstant 4200
    ]
  where
    sample n = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. n - 1 :: Int]]

--------------------------------------------------------------------------------
-- Machine states
--------------------------------------------------------------------------------

machineStateTests :: TestTree
machineStateTests =
  testGroup
    "machine states"
    [ testGroup
        "every mode"
        [ testCase (show mode) $
            passertEval $
              machineT mode #== pconstant (hashMachineState mode 5 hA hB hC 0 1000 20)
        | mode <- [0 .. 8]
        ]
    , testCase "mode nine aborts" $ pfails (machineT 9)
    , testCase "a negative mode aborts" $ pfails (machineT (-1))
    , testCase "an execution index past uint32 aborts" $
        pfails $
          phashMachineStateV1
            # pconstant 0
            # pconstant 4294967296
            # pconstant hA
            # pconstant hB
            # pconstant hC
            # pconstant 0
            # pconstant 0
            # pconstant 0
    , testCase "a short focus root aborts" $
        pfails $
          phashMachineStateV1
            # pconstant 0
            # pconstant 1
            # pconstant short
            # pconstant hB
            # pconstant hC
            # pconstant 0
            # pconstant 0
            # pconstant 0
    , testCase "two states differing only in cpu are different roots" $
        passertEval $
          pconstant @PByteString (hashMachineState 0 5 hA hB hC 0 1000 20)
            #/= pconstant (hashMachineState 0 5 hA hB hC 0 1001 20)
    ]
  where
    machineT :: forall (s :: S). Integer -> Term s PByteString
    machineT mode =
      phashMachineStateV1
        # pconstant mode
        # pconstant 5
        # pconstant hA
        # pconstant hB
        # pconstant hC
        # pconstant 0
        # pconstant 1000
        # pconstant 20

--------------------------------------------------------------------------------
-- The envelope
--------------------------------------------------------------------------------

goodEnvelope :: BS.ByteString
goodEnvelope = encodeEnvelope 1 1 0 hA 8 400

envelopeTests :: TestTree
envelopeTests =
  testGroup
    "the program envelope"
    [ testCase "encodes as the format says" $
        passertEval $
          pencodeProgramEnvelopeV1
            # pconstant 1
            # pconstant 1
            # pconstant 0
            # pconstant hA
            # pconstant 8
            # pconstant 400
            #== pconstant goodEnvelope
    , testCase "and fits the 50-byte cap it declares" $
        assertBool "envelope within cap" (BS.length goodEnvelope <= 50)
    , testCase "the largest legal envelope is exactly the 50 bytes it claims" $
        BS.length (encodeEnvelope 1 1 0 hA 1597819 67108418) @?= 50
    , testCase "…and it is the uint32 node count and DA bound that make it exact" $
        assertBool
          "a uint64 material length would overflow the cap"
          (BS.length (encodeEnvelope 1 1 0 hA 4294967295 18446744073709551615) > 50)
    , testCase "hashes under the envelope domain" $
        passertEval $
          phashProgramEnvelopeV1
            # pconstant 1
            # pconstant 1
            # pconstant 0
            # pconstant hA
            # pconstant 8
            # pconstant 400
            #== pconstant (blake2b256 (envelopeDomain <> goodEnvelope))
    , testCase "round-trips through the inspector" $
        passertEval $ envelopeIs goodEnvelope hA 8 400
    , testCase "…and through the aborting decoder" $
        passertEval $
          pmatch (pdecodeProgramEnvelopeV1 # pconstant goodEnvelope) $
            \(PProgramEnvelopeV1 termRoot _ _) -> pfromData termRoot #== pconstant hA
    , testGroup
        "the inspector declines"
        [ testCase "a UPLC version that is not 1.1.0" $
            passertEval $ declinesEnvelope (encodeEnvelope 1 1 1 hA 8 400)
        , testCase "…at a different major" $
            passertEval $ declinesEnvelope (encodeEnvelope 2 1 0 hA 8 400)
        , testCase "a node count of zero" $
            passertEval $ declinesEnvelope (encodeEnvelope 1 1 0 hA 0 400)
        , testCase "a node count past the structural bound" $
            passertEval $ declinesEnvelope (encodeEnvelope 1 1 0 hA 1597820 400)
        , testCase "…and at the bound it is accepted" $
            passertEval $ envelopeIs (encodeEnvelope 1 1 0 hA 1597819 400) hA 1597819 400
        , testCase "a material byte length of zero" $
            passertEval $ declinesEnvelope (encodeEnvelope 1 1 0 hA 8 0)
        , testCase "a material byte length past the DA bound" $
            passertEval $ declinesEnvelope (encodeEnvelope 1 1 0 hA 8 67108419)
        , testCase "a term root that is not 32 bytes" $
            passertEval $
              declinesEnvelope $
                BS.concat
                  [ "\x85"
                  , cborI 1
                  , "\x83" <> cborI 1 <> cborI 1 <> cborI 0
                  , definiteBytes short
                  , cborI 8
                  , cborI 400
                  ]
        , testCase "trailing bytes" $
            passertEval $ declinesEnvelope (goodEnvelope <> "\x00")
        , testCase "an envelope longer than the cap, before anything is decoded" $
            passertEval $ declinesEnvelope (goodEnvelope <> BS.replicate 40 0x00)
        , testCase "a non-minimal integer header" $
            passertEval $
              declinesEnvelope $
                BS.concat
                  [ "\x85"
                  , "\x18\x01"
                  , "\x83" <> cborI 1 <> cborI 1 <> cborI 0
                  , definiteBytes hA
                  , cborI 8
                  , cborI 400
                  ]
        , testCase "an envelope version that is not 1" $
            passertEval $
              declinesEnvelope $
                BS.concat
                  [ "\x85"
                  , cborI 2
                  , "\x83" <> cborI 1 <> cborI 1 <> cborI 0
                  , definiteBytes hA
                  , cborI 8
                  , cborI 400
                  ]
        , testCase "an empty envelope" $ passertEval $ declinesEnvelope ""
        ]
    , testCase "and the aborting decoder aborts where the inspector declines" $
        pfails $ pdecodeProgramEnvelopeV1 # pconstant (encodeEnvelope 1 1 1 hA 8 400)
    ]

envelopeIs ::
  forall (s :: S).
  BS.ByteString -> BS.ByteString -> Integer -> Integer -> Term s PBool
envelopeIs bytes termRoot nodeCount byteLength =
  pmatch (pinspectProgramEnvelopeV1 # pconstant bytes) $ \case
    PNothing -> pconstant @PBool False
    PJust envelope ->
      pmatch envelope $ \(PProgramEnvelopeV1 gotRoot gotCount gotLength) ->
        pand'List
          [ pfromData gotRoot #== pconstant termRoot
          , pfromData gotCount #== pconstant nodeCount
          , pfromData gotLength #== pconstant byteLength
          ]

declinesEnvelope :: forall (s :: S). BS.ByteString -> Term s PBool
declinesEnvelope bytes =
  pmatch (pinspectProgramEnvelopeV1 # pconstant bytes) $ \case
    PNothing -> pconstant @PBool True
    PJust _ -> pconstant @PBool False

--------------------------------------------------------------------------------
-- Term material inspection
--------------------------------------------------------------------------------

termInspectionTests :: TestTree
termInspectionTests =
  testGroup
    "term material round-trips"
    [ testGroup
        "each source shape"
        [ testCase name $ passertEval $ termInspects preimage
        | (name, preimage) <-
            [ ("a variable", variablePreimage 7)
            , ("a variable at zero", variablePreimage 0)
            , ("a delay", unaryPreimage 1 hA)
            , ("a lambda", unaryPreimage 2 hA)
            , ("a force", unaryPreimage 5 hA)
            , ("a constant", unaryPreimage 4 hA)
            , ("an error", errorPreimage)
            , ("a builtin", builtinPreimage 12)
            , ("a builtin at tag zero", builtinPreimage 0)
            , ("a builtin at the highest tag", builtinPreimage 86)
            , ("an application", applicationPreimage hA hB)
            , ("a constr", constrPreimage 3 2 hA)
            , ("a constr at count zero", constrPreimage 0 0 hA)
            , ("a case", casePreimage hA 2 hB)
            ]
        ]
    , testGroup
        "and declines"
        [ testCase "the runtime-only context constant, tag 10" $
            passertEval $ declinesTerm (unaryPreimage 10 hA)
        , testCase "a tag no shape uses" $
            passertEval $ declinesTerm (unaryPreimage 11 hA)
        , testCase "a builtin one past the highest tag" $
            passertEval $ declinesTerm (builtinPreimage 87)
        , testCase "a negative builtin tag" $
            passertEval $ declinesTerm (builtinPreimage (-1))
        , testCase "a variable past uint32" $
            passertEval $ declinesTerm (variablePreimage 4294967296)
        , testCase "a unary term over a 31-byte child" $
            passertEval $ declinesTerm (unaryPreimage 1 short)
        , testCase "an application at the wrong tag" $
            passertEval $
              declinesTerm (BS.concat ["\x83", cborI 4, definiteBytes hA, definiteBytes hB])
        , testCase "a constr with a count past uint32" $
            passertEval $ declinesTerm (constrPreimage 3 4294967296 hA)
        , testCase "an error at the wrong tag" $
            passertEval $ declinesTerm (BS.concat ["\x81", cborI 5])
        , testCase "an arity no shape uses" $
            passertEval $
              declinesTerm
                (BS.concat ["\x85", cborI 0, cborI 1, cborI 2, cborI 3, cborI 4])
        , testCase "trailing bytes" $
            passertEval $ declinesTerm (errorPreimage <> "\x00")
        , testCase "an indefinite-length array" $
            passertEval $
              declinesTerm (BS.concat ["\x9f", cborI 0, cborI 7, "\xff"])
        , testCase "a non-minimal integer header" $
            passertEval $ declinesTerm (BS.concat ["\x82", cborI 0, "\x18\x07"])
        , testCase "an empty preimage" $ passertEval $ declinesTerm ""
        ]
    ]

termInspects :: forall (s :: S). BS.ByteString -> Term s PBool
termInspects preimage =
  pmatch (pinspectProgramTermMaterialV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool False
    PJust _ -> pconstant @PBool True

declinesTerm :: forall (s :: S). BS.ByteString -> Term s PBool
declinesTerm preimage =
  pmatch (pinspectProgramTermMaterialV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool True
    PJust _ -> pconstant @PBool False

--------------------------------------------------------------------------------
-- Value and sequence material inspection
--------------------------------------------------------------------------------

valueInspectionTests :: TestTree
valueInspectionTests =
  testGroup
    "value and sequence material"
    [ testCase "a constant value round-trips" $
        passertEval $ valueInspects (valuePreimage hA hB 40 hC 12)
    , testCase "…with equal payload and semantic roots" $
        passertEval $ valueInspects (valuePreimage hA hB 40 hB 12)
    , testCase "a value at the wrong tag declines" $
        passertEval $
          declinesValue $
            BS.concat
              [ "\x86"
              , cborI 1
              , definiteBytes hA
              , definiteBytes hB
              , cborI 40
              , definiteBytes hC
              , cborI 12
              ]
    , testCase "a value with a 31-byte payload root declines" $
        passertEval $ declinesValue (valuePreimage hA short 40 hC 12)
    , testCase "a value with a negative memory declines" $
        passertEval $ declinesValue (valuePreimage hA hB 40 hC (-1))
    , testCase "a value at the wrong arity declines" $
        passertEval $ declinesValue (sequencePreimage hA hB 3)
    , testCase "a sequence link round-trips" $
        passertEval $ sequenceInspects (sequencePreimage hA hB 3)
    , testCase "…at length one" $
        passertEval $ sequenceInspects (sequencePreimage hA hB 1)
    , testCase "a sequence link of length zero declines" $
        passertEval $ declinesSequence (sequencePreimage hA hB 0)
    , testCase "a sequence link past uint32 declines" $
        passertEval $ declinesSequence (sequencePreimage hA hB 4294967296)
    , testCase "a sequence link at the wrong tag declines" $
        passertEval $
          declinesSequence
            (BS.concat ["\x84", cborI 2, definiteBytes hA, definiteBytes hB, cborI 3])
    , testCase "a term preimage is not a sequence link" $
        passertEval $ declinesSequence (constrPreimage 3 2 hA)
    ]

valueInspects, declinesValue, sequenceInspects, declinesSequence ::
  forall (s :: S). BS.ByteString -> Term s PBool
valueInspects preimage =
  pmatch (pinspectProgramValueMaterialV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool False
    PJust _ -> pconstant @PBool True
declinesValue preimage =
  pmatch (pinspectProgramValueMaterialV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool True
    PJust _ -> pconstant @PBool False
sequenceInspects preimage =
  pmatch (pinspectProgramSequenceMaterialV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool False
    PJust _ -> pconstant @PBool True
declinesSequence preimage =
  pmatch (pinspectProgramSequenceMaterialV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool True
    PJust _ -> pconstant @PBool False

--------------------------------------------------------------------------------
-- Blob material inspection
--------------------------------------------------------------------------------

blobInspectionTests :: TestTree
blobInspectionTests =
  testGroup
    "blob material"
    [ testCase "a chunk round-trips at kind 3" $
        passertEval $ blobInspects 3 (definiteBytes "midgard")
    , testCase "the empty chunk cannot be published: 0x40 alone is not readable CBOR" $
        passertEval $ declinesBlob 3 (definiteBytes "")
    , testCase "…though a one-byte chunk is" $
        passertEval $ blobInspects 3 (definiteBytes "\x00")
    , testCase "a branch round-trips at kind 4" $
        passertEval $ blobInspects 4 (branchPreimage hA hB 8190)
    , testCase "a chunk read as a branch declines" $
        passertEval $ declinesBlob 4 (definiteBytes "midgard")
    , testCase "a branch read as a chunk declines" $
        passertEval $ declinesBlob 3 (branchPreimage hA hB 8190)
    , testCase "a kind that is neither declines" $
        passertEval $ declinesBlob 5 (definiteBytes "midgard")
    , testCase "a branch with a byte length of zero declines" $
        passertEval $ declinesBlob 4 (branchPreimage hA hB 0)
    , testCase "a branch with a 31-byte side declines" $
        passertEval $ declinesBlob 4 (branchPreimage short hB 8190)
    , testCase "a chunk past the chunk bound declines" $
        passertEval $ declinesBlob 3 (definiteBytes (BS.replicate 4096 0x00))
    , testCase "…and at the bound it is accepted" $
        passertEval $ blobInspects 3 (definiteBytes (BS.replicate 4095 0x00))
    ]

blobInspects, declinesBlob :: forall (s :: S). Integer -> BS.ByteString -> Term s PBool
blobInspects kind preimage =
  pmatch (pinspectProgramBlobMaterialV1 # pconstant kind # pconstant preimage) $ \case
    PNothing -> pconstant @PBool False
    PJust _ -> pconstant @PBool True
declinesBlob kind preimage =
  pmatch (pinspectProgramBlobMaterialV1 # pconstant kind # pconstant preimage) $ \case
    PNothing -> pconstant @PBool True
    PJust _ -> pconstant @PBool False

--------------------------------------------------------------------------------
-- The sidecar
--------------------------------------------------------------------------------

sidecarTests :: TestTree
sidecarTests =
  testGroup
    "the material sidecar"
    [ testCase "an empty sidecar parses" $
        passertEval $ sidecarParses (encodeSidecar [])
    , testCase "the program's own sidecar parses" $
        passertEval $ sidecarParses programSidecar
    , testCase "an unsorted sidecar declines" $
        passertEval $ declinesSidecar (encodeSidecar (reverse programEntries))
    , testCase "a sidecar with a duplicate root declines" $
        passertEval $
          declinesSidecar (encodeSidecar (sortEntries (head programEntries : programEntries)))
    , testCase "a sidecar at the wrong version declines" $
        passertEval $
          declinesSidecar $
            BS.concat
              [ "\x82"
              , cborI 2
              , definiteArrayHeader (length programEntries)
              , BS.concat (map encodeEntry programEntries)
              ]
    , testCase "a sidecar whose array header disagrees with its contents declines" $
        passertEval $
          declinesSidecar $
            BS.concat
              [ "\x82"
              , cborI 1
              , definiteArrayHeader (length programEntries + 1)
              , BS.concat (map encodeEntry programEntries)
              ]
    , testCase "an entry at a kind past seven declines" $
        passertEval $ declinesSidecar (encodeSidecar [Entry 8 hA "\x00"])
    , testCase "an entry with an empty preimage declines" $
        passertEval $ declinesSidecar (encodeSidecar [Entry 0 hA ""])
    , testCase "an entry with a 31-byte root declines" $
        passertEval $ declinesSidecar (encodeSidecar [Entry 0 short "\x00"])
    , testCase "trailing bytes decline" $
        passertEval $ declinesSidecar (programSidecar <> "\x00")
    , testCase "an empty sidecar body declines" $ passertEval $ declinesSidecar ""
    ]

sidecarParses, declinesSidecar :: forall (s :: S). BS.ByteString -> Term s PBool
sidecarParses bytes =
  pmatch (pinspectCompleteProgramMaterialSidecarV1 # pconstant bytes) $ \case
    PNothing -> pconstant @PBool False
    PJust _ -> pconstant @PBool True
declinesSidecar bytes =
  pmatch (pinspectCompleteProgramMaterialSidecarV1 # pconstant bytes) $ \case
    PNothing -> pconstant @PBool True
    PJust _ -> pconstant @PBool False

--------------------------------------------------------------------------------
-- The completeness walk
--------------------------------------------------------------------------------

completenessTests :: TestTree
completenessTests =
  testGroup
    "the completeness walk over a real program"
    [ testCase "the program verifies" $
        passertEval $
          pverifyCompleteProgramMaterialV1
            # pconstant programEnvelope
            # pconstant programSidecar
    , testCase "…and so does the entries form, over the parsed sidecar" $
        passertEval $
          pmatch (pinspectCompleteProgramMaterialSidecarV1 # pconstant programSidecar) $ \case
            PNothing -> pconstant @PBool False
            PJust entries ->
              pverifyCompleteProgramMaterialEntriesV1 # pconstant programEnvelope # entries
    , testGroup
        "and it refuses"
        [ testCase "a node count one too low" $
            refuses (envelopeFor programNodeCount' programByteLength) programSidecar
        , testCase "a node count one too high" $
            refuses (envelopeFor (programNodeCount + 1) programByteLength) programSidecar
        , testCase "a material byte length one off" $
            refuses (envelopeFor programNodeCount (programByteLength + 1)) programSidecar
        , testCase "a term root nothing in the sidecar answers to" $
            refuses (encodeEnvelope 1 1 0 hD programNodeCount programByteLength) programSidecar
        , testCase "an entry the walk never reaches" $
            refuses programEnvelope (encodeSidecar (sortEntries (unreachable : programEntries)))
        , testCase "…even when the envelope's counts are adjusted to admit it" $
            refuses
              (envelopeFor (programNodeCount + 1) (programByteLength + 1))
              (encodeSidecar (sortEntries (unreachable : programEntries)))
        , testCase "a sidecar missing one node" $
            refuses programEnvelope (encodeSidecar (drop 1 programEntries))
        , testCase "a term entry filed under the value kind" $
            refuses programEnvelope (encodeSidecar (sortEntries miskinded))
        , testCase "an entry filed at a root that is not its own hash" $
            refuses programEnvelope (encodeSidecar (sortEntries misfiled))
        , testCase "a malformed envelope" $
            refuses (encodeEnvelope 1 1 1 termRootOf programNodeCount programByteLength) programSidecar
        , testCase "a malformed sidecar" $ refuses programEnvelope (programSidecar <> "\x00")
        ]
    , testGroup
        "the zero-count sequence rule"
        [ testCase "a constr of no terms pointing at the canonical empty root verifies" $
            passertEval $
              pverifyCompleteProgramMaterialV1
                # pconstant (envelopeOf emptyConstrProgram)
                # pconstant (sidecarOf emptyConstrProgram)
        , testCase "…and pointing anywhere else does not" $
            refuses (envelopeOf badEmptyConstrProgram) (sidecarOf badEmptyConstrProgram)
        ]
    ]
  where
    programNodeCount' = programNodeCount - 1
    refuses envelope sidecar =
      passertEval $
        pnot #$ pverifyCompleteProgramMaterialV1 # pconstant envelope # pconstant sidecar
    unreachable = Entry 0 (hashTerm (variablePreimage 99)) (variablePreimage 99)
    -- Pick the entries to mutate by kind, not by position: the sidecar is
    -- sorted by root, so which entry comes first is an accident of hashing.
    miskinded = [if entryKind e == 0 then e {entryKind = 1} else e | e <- programEntries]
    misfiled =
      [ if entryKind e == 5 then e {entryRoot = hD} else e
      | e <- programEntries
      ]
    envelopeFor = encodeEnvelope 1 1 0 termRootOf
    termRootOf = programTermRoot

--------------------------------------------------------------------------------
-- The reference program
--------------------------------------------------------------------------------

{- | @(λx. x) c@, where @c@ is the @Data@ integer 42.

Eight nodes: four terms, one value, the value's type blob, the semantic @Data@
node and that node's own blob leaf. The constant is an integer rather than a
byte string because 'Midgard.CekData.pinspectDataNodePreimageV1' cannot inspect
a byte node at all — see "Testing.CekData".
-}
data Entry = Entry
  { entryKind :: Integer
  , entryRoot :: BS.ByteString
  , entryPreimage :: BS.ByteString
  }
  deriving stock (Show, Eq)

constantData :: PD.Data
constantData = PD.I 42

-- | The blob leaf holding the constant's CBOR, and the type blob beside it.
constantCbor, typeBytes :: BS.ByteString
constantCbor = ser constantData
typeBytes = "\x00"

semanticNodePreimage :: BS.ByteString
semanticNodePreimage =
  BS.concat
    [ arrayHeader 4
    , cborI 4
    , definiteBytes (blobRoot constantCbor)
    , cborI (fromIntegral (BS.length constantCbor))
    , cborI (4 + integerMemorySize 42)
    ]

semanticRoot :: BS.ByteString
semanticRoot = blake2b256 (dataNodeDomain <> semanticNodePreimage)

valuePreimageOfProgram :: BS.ByteString
valuePreimageOfProgram =
  valuePreimage
    (blobRoot typeBytes)
    semanticRoot
    (fromIntegral (BS.length constantCbor))
    semanticRoot
    (4 + integerMemorySize 42)

programTermRoot :: BS.ByteString
programTermRoot =
  hashTerm (applicationPreimage (hashTerm lambdaP) (hashTerm constantTermP))
  where
    lambdaP = unaryPreimage 2 (hashTerm (variablePreimage 0))
    constantTermP = unaryPreimage 4 (hashValue valuePreimageOfProgram)

programEntries :: [Entry]
programEntries = sortEntries (map termEntry terms <> [valueE, typeE, dataE, dataBlobE])
  where
    variableP = variablePreimage 0
    lambdaP = unaryPreimage 2 (hashTerm variableP)
    constantTermP = unaryPreimage 4 (hashValue valuePreimageOfProgram)
    applicationP = applicationPreimage (hashTerm lambdaP) (hashTerm constantTermP)
    terms = [variableP, lambdaP, constantTermP, applicationP]
    termEntry p = Entry 0 (hashTerm p) p
    valueE = Entry 1 (hashValue valuePreimageOfProgram) valuePreimageOfProgram
    typeE = Entry 3 (hashBlobChunk typeBytes) (definiteBytes typeBytes)
    dataE = Entry 5 semanticRoot semanticNodePreimage
    dataBlobE = Entry 3 (hashBlobChunk constantCbor) (definiteBytes constantCbor)

programNodeCount :: Integer
programNodeCount = fromIntegral (length programEntries)

programByteLength :: Integer
programByteLength = sum [fromIntegral (BS.length (entryPreimage e)) | e <- programEntries]

programEnvelope :: BS.ByteString
programEnvelope = encodeEnvelope 1 1 0 programTermRoot programNodeCount programByteLength

programSidecar :: BS.ByteString
programSidecar = encodeSidecar programEntries

{- | A constr term of no branches, and the same with its sequence root moved.

The only difference between the two is where a count of zero points: the
canonical empty-sequence root, or an arbitrary hash. The second must fail, or a
program could hang unwalked material off a count nobody reads.
-}
emptyConstrProgram, badEmptyConstrProgram :: [Entry]
emptyConstrProgram = [Entry 0 (hashTerm p) p]
  where
    p = constrPreimage 0 0 emptySequenceRoot
badEmptyConstrProgram = [Entry 0 (hashTerm p) p]
  where
    p = constrPreimage 0 0 hD

envelopeOf :: [Entry] -> BS.ByteString
envelopeOf entries =
  encodeEnvelope
    1
    1
    0
    (entryRoot (head entries))
    (fromIntegral (length entries))
    (sum [fromIntegral (BS.length (entryPreimage e)) | e <- entries])

sidecarOf :: [Entry] -> BS.ByteString
sidecarOf = encodeSidecar . sortEntries

sortEntries :: [Entry] -> [Entry]
sortEntries = sortOn entryRoot

encodeEntry :: Entry -> BS.ByteString
encodeEntry (Entry kind root preimage) =
  BS.concat
    [ "\x82"
    , definiteBytes root
    , BS.concat ["\x83", cborI 1, cborI kind, definiteBytes preimage]
    ]

encodeSidecar :: [Entry] -> BS.ByteString
encodeSidecar entries =
  BS.concat
    [ "\x82"
    , cborI 1
    , definiteArrayHeader (length entries)
    , BS.concat (map encodeEntry entries)
    ]

--------------------------------------------------------------------------------
-- The reference: domains
--------------------------------------------------------------------------------

termDomain, valueDomain, sequenceDomain :: BS.ByteString
termDomain = "MidgardCekTermNodeV1"
valueDomain = "MidgardCekValueNodeV1"
sequenceDomain = "MidgardCekSequenceNodeV1"

environmentDomain, continuationDomain :: BS.ByteString
environmentDomain = "MidgardCekEnvironmentNodeV1"
continuationDomain = "MidgardCekContinuationNodeV1"

chunkDomain, branchDomain, blsDomain :: BS.ByteString
chunkDomain = "MidgardCekBlobChunkV1"
branchDomain = "MidgardCekBlobBranchV1"
blsDomain = "MidgardCekBlsExpressionV1"

machineDomain, envelopeDomain, dataNodeDomain :: BS.ByteString
machineDomain = "MidgardCekMachineStateV1"
envelopeDomain = "MidgardCekProgramEnvelopeV1"
dataNodeDomain = "MidgardCekDataNodeV1"

emptySequenceRoot :: BS.ByteString
emptySequenceRoot = blake2b256 (sequenceDomain <> "\x81\x00")

--------------------------------------------------------------------------------
-- The reference: term preimages
--------------------------------------------------------------------------------

hashTerm, hashValue :: BS.ByteString -> BS.ByteString
hashTerm = blake2b256 . (termDomain <>)
hashValue = blake2b256 . (valueDomain <>)

variablePreimage :: Integer -> BS.ByteString
variablePreimage index = BS.concat ["\x82", cborI 0, cborI index]

unaryPreimage :: Integer -> BS.ByteString -> BS.ByteString
unaryPreimage tag child = BS.concat ["\x82", cborI tag, definiteBytes child]

applicationPreimage :: BS.ByteString -> BS.ByteString -> BS.ByteString
applicationPreimage f a = BS.concat ["\x83", cborI 3, definiteBytes f, definiteBytes a]

errorPreimage :: BS.ByteString
errorPreimage = BS.concat ["\x81", cborI 6]

builtinPreimage :: Integer -> BS.ByteString
builtinPreimage tag = BS.concat ["\x82", cborI 7, cborI tag]

constrPreimage :: Integer -> Integer -> BS.ByteString -> BS.ByteString
constrPreimage tag count root =
  BS.concat ["\x84", cborI 8, cborI tag, cborI count, definiteBytes root]

casePreimage :: BS.ByteString -> Integer -> BS.ByteString -> BS.ByteString
casePreimage scrutinee count root =
  BS.concat ["\x84", cborI 9, definiteBytes scrutinee, cborI count, definiteBytes root]

hashVariable :: Integer -> BS.ByteString
hashVariable = hashTerm . variablePreimage

hashUnaryTerm :: Integer -> BS.ByteString -> BS.ByteString
hashUnaryTerm tag = hashTerm . unaryPreimage tag

hashApplication :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashApplication f a = hashTerm (applicationPreimage f a)

hashError :: BS.ByteString
hashError = hashTerm errorPreimage

hashBuiltinTerm :: Integer -> BS.ByteString
hashBuiltinTerm = hashTerm . builtinPreimage

hashConstrTerm :: Integer -> Integer -> BS.ByteString -> BS.ByteString
hashConstrTerm tag count root = hashTerm (constrPreimage tag count root)

hashCaseTerm :: BS.ByteString -> Integer -> BS.ByteString -> BS.ByteString
hashCaseTerm scrutinee count root = hashTerm (casePreimage scrutinee count root)

--------------------------------------------------------------------------------
-- The reference: value preimages
--------------------------------------------------------------------------------

valuePreimage ::
  BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString -> Integer -> BS.ByteString
valuePreimage typeRoot payloadRoot payloadLength semantic memory =
  BS.concat
    [ "\x86"
    , cborI 0
    , definiteBytes typeRoot
    , definiteBytes payloadRoot
    , cborI payloadLength
    , definiteBytes semantic
    , cborI memory
    ]

hashConstantValue ::
  BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString -> Integer -> BS.ByteString
hashConstantValue t p l s m = hashValue (valuePreimage t p l s m)

hashClosureValue :: Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
hashClosureValue tag body env =
  hashValue (BS.concat ["\x83", cborI tag, definiteBytes body, definiteBytes env])

hashConstrValue :: Integer -> Integer -> BS.ByteString -> BS.ByteString
hashConstrValue tag count root =
  hashValue (BS.concat ["\x84", cborI 3, cborI tag, cborI count, definiteBytes root])

hashBuiltinValue :: Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString
hashBuiltinValue tag forces args root =
  hashValue $
    BS.concat ["\x85", cborI 4, cborI tag, cborI forces, cborI args, definiteBytes root]

hashMillerLoopValue :: BS.ByteString -> BS.ByteString
hashMillerLoopValue root = hashValue (BS.concat ["\x82", cborI 5, definiteBytes root])

hashBlsExpression :: Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
hashBlsExpression tag left right =
  blake2b256 $
    blsDomain <> BS.concat ["\x83", cborI tag, definiteBytes left, definiteBytes right]

--------------------------------------------------------------------------------
-- The reference: links, frames, states
--------------------------------------------------------------------------------

linkPreimage :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
linkPreimage item tail len =
  BS.concat ["\x84", cborI 1, definiteBytes item, definiteBytes tail, cborI len]

sequencePreimage :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
sequencePreimage = linkPreimage

hashSequence, hashEnvironment :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
hashSequence item tail len = blake2b256 (sequenceDomain <> linkPreimage item tail len)
hashEnvironment item tail len = blake2b256 (environmentDomain <> linkPreimage item tail len)

continuation :: BS.ByteString -> BS.ByteString
continuation = blake2b256 . (continuationDomain <>)

hashForceCont :: BS.ByteString -> BS.ByteString
hashForceCont tail = continuation (BS.concat ["\x83", cborI 1, cborI 0, definiteBytes tail])

hashApplyArgumentCont :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
hashApplyArgumentCont arg env tail =
  continuation $
    BS.concat
      ["\x85", cborI 1, cborI 1, definiteBytes arg, definiteBytes env, definiteBytes tail]

hashApplyFunctionCont :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashApplyFunctionCont fv tail =
  continuation (BS.concat ["\x84", cborI 1, cborI 2, definiteBytes fv, definiteBytes tail])

hashConstrCont ::
  Integer ->
  Integer ->
  BS.ByteString ->
  Integer ->
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString
hashConstrCont tag rc rr vc vr env tail =
  continuation $
    BS.concat
      [ definiteArrayHeader 9
      , cborI 1
      , cborI 3
      , cborI tag
      , cborI rc
      , definiteBytes rr
      , cborI vc
      , definiteBytes vr
      , definiteBytes env
      , definiteBytes tail
      ]

hashCaseCont :: Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
hashCaseCont bc br env tail =
  continuation $
    BS.concat
      [ "\x86"
      , cborI 1
      , cborI 4
      , cborI bc
      , definiteBytes br
      , definiteBytes env
      , definiteBytes tail
      ]

hashApplyValueCont :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashApplyValueCont value tail =
  continuation (BS.concat ["\x84", cborI 1, cborI 5, definiteBytes value, definiteBytes tail])

hashCaseSelectCont :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
hashCaseSelectCont env tail vc =
  continuation $
    BS.concat ["\x85", cborI 1, cborI 6, definiteBytes env, definiteBytes tail, cborI vc]

hashCaseApplyCont :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashCaseApplyCont env built =
  continuation (BS.concat ["\x84", cborI 1, cborI 7, definiteBytes env, definiteBytes built])

hashMachineState ::
  Integer ->
  Integer ->
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  Integer ->
  Integer ->
  Integer ->
  BS.ByteString
hashMachineState mode ei focus env cont aux cpu memory =
  blake2b256 $
    machineDomain
      <> BS.concat
        [ definiteArrayHeader 9
        , cborI 1
        , cborI mode
        , cborI ei
        , definiteBytes focus
        , definiteBytes env
        , definiteBytes cont
        , cborI aux
        , cborI cpu
        , cborI memory
        ]

encodeEnvelope ::
  Integer -> Integer -> Integer -> BS.ByteString -> Integer -> Integer -> BS.ByteString
encodeEnvelope major minor patch termRoot nodeCount byteLength =
  BS.concat
    [ "\x85"
    , cborI 1
    , BS.concat ["\x83", cborI major, cborI minor, cborI patch]
    , definiteBytes termRoot
    , cborI nodeCount
    , cborI byteLength
    ]

--------------------------------------------------------------------------------
-- The reference: blobs
--------------------------------------------------------------------------------

maxChunk, maxBlob :: Int
maxChunk = 4095
maxBlob = 9215

hashBlobChunk :: BS.ByteString -> BS.ByteString
hashBlobChunk chunk
  | BS.length chunk > maxChunk = error "reference blob: chunk too long"
  | otherwise = blake2b256 (chunkDomain <> definiteBytesLong chunk)

hashBlobBranch :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
hashBlobBranch left right byteLength =
  blake2b256 $
    BS.concat [branchDomain, "\x83", definiteBytes left, definiteBytes right, cborI byteLength]

blobRoot :: BS.ByteString -> BS.ByteString
blobRoot bytes
  | len > maxBlob = error "reference blob: too long"
  | len <= maxChunk = hashBlobChunk bytes
  | remaining <= maxChunk = left
  | otherwise = hashBlobBranch left (hashBlobChunk third) (fromIntegral len)
  where
    len = BS.length bytes
    remaining = len - maxChunk
    secondLength = min remaining maxChunk
    left =
      hashBlobBranch
        (hashBlobChunk (BS.take maxChunk bytes))
        (hashBlobChunk (BS.take secondLength (BS.drop maxChunk bytes)))
        (fromIntegral (maxChunk + secondLength))
    third = BS.drop (maxChunk + secondLength) bytes

branchPreimage :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
branchPreimage left right byteLength =
  BS.concat ["\x83", definiteBytes left, definiteBytes right, cborI byteLength]

-- | 'definiteBytes' with the 4,095-byte chunk in range.
definiteBytesLong :: BS.ByteString -> BS.ByteString
definiteBytesLong bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | otherwise =
      BS.pack [0x59, fromIntegral (len `div` 256), fromIntegral (len `mod` 256)] <> bytes
  where
    len = BS.length bytes

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

-- | Aiken's @cbor.serialise@, which is @serialiseData@ on the value read as @Data@.
ser :: PD.Data -> BS.ByteString
ser = Builtins.fromBuiltin . Builtins.serialiseData . BI.BuiltinData

cborI :: Integer -> BS.ByteString
cborI = ser . PD.I

arrayHeader :: Int -> BS.ByteString
arrayHeader = definiteArrayHeader

definiteArrayHeader :: Int -> BS.ByteString
definiteArrayHeader n
  | n <= 23 = BS.pack [fromIntegral (128 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | n <= 65535 = BS.pack [0x99, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
  | otherwise = error "reference definiteArrayHeader: out of fixture range"

-- | The @Data@ integer memory the semantic node commits to.
integerMemorySize :: Integer -> Integer
integerMemorySize v = unsignedByteSize (if v < 0 then (negate v - 1) * 2 else v * 2)
  where
    unsignedByteSize n
      | n < 256 = 1
      | otherwise = 1 + unsignedByteSize (n `div` 256)

-- | Plutarch has no disequality operator; every module that wants one spells it.
(#/=) :: forall (s :: S) (a :: S -> Type). PEq a => Term s a -> Term s a -> Term s PBool
a #/= b = pnot # (a #== b)

infix 4 #/=
