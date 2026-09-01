{-# LANGUAGE OverloadedStrings #-}

module Testing.ScriptSourcesRedeemerNormalization (tests) where

import Data.ByteString qualified as BS
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Aiken.Cbor (pdeserialise)
import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.CekDataFrame (pinitialListFrameV1, pinitialMapFrameV1)
import Midgard.CekDataFrame qualified as Frame
import Midgard.CekDataTraverse (
  PDataTraverseActionV1 (..),
  PDataTraverseControlV1 (..),
 )
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Codec qualified as Codec
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.RedeemerItemProof qualified as Redeemer
import Midgard.ScriptSourcesRedeemerNormalization
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..))
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
  ppreparedResolutionVersion,
  presolutionVersion,
  pwinningResolution,
 )
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationSourceKind (..),
  PValidationVerdict (..),
  phashValidationContext,
  phashWorkWitness,
  pmachineVersion,
 )
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.ScriptSourcesRedeemerNormalization"
  [ handoffTests
  , identityTests
  , foldMapTests
  , finalizeTests
  , stateTests
  , templateTests
  , outerTests
  , attestedTests
  , envelopeTests
  , settlementTests
  , rawEnvelopeTests
  ]

identityTests :: TestTree
identityTests = testGroup "identity"
  [ testCase "narrow commitment rejects wrong family and preimage" $
      passertEvalNoTrace narrowCommitmentMutations
  , testCase "fold-map identity rejects field sibling family and hash mutations" $
      passertEvalNoTrace foldMapIdentityMutations
  , testCase "finalize identity rejects frame parent family and hash mutations" $
      passertEvalNoTrace finalizeIdentityMutations
  ]

stateTests :: TestTree
stateTests = testGroup "state"
  [ testCase "predecessor deployment and route mutations reject" $
      passertEvalNoTrace predecessorDeploymentRouteMutations
  ]

templateTests :: TestTree
templateTests = testGroup "template"
  [ testCase "family templates are exact and corruption changes encoding" $
      passertEvalNoTrace familyTemplatesExact
  ]

outerTests :: TestTree
outerTests = testGroup "outer"
  [ testCase "fold-map normalized state rejects deployment predecessor and route mutations" $
      passertEvalNoTrace foldMapOuterMutations
  , testCase "finalize normalized state rejects stage domain deployment and route mutations" $
      passertEvalNoTrace finalizeOuterMutations
  ]

attestedTests :: TestTree
attestedTests = testGroup "attested"
  [ testCase "common output is exact and minimal for fold-map" $
      passertEvalNoTrace foldMapAttestedOutput
  , testCase "common output is exact for finalize and shared with fold-map" $
      passertEvalNoTrace finalizeAttestedOutput
  ]

envelopeTests :: TestTree
envelopeTests = testGroup "envelope"
  [ testCase "state commitment provenance and route are bound" $
      passertEvalNoTrace envelopeBinding
  ]

settlementTests :: TestTree
settlementTests = testGroup "settlement"
  [ testCase "accepts both bound families" $
      passertEvalNoTrace settlementAcceptsBothFamilies
  , testCase "rejects envelope provenance and domain mutations" $
      passertEvalNoTrace settlementRejectsEnvelopeProvenanceAndDomain
  , testCase "rejects deployment family and route mutations" $
      passertEvalNoTrace settlementRejectsDeploymentFamilyAndRoute
  , testCase "rejects copied hash count and output mutations" $
      passertEvalNoTrace settlementRejectsCopiedHashCountAndOutput
  ]

rawEnvelopeTests :: TestTree
rawEnvelopeTests = testGroup "raw-envelope"
  [ testCase "auxiliary constructor index is the machine witness" $
      passertEvalNoTrace rawEnvelopeAuxiliaryConstructor
  , testCase "accepts canonical stage-one pending witness" $
      passertEvalNoTrace rawEnvelopeAcceptsCanonicalWitness
  , testCase "rejects serialiseData witness and mutated successor" $
      passertEvalNoTrace rawEnvelopeRejectsNonCanonicalAndMutatedSuccessor
  ]

handoffTests :: TestTree
handoffTests = testGroup "handoff"
  [ testCase "fold-map two-hop encoding and hash equal checked generic path" $
      passertEvalNoTrace foldMapTwoHopEncodingAndHash
  , testCase "finalize-frame two-hop encoding and hash equal checked generic path" $
      passertEvalNoTrace finalizeTwoHopEncodingAndHash
  , testCase "traversal hop rejects malformed traversal" $
      passertEvalNoTrace traversalHopRejectsMalformed
  , testCase "outer hop rejects source stage mode index and count mutations" $
      passertEvalNoTrace outerHopRejectsStructuralMutations
  , testCase "outer hop rejects descriptor and execution-unit mutations" $
      passertEvalNoTrace outerHopRejectsDescriptorMutations
  , testCase "outer hop rejects wrong checked encoding and current hash" $
      passertEvalNoTrace outerHopRejectsWrongEncodingAndHash
  ]

foldMapTests :: TestTree
foldMapTests = testGroup "fold-map"
  [ testCase "shared core matches generic step with nontrivial membership" $
      passertEvalNoTrace foldMapSharedCoreMatches
  , testCase "authenticated template equals checked generic next encoding" $
      passertEvalNoTrace foldMapTemplateMatches
  , testCase "authenticated prefix hash equals generic outer and expected hash" $
      passertEvalNoTrace foldMapPrefixHashMatches
  , testCase "shared core rejects stage frame hash index key and value mutations" $
      passertEvalNoTrace foldMapRejectsCoreMutations
  , testCase "shared core rejects each membership sibling path mutation" $
      passertEvalNoTrace foldMapRejectsSiblingMutations
  , testCase "executor rebind rejects wrong family identity and action" $
      passertEvalNoTrace foldMapRejectsRebindMutations
  , testCase "hash boundary rejects expected prefix and template mutations" $
      passertEvalNoTrace foldMapRejectsHashBoundaryMutations
  ]

finalizeTests :: TestTree
finalizeTests = testGroup "finalize"
  [ testCase "shared core matches generic terminal completion" $
      passertEvalNoTrace finalizeTerminalSharedCoreMatches
  , testCase "shared core matches generic authenticated parent append" $
      passertEvalNoTrace finalizeParentSharedCoreMatches
  , testCase "terminal template equals checked generic next encoding" $
      passertEvalNoTrace finalizeTerminalTemplateMatches
  , testCase "parent-append template equals checked generic next encoding" $
      passertEvalNoTrace finalizeParentTemplateMatches
  , testCase "terminal authenticated prefix hash equals generic and expected" $
      passertEvalNoTrace finalizeTerminalPrefixHashMatches
  , testCase "parent-append authenticated prefix hash equals generic and expected" $
      passertEvalNoTrace finalizeParentPrefixHashMatches
  , testCase "terminal rejects frame nonfinalized parent and offset mutations" $
      passertEvalNoTrace finalizeTerminalRejectsMutations
  , testCase "parent append rejects absent wrong malformed and full parent" $
      passertEvalNoTrace finalizeParentRejectsMutations
  , testCase "executor rebind rejects action family and identity mutations" $
      passertEvalNoTrace finalizeRejectsRebindMutations
  , testCase "hash boundary rejects expected prefix template and result mutations" $
      passertEvalNoTrace finalizeRejectsHashBoundaryMutations
  ]

foldMapTwoHopEncodingAndHash :: forall s. Term s PBool
foldMapTwoHopEncodingAndHash = ptwoHopEncodingAndHash pfoldControl

finalizeTwoHopEncodingAndHash :: forall s. Term s PBool
finalizeTwoHopEncodingAndHash = ptwoHopEncodingAndHash pfinalizeControl

ptwoHopEncodingAndHash :: forall s. Term s PDataTraverseControlV1 -> Term s PBool
ptwoHopEncodingAndHash traversal =
  plet (pouterControl traversal) $ \control ->
  plet (Traverse.pencodeControlV1 # traversal) $ \checkedTraversal ->
  plet
    ( Redeemer.pstageDataOuterControlPrefixV1 # control
        <> pconstant "\xd8\x79\x9f"
        <> checkedTraversal
        <> pconstant "\xff"
    )
    $ \assembled ->
  plet (Redeemer.pstageDataSomeTraversalHashPrefixV1 # control) $ \authenticatedPrefix ->
    pand'List
      [ Traverse.pcontrolIsWellFormed # traversal
      , Redeemer.pstageDataOuterFieldsAreWellFormedV1 # control
      , Redeemer.pcontrolIsWellFormed # control
      , assembled #== Redeemer.pencodeControlV1 # control
      , Redeemer.phashStageDataOuterWithCheckedTraversalV1 # control # checkedTraversal
          #== Redeemer.phashControlV1 # control
      , Redeemer.phashStageDataFromAuthenticatedPrefixV1 # authenticatedPrefix # checkedTraversal
          #== Redeemer.phashControlV1 # control
      , pnot #$
          Redeemer.phashStageDataFromAuthenticatedPrefixV1
            # (authenticatedPrefix <> pconstant "\x00") # checkedTraversal
            #== Redeemer.phashControlV1 # control
      ]

traversalHopRejectsMalformed :: forall s. Term s PBool
traversalHopRejectsMalformed =
  pand'List
    [ pnot #$ Traverse.pcontrolIsWellFormed
        # pmutateTraversal pfoldControl Traverse.pstageFold (pconstant $ BS.take 31 hashA)
    , pnot #$ Traverse.pcontrolIsWellFormed
        # pmutateTraversal pfoldControl Traverse.pstageHead (pconstant hashA)
    ]

outerHopRejectsStructuralMutations :: forall s. Term s PBool
outerHopRejectsStructuralMutations = plet (pouterControl pfoldControl) $ \control ->
  pand'List
    [ pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData 0 1 20 0 0 5 10 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageTail 0 1 20 0 0 4 10 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeDescriptor Redeemer.pstageData 0 1 20 0 0 4 10 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData (-1) 1 20 0 0 4 10 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData 0 0 20 0 0 4 10 20
    ]

outerHopRejectsDescriptorMutations :: forall s. Term s PBool
outerHopRejectsDescriptorMutations = plet (pouterControl pfoldControl) $ \control ->
  pand'List
    [ pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData 0 1 20 0 2 4 10 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData 0 1 20 1 0 4 10 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData 0 1 34 0 0 4 10 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData 0 1 20 0 0 4 (-1) 20
    , pnot #$ Redeemer.pstageDataOuterFieldsAreWellFormedV1
        # pmutateOuterControl control Redeemer.pmodeData Redeemer.pstageData 0 1 20 0 0 4 10 (-1)
    ]

outerHopRejectsWrongEncodingAndHash :: forall s. Term s PBool
outerHopRejectsWrongEncodingAndHash =
  plet (pouterControl pfoldControl) $ \control ->
  plet (Traverse.pencodeControlV1 # pfoldControl) $ \checked ->
  plet (Redeemer.phashControlV1 # control) $ \currentHash ->
    pand'List
      [ Redeemer.phashStageDataOuterWithCheckedTraversalV1 # control # checked #== currentHash
      , pnot #$ Redeemer.phashStageDataOuterWithCheckedTraversalV1 # control # (checked <> pconstant "\x00") #== currentHash
      , pnot #$ Redeemer.phashStageDataOuterWithCheckedTraversalV1 # control # checked #== pconstant hashB
      ]

pouterControl :: forall s.
  Term s PDataTraverseControlV1 -> Term s Redeemer.PRedeemerItemProofControlV1
pouterControl traversal = pmatch traversal $ \t ->
  pcon $ Redeemer.PRedeemerItemProofControlV1
    { Redeemer.predeemerControl'version = pdata Redeemer.pversion
    , Redeemer.predeemerControl'mode = pdata Redeemer.pmodeData
    , Redeemer.predeemerControl'stage = pdata Redeemer.pstageData
    , Redeemer.predeemerControl'itemIndex = pdata 0
    , Redeemer.predeemerControl'itemCount = pdata 1
    , Redeemer.predeemerControl'totalLength = pdata $
        pfromData (ptraverse'sourceStart t) + pfromData (ptraverse'sourceLength t) + 6
    , Redeemer.predeemerControl'itemCommitment = pdata $ pconstant hashA
    , Redeemer.predeemerControl'expectedPurposeTag = pdata 0
    , Redeemer.predeemerControl'expectedPointerIndex = pdata 0
    , Redeemer.predeemerControl'purposeTag = pdata 0
    , Redeemer.predeemerControl'pointerIndex = pdata 0
    , Redeemer.predeemerControl'dataOffset = ptraverse'sourceStart t
    , Redeemer.predeemerControl'dataLength = ptraverse'sourceLength t
    , Redeemer.predeemerControl'executionMemory = pdata 10
    , Redeemer.predeemerControl'executionSteps = pdata 20
    , Redeemer.predeemerControl'traversal = pdata $ pcon $ PDJust $ pdata traversal
    }

pmutateTraversal :: forall s.
  Term s PDataTraverseControlV1 -> Term s PInteger -> Term s PByteString ->
  Term s PDataTraverseControlV1
pmutateTraversal traversal stage root = pmatch traversal $ \t ->
  pcon $ PDataTraverseControlV1
    { ptraverse'version = ptraverse'version t
    , ptraverse'stage = pdata stage
    , ptraverse'sourceStart = ptraverse'sourceStart t
    , ptraverse'sourceLength = ptraverse'sourceLength t
    , ptraverse'offset = ptraverse'offset t
    , ptraverse'frameRoot = pdata root
    , ptraverse'pendingLargeExpectedChildren = ptraverse'pendingLargeExpectedChildren t
    , ptraverse'integer = ptraverse'integer t
    , ptraverse'bytes = ptraverse'bytes t
    , ptraverse'result = ptraverse'result t
    }

pmutateOuterControl :: forall s.
  Term s Redeemer.PRedeemerItemProofControlV1 ->
  Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PInteger -> Term s Redeemer.PRedeemerItemProofControlV1
pmutateOuterControl control mode stage itemIndex itemCount total expectedPointer purpose offset memory steps =
  pmatch control $ \c -> pcon $ Redeemer.PRedeemerItemProofControlV1
    { Redeemer.predeemerControl'version = Redeemer.predeemerControl'version c
    , Redeemer.predeemerControl'mode = pdata mode
    , Redeemer.predeemerControl'stage = pdata stage
    , Redeemer.predeemerControl'itemIndex = pdata itemIndex
    , Redeemer.predeemerControl'itemCount = pdata itemCount
    , Redeemer.predeemerControl'totalLength = pdata total
    , Redeemer.predeemerControl'itemCommitment = Redeemer.predeemerControl'itemCommitment c
    , Redeemer.predeemerControl'expectedPurposeTag = Redeemer.predeemerControl'expectedPurposeTag c
    , Redeemer.predeemerControl'expectedPointerIndex = pdata expectedPointer
    , Redeemer.predeemerControl'purposeTag = pdata purpose
    , Redeemer.predeemerControl'pointerIndex = Redeemer.predeemerControl'pointerIndex c
    , Redeemer.predeemerControl'dataOffset = pdata offset
    , Redeemer.predeemerControl'dataLength = Redeemer.predeemerControl'dataLength c
    , Redeemer.predeemerControl'executionMemory = pdata memory
    , Redeemer.predeemerControl'executionSteps = pdata steps
    , Redeemer.predeemerControl'traversal = Redeemer.predeemerControl'traversal c
    }

foldMapSharedCoreMatches :: forall s. Term s PBool
foldMapSharedCoreMatches =
  plet pnontrivialFoldMapControl $ \control ->
  plet pnontrivialMapFrame $ \frame ->
  plet pmapKeySummary $ \key ->
  plet pmapValueSummary $ \value ->
  plet pkeySiblings $ \keySiblings ->
  plet pvalueSiblings $ \valueSiblings ->
  plet (pexpectJustValue $ Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # key # value # keySiblings # valueSiblings) $ \nextRoot ->
  plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # pnontrivialFoldMapAction) $ \genericNext ->
    pand'List
      [ pnot #$ pnull # keySiblings
      , pnot #$ pnull # valueSiblings
      , genericNext #== pmutateTraversal control Traverse.pstageFold nextRoot
      , pframeRootOf genericNext #== nextRoot
      ]

foldMapTemplateMatches :: forall s. Term s PBool
foldMapTemplateMatches =
  plet pouterNormalizedFoldMapState $ \state -> pmatch state $ \st ->
  plet (pfromData $ pouterNormalized'validatedTraversalControl st) $ \control ->
  plet pnontrivialMapFrame $ \frame ->
  plet (pexpectJustValue $ Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # pmapKeySummary # pmapValueSummary # pkeySiblings # pvalueSiblings) $ \nextRoot ->
  pmatch (pfromData $ pouterNormalized'traversalSerializationTemplate st) $ \case
    PFinalizeFrameTemplate _ _ _ -> pconstant False
    PFoldMapFrameRootTemplate prefix suffix ->
      plet (pfromData prefix <> (pencodeDefiniteBytes # nextRoot) <> pfromData suffix) $ \assembled ->
      plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # pnontrivialFoldMapAction) $ \genericNext ->
        assembled #== Traverse.pencodeControlV1 # genericNext

foldMapPrefixHashMatches :: forall s. Term s PBool
foldMapPrefixHashMatches =
  plet pouterNormalizedFoldMapState $ \state -> pmatch state $ \st ->
  plet (pfromData $ pouterNormalized'validatedTraversalControl st) $ \control ->
  plet pnontrivialMapFrame $ \frame ->
  plet (pexpectJustValue $ Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # pmapKeySummary # pmapValueSummary # pkeySiblings # pvalueSiblings) $ \nextRoot ->
  pmatch (pfromData $ pouterNormalized'traversalSerializationTemplate st) $ \case
    PFinalizeFrameTemplate _ _ _ -> pconstant False
    PFoldMapFrameRootTemplate prefix suffix ->
      plet (pfromData prefix <> (pencodeDefiniteBytes # nextRoot) <> pfromData suffix) $ \assembled ->
      plet (Redeemer.phashStageDataFromAuthenticatedPrefixV1
              # pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) # assembled) $ \actual ->
      plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # pnontrivialFoldMapAction) $ \genericNext ->
      plet (pouterControl control) $ \currentOuter ->
      plet (pouterControlWithTraversal currentOuter genericNext) $ \nextOuter ->
        actual #== Redeemer.phashControlV1 # nextOuter
          #&& actual #== pfromData (pouterNormalized'expectedNextItemControlHash st)

foldMapRejectsCoreMutations :: forall s. Term s PBool
foldMapRejectsCoreMutations =
  plet pnontrivialFoldMapControl $ \control ->
  plet pnontrivialMapFrame $ \frame ->
  plet (pmutateSummary pmapKeySummary $ pconstant hashB) $ \wrongKey ->
  plet (pmutateSummary pmapValueSummary $ pconstant hashA) $ \wrongValue ->
    pand'List
      [ pstageOf control #== Traverse.pstageFold
      , pnot #$ Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # pmapKeySummary # pmapValueSummary # pkeySiblings # pvalueSiblings #== pcon PNothing
      , Traverse.pstepV1 # pmutateTraversal control Traverse.pstageClose (pframeRootOf control)
          # pcon PNothing # pnontrivialFoldMapAction #== pcon PNothing
      , Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pconstant hashA # frame # 0 # pmapKeySummary # pmapValueSummary # pkeySiblings # pvalueSiblings #== pcon PNothing
      , Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # (pinitialMapFrameV1 # pconstant "" # 0) # 0
          # pmapKeySummary # pmapValueSummary # pkeySiblings # pvalueSiblings #== pcon PNothing
      , Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 1 # pmapKeySummary # pmapValueSummary # pkeySiblings # pvalueSiblings #== pcon PNothing
      , Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # wrongKey # pmapValueSummary # pkeySiblings # pvalueSiblings #== pcon PNothing
      , Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # pmapKeySummary # wrongValue # pkeySiblings # pvalueSiblings #== pcon PNothing
      ]

foldMapRejectsSiblingMutations :: forall s. Term s PBool
foldMapRejectsSiblingMutations =
  plet pnontrivialFoldMapControl $ \control ->
  plet pnontrivialMapFrame $ \frame ->
    pand'List
      [ pnot #$ pnull # pkeySiblings
      , pnot #$ pnull # pvalueSiblings
      , Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # pmapKeySummary # pmapValueSummary
          # (pcons # pdata (pconstant hashA) # pnil) # pvalueSiblings #== pcon PNothing
      , Traverse.pprevalidatedFoldMapNextFrameRootV1
          # pframeRootOf control # frame # 0 # pmapKeySummary # pmapValueSummary
          # pkeySiblings # (pcons # pdata (pconstant hashA) # pnil) #== pcon PNothing
      ]

foldMapRejectsRebindMutations :: forall s. Term s PBool
foldMapRejectsRebindMutations =
  plet pouterNormalizedFoldMapState $ \state -> pmatch state $ \st ->
  plet (pmutateFoldMapPairIndex pnontrivialFoldMapAction 1) $ \wrongAction ->
    pand'List
      [ ptraversalActionIdentityIsBoundV1 # pnontrivialFoldMapAction # pfoldMapFamily
          # pfromData (pouterNormalized'authenticatedTraversalActionIdentity st)
      , pnot #$ ptraversalActionIdentityIsBoundV1 # wrongAction # pfoldMapFamily
          # pfromData (pouterNormalized'authenticatedTraversalActionIdentity st)
      , pnot #$ ptraversalActionIdentityIsBoundV1 # pnontrivialFoldMapAction # pfinalizeFrameFamily
          # pfromData (pouterNormalized'authenticatedTraversalActionIdentity st)
      , pnot #$ ptraversalActionIdentityIsBoundV1 # pnontrivialFoldMapAction # pfoldMapFamily # pconstant hashA
      ]

foldMapRejectsHashBoundaryMutations :: forall s. Term s PBool
foldMapRejectsHashBoundaryMutations =
  plet pouterNormalizedFoldMapState $ \state -> pmatch state $ \st ->
  plet (pfromData $ pouterNormalized'validatedTraversalControl st) $ \control ->
  plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # pnontrivialFoldMapAction) $ \genericNext ->
  plet (Traverse.pencodeControlV1 # genericNext) $ \checkedNext ->
  plet (Redeemer.phashStageDataFromAuthenticatedPrefixV1
          # pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) # checkedNext) $ \actual ->
  plet (Redeemer.phashStageDataFromAuthenticatedPrefixV1
          # (pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) <> pconstant "\x00") # checkedNext) $ \wrongPrefixHash ->
  pmatch (pfromData $ pouterNormalized'traversalSerializationTemplate st) $ \case
    PFinalizeFrameTemplate _ _ _ -> pconstant False
    PFoldMapFrameRootTemplate prefix suffix ->
      plet (pmutateOuterNormalized state (pdata $ pcon $ PFoldMapFrameRootTemplate
              (pdata $ pfromData prefix <> pconstant "\x00") suffix)
              (pouterNormalized'expectedNextItemControlHash st)) $ \wrongTemplate ->
      plet (pmutateOuterNormalized state (pouterNormalized'traversalSerializationTemplate st)
              (pdata $ pconstant hashA)) $ \wrongExpected -> pmatch wrongExpected $ \wrong ->
        pand'List
          [ actual #== pfromData (pouterNormalized'expectedNextItemControlHash st)
          , pnot #$ wrongPrefixHash #== pfromData (pouterNormalized'expectedNextItemControlHash st)
          , pnot #$ pouterNormalizedStateIsBoundV1 # wrongTemplate # pfoldMapFamily # pconstant hashA
              # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptE
          , pnot #$ actual #== pfromData (pouterNormalized'expectedNextItemControlHash wrong)
          ]

pmapKeySummary, pmapValueSummary :: forall s. Term s PDataSummaryV1
pmapKeySummary = pcon $ PDataSummaryV1 (pdata $ pconstant hashA) (pdata 1) (pdata 4)
pmapValueSummary = pcon $ PDataSummaryV1 (pdata $ pconstant hashB) (pdata 2) (pdata 5)

pnontrivialMapFrame :: forall s. Term s Frame.PDataFrameV1
pnontrivialMapFrame =
  plet (pinitialMapFrameV1 # pconstant "" # 2) $ \initial ->
  plet (pexpectJustValue $ Frame.pappendChildV1 # initial # pmapKeySummary) $ \withKey ->
    pexpectJustValue $ Frame.pappendChildV1 # withKey # pmapValueSummary

pkeySiblings, pvalueSiblings :: forall s. Term s (PBuiltinList (PAsData PByteString))
pkeySiblings = pcons # pdata (Frame.pchildLeafHashV1 # 1 # pmapValueSummary) # pnil
pvalueSiblings = pcons # pdata (Frame.pchildLeafHashV1 # 0 # pmapKeySummary) # pnil

pnontrivialFoldMapAction :: forall s. Term s PDataTraverseActionV1
pnontrivialFoldMapAction = pcon $ PFoldMap
  (pdata pnontrivialMapFrame) (pdata 0) (pdata pmapKeySummary) (pdata pmapValueSummary)
  (pdata pkeySiblings) (pdata pvalueSiblings)

pnontrivialFoldMapControl :: forall s. Term s PDataTraverseControlV1
pnontrivialFoldMapControl = pmutateTraversal pfoldControl Traverse.pstageFold (Frame.phashFrameV1 # pnontrivialMapFrame)

pouterNormalizedFoldMapState :: forall s. Term s POuterNormalizedScriptSourcesRedeemerActionV1
pouterNormalizedFoldMapState =
  plet pnontrivialFoldMapControl $ \control ->
  plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # pnontrivialFoldMapAction) $ \next ->
  plet (pouterControl control) $ \outer ->
  plet (pouterControlWithTraversal outer next) $ \nextOuter ->
    pcon $ POuterNormalizedScriptSourcesRedeemerActionV1
      { pouterNormalized'version = pdata pversion
      , pouterNormalized'domain = pdata pouterNormalizedDomain
      , pouterNormalized'deploymentId = pdata $ pconstant hashA
      , pouterNormalized'baseProvenanceIdentity = pdata $ pconstant hashB
      , pouterNormalized'envelopeBinderScriptHash = pdata $ pconstant scriptA
      , pouterNormalized'traversalNormalizerScriptHash = pdata $ pconstant scriptB
      , pouterNormalized'outerNormalizerScriptHash = pdata $ pconstant scriptC
      , pouterNormalized'semanticExecutorScriptHash = pdata $ pconstant scriptD
      , pouterNormalized'settlementScriptHash = pdata $ pconstant scriptE
      , pouterNormalized'actionFamily = pdata pfoldMapFamily
      , pouterNormalized'canonicalActionHash = pdata $ pconstant hashA
      , pouterNormalized'authenticatedTraversalActionIdentity = pdata $ ptraversalActionIdentityV1 # pnontrivialFoldMapAction
      , pouterNormalized'currentPendingItemControlHash = pdata $ Redeemer.phashControlV1 # outer
      , pouterNormalized'expectedNextItemControlHash = pdata $ Redeemer.phashControlV1 # nextOuter
      , pouterNormalized'redeemerCount = pdata 0
      , pouterNormalized'redeemerTotalCount = pdata 1
      , pouterNormalized'nextItemControlHashPrefixCbor = pdata $ Redeemer.pstageDataSomeTraversalHashPrefixV1 # outer
      , pouterNormalized'validatedTraversalControl = pdata control
      , pouterNormalized'traversalSerializationTemplate = pdata $
          ptraversalSerializationTemplateV1 # control # pfoldMapFamily
      }

pouterControlWithTraversal :: forall s.
  Term s Redeemer.PRedeemerItemProofControlV1 -> Term s PDataTraverseControlV1 ->
  Term s Redeemer.PRedeemerItemProofControlV1
pouterControlWithTraversal control traversal = pmatch control $ \c ->
  pcon $ Redeemer.PRedeemerItemProofControlV1
    { Redeemer.predeemerControl'version = Redeemer.predeemerControl'version c
    , Redeemer.predeemerControl'mode = Redeemer.predeemerControl'mode c
    , Redeemer.predeemerControl'stage = Redeemer.predeemerControl'stage c
    , Redeemer.predeemerControl'itemIndex = Redeemer.predeemerControl'itemIndex c
    , Redeemer.predeemerControl'itemCount = Redeemer.predeemerControl'itemCount c
    , Redeemer.predeemerControl'totalLength = Redeemer.predeemerControl'totalLength c
    , Redeemer.predeemerControl'itemCommitment = Redeemer.predeemerControl'itemCommitment c
    , Redeemer.predeemerControl'expectedPurposeTag = Redeemer.predeemerControl'expectedPurposeTag c
    , Redeemer.predeemerControl'expectedPointerIndex = Redeemer.predeemerControl'expectedPointerIndex c
    , Redeemer.predeemerControl'purposeTag = Redeemer.predeemerControl'purposeTag c
    , Redeemer.predeemerControl'pointerIndex = Redeemer.predeemerControl'pointerIndex c
    , Redeemer.predeemerControl'dataOffset = Redeemer.predeemerControl'dataOffset c
    , Redeemer.predeemerControl'dataLength = Redeemer.predeemerControl'dataLength c
    , Redeemer.predeemerControl'executionMemory = Redeemer.predeemerControl'executionMemory c
    , Redeemer.predeemerControl'executionSteps = Redeemer.predeemerControl'executionSteps c
    , Redeemer.predeemerControl'traversal = pdata $ pcon $ PDJust $ pdata traversal
    }

pmutateSummary :: forall s.
  Term s PDataSummaryV1 -> Term s PByteString -> Term s PDataSummaryV1
pmutateSummary summary root = pmatch summary $ \s ->
  pcon $ PDataSummaryV1 (pdata root) (psummary'cborLength s) (psummary'memory s)

pmutateFoldMapPairIndex :: forall s.
  Term s PDataTraverseActionV1 -> Term s PInteger -> Term s PDataTraverseActionV1
pmutateFoldMapPairIndex action pairIndex = pmatch action $ \case
  PFoldMap frame _ key value keySiblings valueSiblings ->
    pcon $ PFoldMap frame (pdata pairIndex) key value keySiblings valueSiblings
  _ -> perror

pmutateOuterNormalized :: forall s.
  Term s POuterNormalizedScriptSourcesRedeemerActionV1 ->
  Term s (PAsData PFamilyTraversalSerializationTemplateV1) -> Term s (PAsData PByteString) ->
  Term s POuterNormalizedScriptSourcesRedeemerActionV1
pmutateOuterNormalized state template expectedHash = pmatch state $ \st ->
  pcon $ POuterNormalizedScriptSourcesRedeemerActionV1
    { pouterNormalized'version = pouterNormalized'version st
    , pouterNormalized'domain = pouterNormalized'domain st
    , pouterNormalized'deploymentId = pouterNormalized'deploymentId st
    , pouterNormalized'baseProvenanceIdentity = pouterNormalized'baseProvenanceIdentity st
    , pouterNormalized'envelopeBinderScriptHash = pouterNormalized'envelopeBinderScriptHash st
    , pouterNormalized'traversalNormalizerScriptHash = pouterNormalized'traversalNormalizerScriptHash st
    , pouterNormalized'outerNormalizerScriptHash = pouterNormalized'outerNormalizerScriptHash st
    , pouterNormalized'semanticExecutorScriptHash = pouterNormalized'semanticExecutorScriptHash st
    , pouterNormalized'settlementScriptHash = pouterNormalized'settlementScriptHash st
    , pouterNormalized'actionFamily = pouterNormalized'actionFamily st
    , pouterNormalized'canonicalActionHash = pouterNormalized'canonicalActionHash st
    , pouterNormalized'authenticatedTraversalActionIdentity = pouterNormalized'authenticatedTraversalActionIdentity st
    , pouterNormalized'currentPendingItemControlHash = pouterNormalized'currentPendingItemControlHash st
    , pouterNormalized'expectedNextItemControlHash = expectedHash
    , pouterNormalized'redeemerCount = pouterNormalized'redeemerCount st
    , pouterNormalized'redeemerTotalCount = pouterNormalized'redeemerTotalCount st
    , pouterNormalized'nextItemControlHashPrefixCbor = pouterNormalized'nextItemControlHashPrefixCbor st
    , pouterNormalized'validatedTraversalControl = pouterNormalized'validatedTraversalControl st
    , pouterNormalized'traversalSerializationTemplate = template
    }

pframeRootOf :: forall s. Term s PDataTraverseControlV1 -> Term s PByteString
pframeRootOf control = pmatch control $ \c -> pfromData (ptraverse'frameRoot c)

pstageOf :: forall s. Term s PDataTraverseControlV1 -> Term s PInteger
pstageOf control = pmatch control $ \c -> pfromData (ptraverse'stage c)

pexpectJustValue :: forall s a. Term s (PMaybe a) -> Term s a
pexpectJustValue value = pmatch value $ \case PNothing -> perror; PJust result -> result

finalizeTerminalSharedCoreMatches :: forall s. Term s PBool
finalizeTerminalSharedCoreMatches =
  plet (pexpectJustValue $ pfinalizeTransition pterminalFinalizeControl pterminalFinalizeFrame (pcon PDNothing)) $ \transition ->
  plet (pexpectJustValue $ Traverse.pstepV1 # pterminalFinalizeControl # pcon PNothing # pterminalFinalizeAction) $ \genericNext ->
  pmatch transition $ \t ->
    pand'List
      [ Traverse.ptransition'nextStage t #== Traverse.pstageTerminal
      , Traverse.ptransition'nextFrameRoot t #== pconstant ""
      , pnot #$ Traverse.ptransition'nextResult t #== pcon PDNothing
      , genericNext #== papplyFinalizeTransition pterminalFinalizeControl transition
      ]

finalizeParentSharedCoreMatches :: forall s. Term s PBool
finalizeParentSharedCoreMatches =
  plet (pexpectJustValue $ Frame.pfinalizedSummaryV1 # pparentAppendFinalizeFrame) $ \summary ->
  plet (pexpectJustValue $ Frame.pappendChildV1 # pappendParentFrame # summary) $ \nextParent ->
  plet (pexpectJustValue $ pfinalizeTransition pparentAppendFinalizeControl pparentAppendFinalizeFrame
          (pcon $ PDJust $ pdata pappendParentFrame)) $ \transition ->
  plet (pexpectJustValue $ Traverse.pstepV1 # pparentAppendFinalizeControl # pcon PNothing # pparentAppendFinalizeAction) $ \genericNext ->
  pmatch pappendParentFrame $ \parent -> pmatch nextParent $ \next -> pmatch transition $ \t ->
    pand'List
      [ pfromData (Frame.pframe'expectedChildren parent) #== 2
      , pfromData (Frame.pframe'childCount parent) #== 0
      , pfromData (Frame.pframe'childCount next) #== 1
      , Traverse.ptransition'nextStage t #== Traverse.pstageHead
      , Traverse.ptransition'nextFrameRoot t #== Frame.phashFrameV1 # nextParent
      , Traverse.ptransition'nextResult t #== pcon PDNothing
      , genericNext #== papplyFinalizeTransition pparentAppendFinalizeControl transition
      ]

finalizeTerminalTemplateMatches :: forall s. Term s PBool
finalizeTerminalTemplateMatches =
  pfinalizeTemplateMatches pterminalFinalizeState pterminalFinalizeAction

finalizeParentTemplateMatches :: forall s. Term s PBool
finalizeParentTemplateMatches =
  pfinalizeTemplateMatches pparentAppendFinalizeState pparentAppendFinalizeAction

pfinalizeTemplateMatches :: forall s.
  Term s POuterNormalizedScriptSourcesRedeemerActionV1 -> Term s PDataTraverseActionV1 -> Term s PBool
pfinalizeTemplateMatches state action = pmatch state $ \st ->
  plet (pfromData $ pouterNormalized'validatedTraversalControl st) $ \control ->
  plet (pexpectFinalizeTransition control action) $ \transition ->
  plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # action) $ \genericNext ->
    passembleFinalizeTransition state transition #== Traverse.pencodeControlV1 # genericNext

finalizeTerminalPrefixHashMatches :: forall s. Term s PBool
finalizeTerminalPrefixHashMatches =
  pfinalizePrefixHashMatches pterminalFinalizeState pterminalFinalizeAction

finalizeParentPrefixHashMatches :: forall s. Term s PBool
finalizeParentPrefixHashMatches =
  pfinalizePrefixHashMatches pparentAppendFinalizeState pparentAppendFinalizeAction

pfinalizePrefixHashMatches :: forall s.
  Term s POuterNormalizedScriptSourcesRedeemerActionV1 -> Term s PDataTraverseActionV1 -> Term s PBool
pfinalizePrefixHashMatches state action = pmatch state $ \st ->
  plet (pfromData $ pouterNormalized'validatedTraversalControl st) $ \control ->
  plet (pexpectFinalizeTransition control action) $ \transition ->
  plet (passembleFinalizeTransition state transition) $ \assembled ->
  plet (Redeemer.phashStageDataFromAuthenticatedPrefixV1
          # pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) # assembled) $ \actual ->
  plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # action) $ \genericNext ->
  plet (pouterControlWithTraversal (pouterControl control) genericNext) $ \nextOuter ->
    actual #== Redeemer.phashControlV1 # nextOuter
      #&& actual #== pfromData (pouterNormalized'expectedNextItemControlHash st)

finalizeTerminalRejectsMutations :: forall s. Term s PBool
finalizeTerminalRejectsMutations =
  plet (pinitialListFrameV1 # pconstant "" # 1) $ \nonfinalized ->
  plet (pcontrolOffset pterminalFinalizeControl) $ \offset ->
  plet (pcontrolSourceLength pterminalFinalizeControl) $ \sourceLength ->
    pand'List
      [ pnot #$ pmaybeIsNothing $ pfinalizeTransition pterminalFinalizeControl pterminalFinalizeFrame (pcon PDNothing)
      , pmaybeIsNothing $ Traverse.pprevalidatedFinalizeFrameTransitionV1
          # pconstant hashA # offset # sourceLength # pterminalFinalizeFrame # pcon PDNothing
      , pmaybeIsNothing $ Traverse.pprevalidatedFinalizeFrameTransitionV1
          # (Frame.phashFrameV1 # nonfinalized) # offset # sourceLength # nonfinalized # pcon PDNothing
      , pmaybeIsNothing $ Traverse.pprevalidatedFinalizeFrameTransitionV1
          # pframeRootOf pterminalFinalizeControl # offset # sourceLength # pterminalFinalizeFrame
          # (pcon $ PDJust $ pdata pappendParentFrame)
      , pmaybeIsNothing $ Traverse.pprevalidatedFinalizeFrameTransitionV1
          # pframeRootOf pterminalFinalizeControl # (offset - 1) # sourceLength # pterminalFinalizeFrame
          # pcon PDNothing
      ]

finalizeParentRejectsMutations :: forall s. Term s PBool
finalizeParentRejectsMutations =
  plet (pmutateFrameExpectedChildren pappendParentFrame (-1)) $ \malformedParent ->
  plet (pinitialListFrameV1 # pconstant "" # 0) $ \fullParent ->
  plet (pinitialListFrameV1 # (Frame.phashFrameV1 # fullParent) # 0) $ \fullParentChild ->
  plet (pcontrolOffset pparentAppendFinalizeControl) $ \offset ->
  plet (pcontrolSourceLength pparentAppendFinalizeControl) $ \sourceLength ->
    pand'List
      [ pnot #$ pmaybeIsNothing $ pfinalizeTransition pparentAppendFinalizeControl pparentAppendFinalizeFrame
          (pcon $ PDJust $ pdata pappendParentFrame)
      , pmaybeIsNothing $ pfinalizeTransition pparentAppendFinalizeControl pparentAppendFinalizeFrame (pcon PDNothing)
      , pmaybeIsNothing $ Traverse.pprevalidatedFinalizeFrameTransitionV1
          # pframeRootOf pparentAppendFinalizeControl # offset # sourceLength # pparentAppendFinalizeFrame
          # (pcon $ PDJust $ pdata $ pinitialListFrameV1 # pconstant "" # 1)
      , pmaybeIsNothing $ Traverse.pprevalidatedFinalizeFrameTransitionV1
          # pframeRootOf pparentAppendFinalizeControl # offset # sourceLength # pparentAppendFinalizeFrame
          # (pcon $ PDJust $ pdata malformedParent)
      , pmaybeIsNothing $ Traverse.pprevalidatedFinalizeFrameTransitionV1
          # (Frame.phashFrameV1 # fullParentChild) # offset # sourceLength # fullParentChild
          # (pcon $ PDJust $ pdata fullParent)
      ]

finalizeRejectsRebindMutations :: forall s. Term s PBool
finalizeRejectsRebindMutations = pmatch pterminalFinalizeState $ \st ->
  pand'List
    [ ptraversalActionIdentityIsBoundV1 # pterminalFinalizeAction # pfinalizeFrameFamily
        # pfromData (pouterNormalized'authenticatedTraversalActionIdentity st)
    , pnot #$ ptraversalActionIdentityIsBoundV1 # pparentAppendFinalizeAction # pfinalizeFrameFamily
        # pfromData (pouterNormalized'authenticatedTraversalActionIdentity st)
    , pnot #$ ptraversalActionIdentityIsBoundV1 # pterminalFinalizeAction # pfoldMapFamily
        # pfromData (pouterNormalized'authenticatedTraversalActionIdentity st)
    , pnot #$ ptraversalActionIdentityIsBoundV1 # pterminalFinalizeAction # pfinalizeFrameFamily # pconstant hashA
    ]

finalizeRejectsHashBoundaryMutations :: forall s. Term s PBool
finalizeRejectsHashBoundaryMutations =
  plet pterminalFinalizeState $ \state -> pmatch state $ \st ->
  plet (pfromData $ pouterNormalized'validatedTraversalControl st) $ \control ->
  plet (pexpectFinalizeTransition control pterminalFinalizeAction) $ \transition ->
  plet (passembleFinalizeTransition state transition) $ \assembled ->
  plet (Redeemer.phashStageDataFromAuthenticatedPrefixV1
          # pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) # assembled) $ \actual ->
  plet (Redeemer.phashStageDataFromAuthenticatedPrefixV1
          # (pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) <> pconstant "\x00") # assembled) $ \wrongPrefixHash ->
  pmatch transition $ \t -> pmatch (Traverse.ptransition'nextResult t) $ \case
    PDNothing -> pconstant False
    PDJust exactSummaryData ->
      plet (pmutateSummary (pfromData exactSummaryData) $ pconstant hashA) $ \wrongSummary ->
      plet (Traverse.pencodeOptionalSummaryV1 # (pcon $ PDJust $ pdata wrongSummary)) $ \wrongResultCbor ->
      pmatch (pfromData $ pouterNormalized'traversalSerializationTemplate st) $ \case
        PFoldMapFrameRootTemplate _ _ -> pconstant False
        PFinalizeFrameTemplate prefix sourceFields suffix ->
          plet
            ( pfromData prefix
                <> pcborInt (Traverse.ptransition'nextStage t)
                <> pfromData sourceFields
                <> (pencodeDefiniteBytes # Traverse.ptransition'nextFrameRoot t)
                <> pfromData suffix
                <> wrongResultCbor
            )
            $ \wrongResultAssembled ->
          plet (pmutateOuterNormalized state
                  (pdata $ pcon $ PFinalizeFrameTemplate
                    (pdata $ pfromData prefix <> pconstant "\x00") sourceFields suffix)
                  (pouterNormalized'expectedNextItemControlHash st)) $ \wrongTemplate ->
          plet (pmutateOuterNormalized state (pouterNormalized'traversalSerializationTemplate st)
                  (pdata $ pconstant hashA)) $ \wrongExpected -> pmatch wrongExpected $ \wrong ->
            pand'List
              [ actual #== pfromData (pouterNormalized'expectedNextItemControlHash st)
              , pnot #$ wrongPrefixHash #== pfromData (pouterNormalized'expectedNextItemControlHash st)
              , pnot #$ Redeemer.phashStageDataFromAuthenticatedPrefixV1
                  # pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) # wrongResultAssembled
                  #== pfromData (pouterNormalized'expectedNextItemControlHash st)
              , pnot #$ pouterNormalizedStateIsBoundV1 # wrongTemplate # pfinalizeFrameFamily # pconstant hashA
                  # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptE
              , pnot #$ actual #== pfromData (pouterNormalized'expectedNextItemControlHash wrong)
              ]

pterminalFinalizeFrame, pappendParentFrame, pparentAppendFinalizeFrame :: forall s. Term s Frame.PDataFrameV1
pterminalFinalizeFrame = pinitialListFrameV1 # pconstant "" # 0
pappendParentFrame = pinitialListFrameV1 # pconstant "" # 2
pparentAppendFinalizeFrame = pinitialListFrameV1 # (Frame.phashFrameV1 # pappendParentFrame) # 0

pterminalFinalizeAction, pparentAppendFinalizeAction :: forall s. Term s PDataTraverseActionV1
pterminalFinalizeAction = pcon $ PFinalizeFrame (pdata pterminalFinalizeFrame) (pdata $ pcon PDNothing)
pparentAppendFinalizeAction = pcon $ PFinalizeFrame
  (pdata pparentAppendFinalizeFrame) (pdata $ pcon $ PDJust $ pdata pappendParentFrame)

pterminalFinalizeControl, pparentAppendFinalizeControl :: forall s. Term s PDataTraverseControlV1
pterminalFinalizeControl = pfinalizeControlFor 12 (Frame.phashFrameV1 # pterminalFinalizeFrame)
pparentAppendFinalizeControl = pfinalizeControlFor 6 (Frame.phashFrameV1 # pparentAppendFinalizeFrame)

pfinalizeControlFor :: forall s. Term s PInteger -> Term s PByteString -> Term s PDataTraverseControlV1
pfinalizeControlFor offset root = pcon $ PDataTraverseControlV1
  { ptraverse'version = pdata Traverse.pversion
  , ptraverse'stage = pdata Traverse.pstageFold
  , ptraverse'sourceStart = pdata 6
  , ptraverse'sourceLength = pdata 12
  , ptraverse'offset = pdata offset
  , ptraverse'frameRoot = pdata root
  , ptraverse'pendingLargeExpectedChildren = pdata $ pcon PDNothing
  , ptraverse'integer = pdata $ pcon PDNothing
  , ptraverse'bytes = pdata $ pcon PDNothing
  , ptraverse'result = pdata $ pcon PDNothing
  }

pterminalFinalizeState, pparentAppendFinalizeState :: forall s. Term s POuterNormalizedScriptSourcesRedeemerActionV1
pterminalFinalizeState = pouterNormalizedFinalizeState pterminalFinalizeControl pterminalFinalizeAction
pparentAppendFinalizeState = pouterNormalizedFinalizeState pparentAppendFinalizeControl pparentAppendFinalizeAction

pouterNormalizedFinalizeState :: forall s.
  Term s PDataTraverseControlV1 -> Term s PDataTraverseActionV1 ->
  Term s POuterNormalizedScriptSourcesRedeemerActionV1
pouterNormalizedFinalizeState control action =
  plet (pexpectJustValue $ Traverse.pstepV1 # control # pcon PNothing # action) $ \next ->
  plet (pouterControl control) $ \outer ->
  plet (pouterControlWithTraversal outer next) $ \nextOuter ->
    pcon $ POuterNormalizedScriptSourcesRedeemerActionV1
      { pouterNormalized'version = pdata pversion
      , pouterNormalized'domain = pdata pouterNormalizedDomain
      , pouterNormalized'deploymentId = pdata $ pconstant hashA
      , pouterNormalized'baseProvenanceIdentity = pdata $ pconstant hashB
      , pouterNormalized'envelopeBinderScriptHash = pdata $ pconstant scriptA
      , pouterNormalized'traversalNormalizerScriptHash = pdata $ pconstant scriptB
      , pouterNormalized'outerNormalizerScriptHash = pdata $ pconstant scriptC
      , pouterNormalized'semanticExecutorScriptHash = pdata $ pconstant scriptF
      , pouterNormalized'settlementScriptHash = pdata $ pconstant scriptE
      , pouterNormalized'actionFamily = pdata pfinalizeFrameFamily
      , pouterNormalized'canonicalActionHash = pdata $ pconstant hashA
      , pouterNormalized'authenticatedTraversalActionIdentity = pdata $ ptraversalActionIdentityV1 # action
      , pouterNormalized'currentPendingItemControlHash = pdata $ Redeemer.phashControlV1 # outer
      , pouterNormalized'expectedNextItemControlHash = pdata $ Redeemer.phashControlV1 # nextOuter
      , pouterNormalized'redeemerCount = pdata 0
      , pouterNormalized'redeemerTotalCount = pdata 1
      , pouterNormalized'nextItemControlHashPrefixCbor = pdata $ Redeemer.pstageDataSomeTraversalHashPrefixV1 # outer
      , pouterNormalized'validatedTraversalControl = pdata control
      , pouterNormalized'traversalSerializationTemplate = pdata $
          ptraversalSerializationTemplateV1 # control # pfinalizeFrameFamily
      }

pfinalizeTransition :: forall s.
  Term s PDataTraverseControlV1 -> Term s Frame.PDataFrameV1 ->
  Term s (PMaybeData Frame.PDataFrameV1) -> Term s (PMaybe Traverse.PFinalizeFrameTransitionV1)
pfinalizeTransition control frame parent =
  Traverse.pprevalidatedFinalizeFrameTransitionV1
    # pframeRootOf control # pcontrolOffset control # pcontrolSourceLength control # frame # parent

pexpectFinalizeTransition :: forall s.
  Term s PDataTraverseControlV1 -> Term s PDataTraverseActionV1 ->
  Term s Traverse.PFinalizeFrameTransitionV1
pexpectFinalizeTransition control action = pmatch action $ \case
  PFinalizeFrame frame parent ->
    pexpectJustValue $ pfinalizeTransition control (pfromData frame) (pfromData parent)
  _ -> perror

papplyFinalizeTransition :: forall s.
  Term s PDataTraverseControlV1 -> Term s Traverse.PFinalizeFrameTransitionV1 ->
  Term s PDataTraverseControlV1
papplyFinalizeTransition control transition = pmatch control $ \c -> pmatch transition $ \t ->
  pcon $ PDataTraverseControlV1
    { ptraverse'version = ptraverse'version c
    , ptraverse'stage = pdata $ Traverse.ptransition'nextStage t
    , ptraverse'sourceStart = ptraverse'sourceStart c
    , ptraverse'sourceLength = ptraverse'sourceLength c
    , ptraverse'offset = ptraverse'offset c
    , ptraverse'frameRoot = pdata $ Traverse.ptransition'nextFrameRoot t
    , ptraverse'pendingLargeExpectedChildren = pdata $ pcon PDNothing
    , ptraverse'integer = pdata $ pcon PDNothing
    , ptraverse'bytes = pdata $ pcon PDNothing
    , ptraverse'result = pdata $ Traverse.ptransition'nextResult t
    }

passembleFinalizeTransition :: forall s.
  Term s POuterNormalizedScriptSourcesRedeemerActionV1 ->
  Term s Traverse.PFinalizeFrameTransitionV1 -> Term s PByteString
passembleFinalizeTransition state transition = pmatch state $ \st -> pmatch transition $ \t ->
  pmatch (pfromData $ pouterNormalized'traversalSerializationTemplate st) $ \case
    PFoldMapFrameRootTemplate _ _ -> perror
    PFinalizeFrameTemplate prefix sourceFields suffix ->
      pfromData prefix
        <> pcborInt (Traverse.ptransition'nextStage t)
        <> pfromData sourceFields
        <> (pencodeDefiniteBytes # Traverse.ptransition'nextFrameRoot t)
        <> pfromData suffix
        <> (Traverse.pencodeOptionalSummaryV1 # Traverse.ptransition'nextResult t)

pmutateFrameExpectedChildren :: forall s.
  Term s Frame.PDataFrameV1 -> Term s PInteger -> Term s Frame.PDataFrameV1
pmutateFrameExpectedChildren frame expected = pmatch frame $ \f ->
  pcon $ Frame.PDataFrameV1
    { Frame.pframe'kind = Frame.pframe'kind f
    , Frame.pframe'constructor = Frame.pframe'constructor f
    , Frame.pframe'constructorCborRoot = Frame.pframe'constructorCborRoot f
    , Frame.pframe'constructorCborLength = Frame.pframe'constructorCborLength f
    , Frame.pframe'constructorMemory = Frame.pframe'constructorMemory f
    , Frame.pframe'tail = Frame.pframe'tail f
    , Frame.pframe'expectedChildren = pdata expected
    , Frame.pframe'childCount = Frame.pframe'childCount f
    , Frame.pframe'childPeaks = Frame.pframe'childPeaks f
    , Frame.pframe'foldCursor = Frame.pframe'foldCursor f
    , Frame.pframe'sequence = Frame.pframe'sequence f
    }

pcontrolOffset, pcontrolSourceLength :: forall s. Term s PDataTraverseControlV1 -> Term s PInteger
pcontrolOffset control = pmatch control $ \c -> pfromData (ptraverse'offset c)
pcontrolSourceLength control = pmatch control $ \c -> pfromData (ptraverse'sourceLength c)

pmaybeIsNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
pmaybeIsNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

narrowCommitmentMutations :: forall s. Term s PBool
narrowCommitmentMutations =
  plet pcurrentControlData $ \control ->
  plet pfoldMapAction $ \foldAction ->
  plet pfinalizeAction $ \finalizeAction ->
  plet (pnarrowPreimageHashV1 # control # pforgetData (pdata foldAction)) $ \expected ->
    pand'List
      [ pnarrowActionIsBoundV1 # control # foldAction # pfoldMapFamily # expected
      , pnot #$ pnarrowActionIsBoundV1 # control # foldAction # pfinalizeFrameFamily # expected
      , pnot #$ pnarrowActionIsBoundV1 # control # finalizeAction # pfoldMapFamily # expected
      , pnot #$ pnarrowActionIsBoundV1 # control # foldAction # pfoldMapFamily # pconstant hashB
      ]

foldMapIdentityMutations :: forall s. Term s PBool
foldMapIdentityMutations =
  plet pfoldMapAction $ \action ->
  plet (ptraversalActionIdentityV1 # action) $ \identity ->
    pand'List
      [ ptraversalActionIdentityIsBoundV1 # action # pfoldMapFamily # identity
      , pnot #$ ptraversalActionIdentityIsBoundV1 # pfoldMapActionAt 1 psummaryA pnil pnil # pfoldMapFamily # identity
      , pnot #$ ptraversalActionIdentityIsBoundV1 # pfoldMapActionAt 0 psummaryB pnil pnil # pfoldMapFamily # identity
      , pnot #$ ptraversalActionIdentityIsBoundV1 # pfoldMapActionAt 0 psummaryA (pcons # pdata (pconstant hashB) # pnil) pnil # pfoldMapFamily # identity
      , pnot #$ ptraversalActionIdentityIsBoundV1 # pfoldMapActionAt 0 psummaryA pnil (pcons # pdata (pconstant hashB) # pnil) # pfoldMapFamily # identity
      , pnot #$ ptraversalActionIdentityIsBoundV1 # action # pfinalizeFrameFamily # identity
      , pnot #$ ptraversalActionIdentityIsBoundV1 # action # pfoldMapFamily # pconstant hashB
      ]

finalizeIdentityMutations :: forall s. Term s PBool
finalizeIdentityMutations =
  plet pfinalizeAction $ \action ->
  plet (ptraversalActionIdentityV1 # action) $ \identity ->
  plet (pcon $ PFinalizeFrame (pdata $ pinitialMapFrameV1 # pconstant "" # 2) (pdata $ pcon PDNothing)) $ \wrongFrame ->
  plet
    ( pcon $ PFinalizeFrame
        (pdata $ pinitialMapFrameV1 # pconstant "" # 0)
        (pdata $ pcon $ PDJust $ pdata $ pinitialListFrameV1 # pconstant "" # 0)
    )
    $ \wrongParent ->
      pand'List
        [ ptraversalActionIdentityIsBoundV1 # action # pfinalizeFrameFamily # identity
        , pnot #$ ptraversalActionIdentityIsBoundV1 # wrongFrame # pfinalizeFrameFamily # identity
        , pnot #$ ptraversalActionIdentityIsBoundV1 # wrongParent # pfinalizeFrameFamily # identity
        , pnot #$ ptraversalActionIdentityIsBoundV1 # action # pfoldMapFamily # identity
        , pnot #$ ptraversalActionIdentityIsBoundV1 # action # pfinalizeFrameFamily # pconstant hashB
        ]

predecessorDeploymentRouteMutations :: forall s. Term s PBool
predecessorDeploymentRouteMutations = plet ptraversalNormalizedState $ \state ->
  pand'List
    [ ptraversalNormalizedStateIsBoundV1 # state # pconstant hashA # pconstant scriptA # pconstant scriptB
    , pnot #$ ptraversalNormalizedStateIsBoundV1 # state # pconstant hashB # pconstant scriptA # pconstant scriptB
    , pnot #$ ptraversalNormalizedStateIsBoundV1 # state # pconstant hashA # pconstant scriptE # pconstant scriptB
    , pnot #$ ptraversalNormalizedStateIsBoundV1 # state # pconstant hashA # pconstant scriptA # pconstant scriptE
    , pouterNormalizerRouteIsExactV1 # state # pconstant scriptC # pconstant scriptD
    , pnot #$ pouterNormalizerRouteIsExactV1 # state # pconstant scriptB # pconstant scriptD
    , pnot #$ pouterNormalizerRouteIsExactV1 # state # pconstant scriptC # pconstant scriptE
    ]

familyTemplatesExact :: forall s. Term s PBool
familyTemplatesExact =
  plet (Traverse.pencodeControlV1 # pfoldControl) $ \checked ->
  pmatch (ptraversalSerializationTemplateV1 # pfoldControl # pfoldMapFamily) $ \case
    PFinalizeFrameTemplate _ _ _ -> pconstant False
    PFoldMapFrameRootTemplate prefix suffix ->
      plet
        (pfromData prefix <> (pencodeDefiniteBytes # pconstant hashA) <> pfromData suffix)
        $ \assembled ->
        plet (Traverse.pencodeControlV1 # pfinalizeControl) $ \finalizeChecked ->
        pmatch (ptraversalSerializationTemplateV1 # pfinalizeControl # pfinalizeFrameFamily) $ \case
          PFoldMapFrameRootTemplate _ _ -> pconstant False
          PFinalizeFrameTemplate stagePrefix sourceFields resultSuffix ->
            plet
              ( pfromData stagePrefix
                  <> pcborInt Traverse.pstageFold
                  <> pfromData sourceFields
                  <> (pencodeDefiniteBytes # pconstant hashB)
                  <> pfromData resultSuffix
                  <> pconstant "\xd8\x7a\x80"
              )
              $ \finalizeAssembled ->
                assembled #== checked
                  #&& pnot # (assembled <> pconstant "\x00" #== checked)
                  #&& finalizeAssembled #== finalizeChecked
                  #&& pnot # (finalizeAssembled <> pconstant "\x00" #== finalizeChecked)

foldMapOuterMutations :: forall s. Term s PBool
foldMapOuterMutations = plet (pouterState pfoldMapFamily pouterNormalizedDomain pfoldControl scriptD pfoldMapAction) $ \state ->
  pand'List
    [ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashA
        # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptE
    , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashB
        # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptE
    , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashA
        # pconstant scriptE # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptE
    , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashA
        # pconstant scriptA # pconstant scriptE # pconstant scriptC # pconstant scriptD # pconstant scriptE
    , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashA
        # pconstant scriptA # pconstant scriptB # pconstant scriptE # pconstant scriptD # pconstant scriptE
    , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashA
        # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptE # pconstant scriptE
    , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashA
        # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptD
    , psemanticExecutorRouteIsExactV1 # state # pfoldMapFamily # pconstant scriptD # pconstant scriptE # pconstant scriptE
    , pnot #$ psemanticExecutorRouteIsExactV1 # state # pfoldMapFamily # pconstant scriptC # pconstant scriptE # pconstant scriptE
    , pnot #$ psemanticExecutorRouteIsExactV1 # state # pfoldMapFamily # pconstant scriptD # pconstant scriptD # pconstant scriptE
    , pnot #$ psemanticExecutorRouteIsExactV1 # state # pfoldMapFamily # pconstant scriptD # pconstant scriptE # pconstant scriptD
    ]

finalizeOuterMutations :: forall s. Term s PBool
finalizeOuterMutations =
  plet (pouterState pfinalizeFrameFamily pouterNormalizedDomain pfinalizeControl scriptF pfinalizeAction) $ \state ->
  plet (pouterState pfinalizeFrameFamily pouterNormalizedDomain pfinalizeCloseControl scriptF pfinalizeAction) $ \wrongStage ->
  plet (pouterState pfinalizeFrameFamily ptraversalNormalizedDomain pfinalizeControl scriptF pfinalizeAction) $ \wrongDomain ->
    pand'List
      [ pouterNormalizedStateIsBoundV1 # state # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfoldMapFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # wrongStage # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # wrongDomain # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfinalizeFrameFamily # pconstant hashB
          # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptE # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptE # pconstant scriptC # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptE # pconstant scriptF # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptE
      , pnot #$ pouterNormalizedStateIsBoundV1 # state # pfinalizeFrameFamily # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptD
      , psemanticExecutorRouteIsExactV1 # state # pfinalizeFrameFamily # pconstant scriptF # pconstant scriptE # pconstant scriptE
      , pnot #$ psemanticExecutorRouteIsExactV1 # state # pfoldMapFamily # pconstant scriptF # pconstant scriptE # pconstant scriptE
      , pnot #$ psemanticExecutorRouteIsExactV1 # state # pfinalizeFrameFamily # pconstant scriptD # pconstant scriptE # pconstant scriptE
      , pnot #$ psemanticExecutorRouteIsExactV1 # state # pfinalizeFrameFamily # pconstant scriptF # pconstant scriptD # pconstant scriptE
      ]

pouterState :: forall s.
  Term s PInteger -> Term s PByteString -> Term s PDataTraverseControlV1 ->
  BS.ByteString -> Term s PDataTraverseActionV1 ->
  Term s POuterNormalizedScriptSourcesRedeemerActionV1
pouterState family domain control semanticExecutor action =
  pcon $ POuterNormalizedScriptSourcesRedeemerActionV1
    { pouterNormalized'version = pdata pversion
    , pouterNormalized'domain = pdata domain
    , pouterNormalized'deploymentId = pdata $ pconstant hashA
    , pouterNormalized'baseProvenanceIdentity = pdata $ pconstant hashB
    , pouterNormalized'envelopeBinderScriptHash = pdata $ pconstant scriptA
    , pouterNormalized'traversalNormalizerScriptHash = pdata $ pconstant scriptB
    , pouterNormalized'outerNormalizerScriptHash = pdata $ pconstant scriptC
    , pouterNormalized'semanticExecutorScriptHash = pdata $ pconstant semanticExecutor
    , pouterNormalized'settlementScriptHash = pdata $ pconstant scriptE
    , pouterNormalized'actionFamily = pdata family
    , pouterNormalized'canonicalActionHash = pdata $ pconstant hashA
    , pouterNormalized'authenticatedTraversalActionIdentity = pdata $ ptraversalActionIdentityV1 # action
    , pouterNormalized'currentPendingItemControlHash = pdata $ pconstant hashA
    , pouterNormalized'expectedNextItemControlHash = pdata $ pconstant hashB
    , pouterNormalized'redeemerCount = pdata 0
    , pouterNormalized'redeemerTotalCount = pdata 1
    , pouterNormalized'nextItemControlHashPrefixCbor = pdata $ pconstant "\x01"
    , pouterNormalized'validatedTraversalControl = pdata control
    , pouterNormalized'traversalSerializationTemplate = pdata $
        ptraversalSerializationTemplateV1 # control # family
    }

foldMapAttestedOutput :: forall s. Term s PBool
foldMapAttestedOutput =
  plet (pouterState pfoldMapFamily pouterNormalizedDomain pfoldControl scriptD pfoldMapAction) $ \outer ->
  plet (pexecutionAttestedStateV1 # outer # pconstant hashB) $ \expected ->
  plet (pmutateAttested expected pexecutionAttestedDomain pfoldMapFamily hashA) $ \wrong ->
    pmatch expected $ \attested -> pmatch outer $ \state ->
      pfromData (pattested'version attested) #== pversion
        #&& pfromData (pattested'domain attested) #== pexecutionAttestedDomain
        #&& pfromData (pattested'deploymentId attested) #== pfromData (pouterNormalized'deploymentId state)
        #&& pfromData (pattested'baseProvenanceIdentity attested)
          #== pfromData (pouterNormalized'baseProvenanceIdentity state)
        #&& pfromData (pattested'actionFamily attested) #== pfoldMapFamily
        #&& pfromData (pattested'actualNextItemControlHash attested)
          #== pfromData (pattested'expectedNextItemControlHash attested)
        #&& pnot # (expected #== wrong)

finalizeAttestedOutput :: forall s. Term s PBool
finalizeAttestedOutput =
  plet (pouterState pfinalizeFrameFamily pouterNormalizedDomain pfinalizeControl scriptF pfinalizeAction) $ \finalizeOuter ->
  plet (pouterState pfoldMapFamily pouterNormalizedDomain pfoldControl scriptD pfoldMapAction) $ \foldOuter ->
  plet (pexecutionAttestedStateV1 # finalizeOuter # pconstant hashB) $ \finalizeOutput ->
  plet (pexecutionAttestedStateV1 # foldOuter # pconstant hashB) $ \foldOutput ->
  plet (pmutateAttested finalizeOutput pouterNormalizedDomain pfinalizeFrameFamily hashB) $ \wrongDomain ->
  plet (pmutateAttested finalizeOutput pexecutionAttestedDomain pfoldMapFamily hashB) $ \wrongFamily ->
  plet (pmutateAttested finalizeOutput pexecutionAttestedDomain pfinalizeFrameFamily hashA) $ \wrongActual ->
    pmatch finalizeOutput $ \finalize -> pmatch foldOutput $ \fold ->
      pfromData (pattested'domain finalize) #== pexecutionAttestedDomain
        #&& pfromData (pattested'domain fold) #== pexecutionAttestedDomain
        #&& pfromData (pattested'actionFamily finalize) #== pfinalizeFrameFamily
        #&& pfromData (pattested'actionFamily fold) #== pfoldMapFamily
        #&& pfromData (pattested'semanticExecutorScriptHash finalize) #== pconstant scriptF
        #&& pfromData (pattested'semanticExecutorScriptHash fold) #== pconstant scriptD
        #&& pfromData (pattested'actualNextItemControlHash finalize)
          #== pfromData (pattested'expectedNextItemControlHash finalize)
        #&& pnot # (finalizeOutput #== wrongDomain)
        #&& pnot # (finalizeOutput #== wrongFamily)
        #&& pnot # (finalizeOutput #== wrongActual)

pmutateAttested :: forall s.
  Term s PScriptSourcesRedeemerExecutionAttestedStateV1 -> Term s PByteString ->
  Term s PInteger -> BS.ByteString -> Term s PScriptSourcesRedeemerExecutionAttestedStateV1
pmutateAttested state domain family actual = pmatch state $ \s ->
  pcon $ PScriptSourcesRedeemerExecutionAttestedStateV1
    { pattested'version = pattested'version s
    , pattested'domain = pdata domain
    , pattested'deploymentId = pattested'deploymentId s
    , pattested'baseProvenanceIdentity = pattested'baseProvenanceIdentity s
    , pattested'envelopeBinderScriptHash = pattested'envelopeBinderScriptHash s
    , pattested'traversalNormalizerScriptHash = pattested'traversalNormalizerScriptHash s
    , pattested'outerNormalizerScriptHash = pattested'outerNormalizerScriptHash s
    , pattested'semanticExecutorScriptHash = pattested'semanticExecutorScriptHash s
    , pattested'settlementScriptHash = pattested'settlementScriptHash s
    , pattested'actionFamily = pdata family
    , pattested'canonicalActionHash = pattested'canonicalActionHash s
    , pattested'authenticatedTraversalActionIdentity = pattested'authenticatedTraversalActionIdentity s
    , pattested'currentPendingItemControlHash = pattested'currentPendingItemControlHash s
    , pattested'expectedNextItemControlHash = pattested'expectedNextItemControlHash s
    , pattested'actualNextItemControlHash = pdata $ pconstant actual
    , pattested'redeemerCount = pattested'redeemerCount s
    , pattested'redeemerTotalCount = pattested'redeemerTotalCount s
    }

envelopeBinding :: forall s. Term s PBool
envelopeBinding =
  plet ppreparedBase $ \base ->
  plet (presolutionIdentityV1 # base) $ \resolutionIdentity ->
  plet (penvelopeCommitmentFor base resolutionIdentity) $ \commitment ->
  plet (ppreparedEnvelope base resolutionIdentity commitment) $ \envelope ->
  plet (pmutateEnvelopeCommitment envelope (pconstant hashA)) $ \wrongCommitment ->
    pand'List
      [ penvelopeStateIsBoundV1 # envelope # pconstant hashA # pconstant scriptA
      , pnot #$ penvelopeStateIsBoundV1 # envelope # pconstant hashB # pconstant scriptA
      , pnot #$ penvelopeStateIsBoundV1 # envelope # pconstant hashA # pconstant scriptE
      , pnot #$ penvelopeStateIsBoundV1 # wrongCommitment # pconstant hashA # pconstant scriptA
      , ptraversalNormalizerRouteIsExactV1 # envelope # pconstant scriptB # pconstant scriptC
      , pnot #$ ptraversalNormalizerRouteIsExactV1 # envelope # pconstant scriptA # pconstant scriptC
      , pnot #$ ptraversalNormalizerRouteIsExactV1 # envelope # pconstant scriptB # pconstant scriptD
      , plengthBS # (pbaseProvenanceIdentityV1 # envelope) #== 32
      , pnot #$ pbaseProvenanceIdentityV1 # envelope #== pbaseProvenanceIdentityV1 # wrongCommitment
      ]

settlementAcceptsBothFamilies :: forall s. Term s PBool
settlementAcceptsBothFamilies =
  plet (psettlementEnvelope pfoldMapFamily $ pconstant scriptD) $ \foldEnvelope ->
  plet (psettlementAttestation foldEnvelope) $ \foldState ->
  plet (psettlementEnvelope pfinalizeFrameFamily $ pconstant scriptF) $ \finalizeEnvelope ->
  plet (psettlementAttestation finalizeEnvelope) $ \finalizeState ->
    pand'List
      [ pexecutionAttestationSettlementIsExactV1
          # foldState # foldEnvelope # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC
          # pconstant scriptD # pconstant scriptF # pconstant scriptE
          # pconstant scriptG # pconstant scriptG
          # pforgetData (pdata pwinningResolution)
      , pexecutionAttestationSettlementIsExactV1
          # finalizeState # finalizeEnvelope # pconstant hashA
          # pconstant scriptA # pconstant scriptB # pconstant scriptC
          # pconstant scriptD # pconstant scriptF # pconstant scriptE
          # pconstant scriptG # pconstant scriptG
          # pforgetData (pdata pwinningResolution)
      ]

settlementRejectsEnvelopeProvenanceAndDomain :: forall s. Term s PBool
settlementRejectsEnvelopeProvenanceAndDomain =
  plet (psettlementEnvelope pfoldMapFamily $ pconstant scriptD) $ \envelope ->
  plet (psettlementAttestation envelope) $ \state ->
  pmatch envelope $ \env ->
  plet
    (pmutateEnvelopeBaseAux envelope (pfromData $ penvelope'base env) (pconstant hashB))
    $ \wrongEnvelope ->
  plet
    (pmutatePreparedEvidence (pfromData $ penvelope'base env) (pconstant hashA))
    $ \wrongBase ->
  plet (pmutateEnvelopeBaseAux envelope wrongBase (pfromData $ penvelope'canonicalAuxiliaryHash env)) $ \wrongBaseEnvelope ->
  plet (pmutateSettlementAttested state (pconstant hashA) (pconstant scriptD) (pconstant hashB)
          (pconstant hashA) (pconstant hashA) (pconstant hashB) (pconstant hashB) 0 1) $ \wrongProvenance ->
  plet (pmutateAttested state pouterNormalizedDomain pfoldMapFamily hashB) $ \wrongDomain ->
    pand'List
      [ pattestationBound # state # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # state # wrongEnvelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # state # wrongBaseEnvelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # wrongProvenance # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # wrongDomain # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      ]

settlementRejectsDeploymentFamilyAndRoute :: forall s. Term s PBool
settlementRejectsDeploymentFamilyAndRoute =
  plet (psettlementEnvelope pfoldMapFamily $ pconstant scriptD) $ \envelope ->
  plet (psettlementAttestation envelope) $ \state ->
  plet (pmutateAttested state pexecutionAttestedDomain pfinalizeFrameFamily hashB) $ \wrongFamily ->
  plet (pmutateSettlementAttested state (pbaseProvenanceIdentityV1 # envelope) (pconstant scriptF) (pconstant hashB)
          (pconstant hashA) (pconstant hashA) (pconstant hashB) (pconstant hashB) 0 1) $ \wrongExecutor ->
    pand'List
      [ pnot #$ pattestationBound # state # envelope # pconstant hashB # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # state # envelope # pconstant hashA # pconstant scriptF
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # state # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptF # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # state # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptF # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # state # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptF # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # state # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptD
      , pnot #$ pattestationBound # wrongFamily # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      , pnot #$ pattestationBound # wrongExecutor # envelope # pconstant hashA # pconstant scriptA
          # pconstant scriptB # pconstant scriptC # pconstant scriptD # pconstant scriptF # pconstant scriptE
      ]

settlementRejectsCopiedHashCountAndOutput :: forall s. Term s PBool
settlementRejectsCopiedHashCountAndOutput =
  plet (psettlementEnvelope pfinalizeFrameFamily $ pconstant scriptF) $ \envelope ->
  plet (psettlementAttestation envelope) $ \state ->
  plet (pbaseProvenanceIdentityV1 # envelope) $ \provenance ->
  plet (pmutateSettlementAttested state provenance (pconstant scriptF) (pconstant hashA)
          (pconstant hashA) (pconstant hashA) (pconstant hashB) (pconstant hashB) 0 1) $ \wrongAction ->
  plet (pmutateSettlementAttested state provenance (pconstant scriptF) (pconstant hashB)
          (pconstant "") (pconstant hashA) (pconstant hashB) (pconstant hashB) 0 1) $ \wrongIdentity ->
  plet (pmutateSettlementAttested state provenance (pconstant scriptF) (pconstant hashB)
          (pconstant hashA) (pconstant "") (pconstant hashB) (pconstant hashB) 0 1) $ \wrongCurrent ->
  plet (pmutateSettlementAttested state provenance (pconstant scriptF) (pconstant hashB)
          (pconstant hashA) (pconstant hashA) (pconstant "") (pconstant hashB) 0 1) $ \wrongExpected ->
  plet (pmutateSettlementAttested state provenance (pconstant scriptF) (pconstant hashB)
          (pconstant hashA) (pconstant hashA) (pconstant hashB) (pconstant "") 0 1) $ \wrongActual ->
  plet (pmutateSettlementAttested state provenance (pconstant scriptF) (pconstant hashB)
          (pconstant hashA) (pconstant hashA) (pconstant hashB) (pconstant hashB) 1 1) $ \wrongCount ->
  plet (pmutateSettlementAttested state provenance (pconstant scriptF) (pconstant hashB)
          (pconstant hashA) (pconstant hashA) (pconstant hashB) (pconstant hashB) 0 2) $ \wrongTotal ->
    pand'List
      [ pnot #$ psettlementExact # wrongAction # envelope # pconstant scriptG # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # wrongIdentity # envelope # pconstant scriptG # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # wrongCurrent # envelope # pconstant scriptG # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # wrongExpected # envelope # pconstant scriptG # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # wrongActual # envelope # pconstant scriptG # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # wrongCount # envelope # pconstant scriptG # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # wrongTotal # envelope # pconstant scriptG # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # state # envelope # pconstant scriptF # pforgetData (pdata pwinningResolution)
      , pnot #$ psettlementExact # state # envelope # pconstant scriptG # pforgetData (pdata (2 :: Term s PInteger))
      ]

rawEnvelopeAuxiliaryConstructor :: forall s. Term s PBool
rawEnvelopeAuxiliaryConstructor =
  pmatch (pasConstr # prawEnvelopeAuxiliary) $ \(PBuiltinPair tag fields) ->
    tag #== 18 #&& plength # fields #== 3

rawEnvelopeAcceptsCanonicalWitness :: forall s. Term s PBool
rawEnvelopeAcceptsCanonicalWitness =
  plet (prawEnvelopeWitnessCbor $ pconstant hashA) $ \witnessCbor ->
  plet (prawEnvelopeWitnessCbor $ pconstant hashB) $ \successorCbor ->
  plet (prawEnvelopeSerialiseDataWitnessCbor $ pconstant hashA) $ \serialised ->
  pmatch (prawVerifyEnvelope witnessCbor successorCbor $ pconstant hashB) $ \case
    PNothing -> pconstant False
    PJust facts -> pmatch facts $ \observed ->
      pand'List
        [ psliceBS # 0 # 2 # witnessCbor #== Codec.pencodeDefiniteArrayHeader # 31
        , pnot #$ witnessCbor #== serialised
        , successorCbor
            #== psliceBS # 0 # (plengthBS # witnessCbor - 32) # witnessCbor <> pconstant hashB
        , pfromData (penvelopeFacts'currentPendingItemControlHash observed) #== pconstant hashA
        , pfromData (penvelopeFacts'redeemerCount observed) #== 0
        , pfromData (penvelopeFacts'redeemerTotalCount observed) #== 3
        , pfromData (penvelopeFacts'canonicalAuxiliaryHash observed)
            #== pcanonicalAuxiliaryHashV1 # prawEnvelopeAuxiliary
        , pfromData (penvelopeFacts'canonicalActionHash observed)
            #== pcanonicalActionHashV1 # prawEnvelopeAuxiliary # pfoldMapFamily
        ]

rawEnvelopeRejectsNonCanonicalAndMutatedSuccessor :: forall s. Term s PBool
rawEnvelopeRejectsNonCanonicalAndMutatedSuccessor =
  plet (prawEnvelopeWitnessCbor $ pconstant hashA) $ \witnessCbor ->
  plet (prawEnvelopeWitnessCbor $ pconstant hashB) $ \successorCbor ->
  plet (prawEnvelopeSerialiseDataWitnessCbor $ pconstant hashA) $ \serialised ->
  plet (prawEnvelopeSerialiseDataWitnessCbor $ pconstant hashB) $ \serialisedSuccessor ->
  plet (psliceBS # 0 # (plengthBS # successorCbor - 32) # successorCbor <> pconstant hashA) $ \mutatedSuccessor ->
  pmatch (pdeserialise # serialised) $ \case
    PNothing -> pconstant False
    PJust serialisedData ->
      pand'List
        [ plength # (pasList # serialisedData) #== 31
        , pnot #$ serialised #== witnessCbor
        , psliceBS # 0 # 1 # serialised #== pconstant "\x9f"
        , prawVerifyEnvelope serialised serialisedSuccessor (pconstant hashB) #== pcon PNothing
        , prawVerifyEnvelope serialised successorCbor (pconstant hashB) #== pcon PNothing
        , prawVerifyEnvelope witnessCbor mutatedSuccessor (pconstant hashB) #== pcon PNothing
        , prawVerifyEnvelope witnessCbor successorCbor (pconstant hashA) #== pcon PNothing
        , prawVerifyEnvelope witnessCbor successorCbor (pconstant $ BS.take 31 hashB) #== pcon PNothing
        , pnot #$ prawVerifyEnvelope witnessCbor successorCbor (pconstant hashB) #== pcon PNothing
        ]

prawVerifyEnvelope :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s (PMaybe PScriptSourcesRedeemerEnvelopeFactsV1)
prawVerifyEnvelope witnessCbor successorCbor expectedNext =
  pverifyRawEnvelopeV1
    # prawEnvelopePreState
    # (prawEnvelopeTransition witnessCbor successorCbor)
    # prawEnvelopeAuxiliary
    # expectedNext
    # pfoldMapFamily

prawEnvelopeWitnessCbor :: forall s. Term s PByteString -> Term s PByteString
prawEnvelopeWitnessCbor pendingHash =
  Codec.pencodeDefiniteArrayHeader # 31
    <> (pencodeDefiniteBytes # pconstant rawCompactCbor)
    <> (pencodeDefiniteBytes # pconstant rawWitnessSetCompactCbor)
    <> (pencodeDefiniteBytes # pconstant rawFieldPreimageLengthsCbor)
    <> (pencodeDefiniteBytes # pconstant rawContextCbor)
    <> pcborInt 0
    <> (pencodeDefiniteBytes # pconstant zeroHash)
    <> pcborInt 0
    <> (pencodeDefiniteBytes # pconstant zeroHash)
    <> pconstant "\x80"
    <> pcborInt 1
    <> pcborInt 0
    <> pconstant "\x80"
    <> pcborInt 0
    <> pconstant "\x80"
    <> pcborInt 0
    <> (pencodeDefiniteBytes # pconstant zeroHash)
    <> (pencodeDefiniteBytes # pconstant zeroHash)
    <> pcborInt 0
    <> pcborInt 0
    <> pconstant "\x80"
    <> pcborInt 0
    <> pcborInt 0
    <> pconstant "\x80"
    <> pcborInt 0
    <> pconstant "\x86\x00\x80\x00\x40\x40\x80"
    <> pcborInt 0
    <> pcborInt 3
    <> pconstant "\x80"
    <> pcborInt 0
    <> (pencodeDefiniteBytes # pconstant zeroHash)
    <> (pencodeDefiniteBytes # pendingHash)

prawEnvelopeSerialiseDataWitnessCbor :: forall s.
  Term s PByteString -> Term s PByteString
prawEnvelopeSerialiseDataWitnessCbor pendingHash =
  pmatch (pdeserialise # prawEnvelopeWitnessCbor pendingHash) $ \case
    PNothing -> perror
    PJust dat -> pserialiseData # dat

prawEnvelopeAuxiliary :: forall s. Term s PData
prawEnvelopeAuxiliary =
  plet (pconstructorData 1 pnil) $ \none ->
  plet (pforgetData $ pdata (0 :: Term s PInteger)) $ \control ->
  plet (pconstructorData 2 $ pcons # pforgetData (pdata pfoldMapAction) # pnil) $ \itemAction ->
  plet (pconstructorData 0 $ pcons # itemAction #$ pcons # none #$ pcons # none # pnil) $ \witness ->
    pconstructorData 18 $ pcons # none #$ pcons # control #$ pcons # witness # pnil

pconstructorData :: forall s.
  Term s PInteger -> Term s (PBuiltinList PData) -> Term s PData
pconstructorData tag fields = pforgetData $ pconstrBuiltin # tag # fields

prawEnvelopePreState :: forall s. Term s PValidationMachineStateV1
prawEnvelopePreState =
  plet (prawEnvelopeWitnessCbor $ pconstant hashA) $ \witness ->
    prawEnvelopeMachineState
      1
      (phashWorkWitness # pcon PScriptSources # 1 # witness)

prawEnvelopeTransition :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PValidationOneStepWitnessV1
prawEnvelopeTransition witness successorWitness = pcon $ PValidationOneStepWitnessV1
  { poneStep'workWitnessCbor = pdata witness
  , poneStep'claimedSuccessor = pdata $
      prawEnvelopeMachineState
        2
        (phashWorkWitness # pcon PScriptSources # 2 # successorWitness)
  }

prawEnvelopeMachineState :: forall s.
  Term s PInteger -> Term s PByteString -> Term s PValidationMachineStateV1
prawEnvelopeMachineState programCounter workRoot = pcon $ PValidationMachineStateV1
  { pmachineState'machineVersion = pdata pmachineVersion
  , pmachineState'eventKeyHash = pdata $ pconstant hashA
  , pmachineState'transactionId = pdata $ pconstant hashB
  , pmachineState'transactionCommitment = pdata $
      pnativeTxProofCommitmentV1
        # pconstant rawCompactCbor
        # pconstant rawWitnessSetCompactCbor
        # pconstant rawFieldPreimageLengthsCbor
  , pmachineState'validationContextHash = pdata $ phashValidationContext # pconstant rawContextCbor
  , pmachineState'sourceKind = pdata $ pcon PNormal
  , pmachineState'priorLedgerRoot = pdata $ pconstant hashA
  , pmachineState'phase = pdata $ pcon PScriptSources
  , pmachineState'programCounter = pdata programCounter
  , pmachineState'workRoot = pdata workRoot
  , pmachineState'executionCpu = pdata 0
  , pmachineState'executionMemory = pdata 0
  , pmachineState'verdict = pdata $ pcon PPending
  , pmachineState'rejectionCodeHash = pdata $ pconstant zeroHash
  , pmachineState'ledgerDeltaRoot = pdata $ pconstant hashB
  }

pattestationBound :: forall s.
  Term s
    ( PScriptSourcesRedeemerExecutionAttestedStateV1
        :--> PPreparedScriptSourcesRedeemerEnvelopeV1
        :--> PByteString :--> PByteString :--> PByteString :--> PByteString
        :--> PByteString :--> PByteString :--> PByteString :--> PBool
    )
pattestationBound = pexecutionAttestationIsBoundToEnvelopeV1

psettlementExact :: forall s.
  Term s
    ( PScriptSourcesRedeemerExecutionAttestedStateV1
        :--> PPreparedScriptSourcesRedeemerEnvelopeV1
        :--> PByteString :--> PData :--> PBool
    )
psettlementExact = plam $ \state envelope outputScript outputState ->
  pexecutionAttestationSettlementIsExactV1
    # state # envelope # pconstant hashA
    # pconstant scriptA # pconstant scriptB # pconstant scriptC
    # pconstant scriptD # pconstant scriptF # pconstant scriptE
    # pconstant scriptG # outputScript # outputState

psettlementEnvelope :: forall s.
  Term s PInteger -> Term s PByteString -> Term s PPreparedScriptSourcesRedeemerEnvelopeV1
psettlementEnvelope family executor =
  plet ppreparedBase $ \base ->
  plet (presolutionIdentityV1 # base) $ \resolutionIdentity ->
  plet (penvelopeCommitmentForFamily base resolutionIdentity family executor) $ \commitment ->
    ppreparedEnvelopeFor base resolutionIdentity commitment family executor

psettlementAttestation :: forall s.
  Term s PPreparedScriptSourcesRedeemerEnvelopeV1 -> Term s PScriptSourcesRedeemerExecutionAttestedStateV1
psettlementAttestation envelope = pmatch envelope $ \env ->
  pcon $ PScriptSourcesRedeemerExecutionAttestedStateV1
    { pattested'version = pdata pversion
    , pattested'domain = pdata pexecutionAttestedDomain
    , pattested'deploymentId = penvelope'deploymentId env
    , pattested'baseProvenanceIdentity = pdata $ pbaseProvenanceIdentityV1 # envelope
    , pattested'envelopeBinderScriptHash = penvelope'envelopeBinderScriptHash env
    , pattested'traversalNormalizerScriptHash = penvelope'traversalNormalizerScriptHash env
    , pattested'outerNormalizerScriptHash = penvelope'outerNormalizerScriptHash env
    , pattested'semanticExecutorScriptHash = penvelope'semanticExecutorScriptHash env
    , pattested'settlementScriptHash = penvelope'settlementScriptHash env
    , pattested'actionFamily = penvelope'actionFamily env
    , pattested'canonicalActionHash = penvelope'canonicalActionHash env
    , pattested'authenticatedTraversalActionIdentity = pdata $ pconstant hashA
    , pattested'currentPendingItemControlHash = penvelope'currentPendingItemControlHash env
    , pattested'expectedNextItemControlHash = penvelope'expectedNextItemControlHash env
    , pattested'actualNextItemControlHash = penvelope'expectedNextItemControlHash env
    , pattested'redeemerCount = penvelope'redeemerCount env
    , pattested'redeemerTotalCount = penvelope'redeemerTotalCount env
    }

penvelopeCommitmentFor :: forall s.
  Term s PPreparedValidationResolutionStateV1 -> Term s PByteString -> Term s PByteString
penvelopeCommitmentFor base resolutionIdentity =
  penvelopeCommitmentForFamily base resolutionIdentity pfoldMapFamily (pconstant scriptD)

penvelopeCommitmentForFamily :: forall s.
  Term s PPreparedValidationResolutionStateV1 -> Term s PByteString ->
  Term s PInteger -> Term s PByteString -> Term s PByteString
penvelopeCommitmentForFamily base resolutionIdentity family executor = pmatch base $ \prepared ->
  penvelopeCommitmentV1
    # pconstant hashA
    # pfromData (pprepared'evidenceHash prepared)
    # resolutionIdentity
    # family
    # pconstant hashA
    # pconstant hashB
    # pconstant hashA
    # pconstant hashB
    # 0
    # 1
    # pconstant scriptA
    # pconstant scriptB
    # pconstant scriptC
    # executor
    # pconstant scriptE
    # pconstant hashA
    # pconstant hashB

ppreparedEnvelope :: forall s.
  Term s PPreparedValidationResolutionStateV1 -> Term s PByteString ->
  Term s PByteString -> Term s PPreparedScriptSourcesRedeemerEnvelopeV1
ppreparedEnvelope base resolutionIdentity commitment =
  ppreparedEnvelopeFor base resolutionIdentity commitment pfoldMapFamily (pconstant scriptD)

ppreparedEnvelopeFor :: forall s.
  Term s PPreparedValidationResolutionStateV1 -> Term s PByteString ->
  Term s PByteString -> Term s PInteger -> Term s PByteString ->
  Term s PPreparedScriptSourcesRedeemerEnvelopeV1
ppreparedEnvelopeFor base resolutionIdentity commitment family executor = pcon $ PPreparedScriptSourcesRedeemerEnvelopeV1
  { penvelope'version = pdata pversion
  , penvelope'domain = pdata penvelopeDomain
  , penvelope'deploymentId = pdata $ pconstant hashA
  , penvelope'base = pdata base
  , penvelope'resolutionIdentity = pdata resolutionIdentity
  , penvelope'actionFamily = pdata family
  , penvelope'canonicalAuxiliaryHash = pdata $ pconstant hashA
  , penvelope'canonicalActionHash = pdata $ pconstant hashB
  , penvelope'currentPendingItemControlHash = pdata $ pconstant hashA
  , penvelope'expectedNextItemControlHash = pdata $ pconstant hashB
  , penvelope'redeemerCount = pdata 0
  , penvelope'redeemerTotalCount = pdata 1
  , penvelope'envelopeBinderScriptHash = pdata $ pconstant scriptA
  , penvelope'traversalNormalizerScriptHash = pdata $ pconstant scriptB
  , penvelope'outerNormalizerScriptHash = pdata $ pconstant scriptC
  , penvelope'semanticExecutorScriptHash = pdata executor
  , penvelope'settlementScriptHash = pdata $ pconstant scriptE
  , penvelope'envelopeCommitment = pdata commitment
  }

pmutateEnvelopeCommitment :: forall s.
  Term s PPreparedScriptSourcesRedeemerEnvelopeV1 -> Term s PByteString ->
  Term s PPreparedScriptSourcesRedeemerEnvelopeV1
pmutateEnvelopeCommitment envelope commitment = pmatch envelope $ \e ->
  pcon $ PPreparedScriptSourcesRedeemerEnvelopeV1
    { penvelope'version = penvelope'version e
    , penvelope'domain = penvelope'domain e
    , penvelope'deploymentId = penvelope'deploymentId e
    , penvelope'base = penvelope'base e
    , penvelope'resolutionIdentity = penvelope'resolutionIdentity e
    , penvelope'actionFamily = penvelope'actionFamily e
    , penvelope'canonicalAuxiliaryHash = penvelope'canonicalAuxiliaryHash e
    , penvelope'canonicalActionHash = penvelope'canonicalActionHash e
    , penvelope'currentPendingItemControlHash = penvelope'currentPendingItemControlHash e
    , penvelope'expectedNextItemControlHash = penvelope'expectedNextItemControlHash e
    , penvelope'redeemerCount = penvelope'redeemerCount e
    , penvelope'redeemerTotalCount = penvelope'redeemerTotalCount e
    , penvelope'envelopeBinderScriptHash = penvelope'envelopeBinderScriptHash e
    , penvelope'traversalNormalizerScriptHash = penvelope'traversalNormalizerScriptHash e
    , penvelope'outerNormalizerScriptHash = penvelope'outerNormalizerScriptHash e
    , penvelope'semanticExecutorScriptHash = penvelope'semanticExecutorScriptHash e
    , penvelope'settlementScriptHash = penvelope'settlementScriptHash e
    , penvelope'envelopeCommitment = pdata commitment
    }

pmutateEnvelopeBaseAux :: forall s.
  Term s PPreparedScriptSourcesRedeemerEnvelopeV1 ->
  Term s PPreparedValidationResolutionStateV1 -> Term s PByteString ->
  Term s PPreparedScriptSourcesRedeemerEnvelopeV1
pmutateEnvelopeBaseAux envelope base auxiliaryHash = pmatch envelope $ \e ->
  pcon $ PPreparedScriptSourcesRedeemerEnvelopeV1
    { penvelope'version = penvelope'version e
    , penvelope'domain = penvelope'domain e
    , penvelope'deploymentId = penvelope'deploymentId e
    , penvelope'base = pdata base
    , penvelope'resolutionIdentity = penvelope'resolutionIdentity e
    , penvelope'actionFamily = penvelope'actionFamily e
    , penvelope'canonicalAuxiliaryHash = pdata auxiliaryHash
    , penvelope'canonicalActionHash = penvelope'canonicalActionHash e
    , penvelope'currentPendingItemControlHash = penvelope'currentPendingItemControlHash e
    , penvelope'expectedNextItemControlHash = penvelope'expectedNextItemControlHash e
    , penvelope'redeemerCount = penvelope'redeemerCount e
    , penvelope'redeemerTotalCount = penvelope'redeemerTotalCount e
    , penvelope'envelopeBinderScriptHash = penvelope'envelopeBinderScriptHash e
    , penvelope'traversalNormalizerScriptHash = penvelope'traversalNormalizerScriptHash e
    , penvelope'outerNormalizerScriptHash = penvelope'outerNormalizerScriptHash e
    , penvelope'semanticExecutorScriptHash = penvelope'semanticExecutorScriptHash e
    , penvelope'settlementScriptHash = penvelope'settlementScriptHash e
    , penvelope'envelopeCommitment = penvelope'envelopeCommitment e
    }

pmutatePreparedEvidence :: forall s.
  Term s PPreparedValidationResolutionStateV1 -> Term s PByteString ->
  Term s PPreparedValidationResolutionStateV1
pmutatePreparedEvidence base evidence = pmatch base $ \prepared ->
  pcon $ PPreparedValidationResolutionStateV1
    { pprepared'version = pprepared'version prepared
    , pprepared'resolution = pprepared'resolution prepared
    , pprepared'evidenceHash = pdata evidence
    }

pmutateSettlementAttested :: forall s.
  Term s PScriptSourcesRedeemerExecutionAttestedStateV1 ->
  Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PInteger -> Term s PInteger ->
  Term s PScriptSourcesRedeemerExecutionAttestedStateV1
pmutateSettlementAttested state provenance semantic action identity current expected actual count total = pmatch state $ \s ->
  pcon $ PScriptSourcesRedeemerExecutionAttestedStateV1
    { pattested'version = pattested'version s
    , pattested'domain = pattested'domain s
    , pattested'deploymentId = pattested'deploymentId s
    , pattested'baseProvenanceIdentity = pdata provenance
    , pattested'envelopeBinderScriptHash = pattested'envelopeBinderScriptHash s
    , pattested'traversalNormalizerScriptHash = pattested'traversalNormalizerScriptHash s
    , pattested'outerNormalizerScriptHash = pattested'outerNormalizerScriptHash s
    , pattested'semanticExecutorScriptHash = pdata semantic
    , pattested'settlementScriptHash = pattested'settlementScriptHash s
    , pattested'actionFamily = pattested'actionFamily s
    , pattested'canonicalActionHash = pdata action
    , pattested'authenticatedTraversalActionIdentity = pdata identity
    , pattested'currentPendingItemControlHash = pdata current
    , pattested'expectedNextItemControlHash = pdata expected
    , pattested'actualNextItemControlHash = pdata actual
    , pattested'redeemerCount = pdata count
    , pattested'redeemerTotalCount = pdata total
    }

ppreparedBase :: forall s. Term s PPreparedValidationResolutionStateV1
ppreparedBase = pcon $ PPreparedValidationResolutionStateV1
  { pprepared'version = pdata ppreparedResolutionVersion
  , pprepared'resolution = pdata pvalidationResolution
  , pprepared'evidenceHash = pdata $ pconstant hashB
  }

pvalidationResolution :: forall s. Term s PValidationResolutionStateV1
pvalidationResolution = pcon $ PValidationResolutionStateV1
  { presolution'version = pdata presolutionVersion
  , presolution'preState = pdata pmachineState
  , presolution'operatorSuccessorHash = pdata $ pconstant hashA
  , presolution'challengerSuccessorHash = pdata $ pconstant hashB
  }

pmachineState :: forall s. Term s PValidationMachineStateV1
pmachineState = pcon $ PValidationMachineStateV1
  { pmachineState'machineVersion = pdata pmachineVersion
  , pmachineState'eventKeyHash = pdata $ pconstant hashA
  , pmachineState'transactionId = pdata $ pconstant hashB
  , pmachineState'transactionCommitment = pdata $ pconstant hashA
  , pmachineState'validationContextHash = pdata $ pconstant hashB
  , pmachineState'sourceKind = pdata $ pcon PNormal
  , pmachineState'priorLedgerRoot = pdata $ pconstant hashA
  , pmachineState'phase = pdata $ pcon PScriptSources
  , pmachineState'programCounter = pdata 0
  , pmachineState'workRoot = pdata $ pconstant hashA
  , pmachineState'executionCpu = pdata 0
  , pmachineState'executionMemory = pdata 0
  , pmachineState'verdict = pdata $ pcon PPending
  , pmachineState'rejectionCodeHash = pdata $ pconstant (BS.replicate 32 0)
  , pmachineState'ledgerDeltaRoot = pdata $ pconstant hashB
  }

ptraversalNormalizedState :: forall s. Term s PTraversalNormalizedScriptSourcesRedeemerActionV1
ptraversalNormalizedState = pcon $ PTraversalNormalizedScriptSourcesRedeemerActionV1
  { ptraversalNormalized'version = pdata pversion
  , ptraversalNormalized'domain = pdata ptraversalNormalizedDomain
  , ptraversalNormalized'deploymentId = pdata $ pconstant hashA
  , ptraversalNormalized'baseProvenanceIdentity = pdata $ pconstant hashB
  , ptraversalNormalized'envelopeBinderScriptHash = pdata $ pconstant scriptA
  , ptraversalNormalized'traversalNormalizerScriptHash = pdata $ pconstant scriptB
  , ptraversalNormalized'outerNormalizerScriptHash = pdata $ pconstant scriptC
  , ptraversalNormalized'semanticExecutorScriptHash = pdata $ pconstant scriptD
  , ptraversalNormalized'settlementScriptHash = pdata $ pconstant scriptE
  , ptraversalNormalized'actionFamily = pdata pfoldMapFamily
  , ptraversalNormalized'canonicalActionHash = pdata $ pconstant hashA
  , ptraversalNormalized'authenticatedTraversalActionIdentity = pdata $ pconstant hashB
  , ptraversalNormalized'currentPendingItemControlHash = pdata $ pconstant hashA
  , ptraversalNormalized'expectedNextItemControlHash = pdata $ pconstant hashB
  , ptraversalNormalized'redeemerCount = pdata 0
  , ptraversalNormalized'redeemerTotalCount = pdata 1
  , ptraversalNormalized'unvalidatedOuterFields = pdata punvalidatedOuterFields
  , ptraversalNormalized'validatedTraversalControl = pdata pfoldControl
  , ptraversalNormalized'checkedTraversalControlCbor = pdata $ Traverse.pencodeControlV1 # pfoldControl
  }

punvalidatedOuterFields :: forall s. Term s PUnvalidatedRedeemerItemOuterFieldsV1
punvalidatedOuterFields = pcon $ PUnvalidatedRedeemerItemOuterFieldsV1
  { pouterFields'version = pdata 1
  , pouterFields'mode = pdata 1
  , pouterFields'stage = pdata 1
  , pouterFields'itemIndex = pdata 0
  , pouterFields'itemCount = pdata 1
  , pouterFields'totalLength = pdata 20
  , pouterFields'itemCommitment = pdata $ pconstant hashA
  , pouterFields'expectedPurposeTag = pdata 0
  , pouterFields'expectedPointerIndex = pdata 0
  , pouterFields'purposeTag = pdata 0
  , pouterFields'pointerIndex = pdata 0
  , pouterFields'dataOffset = pdata 4
  , pouterFields'dataLength = pdata 10
  , pouterFields'executionMemory = pdata 10
  , pouterFields'executionSteps = pdata 20
  }

pfoldControl :: forall s. Term s PDataTraverseControlV1
pfoldControl = pcon $ PDataTraverseControlV1
  { ptraverse'version = pdata Traverse.pversion
  , ptraverse'stage = pdata Traverse.pstageFold
  , ptraverse'sourceStart = pdata 4
  , ptraverse'sourceLength = pdata 10
  , ptraverse'offset = pdata 10
  , ptraverse'frameRoot = pdata $ pconstant hashA
  , ptraverse'pendingLargeExpectedChildren = pdata $ pcon PDNothing
  , ptraverse'integer = pdata $ pcon PDNothing
  , ptraverse'bytes = pdata $ pcon PDNothing
  , ptraverse'result = pdata $ pcon PDNothing
  }

pfinalizeControl :: forall s. Term s PDataTraverseControlV1
pfinalizeControl = pcon $ PDataTraverseControlV1
  { ptraverse'version = pdata Traverse.pversion
  , ptraverse'stage = pdata Traverse.pstageFold
  , ptraverse'sourceStart = pdata 6
  , ptraverse'sourceLength = pdata 12
  , ptraverse'offset = pdata 12
  , ptraverse'frameRoot = pdata $ pconstant hashB
  , ptraverse'pendingLargeExpectedChildren = pdata $ pcon PDNothing
  , ptraverse'integer = pdata $ pcon PDNothing
  , ptraverse'bytes = pdata $ pcon PDNothing
  , ptraverse'result = pdata $ pcon PDNothing
  }

pfinalizeCloseControl :: forall s. Term s PDataTraverseControlV1
pfinalizeCloseControl = pcon $ PDataTraverseControlV1
  { ptraverse'version = pdata Traverse.pversion
  , ptraverse'stage = pdata Traverse.pstageClose
  , ptraverse'sourceStart = pdata 6
  , ptraverse'sourceLength = pdata 12
  , ptraverse'offset = pdata 12
  , ptraverse'frameRoot = pdata $ pconstant hashB
  , ptraverse'pendingLargeExpectedChildren = pdata $ pcon PDNothing
  , ptraverse'integer = pdata $ pcon PDNothing
  , ptraverse'bytes = pdata $ pcon PDNothing
  , ptraverse'result = pdata $ pcon PDNothing
  }

pfoldMapAction :: forall s. Term s PDataTraverseActionV1
pfoldMapAction = pfoldMapActionAt 0 psummaryA pnil pnil

pfoldMapActionAt :: forall s.
  Term s PInteger -> Term s PDataSummaryV1 ->
  Term s (PBuiltinList (PAsData PByteString)) ->
  Term s (PBuiltinList (PAsData PByteString)) -> Term s PDataTraverseActionV1
pfoldMapActionAt pairIndex key keySiblings valueSiblings = pcon $ PFoldMap
  (pdata $ pinitialMapFrameV1 # pconstant "" # 0)
  (pdata pairIndex)
  (pdata key)
  (pdata psummaryA)
  (pdata keySiblings)
  (pdata valueSiblings)

pfinalizeAction :: forall s. Term s PDataTraverseActionV1
pfinalizeAction = pcon $ PFinalizeFrame
  (pdata $ pinitialMapFrameV1 # pconstant "" # 0)
  (pdata $ pcon PDNothing)

psummaryA, psummaryB :: forall s. Term s PDataSummaryV1
psummaryA = psummary hashA
psummaryB = psummary hashB

psummary :: forall s. BS.ByteString -> Term s PDataSummaryV1
psummary root = pcon $ PDataSummaryV1 (pdata $ pconstant root) (pdata 1) (pdata 4)

pcurrentControlData :: forall s. Term s PData
pcurrentControlData = pforgetData $ pdata (pconstant (42 :: Integer) :: Term s PInteger)

hashA, hashB, scriptA, scriptB, scriptC, scriptD, scriptE, scriptF, scriptG :: BS.ByteString
hashA = BS.replicate 32 0xaa
hashB = BS.replicate 32 0xbb
scriptA = BS.replicate 28 0xaa
scriptB = BS.replicate 28 0xbb
scriptC = BS.replicate 28 0xcc
scriptD = BS.replicate 28 0xdd
scriptE = BS.replicate 28 0xee
scriptF = BS.replicate 28 0xff
scriptG = BS.replicate 28 0x77

zeroHash, rawCompactCbor, rawWitnessSetCompactCbor,
  rawFieldPreimageLengthsCbor, rawContextCbor :: BS.ByteString
zeroHash = BS.replicate 32 0
rawCompactCbor = BS.replicate 80 0xa1
rawWitnessSetCompactCbor = BS.replicate 72 0xb2
rawFieldPreimageLengthsCbor = BS.replicate 40 0xc3
rawContextCbor = BS.replicate 96 0xd4
