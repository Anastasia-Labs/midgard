{-# LANGUAGE OverloadedStrings #-}

module Testing.CekBuiltin (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Kind (Type)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.CekBuiltin (
  PBlsExpressionWitnessV1 (..),
  PRuntimeValueWitnessV1 (..),
  PSemanticBuiltinWitnessV1 (..),
  PValueWitnessV1 (..),
  pargumentsRootV1,
  pbuiltinCostSizesV1,
  pdirectBuiltinBudgetV1,
  pdirectBuiltinFailureBudgetV1,
  pisValidUtf8V1,
  pverifyDirectBuiltinV1,
  pverifyDirectBuiltinFailureV1,
  pverifyDirectBlsFinalRootsV1,
  pverifyDirectBlsFinalV1,
  pverifySemanticBuiltinFailureV1,
  pverifySemanticBuiltinV1,
  pruntimeArgumentsRootV1,
  pverifyBuiltinTypeFailureV1,
 )
import Midgard.CekConstant (PConstantWitnessV1 (..), pconstantRootV1)
import Midgard.CekCost (PBuiltinBudgetV1 (..), pbuiltinArgumentCountV1)
import Midgard.CekData (
  PDataListNodeV1 (..),
  PDataNodeV1 (..),
  PDataSequenceSummaryV1 (..),
  PDataSummaryV1 (..),
  pbytesDataSummaryV1,
  pdataNodeCborLengthV1,
  pdataNodeMemoryV1,
  pemptyDataListRootV1,
  pemptyDataListSummaryV1,
  pemptyDataPairSummaryV1,
  phashDataListNodeV1,
  phashDataNodeV1,
  pintegerDataSummaryV1,
  plistDataSummaryV1,
  pmapDataSummaryV1,
  pprependDataListSummaryV1,
  psmallConstrDataSummaryV1,
 )
import Midgard.CekDataScan (pboundedBlobRootV1)
import Midgard.CekProof (
  phashBlsMillerLoopExpressionV1,
  phashBlsMultiplyExpressionV1,
  phashBuiltinValueV1,
  pemptyEnvironmentRootV1,
  pemptySequenceRootV1,
 )
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Midgard.CekBuiltin"
    [ testCase "add_integer_direct_step_is_authenticated" $
        passertEvalNoTrace addIntegerDirectStepIsAuthenticated
    , testCase "add_integer_direct_budget_matches_reference_cek" $
        passertEvalNoTrace addIntegerDirectBudgetMatchesReferenceCek
    , testCase "empty_trace_message_uses_the_reference_zero_size_quirk" $
        passertEvalNoTrace emptyTraceMessageUsesReferenceZeroSizeQuirk
    , testCase "append_bytearray_direct_step_is_authenticated" $
        passertEvalNoTrace appendBytearrayDirectStepIsAuthenticated
    , testCase "if_then_else_selects_an_opaque_value_without_interpreting_it" $
        passertEvalNoTrace ifThenElseSelectsOpaqueValue
    , testCase "list_head_preserves_the_authenticated_element_type" $
        passertEvalNoTrace listHeadPreservesElementType
    , testCase "i_data_and_un_i_data_round_trip_semantically" $
        passertEvalNoTrace iDataRoundTrip
    , testCase "valid_ecdsa_public_key_with_invalid_signature_returns_false" $
        passertEvalNoTrace validEcdsaKeyWithInvalidSignatureReturnsFalse
    , testCase "miller_loop_expression_and_final_verify_are_authenticated" $
        passertEvalNoTrace millerLoopExpressionAndFinalVerifyAreAuthenticated
    , testCase "six_leaf_miller_loop_proof_fits_the_l1_execution_reserve" $
        passertEvalNoTrace sixLeafMillerLoopProofFitsReserve
    , testCase "ten_leaf_miller_loop_proof_fits_the_l1_execution_reserve" $
        passertEvalNoTrace tenLeafMillerLoopProofFitsReserve
    , testCase "ten_leaf_root_bound_final_proof_fits_the_l1_execution_reserve" $
        passertEvalNoTrace tenLeafRootBoundProofFitsReserve
    , testCase "twelve_leaf_miller_loop_proof_exceeding_the_reserve_fails_closed" $
        pfails twelveLeafMillerLoopProofFailsClosed
    , testCase "unsupported_direct_builtin_stays_closed" $
        passertEvalNoTrace unsupportedDirectBuiltinStaysClosed
    , testCase "wrong_builtin_result_fails_closed" $
        passertEvalNoTrace wrongBuiltinResultFailsClosed
    , testCase "builtin_closure_type_failure_is_authenticated_without_charging" $
        passertEvalNoTrace builtinClosureTypeFailureIsAuthenticated
    , testCase "mk_cons_rejects_an_incongruent_element_type" $
        passertEvalNoTrace mkConsRejectsIncongruentElementType
    , testCase "arbitrary_control_branch_values_are_not_type_failures" $
        passertEvalNoTrace arbitraryControlBranchesAreNotTypeFailures
    , testCase "division_by_zero_failure_is_authenticated_without_charging" $
        passertEvalNoTrace divisionByZeroFailureIsFree
    , testCase "quotient_by_zero_failure_charges_before_the_operation_fails" $
        passertEvalNoTrace quotientByZeroFailureIsPaid
    , testCase "invalid_utf8_failure_is_authenticated_without_decoding" $
        passertEvalNoTrace invalidUtf8FailureIsAuthenticated
    , testCase "integer_to_bytes_size_failure_charges_the_pinned_builtin_cost" $
        passertEvalNoTrace integerToBytesSizeFailureIsPaid
    , testCase "malformed_ecdsa_public_key_failure_is_paid_and_authenticated" $
        passertEvalNoTrace malformedEcdsaFailureIsPaid
    , testCase "malformed_schnorr_x_coordinate_failure_is_paid_and_authenticated" $
        passertEvalNoTrace malformedSchnorrFailureIsPaid
    , testCase "non_residue_ecdsa_x_coordinate_has_a_bounded_failure_proof" $
        passertEvalNoTrace nonResidueEcdsaFailureIsAuthenticated
    , testCase "malformed_bls_g1_compression_header_failure_is_paid" $
        passertEvalNoTrace malformedBlsG1HeaderIsPaid
    , testCase "malformed_bls_g2_compression_header_failure_is_paid" $
        passertEvalNoTrace malformedBlsG2HeaderIsPaid
    , testCase "off_curve_bls_g1_encoding_has_a_bounded_paid_failure_proof" $
        passertEvalNoTrace offCurveBlsG1IsPaid
    , testCase "canonical_bls_infinity_encoding_is_not_a_failure" $
        passertEvalNoTrace canonicalBlsInfinityIsNotFailure
    , testCase "semantic_choose_data_inspects_a_large_context_root_locally" $
        passertEvalNoTrace semanticChooseDataInspectsLargeContextRoot
    , testCase "semantic_unconstr_data_returns_the_exact_large_typed_pair" $
        passertEvalNoTrace semanticUnconstrDataReturnsExactLargeTypedPair
    , testCase "semantic_fst_pair_extracts_the_constructor_without_the_large_field" $
        passertEvalNoTrace semanticFstPairExtractsConstructor
    , testCase "semantic_head_list_extracts_a_9000_byte_data_child_by_root" $
        passertEvalNoTrace semanticHeadListExtractsLargeChild
    , testCase "semantic_wrong_data_variant_failure_is_authenticated_locally" $
        passertEvalNoTrace semanticWrongDataVariantFailureIsAuthenticated
    ]

addIntegerDirectStepIsAuthenticated :: forall (s :: S). Term s PBool
addIntegerDirectStepIsAuthenticated =
  plet (values [integer "1829", integer "01"]) $ \arguments ->
    pverifyDirectBuiltinV1
      # 0
      # (builtinRoot 0 arguments)
      # arguments
      # integer "182a"

addIntegerDirectBudgetMatchesReferenceCek :: forall (s :: S). Term s PBool
addIntegerDirectBudgetMatchesReferenceCek =
  plet (values [integer "01", integer "1880"]) $ \arguments ->
    pverifyDirectBuiltinV1
      # 0
      # (builtinRoot 0 arguments)
      # arguments
      # integer "1881"
      #&& ( pdirectBuiltinBudgetV1 # 0 # arguments
              #== pcon (PBuiltinBudgetV1 (pdata 101628) (pdata 3))
           )

emptyTraceMessageUsesReferenceZeroSizeQuirk :: forall (s :: S). Term s PBool
emptyTraceMessageUsesReferenceZeroSizeQuirk =
  plet (values [constant "9f02ff" "40", integer "01"]) $ \arguments ->
    (pbuiltinCostSizesV1 # 28 # arguments #== integerDataList [0, 1])
      #&& ( pdirectBuiltinBudgetV1 # 28 # arguments
              #== pcon (PBuiltinBudgetV1 (pdata 59498) (pdata 32))
           )

appendBytearrayDirectStepIsAuthenticated :: forall (s :: S). Term s PBool
appendBytearrayDirectStepIsAuthenticated =
  plet (values [bytes "42aabb", bytes "41cc"]) $ \arguments ->
    pverifyDirectBuiltinV1 # 10 # builtinRoot 10 arguments # arguments # bytes "43aabbcc"

ifThenElseSelectsOpaqueValue :: forall (s :: S). Term s PBool
ifThenElseSelectsOpaqueValue =
  plet (pcon $ POpaqueValue $ pdata $ pconstant $ BS.replicate 32 0x11) $ \selected ->
  plet (pcon $ POpaqueValue $ pdata $ pconstant $ BS.replicate 32 0x22) $ \rejected ->
  plet (values [constant "9f04ff" "d87a80", selected, rejected]) $ \arguments ->
    pverifyDirectBuiltinV1 # 26 # builtinRoot 26 arguments # arguments # selected

listHeadPreservesElementType :: forall (s :: S). Term s PBool
listHeadPreservesElementType =
  plet (values [constant "9f0500ff" "9f0102ff"]) $ \arguments ->
    pverifyDirectBuiltinV1 # 33 # builtinRoot 33 arguments # arguments # integer "01"

iDataRoundTrip :: forall (s :: S). Term s PBool
iDataRoundTrip =
  plet (values [integer "182a"]) $ \toDataArguments ->
  plet (constant "9f08ff" "182a") $ \dataValue ->
  plet (values [dataValue]) $ \fromDataArguments ->
    pverifyDirectBuiltinV1 # 40 # builtinRoot 40 toDataArguments # toDataArguments # dataValue
      #&& pverifyDirectBuiltinV1 # 45 # builtinRoot 45 fromDataArguments # fromDataArguments # integer "182a"

validEcdsaKeyWithInvalidSignatureReturnsFalse :: forall (s :: S). Term s PBool
validEcdsaKeyWithInvalidSignatureReturnsFalse =
  plet
    ( values
        [ bytes "58210279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
        , bytes "58200000000000000000000000000000000000000000000000000000000000000000"
        , bytes "584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        ]
    )
    $ \arguments ->
      pverifyDirectBuiltinV1
        # 52
        # builtinRoot 52 arguments
        # arguments
        # constant "9f04ff" "d87980"

millerLoopExpressionAndFinalVerifyAreAuthenticated :: forall (s :: S). Term s PBool
millerLoopExpressionAndFinalVerifyAreAuthenticated =
  plet blsLeafRoot $ \expressionRoot ->
  plet (values [constantValue g1Witness, constantValue g2Witness]) $ \millerArguments ->
  plet (values [blsValue expressionRoot, blsValue expressionRoot]) $ \finalArguments ->
    pverifyDirectBuiltinV1 # 68 # builtinRoot 68 millerArguments # millerArguments # blsValue expressionRoot
      #&& pverifyDirectBlsFinalV1 # builtinRoot 70 finalArguments # blsLeaf # blsLeaf # constant "9f04ff" "d87a80"

sixLeafMillerLoopProofFitsReserve :: forall (s :: S). Term s PBool
sixLeafMillerLoopProofFitsReserve =
  plet tripleExpressionRoot $ \root ->
  plet (values [blsValue root, blsValue root]) $ \arguments ->
    pverifyDirectBlsFinalV1 # builtinRoot 70 arguments # tripleExpression # tripleExpression # constant "9f04ff" "d87a80"

tenLeafMillerLoopProofFitsReserve :: forall (s :: S). Term s PBool
tenLeafMillerLoopProofFitsReserve =
  plet fiveExpressionRoot $ \root ->
  plet (values [blsValue root, blsValue root]) $ \arguments ->
    pverifyDirectBlsFinalV1 # builtinRoot 70 arguments # fiveExpression # fiveExpression # constant "9f04ff" "d87a80"

tenLeafRootBoundProofFitsReserve :: forall (s :: S). Term s PBool
tenLeafRootBoundProofFitsReserve =
  plet fiveExpressionRoot $ \root ->
  plet (values [blsValue root, blsValue root]) $ \arguments ->
    pverifyDirectBlsFinalRootsV1
      # builtinRoot 70 arguments # root # root # fiveExpression # fiveExpression
      # constant "9f04ff" "d87a80"

twelveLeafMillerLoopProofFailsClosed :: forall (s :: S). Term s PBool
twelveLeafMillerLoopProofFailsClosed =
  plet sixExpressionRoot $ \root ->
  plet (values [blsValue root, blsValue root]) $ \arguments ->
    pverifyDirectBlsFinalV1 # builtinRoot 70 arguments # sixExpression # sixExpression # constant "9f04ff" "d87a80"

unsupportedDirectBuiltinStaysClosed :: forall (s :: S). Term s PBool
unsupportedDirectBuiltinStaysClosed =
  pnot #$ pverifyDirectBuiltinV1 # 70 # builtinRoot 70 pnil # pnil # constant "9f04ff" "d87980"

wrongBuiltinResultFailsClosed :: forall (s :: S). Term s PBool
wrongBuiltinResultFailsClosed =
  plet (values [integer "1829", integer "01"]) $ \arguments ->
    pnot #$ pverifyDirectBuiltinV1 # 0 # builtinRoot 0 arguments # arguments # integer "182b"

builtinClosureTypeFailureIsAuthenticated :: forall (s :: S). Term s PBool
builtinClosureTypeFailureIsAuthenticated =
  plet
    ( runtimeValues
        [ pcon $ PRuntimeLambdaValue
            (pdata $ pconstant $ BS.replicate 32 0x11)
            (pdata pemptyEnvironmentRootV1)
        , runtimeConstant "9f00ff" "01"
        ]
    )
    $ \arguments ->
      pverifyBuiltinTypeFailureV1 # 0 # runtimeBuiltinRoot 0 arguments # arguments

mkConsRejectsIncongruentElementType :: forall (s :: S). Term s PBool
mkConsRejectsIncongruentElementType =
  plet
    (runtimeValues [runtimeConstant "9f01ff" "4101", runtimeConstant "9f0500ff" "9f01ff"])
    $ \arguments ->
      pverifyBuiltinTypeFailureV1 # 32 # runtimeBuiltinRoot 32 arguments # arguments

arbitraryControlBranchesAreNotTypeFailures :: forall (s :: S). Term s PBool
arbitraryControlBranchesAreNotTypeFailures =
  plet
    (pcon $ PRuntimeDelayValue (pdata $ pconstant $ BS.replicate 32 0x11) (pdata pemptyEnvironmentRootV1))
    $ \delay ->
  plet
    (pcon $ PRuntimeConstrValue (pdata 0) (pdata 0) (pdata pemptySequenceRootV1))
    $ \constr ->
  plet (runtimeValues [runtimeConstant "9f04ff" "d87a80", delay, constr]) $ \arguments ->
  plet (runtimeValues [runtimeConstant "9f00ff" "00", delay, constr]) $ \illTyped ->
  pmatch (pruntimeArgumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
    argumentsCount #== pbuiltinArgumentCountV1 # 26
      #&& runtimeBuiltinRoot 26 arguments #== phashBuiltinValueV1 # 26 # 0 # argumentsCount # argumentsRoot
      #&& (pnot #$ pverifyBuiltinTypeFailureV1 # 26 # runtimeBuiltinRoot 26 arguments # arguments)
      #&& pverifyBuiltinTypeFailureV1 # 26 # runtimeBuiltinRoot 26 illTyped # illTyped

divisionByZeroFailureIsFree :: forall (s :: S). Term s PBool
divisionByZeroFailureIsFree =
  plet (values [integer "01", integer "00"]) $ \arguments ->
    failureAuthenticated 3 arguments
      #&& pdirectBuiltinFailureBudgetV1 # 3 # arguments #== zeroBudget

quotientByZeroFailureIsPaid :: forall (s :: S). Term s PBool
quotientByZeroFailureIsPaid =
  plet (values [integer "01", integer "00"]) $ \arguments ->
    failureAuthenticated 4 arguments
      #&& pdirectBuiltinFailureBudgetV1 # 4 # arguments #== pdirectBuiltinBudgetV1 # 4 # arguments

invalidUtf8FailureIsAuthenticated :: forall (s :: S). Term s PBool
invalidUtf8FailureIsAuthenticated =
  plet (values [bytes "42c0af"]) $ \arguments ->
    (pnot #$ pisValidUtf8V1 # pconstant (hexOf "c0af"))
      #&& pisValidUtf8V1 # pconstant (hexOf "f09f9880")
      #&& failureAuthenticated 25 arguments

integerToBytesSizeFailureIsPaid :: forall (s :: S). Term s PBool
integerToBytesSizeFailureIsPaid =
  plet (values [constant "9f04ff" "d87a80", integer "01", integer "190100"]) $ \arguments ->
    failureAuthenticated 73 arguments
      #&& pdirectBuiltinFailureBudgetV1 # 73 # arguments
        #== pcon (PBuiltinBudgetV1 (pdata 1351512) (pdata 1))

malformedEcdsaFailureIsPaid :: forall (s :: S). Term s PBool
malformedEcdsaFailureIsPaid =
  paidSignatureFailure 52
    "58210479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

malformedSchnorrFailureIsPaid :: forall (s :: S). Term s PBool
malformedSchnorrFailureIsPaid =
  paidSignatureFailure 53
    "5820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

nonResidueEcdsaFailureIsAuthenticated :: forall (s :: S). Term s PBool
nonResidueEcdsaFailureIsAuthenticated =
  plet (signatureArguments "5821020000000000000000000000000000000000000000000000000000000000000000") $ \arguments ->
    failureAuthenticated 52 arguments

malformedBlsG1HeaderIsPaid :: forall (s :: S). Term s PBool
malformedBlsG1HeaderIsPaid =
  paidFailure 60 "5830000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

malformedBlsG2HeaderIsPaid :: forall (s :: S). Term s PBool
malformedBlsG2HeaderIsPaid =
  paidFailure 67 "5f58400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000ff"

offCurveBlsG1IsPaid :: forall (s :: S). Term s PBool
offCurveBlsG1IsPaid =
  paidFailure 60 "5830800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"

canonicalBlsInfinityIsNotFailure :: forall (s :: S). Term s PBool
canonicalBlsInfinityIsNotFailure =
  plet (values [bytes "5830c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"]) $ \arguments ->
    pnot #$ failureAuthenticated 60 arguments

semanticChooseDataInspectsLargeContextRoot :: forall (s :: S). Term s PBool
semanticChooseDataInspectsLargeContextRoot = withLargeSemanticConstructor $ \source node _ fieldsNode _ ->
  plet (integer "00") $ \selected ->
  plet (values [source, selected, integer "01", integer "02", integer "03", integer "04"]) $ \arguments ->
    pverifySemanticBuiltinV1
      # 36
      # builtinRoot 36 arguments
      # arguments
      # selected
      # semanticWitness [node] [fieldsNode]

semanticUnconstrDataReturnsExactLargeTypedPair :: forall (s :: S). Term s PBool
semanticUnconstrDataReturnsExactLargeTypedPair = withLargeSemanticConstructor $ \source node _ fieldsNode fields ->
  plet (pserialiseData # pforgetData (pdata (pconstant 0 :: Term s PInteger))) $ \constructorCbor ->
  plet (pintegerDataSummaryV1 # 0 # (pboundedBlobRootV1 # constructorCbor)) $ \constructor ->
  plet (plistDataSummaryV1 # fields) $ \fieldsSummary ->
  plet
    ( psmallConstrDataSummaryV1
        # 0
        # ( pprependDataListSummaryV1
              # constructor
              # (pprependDataListSummaryV1 # fieldsSummary # pemptyDataListSummaryV1)
          )
    )
    $ \resultPayload ->
      plet (summaryMemory constructor - 4 + sequenceMemory fields) $ \resultMemory ->
      plet (semanticValue "9f06000508ff" resultPayload resultMemory) $ \result ->
      plet (values [source]) $ \arguments ->
        pverifySemanticBuiltinV1
          # 42
          # builtinRoot 42 arguments
          # arguments
          # result
          # semanticWitness [node] [fieldsNode]

semanticFstPairExtractsConstructor :: forall (s :: S). Term s PBool
semanticFstPairExtractsConstructor = withLargeSemanticConstructor $ \_ _ _ _ fields ->
  plet (pserialiseData # pforgetData (pdata (pconstant 0 :: Term s PInteger))) $ \constructorCbor ->
  plet (pboundedBlobRootV1 # constructorCbor) $ \constructorRoot ->
  plet (pintegerDataSummaryV1 # 0 # constructorRoot) $ \constructor ->
  plet
    ( pcon $ PIntegerDataNode
        (pdata constructorRoot)
        (pdata $ summaryCborLength constructor)
        (pdata $ summaryMemory constructor)
    )
    $ \constructorNode ->
      plet (plistDataSummaryV1 # fields) $ \fieldsSummary ->
      plet
        ( pcon $ PListDataNode
            (pdata $ sequenceLength fields)
            (pdata $ sequenceRoot fields)
            (pdata $ summaryCborLength fieldsSummary)
            (pdata $ summaryMemory fieldsSummary)
        )
        $ \fieldsDataNode ->
          plet
            ( pcon $ PDataListNodeV1
                (pdata $ summaryRoot fieldsSummary)
                (pdata $ summaryCborLength fieldsSummary)
                (pdata $ summaryMemory fieldsSummary)
                (pdata pemptyDataListRootV1)
                (pdata 1)
                (pdata $ summaryCborLength fieldsSummary)
                (pdata $ summaryMemory fieldsSummary)
            )
            $ \secondLink ->
              plet
                ( pcon $ PDataListNodeV1
                    (pdata $ summaryRoot constructor)
                    (pdata $ summaryCborLength constructor)
                    (pdata $ summaryMemory constructor)
                    (pdata $ phashDataListNodeV1 # secondLink)
                    (pdata 2)
                    (pdata $ summaryCborLength constructor + summaryCborLength fieldsSummary)
                    (pdata $ summaryMemory constructor + summaryMemory fieldsSummary)
                )
                $ \firstLink ->
                  plet
                    ( psmallConstrDataSummaryV1
                        # 0
                        # ( pprependDataListSummaryV1
                              # constructor
                              # (pprependDataListSummaryV1 # fieldsSummary # pemptyDataListSummaryV1)
                          )
                    )
                    $ \payload ->
                      plet
                        ( pcon $ PConstrSmallData
                            (pdata 0)
                            (pdata 2)
                            (pdata $ phashDataListNodeV1 # firstLink)
                            (pdata $ summaryCborLength payload)
                            (pdata $ summaryMemory payload)
                        )
                        $ \payloadNode ->
                          plet (summaryMemory constructor - 4 + sequenceMemory fields) $ \sourceMemory ->
                          plet (semanticValue "9f06000508ff" payload sourceMemory) $ \source ->
                          plet (semanticValue "9f00ff" constructor (summaryMemory constructor - 4)) $ \result ->
                          plet (values [source]) $ \arguments ->
                            pverifySemanticBuiltinV1
                              # 29
                              # builtinRoot 29 arguments
                              # arguments
                              # result
                              # semanticWitness [payloadNode, constructorNode, fieldsDataNode] [firstLink, secondLink]

semanticHeadListExtractsLargeChild :: forall (s :: S). Term s PBool
semanticHeadListExtractsLargeChild = withLargeSemanticConstructor $ \_ _ childNode fieldsNode fields ->
  plet (plistDataSummaryV1 # fields) $ \listPayload ->
  plet
    ( pcon $ PListDataNode
        (pdata $ sequenceLength fields)
        (pdata $ sequenceRoot fields)
        (pdata $ summaryCborLength listPayload)
        (pdata $ summaryMemory listPayload)
    )
    $ \sourceNode ->
      plet
        ( pcon $ PDataSummaryV1
            (pdata $ phashDataNodeV1 # childNode)
            (pdata $ pdataNodeCborLengthV1 # childNode)
            (pdata $ pdataNodeMemoryV1 # childNode)
        )
        $ \child ->
          plet (semanticValue "9f0508ff" listPayload (sequenceMemory fields)) $ \source ->
          plet (semanticValue "9f08ff" child (summaryMemory child)) $ \result ->
          plet (values [source]) $ \arguments ->
            pverifySemanticBuiltinV1
              # 33
              # builtinRoot 33 arguments
              # arguments
              # result
              # semanticWitness [sourceNode, childNode] [fieldsNode]

semanticWrongDataVariantFailureIsAuthenticated :: forall (s :: S). Term s PBool
semanticWrongDataVariantFailureIsAuthenticated =
  plet pemptyDataPairSummaryV1 $ \entries ->
  plet (pmapDataSummaryV1 # entries) $ \summary ->
  plet
    ( pcon $ PMapDataNode
        (pdata 0)
        (pdata $ sequenceRoot entries)
        (pdata $ summaryCborLength summary)
        (pdata $ summaryMemory summary)
    )
    $ \node ->
      plet (semanticValue "9f08ff" summary (summaryMemory summary)) $ \source ->
      plet (values [source]) $ \arguments ->
        pverifySemanticBuiltinFailureV1
          # 42
          # builtinRoot 42 arguments
          # arguments
          # semanticWitness [node] []

withLargeSemanticConstructor ::
  forall (s :: S) (r :: S -> Type).
  ( Term s PValueWitnessV1 ->
    Term s PDataNodeV1 ->
    Term s PDataNodeV1 ->
    Term s PDataListNodeV1 ->
    Term s PDataSequenceSummaryV1 ->
    Term s r
  ) ->
  Term s r
withLargeSemanticConstructor k =
  plet (pconstant $ BS.replicate 9000 42) $ \largeBytes ->
  plet (pboundedBlobRootV1 # largeBytes) $ \bytesRoot ->
  plet (pbytesDataSummaryV1 # 9000 # bytesRoot) $ \child ->
  plet
    ( pcon $ PBytesDataNode
        (pdata bytesRoot)
        (pdata 9000)
        (pdata $ summaryCborLength child)
        (pdata $ summaryMemory child)
    )
    $ \childNode ->
      plet
        ( pcon $ PDataListNodeV1
            (pdata $ summaryRoot child)
            (pdata $ summaryCborLength child)
            (pdata $ summaryMemory child)
            (pdata pemptyDataListRootV1)
            (pdata 1)
            (pdata $ summaryCborLength child)
            (pdata $ summaryMemory child)
        )
        $ \fieldsNode ->
          plet (pprependDataListSummaryV1 # child # pemptyDataListSummaryV1) $ \fields ->
          plet (psmallConstrDataSummaryV1 # 0 # fields) $ \summary ->
          plet
            ( pcon $ PConstrSmallData
                (pdata 0)
                (pdata $ sequenceLength fields)
                (pdata $ sequenceRoot fields)
                (pdata $ summaryCborLength summary)
                (pdata $ summaryMemory summary)
            )
            $ \node ->
              k (semanticValue "9f08ff" summary (summaryMemory summary)) node childNode fieldsNode fields

semanticValue ::
  forall (s :: S).
  BS.ByteString -> Term s PDataSummaryV1 -> Term s PInteger -> Term s PValueWitnessV1
semanticValue typeCbor payload memory =
  pcon $ PSemanticConstantValue
    (pdata $ pconstant $ hexOf typeCbor)
    (pdata payload)
    (pdata memory)

semanticWitness ::
  forall (s :: S).
  [Term s PDataNodeV1] -> [Term s PDataListNodeV1] -> Term s PSemanticBuiltinWitnessV1
semanticWitness dataNodes listNodes =
  pcon $ PSemanticBuiltinWitnessV1
    (pdata $ dataList dataNodes)
    (pdata $ dataList listNodes)
    (pdata pnil)
    (pdata pnil)

dataList :: forall (s :: S) (a :: S -> Type). PIsData a => [Term s a] -> Term s (PBuiltinList (PAsData a))
dataList = foldr (\value rest -> pcons # pdata value # rest) pnil

summaryRoot :: forall (s :: S). Term s PDataSummaryV1 -> Term s PByteString
summaryRoot summary = pmatch summary $ \s -> pfromData $ psummary'root s

summaryCborLength :: forall (s :: S). Term s PDataSummaryV1 -> Term s PInteger
summaryCborLength summary = pmatch summary $ \s -> pfromData $ psummary'cborLength s

summaryMemory :: forall (s :: S). Term s PDataSummaryV1 -> Term s PInteger
summaryMemory summary = pmatch summary $ \s -> pfromData $ psummary'memory s

sequenceRoot :: forall (s :: S). Term s PDataSequenceSummaryV1 -> Term s PByteString
sequenceRoot summary = pmatch summary $ \s -> pfromData $ pseq'root s

sequenceLength :: forall (s :: S). Term s PDataSequenceSummaryV1 -> Term s PInteger
sequenceLength summary = pmatch summary $ \s -> pfromData $ pseq'length s

sequenceMemory :: forall (s :: S). Term s PDataSequenceSummaryV1 -> Term s PInteger
sequenceMemory summary = pmatch summary $ \s -> pfromData $ pseq'memory s

failureAuthenticated ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PBool
failureAuthenticated tag arguments =
  pverifyDirectBuiltinFailureV1 # tag # builtinRoot tag arguments # arguments

zeroBudget :: forall (s :: S). Term s PBuiltinBudgetV1
zeroBudget = pcon $ PBuiltinBudgetV1 (pdata 0) (pdata 0)

signatureArguments :: forall (s :: S). BS.ByteString -> Term s (PBuiltinList (PAsData PValueWitnessV1))
signatureArguments key = values
  [ bytes key
  , bytes "58200000000000000000000000000000000000000000000000000000000000000000"
  , bytes "584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  ]

paidSignatureFailure :: forall (s :: S). Term s PInteger -> BS.ByteString -> Term s PBool
paidSignatureFailure tag key =
  plet (signatureArguments key) $ \arguments ->
    failureAuthenticated tag arguments
      #&& pdirectBuiltinFailureBudgetV1 # tag # arguments #== pdirectBuiltinBudgetV1 # tag # arguments

paidFailure :: forall (s :: S). Term s PInteger -> BS.ByteString -> Term s PBool
paidFailure tag payload =
  plet (values [bytes payload]) $ \arguments ->
    failureAuthenticated tag arguments
      #&& pdirectBuiltinFailureBudgetV1 # tag # arguments #== pdirectBuiltinBudgetV1 # tag # arguments

constant :: forall (s :: S). BS.ByteString -> BS.ByteString -> Term s PValueWitnessV1
constant typeCbor payloadCbor =
  constantValue $ witness typeCbor payloadCbor

witness :: forall (s :: S). BS.ByteString -> BS.ByteString -> Term s PConstantWitnessV1
witness typeCbor payloadCbor =
  pcon $ PConstantWitnessV1
    (pdata (pconstant (hexOf typeCbor)))
    (pdata (pconstant (hexOf payloadCbor)))

constantValue :: forall (s :: S). Term s PConstantWitnessV1 -> Term s PValueWitnessV1
constantValue = pcon . PConstantValue . pdata

integer :: forall (s :: S). BS.ByteString -> Term s PValueWitnessV1
integer = constant "9f00ff"

bytes :: forall (s :: S). BS.ByteString -> Term s PValueWitnessV1
bytes = constant "9f01ff"

g1Witness, g2Witness :: forall (s :: S). Term s PConstantWitnessV1
g1Witness = witness "9f09ff" "583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
g2Witness = witness "9f0aff" "5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff"

blsLeaf :: forall (s :: S). Term s PBlsExpressionWitnessV1
blsLeaf = pcon $ PBlsMillerLoopExpression (pdata g1Witness) (pdata g2Witness)

blsMultiply :: forall (s :: S). Term s PBlsExpressionWitnessV1 -> Term s PBlsExpressionWitnessV1 -> Term s PBlsExpressionWitnessV1
blsMultiply left right = pcon $ PBlsMultiplyExpression (pdata left) (pdata right)

doubleExpression, tripleExpression, fiveExpression, sixExpression :: forall (s :: S). Term s PBlsExpressionWitnessV1
doubleExpression = blsMultiply blsLeaf blsLeaf
tripleExpression = blsMultiply doubleExpression blsLeaf
fiveExpression = blsMultiply tripleExpression doubleExpression
sixExpression = blsMultiply tripleExpression tripleExpression

blsLeafRoot :: forall (s :: S). Term s PByteString
blsLeafRoot = phashBlsMillerLoopExpressionV1 # (pconstantRootV1 # g1Witness) # (pconstantRootV1 # g2Witness)

doubleExpressionRoot, tripleExpressionRoot, fiveExpressionRoot, sixExpressionRoot ::
  forall (s :: S). Term s PByteString
doubleExpressionRoot = phashBlsMultiplyExpressionV1 # blsLeafRoot # blsLeafRoot
tripleExpressionRoot = phashBlsMultiplyExpressionV1 # doubleExpressionRoot # blsLeafRoot
fiveExpressionRoot = phashBlsMultiplyExpressionV1 # tripleExpressionRoot # doubleExpressionRoot
sixExpressionRoot = phashBlsMultiplyExpressionV1 # tripleExpressionRoot # tripleExpressionRoot

blsValue :: forall (s :: S). Term s PByteString -> Term s PValueWitnessV1
blsValue root = pcon $ PBlsMillerLoopValue $ pdata root

values :: forall (s :: S). [Term s PValueWitnessV1] -> Term s (PBuiltinList (PAsData PValueWitnessV1))
values = foldr (\value rest -> pcons # pdata value # rest) pnil

runtimeConstant :: forall (s :: S). BS.ByteString -> BS.ByteString -> Term s PRuntimeValueWitnessV1
runtimeConstant typeCbor payloadCbor =
  pcon $ PRuntimeConstantValue $ pdata $ witness typeCbor payloadCbor

runtimeValues :: forall (s :: S). [Term s PRuntimeValueWitnessV1] -> Term s (PBuiltinList (PAsData PRuntimeValueWitnessV1))
runtimeValues = foldr (\value rest -> pcons # pdata value # rest) pnil

integerDataList :: forall (s :: S). [Integer] -> Term s (PBuiltinList (PAsData PInteger))
integerDataList = foldr (\value rest -> pcons # pdata (pconstant value) # rest) pnil

builtinRoot ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PByteString
builtinRoot tag arguments =
  pmatch (pargumentsRootV1 # arguments) $ \(PPair root count) ->
    phashBuiltinValueV1 # tag # 0 # count # root

runtimeBuiltinRoot ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PRuntimeValueWitnessV1)) ->
  Term s PByteString
runtimeBuiltinRoot tag arguments =
  pmatch (pruntimeArgumentsRootV1 # arguments) $ \(PPair root count) ->
    phashBuiltinValueV1 # tag # 0 # count # root

hexOf :: BS.ByteString -> BS.ByteString
hexOf = Base16.decodeLenient
