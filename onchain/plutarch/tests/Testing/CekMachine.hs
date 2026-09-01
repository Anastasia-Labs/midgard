{-# LANGUAGE OverloadedStrings #-}

module Testing.CekMachine (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Aiken.Cbor (pdeserialise)
import Midgard.CekBuiltin qualified as Builtin
import Midgard.CekConstant (PConstantWitnessV1 (..), pconstantRootV1)
import Midgard.CekCost (PBuiltinBudgetV1 (..))
import Midgard.CekData qualified as Data
import Midgard.CekMachine
import Midgard.CekProof (
  pemptyContinuationRootV1,
  pemptyEnvironmentRootV1,
  phashApplicationTermV1,
 )
import Midgard.CekProof qualified as Proof
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Midgard.CekMachine"
  [ testCase "canonical_machine_mode_and_error_tags_match_typescript" $
      passertEvalNoTrace canonicalMachineModeAndErrorTagsMatchTypescript
  , testCase "offchain_core_step_data_abi_vector" $
      passertEvalNoTrace offchainCoreStepDataAbiVector
  , testCase "semantic_map_conversion_scans_one_pair_then_finishes" $
      passertEvalNoTrace semanticMapConversionScansOnePairThenFinishes
  , testCase "application_and_lambda_reduction_are_exact" $
      passertEvalNoTrace applicationAndLambdaReductionAreExact
  , testCase "debruijn_lookup_walks_an_authenticated_environment" $
      passertEvalNoTrace debruijnLookupWalksAuthenticatedEnvironment
  , testCase "builtin_force_and_application_enter_builtin_micro_machine" $
      passertEvalNoTrace builtinForceAndApplicationEnterBuiltinMachine
  , testCase "direct_builtin_executes_with_exact_semantics_and_budget" $
      passertEvalNoTrace directBuiltinExecutesWithExactSemanticsAndBudget
  , testCase "builtin_failure_halts_with_the_authenticated_failure_code" $
      passertEvalNoTrace builtinFailureHaltsWithAuthenticatedFailureCode
  , testCase "semantic_builtin_failure_halts_without_revealing_the_data_value" $
      passertEvalNoTrace semanticBuiltinFailureHaltsWithoutRevealingData
  , testCase "builtin_type_failure_halts_without_charging" $
      passertEvalNoTrace builtinTypeFailureHaltsWithoutCharging
  , testCase "paid_secp_failure_halts_with_the_exact_builtin_budget" $
      passertEvalNoTrace paidSecpFailureHaltsWithExactBuiltinBudget
  , testCase "bls_final_builtin_executes_from_authenticated_expressions" $
      passertEvalNoTrace blsFinalBuiltinExecutesFromAuthenticatedExpressions
  , testCase "ten_leaf_bls_final_transition_fits_the_l1_execution_reserve" $
      passertEvalNoTrace tenLeafBlsFinalTransitionFitsReserve
  , testCase "nonconstant_empty_stack_halts_as_error" $
      passertEvalNoTrace nonconstantEmptyStackHaltsAsError
  , testCase "wrong_successor_budget_fails_closed" $
      passertEvalNoTrace wrongSuccessorBudgetFailsClosed
  , testCase "source_and_runtime_context_constants_use_distinct_exact_steps" $
      passertEvalNoTrace sourceAndRuntimeContextConstantsUseDistinctSteps
  , testCase "constructor_fields_stream_into_a_reversed_authenticated_value_list" $
      passertEvalNoTrace constructorFieldsStreamIntoReversedValueList
  , testCase "case_selection_and_constructor_argument_application_are_streamed" $
      passertEvalNoTrace caseSelectionAndConstructorArgumentsAreStreamed
  , testCase "machine_and_builtin_value_witness_decoders_stay_distinct" $
      passertEvalNoTrace machineAndBuiltinValueWitnessDecodersStayDistinct
  , testCase "machine_value_witness_accepts_a_well_formed_delay_value" $
      passertEvalNoTrace machineValueWitnessAcceptsDelay
  , testCase "machine_value_witness_accepts_a_well_formed_constr_value" $
      passertEvalNoTrace machineValueWitnessAcceptsConstr
  , testCase "machine_value_witness_rejects_a_one_field_delay_value" $
      pfails machineValueWitnessRejectsOneFieldDelay
  , testCase "machine_value_witness_rejects_a_one_field_constr_value" $
      pfails machineValueWitnessRejectsOneFieldConstr
  ]

canonicalMachineModeAndErrorTagsMatchTypescript :: forall s. Term s PBool
canonicalMachineModeAndErrorTagsMatchTypescript =
  integerList
    [ pmodeCompute, pmodeReturn, pmodeLookup, pmodeBuiltin, pmodeHaltSuccess
    , pmodeHaltError, pmodeCaseSelect, pmodeCaseApply, pmodeSemanticBuiltin
    ]
    #== integerList [0, 1, 2, 3, 4, 5, 6, 7, 8]
  #&& integerList
    [ perrorExplicit, perrorUnboundVariable, perrorInvalidApplication, perrorInvalidForce
    , perrorNonconstantHalt, perrorInvalidCaseScrutinee, perrorCaseBranchMissing, perrorBuiltinFailure
    ]
    #== integerList [0, 1, 2, 3, 4, 5, 6, 7]

offchainCoreStepDataAbiVector :: forall s. Term s PBool
offchainCoreStepDataAbiVector =
  plet
    ( pcon $ PMachineStateV1
        (pdata pmodeCompute)
        (pdata 2)
        (pdata $ phashApplicationTermV1 # hash 1 # hash 2)
        (pdata pemptyEnvironmentRootV1)
        (pdata pemptyContinuationRootV1)
        (pdata 0)
        (pdata 10)
        (pdata 11)
    )
    $ \pre ->
      plet
        ( pcon $ PMachineStateV1
            (pdata pmodeCompute)
            (pdata 2)
            (pdata $ hash 1)
            (pdata pemptyEnvironmentRootV1)
            (pdata $ hash 3)
            (pdata 0)
            (pdata 16010)
            (pdata 111)
        )
        $ \post ->
          plet
            (pcon $ PCoreStepEvidenceV1
              (pdata pre)
              (pdata post)
              (pdata $ pcon $ PComputeApplication (pdata $ hash 1) (pdata $ hash 2)))
            $ \evidence ->
              pserialiseData # pforgetData (pdata evidence)
                #== pconstant (hex "d8799fd8799f000258202a37aa5b923cf90c6f3c8849e8fe2b28adcda97ccd736af6bf35b8312035f43158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b000a0bffd8799f00025820010101010101010101010101010101010101010101010101010101010101010158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a75820030303030303030303030303030303030303030303030303030303030303030300193e8a186fffd87d9f5820010101010101010101010101010101010101010101010101010101010101010158200202020202020202020202020202020202020202020202020202020202020202ffff")

semanticMapConversionScansOnePairThenFinishes :: forall s. Term s PBool
semanticMapConversionScansOnePairThenFinishes =
  plet (pserialiseData # pforgetData (pdata (pconstant 1 :: Term s PInteger))) $ \keyCbor ->
  plet (Data.pintegerDataSummaryV1 # 1 # (Proof.phashBlobChunkV1 # keyCbor)) $ \keySummary ->
  plet
    (pcon $ Data.PIntegerDataNode
      (pdata $ Proof.phashBlobChunkV1 # keyCbor)
      (pdata $ summaryCborLength keySummary)
      (pdata $ summaryMemory keySummary))
    $ \keyNode ->
  plet (Data.pbytesDataSummaryV1 # 1 # (Proof.phashBlobChunkV1 # pconstant "\xaa")) $ \valueSummary ->
  plet
    (pcon $ Data.PBytesDataNode
      (pdata $ Proof.phashBlobChunkV1 # pconstant "\xaa")
      (pdata 1)
      (pdata $ summaryCborLength valueSummary)
      (pdata $ summaryMemory valueSummary))
    $ \valueNode ->
  plet
    (pcon $ Data.PDataListNodeV1
      (pdata $ summaryRoot valueSummary) (pdata $ summaryCborLength valueSummary) (pdata $ summaryMemory valueSummary)
      (pdata Data.pemptyDataListRootV1) (pdata 1)
      (pdata $ summaryCborLength valueSummary) (pdata $ summaryMemory valueSummary))
    $ \second ->
  plet
    (pcon $ Data.PDataListNodeV1
      (pdata $ summaryRoot keySummary) (pdata $ summaryCborLength keySummary) (pdata $ summaryMemory keySummary)
      (pdata $ Data.phashDataListNodeV1 # second) (pdata 2)
      (pdata $ summaryCborLength keySummary + summaryCborLength valueSummary)
      (pdata $ summaryMemory keySummary + summaryMemory valueSummary))
    $ \first ->
  plet (Data.pprependDataListSummaryV1 # keySummary
    # (Data.pprependDataListSummaryV1 # valueSummary # Data.pemptyDataListSummaryV1)) $ \pairSequence ->
  plet (Data.psmallConstrDataSummaryV1 # 0 # pairSequence) $ \pairSummary ->
  plet
    (pcon $ Data.PConstrSmallData (pdata 0) (pdata 2) (pdata $ sequenceRoot pairSequence)
      (pdata $ summaryCborLength pairSummary) (pdata $ summaryMemory pairSummary))
    $ \pairNode ->
  plet
    (pcon $ Data.PDataListNodeV1
      (pdata $ summaryRoot pairSummary) (pdata $ summaryCborLength pairSummary) (pdata $ summaryMemory pairSummary)
      (pdata Data.pemptyDataListRootV1) (pdata 1)
      (pdata $ summaryCborLength pairSummary) (pdata $ summaryMemory pairSummary))
    $ \source ->
  plet
    (pcon $ Data.PDataPairNodeV1
      (pdata $ summaryRoot keySummary) (pdata $ summaryCborLength keySummary) (pdata $ summaryMemory keySummary)
      (pdata $ summaryRoot valueSummary) (pdata $ summaryCborLength valueSummary) (pdata $ summaryMemory valueSummary)
      (pdata Data.pemptyDataPairRootV1) (pdata 1)
      (pdata $ summaryCborLength keySummary + summaryCborLength valueSummary)
      (pdata $ summaryMemory keySummary + summaryMemory valueSummary))
    $ \destination ->
  plet
    (pcon $ PMapConversionControlV1
      (pdata 38) (pdata $ hash 3) (pdata $ Data.phashDataListNodeV1 # source) (pdata 1)
      (pdata $ summaryCborLength pairSummary) (pdata $ summaryMemory pairSummary)
      (pdata $ Data.phashDataPairNodeV1 # destination) (pdata 1)
      (pdata $ summaryCborLength keySummary + summaryCborLength valueSummary)
      (pdata $ summaryMemory keySummary + summaryMemory valueSummary)
      (pdata 111) (pdata 222))
    $ \control ->
  plet
    (pcon $ PMapConversionControlV1
      (pdata 38) (pdata $ hash 3) (pdata Data.pemptyDataListRootV1) (pdata 0) (pdata 0) (pdata 0)
      (pdata Data.pemptyDataPairRootV1) (pdata 0) (pdata 0) (pdata 0) (pdata 111) (pdata 222))
    $ \completed ->
  plet (machineState pmodeSemanticBuiltin (phashMapConversionControlV1 # control)
    Proof.pemptyEnvironmentRootV1 (hash 2) 0 10 20) $ \pre ->
  plet (machineState pmodeSemanticBuiltin (phashMapConversionControlV1 # completed)
    Proof.pemptyEnvironmentRootV1 (hash 2) 0 10 20) $ \scanned ->
  plet (machineState pmodeReturn (hash 3) Proof.pemptyEnvironmentRootV1 (hash 2) 0 121 242) $ \finished ->
    pverifySemanticBuiltinControlStep # pre # scanned
      # (pcon $ PStepBuiltinListToMap (pdata control) (pdata source) (pdata pairNode)
          (pdata first) (pdata second) (pdata keyNode) (pdata valueNode) (pdata destination))
      #&& pverifySemanticBuiltinControlStep # scanned # finished
        # (pcon $ PFinishBuiltinMapConversion $ pdata completed)

applicationAndLambdaReductionAreExact :: forall s. Term s PBool
applicationAndLambdaReductionAreExact =
  plet (Proof.phashLambdaTermV1 # hash 1) $ \lambdaTerm ->
  plet (Proof.phashConstantTermV1 # hash 2) $ \constantTerm ->
  plet (Proof.phashApplicationTermV1 # lambdaTerm # constantTerm) $ \application ->
  plet (machineState pmodeCompute application Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 100 100) $ \pre ->
  plet (Proof.phashApplyArgumentContinuationV1 # constantTerm # Proof.pemptyEnvironmentRootV1 # Proof.pemptyContinuationRootV1) $ \applyContinuation ->
  plet (machineState pmodeCompute lambdaTerm Proof.pemptyEnvironmentRootV1 applyContinuation 0 16100 200) $ \afterApplication ->
  plet (Proof.phashLambdaValueV1 # hash 1 # Proof.pemptyEnvironmentRootV1) $ \lambdaValue ->
  plet (machineState pmodeReturn lambdaValue Proof.pemptyEnvironmentRootV1 applyContinuation 0 32100 300) $ \afterLambda ->
  plet (Proof.phashApplyFunctionContinuationV1 # lambdaValue # Proof.pemptyContinuationRootV1) $ \functionContinuation ->
  plet (machineState pmodeCompute constantTerm Proof.pemptyEnvironmentRootV1 functionContinuation 0 32100 300) $ \beforeArgument ->
    pverifyComputeStep # pre # afterApplication
      # (pcon $ PComputeApplication (pdata lambdaTerm) (pdata constantTerm))
    #&& pverifyComputeStep # afterApplication # afterLambda # (pcon $ PComputeLambda $ pdata $ hash 1)
    #&& pverifyReturnStep # afterLambda # beforeArgument
      # (pcon $ PReturnApplyArgument (pdata constantTerm) (pdata Proof.pemptyEnvironmentRootV1) (pdata Proof.pemptyContinuationRootV1))

debruijnLookupWalksAuthenticatedEnvironment :: forall s. Term s PBool
debruijnLookupWalksAuthenticatedEnvironment =
  plet (Proof.phashEnvironmentNodeV1 # hash 1 # Proof.pemptyEnvironmentRootV1 # 1) $ \tailRoot ->
  plet (Proof.phashEnvironmentNodeV1 # hash 2 # tailRoot # 2) $ \environment ->
  plet (machineState pmodeCompute (Proof.phashVariableTermV1 # 1) environment Proof.pemptyContinuationRootV1 0 100 100) $ \pre ->
  plet (machineState pmodeLookup environment environment Proof.pemptyContinuationRootV1 1 16100 200) $ \lookup ->
  plet (machineState pmodeLookup tailRoot tailRoot Proof.pemptyContinuationRootV1 0 16100 200) $ \lookupTail ->
  plet (machineState pmodeReturn (hash 1) Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16100 200) $ \found ->
    pverifyComputeStep # pre # lookup # (pcon $ PComputeVariable $ pdata 1)
      #&& pverifyLookupStep # lookup # lookupTail
        # (pcon $ PLookupEnvironment (pdata $ hash 2) (pdata tailRoot) (pdata 2))
      #&& pverifyLookupStep # lookupTail # found
        # (pcon $ PLookupEnvironment (pdata $ hash 1) (pdata Proof.pemptyEnvironmentRootV1) (pdata 1))

builtinForceAndApplicationEnterBuiltinMachine :: forall s. Term s PBool
builtinForceAndApplicationEnterBuiltinMachine =
  plet (Proof.phashBuiltinValueV1 # 26 # 1 # 0 # Proof.pemptySequenceRootV1) $ \initialValue ->
  plet (Proof.phashForceContinuationV1 # Proof.pemptyContinuationRootV1) $ \forceContinuation ->
  plet (machineState pmodeReturn initialValue Proof.pemptyEnvironmentRootV1 forceContinuation 0 16100 200) $ \preForce ->
  plet (Proof.phashBuiltinValueV1 # 26 # 0 # 0 # Proof.pemptySequenceRootV1) $ \forcedValue ->
  plet (machineState pmodeReturn forcedValue Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16100 200) $ \postForce ->
  plet (Proof.phashBuiltinValueV1 # 18 # 0 # 0 # Proof.pemptySequenceRootV1) $ \functionValue ->
  plet (Proof.phashApplyFunctionContinuationV1 # functionValue # Proof.pemptyContinuationRootV1) $ \functionContinuation ->
  plet (machineState pmodeReturn (hash 3) Proof.pemptyEnvironmentRootV1 functionContinuation 0 16100 200) $ \preApply ->
  plet (Proof.phashSequenceNodeV1 # hash 3 # Proof.pemptySequenceRootV1 # 1) $ \argumentsRoot ->
  plet (machineState pmodeBuiltin (Proof.phashBuiltinValueV1 # 18 # 0 # 1 # argumentsRoot)
    Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16100 200) $ \postApply ->
    pverifyReturnStep # preForce # postForce
      # (pcon $ PReturnForceBuiltin (pdata 26) (pdata 1) (pdata 0)
          (pdata Proof.pemptySequenceRootV1) (pdata Proof.pemptyContinuationRootV1))
      #&& pverifyReturnStep # preApply # postApply
        # (pcon $ PReturnApplyBuiltin (pdata 18) (pdata 0) (pdata 0)
            (pdata Proof.pemptySequenceRootV1) (pdata Proof.pemptyContinuationRootV1))

directBuiltinExecutesWithExactSemanticsAndBudget :: forall s. Term s PBool
directBuiltinExecutesWithExactSemanticsAndBudget =
  plet (valueList [integer "01", integer "1880"]) $ \arguments ->
  plet (integer "1881") $ \result ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (Proof.phashBuiltinValueV1 # 0 # 0 # argumentsCount # argumentsRoot) $ \builtinRoot ->
  plet (machineState pmodeBuiltin builtinRoot Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 100 100) $ \pre ->
  plet (machineState pmodeReturn (Builtin.presultRootV1 # result) Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 101728 103) $ \post ->
  plet (machineState pmodeReturn (Builtin.presultRootV1 # result) Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 101727 103) $ \wrong ->
    pverifyBuiltinDirectStep pre post 0 arguments result
      #&& (pnot #$ pverifyBuiltinDirectStep pre wrong 0 arguments result)

builtinFailureHaltsWithAuthenticatedFailureCode :: forall s. Term s PBool
builtinFailureHaltsWithAuthenticatedFailureCode =
  plet (valueList [integer "01", integer "00"]) $ \arguments ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (machineState pmodeBuiltin (Proof.phashBuiltinValueV1 # 3 # 0 # argumentsCount # argumentsRoot)
    Proof.pemptyEnvironmentRootV1 (hash 1) 0 100 100) $ \pre ->
  plet (machineState pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 perrorBuiltinFailure 100 100) $ \post ->
    pverifyBuiltinFailureStep pre post 3 arguments

semanticBuiltinFailureHaltsWithoutRevealingData :: forall s. Term s PBool
semanticBuiltinFailureHaltsWithoutRevealingData =
  plet Data.pemptyDataPairSummaryV1 $ \entries ->
  plet (Data.pmapDataSummaryV1 # entries) $ \summary ->
  plet
    (pcon $ Data.PMapDataNode (pdata 0) (pdata $ sequenceRoot entries)
      (pdata $ summaryCborLength summary) (pdata $ summaryMemory summary))
    $ \node ->
  plet
    (pcon $ Builtin.PSemanticConstantValue (pdata $ pconstant $ hex "9f08ff")
      (pdata summary) (pdata $ summaryMemory summary))
    $ \source ->
  plet (valueList [source]) $ \arguments ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (machineState pmodeBuiltin (Proof.phashBuiltinValueV1 # 42 # 0 # argumentsCount # argumentsRoot)
    Proof.pemptyEnvironmentRootV1 (hash 1) 0 100 100) $ \pre ->
  plet (machineState pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 perrorBuiltinFailure 100 100) $ \post ->
  plet
    (pcon $ Builtin.PSemanticBuiltinWitnessV1
      (pdata $ pcons # pdata node # pnil) (pdata pnil) (pdata pnil) (pdata pnil))
    $ \material ->
      pverifyBuiltinSemanticFailureStep pre post 42 arguments material

builtinTypeFailureHaltsWithoutCharging :: forall s. Term s PBool
builtinTypeFailureHaltsWithoutCharging =
  plet
    ( runtimeValueList
        [ pcon $ Builtin.PRuntimeLambdaValue (pdata $ hash 2) (pdata Proof.pemptyEnvironmentRootV1)
        , pcon $ Builtin.PRuntimeConstantValue $ pdata $ constantWitness "9f00ff" "01"
        ]
    )
    $ \arguments ->
      pmatch (Builtin.pruntimeArgumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
      plet (machineState pmodeBuiltin (Proof.phashBuiltinValueV1 # 0 # 0 # argumentsCount # argumentsRoot)
        Proof.pemptyEnvironmentRootV1 (hash 1) 0 100 100) $ \pre ->
      plet (machineState pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
        Proof.pemptyContinuationRootV1 perrorBuiltinFailure 100 100) $ \post ->
        pverifyBuiltinTypeFailureStep pre post 0 arguments

paidSecpFailureHaltsWithExactBuiltinBudget :: forall s. Term s PBool
paidSecpFailureHaltsWithExactBuiltinBudget =
  plet
    ( valueList
        [ bytes "5821020000000000000000000000000000000000000000000000000000000000000000"
        , bytes "58200000000000000000000000000000000000000000000000000000000000000000"
        , bytes "584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        ]
    )
    $ \arguments ->
      pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
      pmatch (Builtin.pdirectBuiltinBudgetV1 # 52 # arguments) $ \budget ->
      plet (machineState pmodeBuiltin (Proof.phashBuiltinValueV1 # 52 # 0 # argumentsCount # argumentsRoot)
        Proof.pemptyEnvironmentRootV1 (hash 1) 0 100 100) $ \pre ->
      plet (machineState pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
        Proof.pemptyContinuationRootV1 perrorBuiltinFailure
        (100 + pfromData (pbudget'cpu budget)) (100 + pfromData (pbudget'memory budget))) $ \post ->
        pverifyBuiltinFailureStep pre post 52 arguments

blsFinalBuiltinExecutesFromAuthenticatedExpressions :: forall s. Term s PBool
blsFinalBuiltinExecutesFromAuthenticatedExpressions =
  plet blsLeafRoot $ \expressionRoot ->
  plet (valueList [blsValue expressionRoot, blsValue expressionRoot]) $ \arguments ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (machineState pmodeBuiltin (Proof.phashBuiltinValueV1 # 70 # 0 # argumentsCount # argumentsRoot)
    Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 100 100) $ \pre ->
  plet (boolean "d87a80") $ \result ->
  plet (machineState pmodeReturn (Builtin.presultRootV1 # result) Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 0 333849814 101) $ \post ->
    pverifyBuiltinBlsFinalStep pre post expressionRoot expressionRoot blsLeaf blsLeaf result

tenLeafBlsFinalTransitionFitsReserve :: forall s. Term s PBool
tenLeafBlsFinalTransitionFitsReserve =
  plet fiveExpressionRoot $ \expressionRoot ->
  plet (valueList [blsValue expressionRoot, blsValue expressionRoot]) $ \arguments ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (machineState pmodeBuiltin (Proof.phashBuiltinValueV1 # 70 # 0 # argumentsCount # argumentsRoot)
    Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 100 100) $ \pre ->
  plet (boolean "d87a80") $ \result ->
  plet (machineState pmodeReturn (Builtin.presultRootV1 # result) Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 0 333849814 101) $ \post ->
    pverifyBuiltinBlsFinalStep pre post expressionRoot expressionRoot fiveExpression fiveExpression result

nonconstantEmptyStackHaltsAsError :: forall s. Term s PBool
nonconstantEmptyStackHaltsAsError =
  plet (Proof.phashLambdaValueV1 # hash 1 # Proof.pemptyEnvironmentRootV1) $ \lambdaValue ->
  plet (machineState pmodeReturn lambdaValue Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16100 200) $ \pre ->
  plet (machineState pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 perrorNonconstantHalt 16100 200) $ \post ->
    pverifyReturnStep # pre # post # (pcon $ PReturnEmptyContinuation $ pdata $
      pcon $ PMachineLambdaValue (pdata $ hash 1) (pdata Proof.pemptyEnvironmentRootV1))

wrongSuccessorBudgetFailsClosed :: forall s. Term s PBool
wrongSuccessorBudgetFailsClosed =
  plet (constantWitness "9f00ff" "01") $ \value ->
  plet (pconstantRootV1 # value) $ \valueRoot ->
  plet (machineState pmodeCompute (Proof.phashConstantTermV1 # valueRoot) Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 0 100 100) $ \pre ->
  plet (machineState pmodeReturn valueRoot Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16099 200) $ \wrong ->
    pnot #$ pverifyComputeStep # pre # wrong # (pcon $ PComputeConstant $ pdata value)

sourceAndRuntimeContextConstantsUseDistinctSteps :: forall s. Term s PBool
sourceAndRuntimeContextConstantsUseDistinctSteps =
  plet (constantWitness "9f00ff" "01") $ \sourceValue ->
  plet (pconstantRootV1 # sourceValue) $ \sourceRoot ->
  plet (machineState pmodeCompute (Proof.phashConstantTermV1 # sourceRoot) Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 0 0 0) $ \sourcePre ->
  plet (machineState pmodeReturn sourceRoot Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16000 100) $ \sourcePost ->
  plet (machineState pmodeCompute (Proof.phashContextConstantTermV1 # hash 1) Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 0 0 0) $ \contextPre ->
  plet (machineState pmodeReturn (hash 1) Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16000 100) $ \contextPost ->
    pverifyComputeStep # sourcePre # sourcePost # (pcon $ PComputeConstant $ pdata sourceValue)
      #&& pverifyComputeStep # contextPre # contextPost # (pcon $ PComputeContextConstant $ pdata $ hash 1)
      #&& (pnot #$ pverifyComputeStep # sourcePre # sourcePost # (pcon $ PComputeContextConstant $ pdata sourceRoot))

constructorFieldsStreamIntoReversedValueList :: forall s. Term s PBool
constructorFieldsStreamIntoReversedValueList =
  plet (Proof.phashConstantTermV1 # hash 1) $ \term1 ->
  plet (Proof.phashConstantTermV1 # hash 2) $ \term2 ->
  plet (Proof.phashSequenceNodeV1 # term2 # Proof.pemptySequenceRootV1 # 1) $ \termTail ->
  plet (Proof.phashSequenceNodeV1 # term1 # termTail # 2) $ \termsRoot ->
  plet (machineState pmodeCompute (Proof.phashConstrTermV1 # 7 # 2 # termsRoot)
    Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 100 100) $ \pre ->
  plet (Proof.phashConstrContinuationV1 # 7 # 1 # termTail # 0 # Proof.pemptySequenceRootV1
    # Proof.pemptyEnvironmentRootV1 # Proof.pemptyContinuationRootV1) $ \firstFrame ->
  plet (machineState pmodeCompute term1 Proof.pemptyEnvironmentRootV1 firstFrame 0 16100 200) $ \computingFirst ->
  plet (Proof.phashSequenceNodeV1 # hash 1 # Proof.pemptySequenceRootV1 # 1) $ \firstValues ->
  plet (Proof.phashConstrContinuationV1 # 7 # 0 # Proof.pemptySequenceRootV1 # 1 # firstValues
    # Proof.pemptyEnvironmentRootV1 # Proof.pemptyContinuationRootV1) $ \finalFrame ->
  plet (machineState pmodeReturn (hash 1) Proof.pemptyEnvironmentRootV1 firstFrame 0 16100 200) $ \returnedFirst ->
  plet (machineState pmodeCompute term2 Proof.pemptyEnvironmentRootV1 finalFrame 0 16100 200) $ \computingSecond ->
  plet (machineState pmodeReturn (hash 2) Proof.pemptyEnvironmentRootV1 finalFrame 0 16100 200) $ \returnedSecond ->
  plet (Proof.phashSequenceNodeV1 # hash 2 # firstValues # 2) $ \finalValues ->
  plet (machineState pmodeReturn (Proof.phashConstrValueV1 # 7 # 2 # finalValues)
    Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 0 16100 200) $ \returnedConstr ->
    pverifyComputeStep # pre # computingFirst
      # (pcon $ PComputeConstrNonEmpty (pdata 7) (pdata 2) (pdata term1) (pdata termTail))
    #&& pverifyReturnStep # returnedFirst # computingSecond
      # (pcon $ PReturnConstrNext (pdata 7) (pdata 1) (pdata term2) (pdata Proof.pemptySequenceRootV1)
          (pdata 0) (pdata Proof.pemptySequenceRootV1) (pdata Proof.pemptyEnvironmentRootV1)
          (pdata Proof.pemptyContinuationRootV1))
    #&& pverifyReturnStep # returnedSecond # returnedConstr
      # (pcon $ PReturnConstrDone (pdata 7) (pdata 1) (pdata firstValues)
          (pdata Proof.pemptyEnvironmentRootV1) (pdata Proof.pemptyContinuationRootV1))

caseSelectionAndConstructorArgumentsAreStreamed :: forall s. Term s PBool
caseSelectionAndConstructorArgumentsAreStreamed =
  plet (Proof.phashSequenceNodeV1 # hash 2 # Proof.pemptySequenceRootV1 # 1) $ \branchTail ->
  plet (Proof.phashSequenceNodeV1 # hash 1 # branchTail # 2) $ \branchesRoot ->
  plet (Proof.phashSequenceNodeV1 # hash 1 # Proof.pemptySequenceRootV1 # 1) $ \valueTail ->
  plet (Proof.phashSequenceNodeV1 # hash 2 # valueTail # 2) $ \valuesRoot ->
  plet (Proof.phashCaseContinuationV1 # 2 # branchesRoot # Proof.pemptyEnvironmentRootV1
    # Proof.pemptyContinuationRootV1) $ \caseFrame ->
  plet (machineState pmodeReturn (Proof.phashConstrValueV1 # 1 # 2 # valuesRoot)
    Proof.pemptyEnvironmentRootV1 caseFrame 0 16100 200) $ \pre ->
  plet (Proof.phashCaseSelectContinuationV1 # Proof.pemptyEnvironmentRootV1 # Proof.pemptyContinuationRootV1 # 2) $ \selectWork ->
  plet (machineState pmodeCaseSelect branchesRoot valuesRoot selectWork 1 16100 200) $ \selectingFirst ->
  plet (machineState pmodeCaseSelect branchTail valuesRoot selectWork 0 16100 200) $ \selectingSecond ->
  plet (Proof.phashCaseApplyContinuationV1 # Proof.pemptyEnvironmentRootV1 # Proof.pemptyContinuationRootV1) $ \caseApplyWork ->
  plet (machineState pmodeCaseApply valuesRoot (hash 2) caseApplyWork 2 16100 200) $ \applyingValues ->
  plet (Proof.phashApplyValueContinuationV1 # hash 2 # Proof.pemptyContinuationRootV1) $ \appliedLastValue ->
  plet (machineState pmodeCaseApply valueTail (hash 2)
    (Proof.phashCaseApplyContinuationV1 # Proof.pemptyEnvironmentRootV1 # appliedLastValue) 1 16100 200) $ \applyingFirstValue ->
  plet (Proof.phashApplyValueContinuationV1 # hash 1 # appliedLastValue) $ \finalContinuation ->
  plet (machineState pmodeCompute (hash 2) Proof.pemptyEnvironmentRootV1 finalContinuation 0 16100 200) $ \computingBranch ->
    pverifyReturnStep # pre # selectingFirst
      # (pcon $ PReturnCaseConstr (pdata 1) (pdata 2) (pdata valuesRoot) (pdata 2) (pdata branchesRoot)
          (pdata Proof.pemptyEnvironmentRootV1) (pdata Proof.pemptyContinuationRootV1))
    #&& pverifyCaseSelectStep # selectingFirst # selectingSecond
      # (pcon $ PSelectCaseBranch (pdata $ hash 1) (pdata branchTail) (pdata 2)
          (pdata Proof.pemptyEnvironmentRootV1) (pdata Proof.pemptyContinuationRootV1) (pdata 2))
    #&& pverifyCaseSelectStep # selectingSecond # applyingValues
      # (pcon $ PSelectCaseBranch (pdata $ hash 2) (pdata Proof.pemptySequenceRootV1) (pdata 1)
          (pdata Proof.pemptyEnvironmentRootV1) (pdata Proof.pemptyContinuationRootV1) (pdata 2))
    #&& pverifyCaseApplyStep # applyingValues # applyingFirstValue
      # (pcon $ PApplyCaseValue (pdata $ hash 2) (pdata valueTail) (pdata 2)
          (pdata Proof.pemptyEnvironmentRootV1) (pdata Proof.pemptyContinuationRootV1))
    #&& pverifyCaseApplyStep # applyingFirstValue # computingBranch
      # (pcon $ PApplyCaseValue (pdata $ hash 1) (pdata Proof.pemptySequenceRootV1) (pdata 1)
          (pdata Proof.pemptyEnvironmentRootV1) (pdata appliedLastValue))

machineAndBuiltinValueWitnessDecodersStayDistinct :: forall s. Term s PBool
machineAndBuiltinValueWitnessDecodersStayDistinct =
  plet (decodeMachineValue "d87b9f41004111ff") $ \machineValue ->
  plet (decodeBuiltinValue "d87b9f4100ff") $ \builtinValue ->
    machineValue #== pcon (PMachineDelayValue (pdata $ pconstant "\x00") (pdata $ pconstant "\x11"))
      #&& builtinValue #== pcon (Builtin.POpaqueValue $ pdata $ pconstant "\x00")

machineValueWitnessAcceptsDelay :: forall s. Term s PBool
machineValueWitnessAcceptsDelay =
  decodeMachineValue "d87b9f41004111ff"
    #== pcon (PMachineDelayValue (pdata $ pconstant "\x00") (pdata $ pconstant "\x11"))

machineValueWitnessAcceptsConstr :: forall s. Term s PBool
machineValueWitnessAcceptsConstr =
  decodeMachineValue "d87c9f010241aaff"
    #== pcon (PMachineConstrValue (pdata 1) (pdata 2) (pdata $ pconstant "\xaa"))

machineValueWitnessRejectsOneFieldDelay :: forall s. Term s PBool
machineValueWitnessRejectsOneFieldDelay =
  plet (decodeMachineValue "d87b9f4100ff") $ \decoded -> decoded #== decoded

machineValueWitnessRejectsOneFieldConstr :: forall s. Term s PBool
machineValueWitnessRejectsOneFieldConstr =
  plet (decodeMachineValue "d87c9f4100ff") $ \decoded -> decoded #== decoded

decodeMachineValue :: forall s. BS.ByteString -> Term s PMachineValueWitnessV1
decodeMachineValue cbor = pmatch (pdeserialise # pconstant (hex cbor)) $ \case
  PNothing -> perror
  PJust value -> pfromData (punsafeCoerce value)

decodeBuiltinValue :: forall s. BS.ByteString -> Term s Builtin.PValueWitnessV1
decodeBuiltinValue cbor = pmatch (pdeserialise # pconstant (hex cbor)) $ \case
  PNothing -> perror
  PJust value -> pfromData (punsafeCoerce value)

machineState ::
  forall s.
  Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PMachineStateV1
machineState mode focus environment continuation auxiliary cpu memory =
  pcon $ PMachineStateV1
    (pdata mode) (pdata 0) (pdata focus) (pdata environment) (pdata continuation)
    (pdata auxiliary) (pdata cpu) (pdata memory)

constantWitness :: forall s. BS.ByteString -> BS.ByteString -> Term s PConstantWitnessV1
constantWitness typeCbor payloadCbor =
  pcon $ PConstantWitnessV1 (pdata $ pconstant $ hex typeCbor) (pdata $ pconstant $ hex payloadCbor)

constantValue :: forall s. BS.ByteString -> BS.ByteString -> Term s Builtin.PValueWitnessV1
constantValue typeCbor payloadCbor = pcon $ Builtin.PConstantValue $ pdata $ constantWitness typeCbor payloadCbor

integer :: forall s. BS.ByteString -> Term s Builtin.PValueWitnessV1
integer = constantValue "9f00ff"

bytes :: forall s. BS.ByteString -> Term s Builtin.PValueWitnessV1
bytes = constantValue "9f01ff"

boolean :: forall s. BS.ByteString -> Term s Builtin.PValueWitnessV1
boolean = constantValue "9f04ff"

g1Witness, g2Witness :: forall s. Term s PConstantWitnessV1
g1Witness = constantWitness "9f09ff" "583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
g2Witness = constantWitness "9f0aff" "5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff"

blsLeaf :: forall s. Term s Builtin.PBlsExpressionWitnessV1
blsLeaf = pcon $ Builtin.PBlsMillerLoopExpression (pdata g1Witness) (pdata g2Witness)

blsMultiply :: forall s. Term s Builtin.PBlsExpressionWitnessV1 -> Term s Builtin.PBlsExpressionWitnessV1 -> Term s Builtin.PBlsExpressionWitnessV1
blsMultiply left right = pcon $ Builtin.PBlsMultiplyExpression (pdata left) (pdata right)

doubleExpression, tripleExpression, fiveExpression :: forall s. Term s Builtin.PBlsExpressionWitnessV1
doubleExpression = blsMultiply blsLeaf blsLeaf
tripleExpression = blsMultiply doubleExpression blsLeaf
fiveExpression = blsMultiply tripleExpression doubleExpression

blsLeafRoot :: forall s. Term s PByteString
blsLeafRoot = Proof.phashBlsMillerLoopExpressionV1 # (pconstantRootV1 # g1Witness) # (pconstantRootV1 # g2Witness)

doubleExpressionRoot, tripleExpressionRoot, fiveExpressionRoot :: forall s. Term s PByteString
doubleExpressionRoot = Proof.phashBlsMultiplyExpressionV1 # blsLeafRoot # blsLeafRoot
tripleExpressionRoot = Proof.phashBlsMultiplyExpressionV1 # doubleExpressionRoot # blsLeafRoot
fiveExpressionRoot = Proof.phashBlsMultiplyExpressionV1 # tripleExpressionRoot # doubleExpressionRoot

blsValue :: forall s. Term s PByteString -> Term s Builtin.PValueWitnessV1
blsValue root = pcon $ Builtin.PBlsMillerLoopValue $ pdata root

valueList :: forall s. [Term s Builtin.PValueWitnessV1] -> Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1))
valueList = foldr (\value rest -> pcons # pdata value # rest) pnil

runtimeValueList :: forall s. [Term s Builtin.PRuntimeValueWitnessV1] -> Term s (PBuiltinList (PAsData Builtin.PRuntimeValueWitnessV1))
runtimeValueList = foldr (\value rest -> pcons # pdata value # rest) pnil

summaryRoot :: forall s. Term s Data.PDataSummaryV1 -> Term s PByteString
summaryRoot summary = pmatch summary $ \value -> pfromData $ Data.psummary'root value

summaryCborLength :: forall s. Term s Data.PDataSummaryV1 -> Term s PInteger
summaryCborLength summary = pmatch summary $ \value -> pfromData $ Data.psummary'cborLength value

summaryMemory :: forall s. Term s Data.PDataSummaryV1 -> Term s PInteger
summaryMemory summary = pmatch summary $ \value -> pfromData $ Data.psummary'memory value

sequenceRoot :: forall s. Term s Data.PDataSequenceSummaryV1 -> Term s PByteString
sequenceRoot summary = pmatch summary $ \value -> pfromData $ Data.pseq'root value

integerList :: forall s. [Term s PInteger] -> Term s (PBuiltinList PInteger)
integerList = foldr (\value rest -> pcons # value # rest) pnil

hash :: forall s. Word8 -> Term s PByteString
hash byte = pconstant $ BS.replicate 32 byte

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
