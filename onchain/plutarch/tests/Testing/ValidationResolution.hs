{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationResolution (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (ScriptCredential),
  Datum (..),
  OutputDatum (OutputDatum),
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PScriptHash, PTxInInfo)
import Plutarch.Prelude

import Midgard.ValidationMachine
import Midgard.ValidationDispute (popen)
import Midgard.ValidationResolution
import Midgard.ValidationResolver
import Midgard.ValidationSemantic (psemanticHandoffIsValid)
import Midgard.ValidationTrace
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  ppreparedSelectionIsValid,
 )
import Testing.Eval (passertEvalNoTrace, pfails)
import Testing.ScriptContextBuilder (mkAdaValue)

tests :: TestTree
tests = testGroup "Midgard.ValidationResolution"
  [ testCase "one_step_boundary_is_authenticated_before_resolver_handoff" $
      passertEvalNoTrace oneStepBoundaryIsAuthenticatedBeforeHandoff
  , testCase "resolver_routing_is_total_for_nonterminal_phases_and_rejects_terminal" $
      passertEvalNoTrace resolverIndicesCoverNonterminalPhases
  , testCase "malformed_resolution_states_fail_closed" $
      passertEvalNoTrace malformedResolutionStatesFailClosed
  , testCase "challenger_resolution_requires_exact_hashes_and_a_unique_valid_successor" $
      passertEvalNoTrace challengerResolutionRequiresUniqueValidSuccessor
  , testCase "resolver_indices_cover_exactly_the_nonterminal_phases" $
      passertEvalNoTrace resolverIndicesCoverNonterminalPhases
  , testCase "structural_transition_authenticates_the_exact_successor" $
      passertEvalNoTrace structuralTransitionAuthenticatesExactSuccessor
  , testCase "structural_transition_freezes_non_cek_budget" $
      passertEvalNoTrace structuralTransitionFreezesNonCekBudget
  , testCase "challenger_wins_only_with_the_committed_valid_successor" $
      passertEvalNoTrace challengerWinsOnlyWithCommittedSuccessor
  , testCase "winning_resolution_version_is_exact" $
      passertEvalNoTrace $ pwinningResolutionIsWellFormed # pwinningResolution
  , testCase "compact_binding_witness_cbor_is_canonical" $
      passertEvalNoTrace compactBindingWitnessCborIsCanonical
  , testCase "transaction_field_scan_witness_cbor_is_canonical" $
      passertEvalNoTrace transactionFieldScanWitnessCborIsCanonical
  , testCase "transaction_field_scan_rejects_an_out_of_range_field" $
      pfails $ pencodeTransactionFieldScanWitness
        # pconstant "" # pconstant "" # pconstant "" # pconstant ""
        # 9 # 0 # 0 # (-1) # 0
  , testCase "semantic_resolution_binds_transition_and_auxiliary" $
      passertEvalNoTrace semanticResolutionBindsEvidence
  , testCase "phase_a_applied_group_routes_through_the_production_prepare_path" $
      passertEvalNoTrace $ allPreparedRoutes PPhaseAScriptPreconditions 2
  , testCase "script_sources_applied_group_routes_through_the_production_prepare_path" $
      passertEvalNoTrace $ allPreparedRoutes PScriptSources 29
  , testCase "production_prepare_rejects_a_wrong_semantic_resolver_index" $
      pfails productionPrepareWrongResolverIndex
  , testCase "production_prepare_rejects_a_wrong_output_script_hash" $
      passertEvalNoTrace productionPrepareRejectsWrongOutputHash
  , testCase "production_prepare_rejects_same_cardinality_group_substitution" $
      passertEvalNoTrace productionPrepareRejectsGroupSubstitution
  , testCase "semantic_handoff_accepts_exact_prepared_phase_a_evidence" $
      passertEvalNoTrace semanticHandoffAcceptsExactEvidence
  , testCase "semantic_handoff_rejects_script_sources_auxiliary_substitution" $
      passertEvalNoTrace semanticHandoffRejectsAuxiliarySubstitution
  , testCase "semantic_handoff_rejects_cross_family_action_substitution" $
      passertEvalNoTrace semanticHandoffRejectsCrossFamilyAction
  , testCase "cek_material_reference_indices_are_unique_reference_selectors" $
      passertEvalNoTrace cekMaterialReferenceIndicesAreUnique
  , testCase "cek_direct_route_binds_the_selected_envelope" $
      passertEvalNoTrace cekDirectRouteBindsSelectedEnvelope
  , testCase "cek_incremental_route_rejects_zero_published_material" $
      passertEvalNoTrace cekIncrementalRouteRejectsZeroMaterial
  , testCase "cek_incremental_route_rejects_complete_published_material" $
      passertEvalNoTrace cekIncrementalRouteRejectsCompleteMaterial
  , testCase "cek_incremental_route_rejects_substituted_published_material" $
      passertEvalNoTrace cekIncrementalRouteRejectsSubstitutedMaterial
  , testCase "cek_complete_multi_output_route_accepts_the_same_material" $
      passertEvalNoTrace cekCompleteMultiOutputAcceptsSameMaterial
  , testCase "cek_incremental_route_fails_closed_with_self_consistent_hash" $
      passertEvalNoTrace cekIncrementalRouteFailsClosedWithSelfConsistentHash
  , testCase "cek_incremental_route_fails_closed_with_partial_material" $
      passertEvalNoTrace cekIncrementalRouteFailsClosedWithPartialMaterial
  , testCase "cek_complete_item_carriage_survives_the_incremental_closure" $
      passertEvalNoTrace cekCompleteItemCarriageSurvivesIncrementalClosure
  , testCase "cek_incremental_route_fails_closed_with_complete_material_control" $
      passertEvalNoTrace cekIncrementalRouteFailsClosed
  , testCase "cek_single_and_multi_reference_routes_verify_typed_material" $
      passertEvalNoTrace cekReferenceRoutesVerifyTypedMaterial
  ]

resolverIndicesCoverNonterminalPhases :: forall s. Term s PBool
resolverIndicesCoverNonterminalPhases =
  phaseIndices
    [ PCanonicalDecode, PCompactBinding, PStaticLedgerRules, PInputSets, PSignatures
    , PPhaseANativeScripts, PPhaseAScriptPreconditions, PResolveInputs, PScriptSources
    , PNativeScripts, PScriptIntegrity, PCek, PValueAndMint, PLedgerDelta, PTerminal
    ]
    #== integerList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, -1]
  #&& presolverCount #== 14
  where
    phaseIndices = foldr (\phase rest -> pcons # (presolverIndex # pcon phase) # rest) pnil

oneStepBoundaryIsAuthenticatedBeforeHandoff :: forall s. Term s PBool
oneStepBoundaryIsAuthenticatedBeforeHandoff =
  plet (phexByteStr "80") $ \workWitnessCbor ->
  plet
    ( state PCompactBinding 0
        (phashWorkWitness # pcon PCompactBinding # 0 # workWitnessCbor) 0 0
    )
    $ \pre ->
  plet (state PInputSets 1 (hash 3) 0 0) $ \operatorSuccessor ->
  plet (state PInputSets 1 (hash 4) 0 0) $ \challengerSuccessor ->
  plet (phashMachineState # pre) $ \preHash ->
  plet (phashMachineState # operatorSuccessor) $ \operatorHash ->
  plet (phashMachineState # challengerSuccessor) $ \challengerHash ->
  plet (twoStateDescriptor preHash operatorHash) $ \operatorDescriptor ->
  plet (twoStateDescriptor preHash challengerHash) $ \challengerDescriptor ->
  plet (popen # operatorDescriptor # challengerDescriptor # 100) $ \ready ->
  plet (twoStateTerminalProof operatorHash preHash) $ \operatorPost ->
  plet (twoStateTerminalProof challengerHash preHash) $ \challengerPost ->
  plet
    ( pprepareValidationResolution
        # ready # pre # operatorPost # challengerPost
    )
    $ \resolution ->
  pmatch resolution $ \r ->
    pand'List
      [ pfromData (presolution'version r) #== presolutionVersion
      , presolution'preState r #== pdata pre
      , pfromData (presolution'operatorSuccessorHash r) #== operatorHash
      , pfromData (presolution'challengerSuccessorHash r) #== challengerHash
      , presolverIndex # pcon PCompactBinding #== 1
      , pvalidationResolutionStateIsWellFormed # resolution
      ]

twoStateDescriptor :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PValidationTraceDescriptorV1
twoStateDescriptor initial terminal =
  pcon $ PValidationTraceDescriptorV1
    (pdata pdescriptorVersion)
    (pdata pmachineVersion)
    (pdata $ ptraceBranchHash # (ptraceLeafHash # initial) # (ptraceLeafHash # terminal))
    (pdata 1)
    (pdata initial)
    (pdata terminal)
    (pdata $ pcon PAccepted)
    (pdata $ hash 0)

twoStateTerminalProof :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PValidationTraceProof
twoStateTerminalProof terminal initial =
  pcon $ PValidationTraceProof
    (pdata 1)
    (pdata terminal)
    (pdata $ pcons # pdata (ptraceLeafHash # initial) # pnil)

structuralTransitionAuthenticatesExactSuccessor :: forall s. Term s PBool
structuralTransitionAuthenticatesExactSuccessor =
  plet (pconstant "witness") $ \work ->
  plet (state PCanonicalDecode 0 (phashWorkWitness # pcon PCanonicalDecode # 0 # work) 0 0) $ \pre ->
  plet (state PCompactBinding 1 (hash 8) 0 0) $ \post ->
    pstructuralTransitionIsValid # pre
      # (pcon $ PValidationOneStepWitnessV1 (pdata work) (pdata post))

structuralTransitionFreezesNonCekBudget :: forall s. Term s PBool
structuralTransitionFreezesNonCekBudget =
  plet (pconstant "witness") $ \work ->
  plet (state PCanonicalDecode 0 (phashWorkWitness # pcon PCanonicalDecode # 0 # work) 0 0) $ \pre ->
  plet (state PCompactBinding 1 (hash 8) 1 0) $ \post ->
    pnot # (pstructuralTransitionIsValid # pre
      # (pcon $ PValidationOneStepWitnessV1 (pdata work) (pdata post)))

challengerWinsOnlyWithCommittedSuccessor :: forall s. Term s PBool
challengerWinsOnlyWithCommittedSuccessor =
  plet (state PCanonicalDecode 0 (hash 7) 0 0) $ \pre ->
  plet (state PCompactBinding 1 (hash 8) 0 0) $ \challenger ->
  plet (pcon $ PValidationResolutionStateV1
    (pdata presolutionVersion) (pdata pre) (pdata $ hash 9) (pdata $ phashMachineState # challenger)) $ \resolution ->
    pchallengerWinsWithValidSuccessor # resolution # challenger # pconstant True
      #&& pnot # (pchallengerWinsWithValidSuccessor # resolution # challenger # pconstant False)

malformedResolutionStatesFailClosed :: forall s. Term s PBool
malformedResolutionStatesFailClosed =
  plet (state PCanonicalDecode 0 (hash 7) 0 0) $ \pre ->
  plet
    ( pcon $ PValidationResolutionStateV1
        (pdata presolutionVersion) (pdata pre) (pdata $ hash 2) (pdata $ hash 3)
    )
    $ \valid ->
  pmatch valid $ \v ->
    pand'List
      [ pvalidationResolutionStateIsWellFormed # valid
      , pnot #$ pvalidationResolutionStateIsWellFormed
          # pcon v {presolution'version = pdata 2}
      , pnot #$ pvalidationResolutionStateIsWellFormed
          # pcon v {presolution'operatorSuccessorHash = pdata $ hash 3}
      , pnot #$ pvalidationResolutionStateIsWellFormed
          # pcon v {presolution'challengerSuccessorHash = pdata $ phexByteStr "00"}
      , pnot #$ pvalidationResolutionStateIsWellFormed
          # pcon v {presolution'preState = pdata $ state PTerminal 0 (hash 7) 0 0}
      ]

challengerResolutionRequiresUniqueValidSuccessor :: forall s. Term s PBool
challengerResolutionRequiresUniqueValidSuccessor =
  plet (state PCanonicalDecode 0 (hash 7) 0 0) $ \pre ->
  plet (state PCanonicalDecode 1 (hash 2) 0 0) $ \operator ->
  plet (state PCanonicalDecode 1 (hash 3) 0 0) $ \challenger ->
  plet
    ( pcon $ PValidationResolutionStateV1
        (pdata presolutionVersion)
        (pdata pre)
        (pdata $ phashMachineState # operator)
        (pdata $ phashMachineState # challenger)
    )
    $ \resolution ->
      pand'List
        [ pchallengerUniquelyWins
            # resolution # operator # challenger # pconstant False # pconstant True
        , pnot #$ pchallengerUniquelyWins
            # resolution # operator # challenger # pconstant True # pconstant True
        , pnot #$ pchallengerUniquelyWins
            # resolution # operator # challenger # pconstant False # pconstant False
        , pnot #$ pchallengerUniquelyWins
            # resolution # challenger # operator # pconstant False # pconstant True
        ]

compactBindingWitnessCborIsCanonical :: forall s. Term s PBool
compactBindingWitnessCborIsCanonical =
  pencodeCompactBindingWitness
    # pconstant "\xaa" # pconstant "\xbb" # pconstant "\xcc"
    # pconstant "\xdd" # pconstant "\xee" # pconstant "\xff"
    #== pconstant (hex "8641aa41bb41cc41dd41ee41ff")

transactionFieldScanWitnessCborIsCanonical :: forall s. Term s PBool
transactionFieldScanWitnessCborIsCanonical =
  pencodeTransactionFieldScanWitness
    # pconstant "\xaa" # pconstant "\xbb" # pconstant "\xcc" # pconstant "\xdd"
    # 0 # 1 # 2 # (-1) # 24
    #== pconstant (hex "8941aa41bb41cc41dd000102201818")

semanticResolutionBindsEvidence :: forall s. Term s PBool
semanticResolutionBindsEvidence =
  plet (pconstant "witness") $ \work ->
  plet (state PCanonicalDecode 0 (phashWorkWitness # pcon PCanonicalDecode # 0 # work) 0 0) $ \pre ->
  plet (state PCompactBinding 1 (hash 8) 0 0) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata work) (pdata post)) $ \transition ->
  plet (pforgetData $ pdata (42 :: Term s PInteger)) $ \auxiliary ->
  plet (pcon $ PValidationResolutionStateV1
    (pdata presolutionVersion) (pdata pre) (pdata $ hash 9) (pdata $ phashMachineState # post)) $ \resolution ->
    pprepareSemanticResolution # resolution # transition # auxiliary
      #== pcon (PPreparedValidationResolutionStateV1
        (pdata ppreparedResolutionVersion)
        (pdata resolution)
        (pdata $ phashOneStepEvidence # pforgetData (pdata transition) # auxiliary))

allPreparedRoutes :: forall s. PValidationPhase s -> Integer -> Term s PBool
allPreparedRoutes phase count =
  withPreparedFixture phase $ \_pre transition resolution auxiliary prepared ->
  plet (resolverGroup count 1) $ \resolvers ->
    ( pfix $ \self -> plam $ \remaining resolverIndex ->
        pelimList
          ( \resolverHashD rest ->
              ppreparedSelectionIsValid (pcon phase) (pconstant count)
                # resolvers
                # resolution
                # resolverIndex
                # transition
                # auxiliary
                # pfromData resolverHashD
                # pforgetData (pdata prepared)
                #&& self # rest # (resolverIndex + 1)
          )
          (pconstant True)
          remaining
    )
      # resolvers
      # 0

productionPrepareWrongResolverIndex :: forall s. Term s PBool
productionPrepareWrongResolverIndex =
  withPreparedFixture PPhaseAScriptPreconditions $ \_pre transition resolution auxiliary prepared ->
  ppreparedSelectionIsValid (pcon PPhaseAScriptPreconditions) 2
    # resolverGroup 2 1
    # resolution
    # 2
    # transition
    # auxiliary
    # scriptHash 1
    # pforgetData (pdata prepared)

productionPrepareRejectsWrongOutputHash :: forall s. Term s PBool
productionPrepareRejectsWrongOutputHash =
  withPreparedFixture PPhaseAScriptPreconditions $ \_pre transition resolution auxiliary prepared ->
  pnot #$
    ppreparedSelectionIsValid (pcon PPhaseAScriptPreconditions) 2
      # resolverGroup 2 1
      # resolution
      # 1
      # transition
      # auxiliary
      # scriptHash 1
      # pforgetData (pdata prepared)

productionPrepareRejectsGroupSubstitution :: forall s. Term s PBool
productionPrepareRejectsGroupSubstitution =
  withPreparedFixture PPhaseAScriptPreconditions $ \_pre transition resolution auxiliary prepared ->
  pnot #$
    ppreparedSelectionIsValid (pcon PPhaseAScriptPreconditions) 2
      # resolverGroup 2 11
      # resolution
      # 0
      # transition
      # auxiliary
      # scriptHash 1
      # pforgetData (pdata prepared)

semanticHandoffAcceptsExactEvidence :: forall s. Term s PBool
semanticHandoffAcceptsExactEvidence =
  withPreparedFixture PPhaseAScriptPreconditions $ \_pre transition _resolution auxiliary prepared ->
  psemanticHandoffIsValid
    # pcon PPhaseAScriptPreconditions
    # prepared
    # transition
    # auxiliary
    # pconstant True
    # scriptHash 21
    # scriptHash 21
    # pforgetData (pdata pwinningResolution)

semanticHandoffRejectsAuxiliarySubstitution :: forall s. Term s PBool
semanticHandoffRejectsAuxiliarySubstitution =
  withPreparedFixture PPhaseAScriptPreconditions $ \_pre transition _resolution _auxiliary prepared ->
  pnot #$
    psemanticHandoffIsValid
      # pcon PPhaseAScriptPreconditions
      # prepared
      # transition
      # pforgetData (pdata (42 :: Term s PInteger))
      # pconstant True
      # scriptHash 21
      # scriptHash 21
      # pforgetData (pdata pwinningResolution)

semanticHandoffRejectsCrossFamilyAction :: forall s. Term s PBool
semanticHandoffRejectsCrossFamilyAction =
  withPreparedFixture PPhaseAScriptPreconditions $ \_pre transition _resolution auxiliary prepared ->
  pnot #$
    psemanticHandoffIsValid
      # pcon PPhaseAScriptPreconditions
      # prepared
      # transition
      # auxiliary
      # pconstant False
      # scriptHash 21
      # scriptHash 21
      # pforgetData (pdata pwinningResolution)

withPreparedFixture ::
  forall s.
  PValidationPhase s ->
  ( Term s PValidationMachineStateV1 ->
    Term s PValidationOneStepWitnessV1 ->
    Term s PValidationResolutionStateV1 ->
    Term s PData ->
    Term s PPreparedValidationResolutionStateV1 ->
    Term s PBool
  ) ->
  Term s PBool
withPreparedFixture phase k =
  plet (pconstant "prepared-route-witness") $ \work ->
  plet (state phase 6 (phashWorkWitness # pcon phase # 6 # work) 0 0) $ \pre ->
  plet (state phase 7 (hash 8) 0 0) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata work) (pdata post)) $ \transition ->
  plet
    ( pcon $ PValidationResolutionStateV1
        (pdata presolutionVersion)
        (pdata pre)
        (pdata $ hash 9)
        (pdata $ phashMachineState # post)
    )
    $ \resolution ->
    plet (pforgetData $ pdata $ pcon PNoAuxiliaryWitness) $ \auxiliary ->
    plet (pprepareSemanticResolution # resolution # transition # auxiliary) $ \prepared ->
      k pre transition resolution auxiliary prepared

resolverGroup :: forall s. Integer -> Word8 -> Term s (PBuiltinList (PAsData PScriptHash))
resolverGroup count firstByte = go count firstByte
  where
    go 0 _ = pnil
    go remaining byte = pcons # pdata (scriptHash byte) # go (remaining - 1) (byte + 1)

scriptHash :: forall s. Word8 -> Term s PScriptHash
scriptHash byte = pconstant $ ScriptHash $ toBuiltin $ BS.replicate 28 byte

cekMaterialReferenceIndicesAreUnique :: forall s. Term s PBool
cekMaterialReferenceIndicesAreUnique =
  pcekReferenceIndicesUniqueNonnegativeV1 # integerList []
    #&& pcekReferenceIndicesUniqueNonnegativeV1 # integerList [0]
    #&& pcekReferenceIndicesUniqueNonnegativeV1 # integerList [2, 0, 1]
    #&& pnot # (pcekReferenceIndicesUniqueNonnegativeV1 # integerList [-1])
    #&& pnot # (pcekReferenceIndicesUniqueNonnegativeV1 # integerList [1, 1])

cekDirectRouteBindsSelectedEnvelope :: forall s. Term s PBool
cekDirectRouteBindsSelectedEnvelope =
  verifyMaterialRoute directRoute [] materialScriptHash
    #&& pnot # (verifyMaterialRoute wrongEnvelopeRoute [] materialScriptHash)
    #&& pnot # (verifyMaterialRoute badSidecarRoute [] materialScriptHash)
    #&& pnot # (verifyMaterialRoute (pcon PNoCekMaterial) [] materialScriptHash)
  where
    directRoute = pcon $ PDirectCekMaterial (pdata $ pconstant oneNodeEnvelope) (pdata $ pconstant oneNodeSidecar)
    wrongEnvelopeRoute = pcon $ PDirectCekMaterial (pdata $ pconstant "\x81\x06") (pdata $ pconstant oneNodeSidecar)
    badSidecarRoute = pcon $ PDirectCekMaterial (pdata $ pconstant oneNodeEnvelope) (pdata $ pconstant $ oneNodeSidecar <> "\x00")

cekIncrementalRouteRejectsZeroMaterial :: forall s. Term s PBool
cekIncrementalRouteRejectsZeroMaterial =
  pnot # verifyMaterialRoute incrementalRoute [] materialScriptHash

cekIncrementalRouteRejectsCompleteMaterial :: forall s. Term s PBool
cekIncrementalRouteRejectsCompleteMaterial =
  pnot # verifyMaterialRoute incrementalRoute [materialEntryInput materialScriptHash] materialScriptHash

cekIncrementalRouteRejectsSubstitutedMaterial :: forall s. Term s PBool
cekIncrementalRouteRejectsSubstitutedMaterial =
  pnot # verifyMaterialRoute incrementalRoute [substitutedMaterialInput materialScriptHash] materialScriptHash

cekCompleteMultiOutputAcceptsSameMaterial :: forall s. Term s PBool
cekCompleteMultiOutputAcceptsSameMaterial =
  verifyMaterialRoute completeMultiRoute [materialEntryInput materialScriptHash] materialScriptHash

cekIncrementalRouteFailsClosedWithSelfConsistentHash :: forall s. Term s PBool
cekIncrementalRouteFailsClosedWithSelfConsistentHash =
  verifyMaterialRoute directRoute [] materialScriptHash
    #&& verifyMaterialRoute completeMultiRoute [materialEntryInput materialScriptHash] materialScriptHash
    #&& pnot # verifyMaterialRoute incrementalRoute [] materialScriptHash
    #&& pnot # verifyMaterialRoute incrementalRoute [materialEntryInput materialScriptHash] materialScriptHash
    #&& pnot # verifyMaterialRoute incrementalRoute [substitutedMaterialInput materialScriptHash] materialScriptHash
    #&& pnot # verifyMaterialRoute completeMultiRoute [substitutedMaterialInput materialScriptHash] materialScriptHash
    #&& pnot # verifyMaterialRoute (incrementalRouteWith $ BS.replicate 32 0xaa) [] materialScriptHash
    #&& pnot # verifyMaterialRoute (incrementalRouteWith "") [] materialScriptHash
  where
    directRoute = pcon $ PDirectCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata $ pconstant oneNodeSidecar)

cekIncrementalRouteFailsClosedWithPartialMaterial :: forall s. Term s PBool
cekIncrementalRouteFailsClosedWithPartialMaterial =
  verifyMaterialRouteFor twoNodeEnvelope completeRoute completeInputs materialScriptHash
    #&& pnot # verifyMaterialRouteFor twoNodeEnvelope partialRoute partialInputs materialScriptHash
    #&& pnot # verifyMaterialRouteFor twoNodeEnvelope incrementalRouteTwoNodes partialInputs materialScriptHash
    #&& pnot # verifyMaterialRouteFor twoNodeEnvelope incrementalRouteTwoNodes completeInputs materialScriptHash
  where
    completeRoute = pcon $ PMinimumMultiOutputCekMaterial
      (pdata $ pconstant twoNodeEnvelope)
      (pdata $ pconstant [0 :: Integer, 1])
    partialRoute = pcon $ PMinimumMultiOutputCekMaterial
      (pdata $ pconstant twoNodeEnvelope)
      (pdata $ pconstant [0 :: Integer])
    incrementalRouteTwoNodes = incrementalRouteWith twoNodeEnvelopeHash
    completeInputs = fmap ($ materialScriptHash) twoNodeMaterialInputs
    partialInputs = [twoNodeRootInput materialScriptHash]

cekCompleteItemCarriageSurvivesIncrementalClosure :: forall s. Term s PBool
cekCompleteItemCarriageSurvivesIncrementalClosure =
  verifyMaterialRoute directRoute [] materialScriptHash
    #&& verifyMaterialRoute singleRoute [singlePublicationInput materialScriptHash] materialScriptHash
    #&& verifyMaterialRoute completeMultiRoute [materialEntryInput materialScriptHash] materialScriptHash
    #&& pnot # verifyMaterialRoute incrementalRoute [materialEntryInput materialScriptHash] materialScriptHash
    #&& pnot # verifyMaterialRoute (pcon PNoCekMaterial) [] materialScriptHash
  where
    directRoute = pcon $ PDirectCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata $ pconstant oneNodeSidecar)
    singleRoute = pcon $ PSinglePublicationCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata 0)

incrementalRoute :: forall s. Term s PCekMaterialRouteV1
incrementalRoute = incrementalRouteWith oneNodeEnvelopeHash

incrementalRouteWith :: forall s. BS.ByteString -> Term s PCekMaterialRouteV1
incrementalRouteWith envelopeHash =
  pcon $ PIncrementalCekMaterial (pdata $ pconstant envelopeHash)

completeMultiRoute :: forall s. Term s PCekMaterialRouteV1
completeMultiRoute = pcon $ PMinimumMultiOutputCekMaterial
  (pdata $ pconstant oneNodeEnvelope)
  (pdata $ pconstant [0 :: Integer])

cekIncrementalRouteFailsClosed :: forall s. Term s PBool
cekIncrementalRouteFailsClosed =
  verifyMaterialRoute multiRoute [materialEntryInput materialScriptHash] materialScriptHash
    #&& pnot # (verifyMaterialRoute incrementalRoute [] materialScriptHash)
    #&& pnot # (verifyMaterialRoute incrementalRoute [materialEntryInput materialScriptHash] materialScriptHash)
  where
    multiRoute = pcon $ PMinimumMultiOutputCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata $ pconstant [0 :: Integer])
    incrementalRoute = pcon $ PIncrementalCekMaterial (pdata $ pconstant oneNodeEnvelopeHash)

cekReferenceRoutesVerifyTypedMaterial :: forall s. Term s PBool
cekReferenceRoutesVerifyTypedMaterial =
  verifyMaterialRoute singleRoute [singlePublicationInput materialScriptHash] materialScriptHash
    #&& pnot # (verifyMaterialRoute singleOutOfRange [singlePublicationInput materialScriptHash] materialScriptHash)
    #&& pnot # (verifyMaterialRoute singleRoute [singlePublicationInput otherScriptHash] materialScriptHash)
    #&& verifyMaterialRoute multiRoute [materialEntryInput materialScriptHash] materialScriptHash
    #&& pnot # (verifyMaterialRoute duplicateMultiRoute [materialEntryInput materialScriptHash] materialScriptHash)
    #&& pnot # (verifyMaterialRoute emptyMultiRoute [materialEntryInput materialScriptHash] materialScriptHash)
  where
    singleRoute = pcon $ PSinglePublicationCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata 0)
    singleOutOfRange = pcon $ PSinglePublicationCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata 1)
    multiRoute = pcon $ PMinimumMultiOutputCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata $ pconstant [0 :: Integer])
    duplicateMultiRoute = pcon $ PMinimumMultiOutputCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata $ pconstant [0 :: Integer, 0])
    emptyMultiRoute = pcon $ PMinimumMultiOutputCekMaterial
      (pdata $ pconstant oneNodeEnvelope)
      (pdata $ pconstant ([] :: [Integer]))

verifyMaterialRoute ::
  forall s.
  Term s PCekMaterialRouteV1 ->
  [TxInInfo] ->
  ScriptHash ->
  Term s PBool
verifyMaterialRoute route referenceInputs scriptHash =
  verifyMaterialRouteFor oneNodeEnvelope route referenceInputs scriptHash

verifyMaterialRouteFor ::
  forall s.
  BS.ByteString ->
  Term s PCekMaterialRouteV1 ->
  [TxInInfo] ->
  ScriptHash ->
  Term s PBool
verifyMaterialRouteFor selectedEnvelope route referenceInputs scriptHash =
  pverifyCekMaterialRouteForSelectedEnvelopeV1
    # pconstant selectedEnvelope
    # route
    # pconstant @(PBuiltinList (PAsData PTxInInfo)) referenceInputs
    # pconstant @(PAsData PScriptHash) scriptHash

materialEntryInput :: ScriptHash -> TxInInfo
materialEntryInput scriptHash = materialReferenceInput scriptHash $
  PD.Constr 0 [PD.I 0, PD.B oneNodeRoot, PD.B oneNodePreimage]

singlePublicationInput :: ScriptHash -> TxInInfo
singlePublicationInput scriptHash = materialReferenceInput scriptHash $
  PD.Constr 0 [PD.I 1, PD.B oneNodeEnvelopeHash, PD.B oneNodeSidecar]

substitutedMaterialInput :: ScriptHash -> TxInInfo
substitutedMaterialInput scriptHash = materialReferenceInput scriptHash $
  PD.Constr 0 [PD.I 0, PD.B substitutedRoot, PD.B substitutedPreimage]

twoNodeRootInput, twoNodeChildInput :: ScriptHash -> TxInInfo
twoNodeRootInput scriptHash = materialReferenceInput scriptHash $
  PD.Constr 0 [PD.I 0, PD.B twoNodeRoot, PD.B twoNodeRootPreimage]
twoNodeChildInput scriptHash = materialReferenceInput scriptHash $
  PD.Constr 0 [PD.I 0, PD.B oneNodeRoot, PD.B oneNodePreimage]

twoNodeMaterialInputs :: [ScriptHash -> TxInInfo]
twoNodeMaterialInputs =
  if twoNodeRoot < oneNodeRoot
    then [twoNodeRootInput, twoNodeChildInput]
    else [twoNodeChildInput, twoNodeRootInput]

materialReferenceInput :: ScriptHash -> PD.Data -> TxInInfo
materialReferenceInput scriptHash datum =
  TxInInfo
    (TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x55) 0)
    ( TxOut
        (Address (ScriptCredential scriptHash) Nothing)
        (mkAdaValue 2_000_000)
        (OutputDatum $ Datum $ dataToBuiltinData datum)
        Nothing
    )

materialScriptHash, otherScriptHash :: ScriptHash
materialScriptHash = ScriptHash $ toBuiltin $ BS.replicate 28 0x77
otherScriptHash = ScriptHash $ toBuiltin $ BS.replicate 28 0x33

oneNodePreimage, oneNodeRoot, oneNodeEnvelope, oneNodeEnvelopeHash, oneNodeSidecar :: BS.ByteString
oneNodePreimage = "\x81\x06"
oneNodeRoot = blake2b256 $ "MidgardCekTermNodeV1" <> oneNodePreimage
oneNodeEnvelope = "\x85\x01\x83\x01\x01\x00\x58\x20" <> oneNodeRoot <> "\x01\x02"
oneNodeEnvelopeHash = blake2b256 $ "MidgardCekProgramEnvelopeV1" <> oneNodeEnvelope
oneNodeSidecar =
  "\x82\x01\x81\x82\x58\x20" <> oneNodeRoot <> "\x83\x01\x00\x42" <> oneNodePreimage

substitutedPreimage, substitutedRoot, twoNodeRootPreimage, twoNodeRoot, twoNodeEnvelope, twoNodeEnvelopeHash :: BS.ByteString
substitutedPreimage = "\x82\x00\x00"
substitutedRoot = blake2b256 $ "MidgardCekTermNodeV1" <> substitutedPreimage
twoNodeRootPreimage = "\x82\x01\x58\x20" <> oneNodeRoot
twoNodeRoot = blake2b256 $ "MidgardCekTermNodeV1" <> twoNodeRootPreimage
twoNodeEnvelope = "\x85\x01\x83\x01\x01\x00\x58\x20" <> twoNodeRoot <> "\x02\x18\x26"
twoNodeEnvelopeHash = blake2b256 $ "MidgardCekProgramEnvelopeV1" <> twoNodeEnvelope

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

state ::
  forall s.
  PValidationPhase s ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
state phase counter workRoot cpu memory = pcon $ PValidationMachineStateV1
  (pdata pmachineVersion)
  (pdata $ hash 1)
  (pdata $ hash 2)
  (pdata $ hash 3)
  (pdata $ hash 4)
  (pdata $ pcon PNormal)
  (pdata $ hash 5)
  (pdata $ pcon phase)
  (pdata counter)
  (pdata workRoot)
  (pdata cpu)
  (pdata memory)
  (pdata $ pcon PPending)
  (pdata $ hash 0)
  (pdata $ hash 6)

hash :: forall s. Word8 -> Term s PByteString
hash byte = pconstant $ BS.replicate 32 byte

integerList :: forall s. [Integer] -> Term s (PBuiltinList PInteger)
integerList = foldr (\n rest -> pcons # pconstant n # rest) pnil

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
