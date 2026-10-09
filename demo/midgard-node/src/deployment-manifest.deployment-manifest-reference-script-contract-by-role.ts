import "./deployment-manifest.required-transaction-order-contracts.js";

export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze({
    "reference-script-auth minting": "referenceScriptAuthMint",
    "hub-oracle minting": "hubOracleMint",
    "da-params-governor spending": "daParamsGovernorSpend",
    "da-params-governor minting": "daParamsGovernorMint",
    "da-bond-pool spending": "daBondPoolSpend",
    "da-bond-pool minting": "daBondPoolMint",
    "da-attestation spending": "daAttestationSpend",
    "da-attestation minting": "daAttestationMint",
    "state-queue spending": "stateQueueSpend",
    "state-queue minting": "stateQueueMint",
    "state-queue commit withdrawal": "stateQueueCommitWithdraw",
    "state-queue unattested-timeout withdrawal":
      "stateQueueUnattestedTimeoutWithdraw",
    "state-queue unavailable-timeout withdrawal":
      "stateQueueUnavailableTimeoutWithdraw",
    "state-queue fraud-removal withdrawal": "stateQueueFraudRemovalWithdraw",
    "state-queue merge withdrawal": "stateQueueMergeWithdraw",
    "scheduler spending": "schedulerSpend",
    "scheduler minting": "schedulerMint",
    "registered-operators spending": "registeredOperatorsSpend",
    "registered-operators minting": "registeredOperatorsMint",
    "active-operators spending": "activeOperatorsSpend",
    "active-operators minting": "activeOperatorsMint",
    "retired-operators spending": "retiredOperatorsSpend",
    "retired-operators minting": "retiredOperatorsMint",
    "fraud-proof-catalogue minting": "fraudProofCatalogueMint",
    "deposit history retention": "depositHistoryRetentionSpend",
    "deposit history retirement": "depositHistoryRetirementWithdraw",
    "withdrawal history retention": "withdrawalHistoryRetentionSpend",
    "withdrawal history retirement": "withdrawalHistoryRetirementWithdraw",
    "deposit spending": "depositSpend",
    "deposit minting": "depositMint",
    "withdrawal spending": "withdrawalSpend",
    "withdrawal minting": "withdrawalMint",
    "settlement minting": "settlementMint",
    "payout spending": "payoutSpend",
    "payout minting": "payoutMint",
    "reserve spending": "reserveSpend",
    "reserve observer": "reserveWithdraw",
    "membership proof withdrawal": "phasMembershipWithdraw",
    // #579. Mirrors `midgard-core`'s role-map removal; ABI-03 fails closed on
    // divergence.
    "V1 field-preimage certificate": "fieldPreimageCertificateSpend",
    "V1 field-preimage certificate minting": "fieldPreimageCertificateMint",
    "V1 immutable CEK program-material publication": "cekProgramMaterialSpend",
    "V1 validation-trace redeemer item traversal normalizer":
      "validationTraceDisputeRedeemerItemTraversalNormalizer",
    "V1 validation-trace redeemer item outer normalizer":
      "validationTraceDisputeRedeemerItemOuterNormalizer",
    "V1 validation-trace redeemer item source authenticator":
      "validationTraceDisputeRedeemerItemSourceAuthenticator",
    "V1 validation-trace redeemer item fold map executor":
      "validationTraceDisputeRedeemerItemFoldMapExecutor",
    "V1 validation-trace redeemer item finalize frame executor":
      "validationTraceDisputeRedeemerItemFinalizeFrameExecutor",
    "V1 validation-trace redeemer item open header executor":
      "validationTraceDisputeRedeemerItemOpenHeaderExecutor",
    "V1 validation-trace redeemer item open tail executor":
      "validationTraceDisputeRedeemerItemOpenTailExecutor",
    "V1 validation-trace redeemer item head scalar executor":
      "validationTraceDisputeRedeemerItemHeadScalarExecutor",
    "V1 validation-trace redeemer item head sequence executor":
      "validationTraceDisputeRedeemerItemHeadSequenceExecutor",
    "V1 validation-trace redeemer item head map executor":
      "validationTraceDisputeRedeemerItemHeadMapExecutor",
    "V1 validation-trace redeemer item head large constructor executor":
      "validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor",
    "V1 validation-trace redeemer item attach integer executor":
      "validationTraceDisputeRedeemerItemAttachIntegerExecutor",
    "V1 validation-trace redeemer item attach bytes executor":
      "validationTraceDisputeRedeemerItemAttachBytesExecutor",
    "V1 validation-trace redeemer item fold list executor":
      "validationTraceDisputeRedeemerItemFoldListExecutor",
    "V1 validation-trace redeemer item advance integer executor":
      "validationTraceDisputeRedeemerItemAdvanceIntegerExecutor",
    "V1 validation-trace redeemer item advance bytes executor":
      "validationTraceDisputeRedeemerItemAdvanceBytesExecutor",
    "V1 validation-trace redeemer item advance large constructor executor":
      "validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor",
    "V1 validation-trace redeemer item advance large fields executor":
      "validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor",
    "V1 validation-trace redeemer item close executor":
      "validationTraceDisputeRedeemerItemCloseExecutor",
    "V1 validation-trace redeemer item finish data executor":
      "validationTraceDisputeRedeemerItemFinishDataExecutor",
    "V1 validation-trace redeemer item invalid header executor":
      "validationTraceDisputeRedeemerItemInvalidHeaderExecutor",
    "V1 validation-trace redeemer item invalid tail executor":
      "validationTraceDisputeRedeemerItemInvalidTailExecutor",
    "V1 validation-trace redeemer item invalid data executor":
      "validationTraceDisputeRedeemerItemInvalidDataExecutor",
    "V1 validation-trace redeemer item settlement":
      "validationTraceDisputeRedeemerItemSettlement",
    "V1 validation-trace CEK context settle":
      "validationTraceDisputeCekContextSettle",
    "V1 validation-trace CEK context control":
      "validationTraceDisputeCekContextControl",
    "V1 validation-trace CEK context reference":
      "validationTraceDisputeCekContextReference",
    "V1 validation-trace CEK context spend":
      "validationTraceDisputeCekContextSpend",
    "V1 validation-trace CEK context output":
      "validationTraceDisputeCekContextOutput",
    "V1 validation-trace CEK context signer":
      "validationTraceDisputeCekContextSigner",
    "V1 validation-trace CEK context observer authenticate":
      "validationTraceDisputeCekContextObserverAuthenticate",
    "V1 validation-trace CEK context observer fold":
      "validationTraceDisputeCekContextObserverFold",
    "V1 validation-trace CEK context mint init":
      "validationTraceDisputeCekContextMintInit",
    "V1 validation-trace CEK context mint item":
      "validationTraceDisputeCekContextMintItem",
    "V1 validation-trace CEK context assemble":
      "validationTraceDisputeCekContextAssemble",
    "V1 validation-trace CEK context tx info":
      "validationTraceDisputeCekContextTxInfo",
    "V1 validation-trace CEK context seed":
      "validationTraceDisputeCekContextSeed",
    "V1 validation-trace CEK context finalize authenticate":
      "validationTraceDisputeCekContextFinalizeAuthenticate",
    "V1 validation-trace CEK context finalize spend":
      "validationTraceDisputeCekContextFinalizeSpend",
    "V1 validation-trace CEK context finalize mint":
      "validationTraceDisputeCekContextFinalizeMint",
    "V1 validation-trace CEK context finalize withdraw":
      "validationTraceDisputeCekContextFinalizeWithdraw",
    "V1 validation-trace CEK context finalize observe":
      "validationTraceDisputeCekContextFinalizeObserve",
    "V1 validation-trace CEK context finalize midgard":
      "validationTraceDisputeCekContextFinalizeMidgard",
    "V1 validation-trace CEK context redeemer begin":
      "validationTraceDisputeCekContextRedeemerBegin",
    "V1 validation-trace CEK context redeemer select authenticate":
      "validationTraceDisputeCekContextRedeemerSelectAuthenticate",
    "V1 validation-trace CEK context redeemer select initialize":
      "validationTraceDisputeCekContextRedeemerSelectInitialize",
    "V1 validation-trace CEK context redeemer select hash":
      "validationTraceDisputeCekContextRedeemerSelectHash",
    "V1 validation-trace CEK context redeemer select finish":
      "validationTraceDisputeCekContextRedeemerSelectFinish",
    "V1 validation-trace CEK context item bind":
      "validationTraceDisputeCekContextItemBind",
    "V1 validation-trace CEK context item return":
      "validationTraceDisputeCekContextItemReturn",
    "V1 validation-trace CEK context item selection hash":
      "validationTraceDisputeCekContextItemSelectionHash",
    "V1 validation-trace CEK context item data hash":
      "validationTraceDisputeCekContextItemDataHash",
    "V1 validation-trace CEK context item finalize":
      "validationTraceDisputeCekContextItemFinalize",
    "V1 validation-trace CEK context item selection continue":
      "validationTraceDisputeCekContextItemSelectionContinue",
    "V1 validation-trace CEK context item selection finish":
      "validationTraceDisputeCekContextItemSelectionFinish",
    "V1 validation-trace CEK context item data continue":
      "validationTraceDisputeCekContextItemDataContinue",
    "V1 validation-trace CEK context item data finish descriptor":
      "validationTraceDisputeCekContextItemDataFinishDescriptor",
    "V1 validation-trace CEK context item data finish value":
      "validationTraceDisputeCekContextItemDataFinishValue",
    "V1 validation-trace CEK context item entry":
      "validationTraceDisputeCekContextItemEntry",
    "V1 validation-trace CEK context item settlement":
      "validationTraceDisputeCekContextItemSettlement",
    "V1 validation-trace CEK core settle":
      "validationTraceDisputeCekCoreSettle",
    "V1 validation-trace CEK core compute":
      "validationTraceDisputeCekCoreCompute",
    "V1 validation-trace CEK core machine":
      "validationTraceDisputeCekCoreMachine",
    "V1 validation-trace CEK core map conversion":
      "validationTraceDisputeCekCoreMapConversion",
    "V1 validation-trace CEK core direct scalar":
      "validationTraceDisputeCekCoreDirectScalar",
    "V1 validation-trace CEK core direct structured":
      "validationTraceDisputeCekCoreDirectStructured",
    "V1 validation-trace CEK core direct scalar budget":
      "validationTraceDisputeCekCoreDirectScalarBudget",
    "V1 validation-trace CEK core direct structured budget":
      "validationTraceDisputeCekCoreDirectStructuredBudget",
    "V1 validation-trace CEK core direct scalar roots":
      "validationTraceDisputeCekCoreDirectScalarRoots",
    "V1 validation-trace CEK core direct structured roots":
      "validationTraceDisputeCekCoreDirectStructuredRoots",
    "V1 validation-trace CEK core semantic pair":
      "validationTraceDisputeCekCoreSemanticPair",
    "V1 validation-trace CEK core semantic list construct":
      "validationTraceDisputeCekCoreSemanticListConstruct",
    "V1 validation-trace CEK core semantic list select":
      "validationTraceDisputeCekCoreSemanticListSelect",
    "V1 validation-trace CEK core semantic choose":
      "validationTraceDisputeCekCoreSemanticChoose",
    "V1 validation-trace CEK core semantic data construct":
      "validationTraceDisputeCekCoreSemanticDataConstruct",
    "V1 validation-trace CEK core semantic data scalar":
      "validationTraceDisputeCekCoreSemanticDataScalar",
    "V1 validation-trace CEK core semantic data misc":
      "validationTraceDisputeCekCoreSemanticDataMisc",
    "V1 validation-trace CEK core semantic budget":
      "validationTraceDisputeCekCoreSemanticBudget",
    "V1 validation-trace CEK core semantic roots":
      "validationTraceDisputeCekCoreSemanticRoots",
    "V1 validation-trace CEK core semantic result":
      "validationTraceDisputeCekCoreSemanticResult",
    "V1 validation-trace CEK core map start nodes":
      "validationTraceDisputeCekCoreMapStartNodes",
    "V1 validation-trace CEK core map start budget":
      "validationTraceDisputeCekCoreMapStartBudget",
    "V1 validation-trace CEK core map start roots":
      "validationTraceDisputeCekCoreMapStartRoots",
    "V1 validation-trace CEK core semantic failure material":
      "validationTraceDisputeCekCoreSemanticFailureMaterial",
    "V1 validation-trace CEK core semantic failure roots":
      "validationTraceDisputeCekCoreSemanticFailureRoots",
    "V1 validation-trace CEK core bls final":
      "validationTraceDisputeCekCoreBlsFinal",
    "V1 validation-trace CEK core bls budget":
      "validationTraceDisputeCekCoreBlsBudget",
    "V1 validation-trace CEK core bls roots":
      "validationTraceDisputeCekCoreBlsRoots",
    "V1 validation-trace CEK core failure budget":
      "validationTraceDisputeCekCoreFailureBudget",
    "V1 validation-trace CEK core failure known":
      "validationTraceDisputeCekCoreFailureKnown",
    "V1 validation-trace CEK core type failure kinds":
      "validationTraceDisputeCekCoreTypeFailureKinds",
    "V1 validation-trace CEK core type failure roots":
      "validationTraceDisputeCekCoreTypeFailureRoots",
    "V1 validation-trace dispute": "validationTraceDispute",
    "V1 validation-trace source": "validationTraceDisputeSource",
    "V1 validation-trace game": "validationTraceDisputeGame",
    "V1 validation-trace boundary": "validationTraceDisputeBoundary",
    "V1 validation-trace timeout": "validationTraceDisputeTimeout",
    "V1 validation-trace award": "validationTraceDisputeAward",
    "V1 validation-trace canonical-decode prepare":
      "validationTraceDisputeCanonicalDecodePrepare",
    "V1 validation-trace canonical-decode empty semantic":
      "validationTraceDisputeCanonicalDecodeEmptySemantic",
    "V1 validation-trace canonical-decode item semantic":
      "validationTraceDisputeCanonicalDecodeItemSemantic",
    "V1 validation-trace canonical-decode item source":
      "validationTraceDisputeCanonicalDecodeItemSource",
    "V1 validation-trace canonical-decode item observe":
      "validationTraceDisputeCanonicalDecodeItemObserve",
    "V1 validation-trace canonical-decode item proof":
      "validationTraceDisputeCanonicalDecodeItemProof",
    "V1 validation-trace canonical-decode item settlement":
      "validationTraceDisputeCanonicalDecodeItemSettlement",
    "V1 validation-trace proof-item publication":
      "validationTraceDisputeProofItem",
    "V1 fraud-proof fabricated-deposit step-01": "fraudProofFabricatedDeposit",
    "V1 fraud-proof fabricated-deposit step-02":
      "fraudProofFabricatedDepositStep02",
    "V1 fraud-proof fabricated-deposit step-03":
      "fraudProofFabricatedDepositStep03",
    "V1 fraud-proof fabricated-deposit step-04":
      "fraudProofFabricatedDepositStep04",
    "V1 fraud-proof fabricated-withdrawal step-01":
      "fraudProofFabricatedWithdrawal",
    "V1 fraud-proof fabricated-withdrawal step-02":
      "fraudProofFabricatedWithdrawalStep02",
    "V1 fraud-proof fabricated-withdrawal step-03":
      "fraudProofFabricatedWithdrawalStep03",
    "V1 fraud-proof fabricated-withdrawal step-04":
      "fraudProofFabricatedWithdrawalStep04",
    "V1 fraud-proof native-script-decoding step-01":
      "fraudProofNativeScriptDecoding",
    "V1 fraud-proof native-script-decoding step-02":
      "fraudProofNativeScriptDecodingStep02",
    "V1 fraud-proof native-script-decoding step-03 open-subject":
      "fraudProofNativeScriptDecodingStep03OpenSubject",
    "V1 fraud-proof native-script-decoding step-03 bind-descriptor":
      "fraudProofNativeScriptDecodingStep03BindDescriptor",
    "V1 fraud-proof native-script-decoding step-03 advance-or-close":
      "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
    "V1 fraud-proof native-script-decoding step-04":
      "fraudProofNativeScriptDecodingStep04",
    "V1 fraud-proof missing-signature step-01": "fraudProofMissingSignature",
    "V1 fraud-proof missing-signature forced step":
      "fraudProofMissingSignatureForcedStep",
    "V1 fraud-proof missing-signature forced signer":
      "fraudProofMissingSignatureForcedSigner",
    "V1 fraud-proof missing-signature forced witness":
      "fraudProofMissingSignatureForcedWitness",
    "V1 fraud-proof missing-signature step-02":
      "fraudProofMissingSignatureStep02",
    "V1 fraud-proof missing-signature step-03":
      "fraudProofMissingSignatureStep03",
    "V1 fraud-proof missing-signature step-04":
      "fraudProofMissingSignatureStep04",
    "V1 fraud-proof withdrawn-reference-input step-01":
      "fraudProofWithdrawnReferenceInput",
    "V1 fraud-proof withdrawn-reference-input step-02":
      "fraudProofWithdrawnReferenceInputStep02",
    "V1 fraud-proof withdrawn-reference-input step-03":
      "fraudProofWithdrawnReferenceInputStep03",
    "V1 fraud-proof canonical-decodability step-01":
      "fraudProofCanonicalDecodability",
    "V1 fraud-proof canonical-decodability step-02":
      "fraudProofCanonicalDecodabilityStep02",
    "V1 fraud-proof committed-field-shape step-01":
      "fraudProofCommittedFieldShape",
    "V1 fraud-proof committed-field-shape step-02":
      "fraudProofCommittedFieldShapeStep02",
    "V1 fraud-proof min-fee step-01": "fraudProofMinFee",
    "V1 fraud-proof min-fee step-02": "fraudProofMinFeeStep02",
    "V1 fraud-proof withdrawal-mistag step-01": "fraudProofWithdrawalMistag",
    "V1 fraud-proof withdrawal-mistag step-02":
      "fraudProofWithdrawalMistagStep02",
    "V1 fraud-proof withdrawal-mistag step-03":
      "fraudProofWithdrawalMistagStep03",
    "V1 fraud-proof withdrawal-mistag step-04":
      "fraudProofWithdrawalMistagStep04",
    "V1 fraud-proof withdrawal-mistag step-05":
      "fraudProofWithdrawalMistagStep05",
    "V1 fraud-proof double-withdraw step-01": "fraudProofDoubleWithdraw",
    "V1 fraud-proof double-withdraw step-02": "fraudProofDoubleWithdrawStep02",
    "V1 fraud-proof cross-block-duplicate-event step-01":
      "fraudProofCrossBlockDuplicateEvent",
    "V1 fraud-proof cross-block-duplicate-event step-02":
      "fraudProofCrossBlockDuplicateEventStep02",
    "V1 fraud-proof l2-tx-mistag step-01": "fraudProofL2TxMistag",
    "V1 fraud-proof l2-tx-mistag step-02": "fraudProofL2TxMistagStep02",
    "V1 fraud-proof withdrawn-input step-01": "fraudProofWithdrawnInput",
    "V1 fraud-proof withdrawn-input step-02": "fraudProofWithdrawnInputStep02",
    "V1 fraud-proof withdrawn-input step-03": "fraudProofWithdrawnInputStep03",
    "V1 fraud-proof value-not-preserved step-01": "fraudProofValueNotPreserved",
    "V1 fraud-proof value-not-preserved step-02":
      "fraudProofValueNotPreservedStep02",
    "V1 fraud-proof value-not-preserved step-03":
      "fraudProofValueNotPreservedStep03",
    "V1 fraud-proof value-not-preserved step-04":
      "fraudProofValueNotPreservedStep04",
    "value conservation accepted-source":
      "fraudProofValueNotPreservedUnionAcceptedSource",
    "value conservation forced-source":
      "fraudProofValueNotPreservedUnionForcedSource",
    "value conservation event": "fraudProofValueNotPreservedUnionEvent",
    "value conservation pre-state": "fraudProofValueNotPreservedUnionPreState",
    "value conservation inputs": "fraudProofValueNotPreservedUnionInputs",
    "value conservation input-value":
      "fraudProofValueNotPreservedUnionInputValue",
    "value conservation assets": "fraudProofValueNotPreservedUnionAssets",
    "value conservation field-grammar":
      "fraudProofValueNotPreservedUnionFieldGrammar",
    "value conservation outputs": "fraudProofValueNotPreservedUnionOutputs",
    "value conservation output-scan":
      "fraudProofValueNotPreservedUnionOutputScan",
    "value conservation mint": "fraudProofValueNotPreservedUnionMint",
    "value conservation update": "fraudProofValueNotPreservedUnionUpdate",
    "value conservation terminal": "fraudProofValueNotPreservedUnionTerminal",

    "V1 fraud-proof input-set-uniqueness step-01":
      "fraudProofInputSetUniqueness",
    "V1 fraud-proof input-set-uniqueness step-02":
      "fraudProofInputSetUniquenessStep02",
    "V1 fraud-proof input-set-uniqueness step-03":
      "fraudProofInputSetUniquenessStep03",
    "V1 fraud-proof input-set-uniqueness step-04":
      "fraudProofInputSetUniquenessStep04",
    "V1 fraud-proof mint-authorization step-01": "fraudProofMintAuthorization",
    "V1 fraud-proof mint-authorization step-02":
      "fraudProofMintAuthorizationStep02",
    "V1 fraud-proof mint-authorization step-03":
      "fraudProofMintAuthorizationStep03",
    "V1 fraud-proof mint-authorization step-04":
      "fraudProofMintAuthorizationStep04",
    "V1 fraud-proof mint-authorization step-05":
      "fraudProofMintAuthorizationStep05",
    "V1 fraud-proof mint-authorization step-06":
      "fraudProofMintAuthorizationStep06",
    "V1 fraud-proof mint-authorization step-07":
      "fraudProofMintAuthorizationStep07",
    "V1 fraud-proof transition-trace route": "fraudProofTransitionTrace",
    "V1 fraud-proof transition-trace final-0":
      "fraudProofTransitionTraceControl",
    "V1 fraud-proof transition-trace final-1":
      "fraudProofTransitionTraceSource",
    "V1 fraud-proof transition-trace final-2":
      "fraudProofTransitionTraceWithdrawal",
    "V1 fraud-proof transition-trace final-3":
      "fraudProofTransitionTraceForced",
    "V1 fraud-proof transition-trace final-4":
      "fraudProofTransitionTraceAcceptedTransaction",
    "V1 fraud-proof transition-trace final-5":
      "fraudProofTransitionTraceDeposit",
    "V1 fraud-proof transition-trace final-6":
      "fraudProofTransitionTraceL1Event",
    "V1 fraud-proof transition-trace final-7":
      "fraudProofTransitionTraceDuplicate",
    "V1 fraud-proof network-id step-01": "fraudProofNetworkId",
    "V1 fraud-proof network-id step-02": "fraudProofNetworkIdStep02",
    "V1 fraud-proof network-id forced step": "fraudProofNetworkIdForcedStep",
    "V1 fraud-proof network-id forced scan": "fraudProofNetworkIdForcedScan",
    "V1 fraud-proof computation-thread minting": "computationThreadMint",
    "V1 fraud-proof token minting": "fraudProofMint",
    "V1 MPF chunked-verify withdrawal": "chunkedVerifyWithdraw",
    "V1 MPF pexcludes withdrawal": "pexcludesWithdraw",
    "V1 fraud-proof double-spend step-01": "fraudProofDoubleSpend",
    "V1 fraud-proof double-spend step-02": "fraudProofDoubleSpendStep02",
    "V1 fraud-proof double-spend step-03": "fraudProofDoubleSpendStep03",
    "V1 fraud-proof double-spend step-04": "fraudProofDoubleSpendStep04",
    "V1 fraud-proof non-existent-input step-01": "fraudProofNonExistentInput",
    "V1 fraud-proof non-existent-input step-02":
      "fraudProofNonExistentInputStep02",
    "V1 fraud-proof non-existent-input step-03":
      "fraudProofNonExistentInputStep03",
    "V1 fraud-proof non-existent-input step-04":
      "fraudProofNonExistentInputStep04",
    "V1 fraud-proof non-existent-input-no-index step-01":
      "fraudProofNonExistentInputNoIndex",
    "V1 fraud-proof non-existent-input-no-index step-02":
      "fraudProofNonExistentInputNoIndexStep02",
    "V1 fraud-proof non-existent-input-no-index step-03":
      "fraudProofNonExistentInputNoIndexStep03",
    "V1 fraud-proof non-existent-input-no-index step-04":
      "fraudProofNonExistentInputNoIndexStep04",
    "V1 fraud-proof invalid-range step-01": "fraudProofInvalidRange",
    "V1 fraud-proof invalid-range step-02": "fraudProofInvalidRangeStep02",
    "V1 fraud-proof zero-input step-01": "fraudProofZeroInput",
    "V1 fraud-proof zero-input step-02": "fraudProofZeroInputStep02",
    "V1 fraud-proof da-hash-preimage step-01": "fraudProofDaHashPreimage",
    "V1 fraud-proof da-hash-preimage step-02": "fraudProofDaHashPreimageStep02",
    "V1 fraud-proof no-reference-input step-01": "fraudProofNoReferenceInput",
    "V1 fraud-proof no-reference-input step-02":
      "fraudProofNoReferenceInputStep02",
    "V1 fraud-proof no-reference-input step-03":
      "fraudProofNoReferenceInputStep03",
    "V1 fraud-proof no-reference-input step-04":
      "fraudProofNoReferenceInputStep04",
    "V1 fraud-proof reference-input-no-idx step-01":
      "fraudProofReferenceInputNoIdx",
    "V1 fraud-proof reference-input-no-idx step-02":
      "fraudProofReferenceInputNoIdxStep02",
    "V1 fraud-proof reference-input-no-idx step-03":
      "fraudProofReferenceInputNoIdxStep03",
    "V1 fraud-proof reference-input-no-idx step-04":
      "fraudProofReferenceInputNoIdxStep04",
    "V1 fraud-proof invalid-signature step-01": "fraudProofInvalidSignature",
    "V1 fraud-proof invalid-signature step-02":
      "fraudProofInvalidSignatureStep02",
    "V1 fraud-proof native-script-invalid step-01":
      "fraudProofNativeScriptInvalid",
    "V1 fraud-proof native-script-invalid step-02":
      "fraudProofNativeScriptInvalidStep02",
    "V1 fraud-proof native-script-invalid step-03":
      "fraudProofNativeScriptInvalidStep03",
    "V1 fraud-proof native-script-invalid step-04":
      "fraudProofNativeScriptInvalidStep04",
    "V1 fraud-proof native-script-invalid step-05":
      "fraudProofNativeScriptInvalidStep05",
    "V1 fraud-proof min-ada step-01": "fraudProofMinAda",
    "V1 fraud-proof min-ada step-02": "fraudProofMinAdaStep02",
    "V1 validation-trace resolve-inputs Initial semantic":
      "validationTraceDisputeResolveInputsInitialSemantic",
    "V1 validation-trace resolve-inputs Finish semantic":
      "validationTraceDisputeResolveInputsFinishSemantic",
    "V1 validation-trace resolve-inputs MembershipBegin semantic":
      "validationTraceDisputeResolveInputsMembershipBeginSemantic",
    "V1 validation-trace resolve-inputs MembershipStep semantic":
      "validationTraceDisputeResolveInputsMembershipStepSemantic",
    "V1 validation-trace resolve-inputs MembershipFinalize semantic":
      "validationTraceDisputeResolveInputsMembershipFinalizeSemantic",
    "V1 validation-trace resolve-inputs NonMembership semantic":
      "validationTraceDisputeResolveInputsNonMembershipSemantic",
    "V1 validation-trace script-sources NonOutput semantic":
      "validationTraceDisputeScriptSourcesNonOutputSemantic",
    "V1 validation-trace script-sources OutputProofBegin semantic":
      "validationTraceDisputeScriptSourcesOutputProofBeginSemantic",
    "V1 validation-trace script-sources OutputProofStep semantic":
      "validationTraceDisputeScriptSourcesOutputProofStepSemantic",
    "V1 validation-trace script-sources OutputProofFinalize semantic":
      "validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic",
    "V1 validation-trace script-sources OutputProofFinish semantic":
      "validationTraceDisputeScriptSourcesOutputProofFinishSemantic",
    "V1 validation-trace script-sources StageZeroBegin semantic":
      "validationTraceDisputeScriptSourcesStageZeroBeginSemantic",
    "V1 validation-trace script-sources StageZeroFinish semantic":
      "validationTraceDisputeScriptSourcesStageZeroFinishSemantic",
    "V1 validation-trace script-sources StageZeroHashBlock semantic":
      "validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic",
    "V1 validation-trace script-sources StageZeroHashAdvance semantic":
      "validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic",
    "V1 validation-trace script-sources StageZeroHashTerminal semantic":
      "validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic",
    "V1 validation-trace script-sources StageNineMismatch semantic":
      "validationTraceDisputeScriptSourcesStageNineMismatchSemantic",
    "V1 validation-trace script-sources StageNineNativeMatch semantic":
      "validationTraceDisputeScriptSourcesStageNineNativeMatchSemantic",
    "V1 validation-trace script-sources StageNineEffectfulMatch semantic":
      "validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic",
    "V1 validation-trace script-sources StageNineMissing semantic":
      "validationTraceDisputeScriptSourcesStageNineMissingSemantic",
    "V1 validation-trace script-sources StageOneFinish semantic":
      "validationTraceDisputeScriptSourcesStageOneFinishSemantic",
    "V1 validation-trace script-sources StageOneRedeemer semantic":
      "validationTraceDisputeScriptSourcesStageOneRedeemerSemantic",
    "V1 validation-trace script-sources StageElevenFinish semantic":
      "validationTraceDisputeScriptSourcesStageElevenFinishSemantic",
    "V1 validation-trace script-sources StageElevenSource semantic":
      "validationTraceDisputeScriptSourcesStageElevenSourceSemantic",
    "V1 validation-trace script-sources StageTwelveFinish semantic":
      "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic",
    "V1 validation-trace script-sources StageTwelveRedeemer semantic":
      "validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic",
    "V1 validation-trace script-sources StageTenMissing semantic":
      "validationTraceDisputeScriptSourcesStageTenMissingSemantic",
    "V1 validation-trace script-sources StageTenMismatch semantic":
      "validationTraceDisputeScriptSourcesStageTenMismatchSemantic",
    "V1 validation-trace script-sources StageTenMatch semantic":
      "validationTraceDisputeScriptSourcesStageTenMatchSemantic",
    "V1 validation-trace script-sources StageEightFinish semantic":
      "validationTraceDisputeScriptSourcesStageEightFinishSemantic",
    "V1 validation-trace script-sources StageEightPurpose semantic":
      "validationTraceDisputeScriptSourcesStageEightPurposeSemantic",
    "V1 validation-trace script-sources StageSevenObserver semantic":
      "validationTraceDisputeScriptSourcesStageSevenObserverSemantic",
    "V1 validation-trace script-sources StageSevenReceive semantic":
      "validationTraceDisputeScriptSourcesStageSevenReceiveSemantic",
    "V1 validation-trace script-sources StageSevenFinish semantic":
      "validationTraceDisputeScriptSourcesStageSevenFinishSemantic",
    "V1 validation-trace script-sources RedeemerNormalization semantic":
      "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic",
    "V1 validation-trace script-sources StageTwoAdvance yield":
      "validationTraceDisputeScriptSourcesStageTwoAdvanceWithdraw",
    "V1 validation-trace script-sources StageThreeReplay yield":
      "validationTraceDisputeScriptSourcesStageThreeReplayWithdraw",
    "V1 validation-trace script-sources StageThreeFinish yield":
      "validationTraceDisputeScriptSourcesStageThreeFinishWithdraw",
    "V1 validation-trace script-sources StageFourBegin yield":
      "validationTraceDisputeScriptSourcesStageFourBeginWithdraw",
    "V1 validation-trace script-sources StageFourFinish yield":
      "validationTraceDisputeScriptSourcesStageFourFinishWithdraw",
    "V1 validation-trace script-sources StageSixBeginPolicy yield":
      "validationTraceDisputeScriptSourcesStageSixBeginPolicyWithdraw",
    "V1 validation-trace script-sources StageSixFoldAsset yield":
      "validationTraceDisputeScriptSourcesStageSixFoldAssetWithdraw",
    "V1 validation-trace script-sources StageSixFinish yield":
      "validationTraceDisputeScriptSourcesStageSixFinishWithdraw",
    "V1 validation-trace script-sources observer item yield":
      "validationTraceDisputeScriptSourcesObserverItemWithdraw",
    "V1 validation-trace script-sources observer bound yield":
      "validationTraceDisputeScriptSourcesObserverBoundWithdraw",
    "V1 validation-trace script-sources redeemer descriptor yield":
      "validationTraceDisputeScriptSourcesRedeemerDescriptorWithdraw",
    "V1 validation-trace ledger-output-proof structure yield":
      "validationTraceDisputeLedgerOutputProofStructureWithdraw",
    "V1 validation-trace ledger-output-proof value yield":
      "validationTraceDisputeLedgerOutputProofValueWithdraw",
    "V1 validation-trace ledger-output-proof datum fold-map yield":
      "validationTraceDisputeLedgerOutputProofDatumFoldMapWithdraw",
    "V1 validation-trace ledger-output-proof datum finalize-frame yield":
      "validationTraceDisputeLedgerOutputProofDatumFinalizeFrameWithdraw",
    "V1 validation-trace ledger-output-proof datum head-scalar yield":
      "validationTraceDisputeLedgerOutputProofDatumHeadScalarWithdraw",
    "V1 validation-trace ledger-output-proof datum attach-integer yield":
      "validationTraceDisputeLedgerOutputProofDatumAttachIntegerWithdraw",
    "V1 validation-trace ledger-output-proof datum fold-list yield":
      "validationTraceDisputeLedgerOutputProofDatumFoldListWithdraw",
    "V1 validation-trace ledger-output-proof datum advance-integer yield":
      "validationTraceDisputeLedgerOutputProofDatumAdvanceIntegerWithdraw",
    "V1 validation-trace ledger-output-proof reference-script yield":
      "validationTraceDisputeLedgerOutputProofReferenceScriptWithdraw",
    "V1 validation-trace ledger-output-proof script-hash yield":
      "validationTraceDisputeLedgerOutputProofScriptHashWithdraw",
    "V1 validation-trace ledger-output-proof native-script yield":
      "validationTraceDisputeLedgerOutputProofNativeScriptWithdraw",
    "V1 validation-trace ledger-output-proof structure assets yield":
      "validationTraceDisputeLedgerOutputProofStructureAssetsWithdraw",
    "V1 validation-trace ledger-output-proof structure optional yield":
      "validationTraceDisputeLedgerOutputProofStructureOptionalWithdraw",
    "V1 validation-trace ledger-output-proof structure finish yield":
      "validationTraceDisputeLedgerOutputProofStructureFinishWithdraw",
    "V1 validation-trace ledger-output-proof datum head-sequence yield":
      "validationTraceDisputeLedgerOutputProofDatumHeadSequenceWithdraw",
    "V1 validation-trace ledger-output-proof datum head-map yield":
      "validationTraceDisputeLedgerOutputProofDatumHeadMapWithdraw",
    "V1 validation-trace ledger-output-proof datum head-large-constructor yield":
      "validationTraceDisputeLedgerOutputProofDatumHeadLargeConstructorWithdraw",
    "V1 validation-trace ledger-output-proof datum attach-bytes yield":
      "validationTraceDisputeLedgerOutputProofDatumAttachBytesWithdraw",
    "V1 validation-trace ledger-output-proof datum advance-bytes yield":
      "validationTraceDisputeLedgerOutputProofDatumAdvanceBytesWithdraw",
    "V1 validation-trace ledger-output-proof datum finish yield":
      "validationTraceDisputeLedgerOutputProofDatumFinishWithdraw",
    "V1 validation-trace ledger-output-proof datum large-constructor yield":
      "validationTraceDisputeLedgerOutputProofDatumLargeConstructorWithdraw",
    "V1 validation-trace ledger-output-proof datum large-fields yield":
      "validationTraceDisputeLedgerOutputProofDatumLargeFieldsWithdraw",
    "V1 validation-trace ledger-output-proof datum close yield":
      "validationTraceDisputeLedgerOutputProofDatumCloseWithdraw",
    "V1 validation-trace ledger-output-proof span yield":
      "validationTraceDisputeLedgerOutputProofSpanWithdraw",
    "V1 validation-trace ledger-output-proof scalar-integer yield":
      "validationTraceDisputeLedgerOutputProofScalarIntegerWithdraw",
    "V1 validation-trace ledger-output-proof scalar-bytes yield":
      "validationTraceDisputeLedgerOutputProofScalarBytesWithdraw",
    "V1 validation-trace ledger-output-descriptor scan-facts yield":
      "validationTraceDisputeLedgerOutputDescriptorScanFactsWithdraw",
    "V1 validation-trace ledger-output-descriptor reference-script yield":
      "validationTraceDisputeLedgerOutputDescriptorReferenceScriptWithdraw",
    "V1 validation-trace ledger-output-descriptor datum-summary yield":
      "validationTraceDisputeLedgerOutputDescriptorDatumSummaryWithdraw",
    "V1 validation-trace ledger-output-descriptor value-summary yield":
      "validationTraceDisputeLedgerOutputDescriptorValueSummaryWithdraw",
    "V1 validation-trace phase-A native Advance semantic":
      "validationTraceDisputePhaseANativeScriptsAdvanceSemantic",
    "V1 validation-trace phase-A native Item semantic":
      "validationTraceDisputePhaseANativeScriptsItemSemantic",
    "V1 validation-trace phase-A native TokenHead semantic":
      "validationTraceDisputePhaseANativeScriptsTokenHeadSemantic",
    "V1 validation-trace phase-A native AllOrAnyContainerFramePayload semantic":
      "validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic",
    "V1 validation-trace phase-A native AllOrAnyEmptyContainerPayload semantic":
      "validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic",
    "V1 validation-trace phase-A native AtLeastContainerFramePayload semantic":
      "validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic",
    "V1 validation-trace phase-A native AtLeastEmptyContainerPayload semantic":
      "validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic",
    "V1 validation-trace phase-A native TimelockPayload semantic":
      "validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic",
    "V1 validation-trace phase-A native SignatureMembershipPayload semantic":
      "validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic",
    "V1 validation-trace phase-A native SignatureEmptyPayload semantic":
      "validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic",
    "V1 validation-trace phase-A native SignatureBelowFirstPayload semantic":
      "validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic",
    "V1 validation-trace phase-A native SignatureAboveLastPayload semantic":
      "validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic",
    "V1 validation-trace phase-A native SignatureBetweenPayload semantic":
      "validationTraceDisputePhaseANativeScriptsSignatureBetweenPayloadSemantic",
    "V1 validation-trace phase-A native Frame semantic":
      "validationTraceDisputePhaseANativeScriptsFrameSemantic",
    "V1 validation-trace phase-A preconditions Finalize semantic":
      "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
    "V1 validation-trace phase-A preconditions Item semantic":
      "validationTraceDisputePhaseAScriptPreconditionsItemSemantic",
    "V1 validation-trace value-and-mint asset-fold yield":
      "validationTraceDisputeValueAndMintAssetFoldWithdraw",
    "V1 validation-trace phase-A native item native yield":
      "validationTraceDisputePhaseANativeItemNativeWithdraw",
    "V1 validation-trace phase-A native item foreign yield":
      "validationTraceDisputePhaseANativeItemForeignWithdraw",
    "V1 validation-trace CEK material traversal":
      "validationTraceDisputeCekMaterialTraversal",
    "V1 validation-trace CEK material program task yield":
      "validationTraceDisputeCekMaterialProgramTaskWithdraw",
    "V1 validation-trace CEK material Data task yield":
      "validationTraceDisputeCekMaterialDataTaskWithdraw",
    "V1 validation-trace CEK selection authenticate yield":
      "validationTraceDisputeCekSelectionAuthenticateWithdraw",
    "V1 validation-trace CEK selection successor yield":
      "validationTraceDisputeCekSelectionSuccessorWithdraw",
    "V1 validation-trace CEK selection material program yield":
      "validationTraceDisputeCekSelectionMaterialProgramWithdraw",
    "V1 validation-trace CEK selection material data yield":
      "validationTraceDisputeCekSelectionMaterialDataWithdraw",
    "V1 fraud-proof transition-trace final-5 replay yield":
      "fraudProofTransitionTraceDepositReplayWithdraw",
    "V1 fraud-proof min-ada step-02 tx yield":
      "fraudProofMinAdaStep02TxWithdraw",
    "V1 fraud-proof transition-trace final-4 L2 assembly yield":
      "fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw",
    "V1 fraud-proof transition-trace final-4 L2 scan yield":
      "fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw",
    "V1 fraud-proof transition-trace final-4 L2 value yield":
      "fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw",
    "V1 fraud-proof transition-trace final-5 assembly yield":
      "fraudProofTransitionTraceDepositAssemblyWithdraw",
    "V1 fraud-proof transition-trace final-5 scan yield":
      "fraudProofTransitionTraceDepositScanWithdraw",
    "V1 fraud-proof transition-trace final-5 value yield":
      "fraudProofTransitionTraceDepositValueWithdraw",

    "V1 fraud-proof transition-trace final-4 L2 open yield":
      "fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw",
    "V1 fraud-proof transition-trace final-4 L2 summaries yield":
      "fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw",
    "V1 fraud-proof transition-trace final-4 L2 replay yield":
      "fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw",
    "V1 fraud-proof transition-trace final-4 claim structure yield":
      "fraudProofTransitionTraceAcceptedTransactionClaimStructureWithdraw",
    "V1 fraud-proof transition-trace final-4 claim source yield":
      "fraudProofTransitionTraceAcceptedTransactionClaimSourceWithdraw",
    "V1 fraud-proof transition-trace final-4 claim endpoints yield":
      "fraudProofTransitionTraceAcceptedTransactionClaimEndpointsWithdraw",
    "V1 fraud-proof transition-trace final-5 projection yield":
      "fraudProofTransitionTraceDepositProjectionWithdraw",
    "V1 fraud-proof transition-trace final-6 L1 event yield":
      "fraudProofTransitionTraceL1EventWithdraw",
    "V1 fraud-proof transition-trace final-6 forced timing yield":
      "fraudProofTransitionTraceForcedTimingWithdraw",
    "V1 fraud-proof transition-trace final-5 summaries yield":
      "fraudProofTransitionTraceDepositSummariesWithdraw",

    "V1 fraud-proof min-ada step-02 UTxO yield":
      "fraudProofMinAdaStep02UtxoWithdraw",
    "correction-lock spending": "correctionLockSpend",
    "V1 fraud-proof min-ada step-03": "fraudProofMinAdaStep03",
    "V1 fraud-proof min-ada step-04": "fraudProofMinAdaStep04",
    "V1 fraud-proof min-ada step-05": "fraudProofMinAdaStep05",
    "V1 fraud-proof field-preimage-length-mismatch step-01":
      "fraudProofFieldPreimageLengthMismatch",
    "V1 fraud-proof field-preimage-length-mismatch step-02 accepted":
      "fraudProofFieldPreimageLengthMismatchStep02Accepted",
    "V1 fraud-proof field-preimage-length-mismatch step-02 forced":
      "fraudProofFieldPreimageLengthMismatchStep02Forced",
    "V1 fraud-proof field-preimage-length-mismatch step-03":
      "fraudProofFieldPreimageLengthMismatchStep03",
    "V1 fraud-proof field-item-width-illegal step-01":
      "fraudProofFieldItemWidthIllegal",
    "V1 fraud-proof field-item-width-illegal step-02":
      "fraudProofFieldItemWidthIllegalStep02",
    "V1 fraud-proof field-item-width-illegal step-03":
      "fraudProofFieldItemWidthIllegalStep03",
    "V1 fraud-proof witness-script-decoding step-01":
      "fraudProofWitnessScriptDecoding",
    "V1 fraud-proof witness-script-decoding step-02":
      "fraudProofWitnessScriptDecodingStep02",
    "V1 fraud-proof witness-script-decoding step-03":
      "fraudProofWitnessScriptDecodingStep03",
    "V1 fraud-proof witness-script-decoding step-04":
      "fraudProofWitnessScriptDecodingStep04",
    "V1 fraud-proof script-integrity-hash-missing step-01":
      "fraudProofScriptIntegrityHashMissing",
    "V1 fraud-proof script-integrity-hash-missing step-02":
      "fraudProofScriptIntegrityHashMissingStep02",
    "V1 fraud-proof script-integrity-hash-missing step-03":
      "fraudProofScriptIntegrityHashMissingStep03",
    "V1 fraud-proof script-integrity-hash-missing script-grammar":
      "fraudProofScriptIntegrityHashMissingScriptGrammar",
    "V1 fraud-proof script-integrity-hash-missing script-scan":
      "fraudProofScriptIntegrityHashMissingScriptScan",
    "V1 fraud-proof script-integrity-hash-missing redeemer-grammar":
      "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
    "V1 fraud-proof script-integrity-hash-missing step-04":
      "fraudProofScriptIntegrityHashMissingStep04",
    "V1 fraud-proof transaction-output-non-canonical step-01":
      "fraudProofTransactionOutputNonCanonical",
    "V1 fraud-proof mint-item-non-canonical step-01":
      "fraudProofMintItemNonCanonical",
    "V1 fraud-proof transaction-output-non-canonical step-02":
      "fraudProofTransactionOutputNonCanonicalStep02",
    "V1 fraud-proof mint-item-non-canonical step-02":
      "fraudProofMintItemNonCanonicalStep02",
    "V1 fraud-proof transaction-output-non-canonical step-03":
      "fraudProofTransactionOutputNonCanonicalStep03",
    "V1 fraud-proof mint-item-non-canonical step-03":
      "fraudProofMintItemNonCanonicalStep03",
    "V1 fraud-proof transaction-output-non-canonical step-04":
      "fraudProofTransactionOutputNonCanonicalStep04",
    "V1 fraud-proof mint-item-non-canonical step-04":
      "fraudProofMintItemNonCanonicalStep04",
    "V1 fraud-proof resolved-output-non-canonical step-01":
      "fraudProofResolvedOutputNonCanonical",
    "V1 fraud-proof resolved-output-non-canonical step-02":
      "fraudProofResolvedOutputNonCanonicalStep02",
    "V1 fraud-proof resolved-output-non-canonical step-03":
      "fraudProofResolvedOutputNonCanonicalStep03",
    "V1 fraud-proof resolved-output-non-canonical step-04":
      "fraudProofResolvedOutputNonCanonicalStep04",
    "V1 fraud-proof resolved-output-non-canonical step-05":
      "fraudProofResolvedOutputNonCanonicalStep05",
    "V1 fraud-proof mint-declared-asset-limit step-01":
      "fraudProofMintDeclaredAssetLimit",
    "V1 fraud-proof mint-declared-asset-limit step-02":
      "fraudProofMintDeclaredAssetLimitStep02",
    "V1 fraud-proof mint-declared-asset-limit step-03":
      "fraudProofMintDeclaredAssetLimitStep03",
    "V1 fraud-proof mint-declared-asset-limit step-04":
      "fraudProofMintDeclaredAssetLimitStep04",
    "V1 fraud-proof spend-input-signer-missing step-01":
      "fraudProofSpendInputSignerMissing",
    "V1 fraud-proof spend-input-signer-missing step-02":
      "fraudProofSpendInputSignerMissingStep02",
    "V1 fraud-proof spend-input-signer-missing step-03":
      "fraudProofSpendInputSignerMissingStep03",
    "V1 fraud-proof spend-input-signer-missing step-04":
      "fraudProofSpendInputSignerMissingStep04",
    "V1 fraud-proof spend-input-signer-missing step-05":
      "fraudProofSpendInputSignerMissingStep05",
    "V1 fraud-proof protected-output-signer-missing step-01":
      "fraudProofProtectedOutputSignerMissing",
    "V1 fraud-proof protected-output-signer-missing step-02":
      "fraudProofProtectedOutputSignerMissingStep02",
    "V1 fraud-proof protected-output-signer-missing step-03":
      "fraudProofProtectedOutputSignerMissingStep03",
    "V1 fraud-proof protected-output-signer-missing step-04":
      "fraudProofProtectedOutputSignerMissingStep04",
    "V1 fraud-proof protected-output-signer-missing step-05":
      "fraudProofProtectedOutputSignerMissingStep05",
    "V1 fraud-proof observers-forbidden-on-untagged-network step-01":
      "fraudProofObserversForbiddenOnUntaggedNetwork",
    "V1 fraud-proof observers-forbidden-on-untagged-network step-02":
      "fraudProofObserversForbiddenOnUntaggedNetworkStep02",
    "V1 fraud-proof output-reference-script-decoding step-01":
      "fraudProofOutputReferenceScriptDecoding",
    "V1 fraud-proof output-reference-script-decoding step-02":
      "fraudProofOutputReferenceScriptDecodingStep02",
    "V1 fraud-proof output-reference-script-decoding step-03":
      "fraudProofOutputReferenceScriptDecodingStep03",
    "V1 fraud-proof output-reference-script-decoding step-04":
      "fraudProofOutputReferenceScriptDecodingStep04",
    "V1 fraud-proof output-reference-script-decoding step-05":
      "fraudProofOutputReferenceScriptDecodingStep05",
    "V1 fraud-proof output-reference-script-decoding step-06":
      "fraudProofOutputReferenceScriptDecodingStep06",
    "V1 fraud-proof execution-source-script-decoding step-01":
      "fraudProofExecutionSourceScriptDecoding",
    "V1 fraud-proof execution-source-script-decoding step-02":
      "fraudProofExecutionSourceScriptDecodingStep02",
    "V1 fraud-proof execution-source-script-decoding step-03":
      "fraudProofExecutionSourceScriptDecodingStep03",
    "V1 fraud-proof execution-source-script-decoding step-04":
      "fraudProofExecutionSourceScriptDecodingStep04",
    "V1 fraud-proof execution-source-script-decoding step-05":
      "fraudProofExecutionSourceScriptDecodingStep05",
    "V1 fraud-proof observer-order-invalid step-01":
      "fraudProofObserverOrderInvalid",
    "V1 fraud-proof observer-order-invalid step-02":
      "fraudProofObserverOrderInvalidStep02",
    "V1 fraud-proof observer-order-invalid step-03":
      "fraudProofObserverOrderInvalidStep03",
    "V1 fraud-proof observer-order-invalid step-04":
      "fraudProofObserverOrderInvalidStep04",
    "V1 fraud-proof redeemer-canonicity step-01":
      "fraudProofRedeemerCanonicity",
    "V1 fraud-proof redeemer-canonicity step-02":
      "fraudProofRedeemerCanonicityStep02",
    "V1 fraud-proof redeemer-canonicity step-03":
      "fraudProofRedeemerCanonicityStep03",
    "V1 fraud-proof receive-purpose-language step-01":
      "fraudProofReceivePurposeLanguage",
    "V1 fraud-proof receive-purpose-language step-02":
      "fraudProofReceivePurposeLanguageStep02",
    "V1 fraud-proof receive-purpose-language step-03":
      "fraudProofReceivePurposeLanguageStep03",
    "V1 fraud-proof unused-script-witness step-01":
      "fraudProofUnusedScriptWitness",
    "V1 fraud-proof unused-script-witness step-02":
      "fraudProofUnusedScriptWitnessStep02",
    "V1 fraud-proof unused-script-witness step-03":
      "fraudProofUnusedScriptWitnessStep03",
    "V1 fraud-proof unused-script-witness step-04":
      "fraudProofUnusedScriptWitnessStep04",
    "V1 fraud-proof unused-script-witness step-05":
      "fraudProofUnusedScriptWitnessStep05",
    "V1 fraud-proof unused-script-witness step-06":
      "fraudProofUnusedScriptWitnessStep06",
    "V1 fraud-proof missing-script-source step-01":
      "fraudProofMissingScriptSource",
    "V1 fraud-proof missing-script-source step-02":
      "fraudProofMissingScriptSourceStep02",
    "V1 fraud-proof missing-script-source step-03":
      "fraudProofMissingScriptSourceStep03",
    "V1 fraud-proof missing-script-source step-04":
      "fraudProofMissingScriptSourceStep04",
    "V1 fraud-proof missing-script-source step-05":
      "fraudProofMissingScriptSourceStep05",
    "V1 fraud-proof missing-script-source step-06":
      "fraudProofMissingScriptSourceStep06",
    "V1 fraud-proof missing-redeemer step-01": "fraudProofMissingRedeemer",
    "V1 fraud-proof missing-redeemer step-02":
      "fraudProofMissingRedeemerStep02",
    "V1 fraud-proof missing-redeemer step-02a":
      "fraudProofMissingRedeemerStep02a",
    "V1 fraud-proof missing-redeemer step-02b":
      "fraudProofMissingRedeemerStep02b",
    "V1 fraud-proof missing-redeemer step-03":
      "fraudProofMissingRedeemerStep03",
    "V1 fraud-proof missing-redeemer step-04":
      "fraudProofMissingRedeemerStep04",
    "V1 fraud-proof missing-redeemer step-05":
      "fraudProofMissingRedeemerStep05",
    "V1 fraud-proof unused-redeemer step-01": "fraudProofUnusedRedeemer",
    "V1 fraud-proof unused-redeemer step-02": "fraudProofUnusedRedeemerStep02",
    "V1 fraud-proof unused-redeemer step-02a":
      "fraudProofUnusedRedeemerStep02a",
    "V1 fraud-proof unused-redeemer step-02b":
      "fraudProofUnusedRedeemerStep02b",
    "V1 fraud-proof unused-redeemer step-02c":
      "fraudProofUnusedRedeemerStep02c",
    "V1 fraud-proof unused-redeemer step-03": "fraudProofUnusedRedeemerStep03",
    "V1 fraud-proof unused-redeemer step-04": "fraudProofUnusedRedeemerStep04",
    "V1 fraud-proof unused-redeemer step-05": "fraudProofUnusedRedeemerStep05",
    "V1 fraud-proof unused-redeemer step-06": "fraudProofUnusedRedeemerStep06",
    "V1 fraud-proof execution-native-script-invalid step-01":
      "fraudProofExecutionNativeScriptInvalid",
    "V1 fraud-proof execution-native-script-invalid step-02":
      "fraudProofExecutionNativeScriptInvalidStep02",
    "V1 fraud-proof execution-native-script-invalid step-03":
      "fraudProofExecutionNativeScriptInvalidStep03",
    "V1 fraud-proof execution-native-script-invalid step-04":
      "fraudProofExecutionNativeScriptInvalidStep04",
    "V1 fraud-proof execution-native-script-invalid step-05":
      "fraudProofExecutionNativeScriptInvalidStep05",
    "V1 fraud-proof execution-native-script-invalid step-06":
      "fraudProofExecutionNativeScriptInvalidStep06",
    "V1 fraud-proof execution-native-script-invalid accepted-reconstruction-init":
      "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
    "V1 fraud-proof execution-native-script-invalid accepted-spend-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-mint-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-observer-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-receive-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-inline-source":
      "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
    "V1 fraud-proof execution-native-script-invalid accepted-reference-source":
      "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
    "V1 fraud-proof script-integrity-hash-mismatch step-01":
      "fraudProofScriptIntegrityHashMismatch",
    "V1 fraud-proof script-integrity-hash-mismatch step-02":
      "fraudProofScriptIntegrityHashMismatchStep02",
    "V1 fraud-proof script-integrity-hash-mismatch step-03":
      "fraudProofScriptIntegrityHashMismatchStep03",
    "V1 fraud-proof script-integrity-hash-mismatch step-04":
      "fraudProofScriptIntegrityHashMismatchStep04",
    "V1 fraud-proof script-integrity-hash-mismatch step-05":
      "fraudProofScriptIntegrityHashMismatchStep05",
    "V1 fraud-proof distinct-asset-accumulation-limit step-01":
      "fraudProofDistinctAssetAccumulationLimit",
    "V1 fraud-proof distinct-asset-accumulation-limit step-02":
      "fraudProofDistinctAssetAccumulationLimitStep02",
    "V1 fraud-proof distinct-asset-accumulation-limit step-03":
      "fraudProofDistinctAssetAccumulationLimitStep03",
    "V1 fraud-proof distinct-asset-accumulation-limit step-04":
      "fraudProofDistinctAssetAccumulationLimitStep04",
    "V1 fraud-proof distinct-asset-accumulation-limit step-05":
      "fraudProofDistinctAssetAccumulationLimitStep05",
    "V1 fraud-proof distinct-asset-accumulation-limit step-06":
      "fraudProofDistinctAssetAccumulationLimitStep06",
    "availability-challenge spending": "availabilityChallengeSpend",
    "availability-challenge minting": "availabilityChallengeMint",
    "availability-challenge open withdrawal":
      "availabilityChallengeOpenWithdraw",
    "availability-challenge settle withdrawal":
      "availabilityChallengeSettleWithdraw",
    "availability-challenge close withdrawal":
      "availabilityChallengeCloseWithdraw",
    "availability-challenge timeout withdrawal":
      "availabilityChallengeTimeoutWithdraw",
  } as const);
