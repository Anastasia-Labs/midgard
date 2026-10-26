export const DEPLOYMENT_MANIFEST_BASE_REFERENCE_TOKEN_NAMES = Object.freeze({
  "V1 validation-trace resolve-inputs Initial semantic": "V1VtRiInitial",
  "V1 validation-trace resolve-inputs Finish semantic": "V1VtRiFinish",
  "V1 validation-trace resolve-inputs MembershipBegin semantic":
    "V1VtRiMembershipBegin",
  "V1 validation-trace resolve-inputs MembershipStep semantic":
    "V1VtRiMembershipStep",
  "V1 validation-trace resolve-inputs MembershipFinalize semantic":
    "V1VtRiMembershipFinalize",
  "V1 validation-trace resolve-inputs NonMembership semantic":
    "V1VtRiNonMembership",
  "V1 validation-trace script-sources NonOutput semantic": "V1VtSsNonOutput",
  "V1 validation-trace script-sources OutputProofBegin semantic":
    "V1VtSsOutputProofBegin",
  "V1 validation-trace script-sources OutputProofStep semantic":
    "V1VtSsOutputProofStep",
  "V1 validation-trace script-sources OutputProofFinalize semantic":
    "V1VtSsOutputProofFinalize",
  "V1 validation-trace script-sources OutputProofFinish semantic":
    "V1VtSsOutputProofFinish",
  "V1 validation-trace script-sources StageZeroBegin semantic": "V1VtSsS0Begin",
  "V1 validation-trace script-sources StageZeroFinish semantic":
    "V1VtSsS0Finish",
  "V1 validation-trace script-sources StageZeroHashBlock semantic":
    "V1VtSsS0HashBlock",
  "V1 validation-trace script-sources StageZeroHashAdvance semantic":
    "V1VtSsS0HashAdvance",
  "V1 validation-trace script-sources StageZeroHashTerminal semantic":
    "V1VtSsS0HashTerminal",
  "V1 validation-trace script-sources StageNineMismatch semantic":
    "V1VtSsS9Mismatch",
  "V1 validation-trace script-sources StageNineNativeMatch semantic":
    "V1VtSsS9NativeMatch",
  "V1 validation-trace script-sources StageNineEffectfulMatch semantic":
    "V1VtSsS9EffectfulMatch",
  "V1 validation-trace script-sources StageNineMissing semantic":
    "V1VtSsS9Missing",
  "V1 validation-trace script-sources StageOneFinish semantic":
    "V1VtSsS1Finish",
  "V1 validation-trace script-sources StageOneRedeemer semantic":
    "V1VtSsS1Redeemer",
  "V1 validation-trace script-sources StageElevenFinish semantic":
    "V1VtSsS11Finish",
  "V1 validation-trace script-sources StageElevenSource semantic":
    "V1VtSsS11Source",
  "V1 validation-trace script-sources StageTwelveFinish semantic":
    "V1VtSsS12Finish",
  "V1 validation-trace script-sources StageTwelveRedeemer semantic":
    "V1VtSsS12Redeemer",
  "V1 validation-trace script-sources StageTenMissing semantic":
    "V1VtSsS10Missing",
  "V1 validation-trace script-sources StageTenMismatch semantic":
    "V1VtSsS10Mismatch",
  "V1 validation-trace script-sources StageTenMatch semantic": "V1VtSsS10Match",
  "V1 validation-trace script-sources StageEightFinish semantic":
    "V1VtSsS8Finish",
  "V1 validation-trace script-sources StageEightPurpose semantic":
    "V1VtSsS8Purpose",
  "V1 validation-trace script-sources StageSevenObserver semantic":
    "V1VtSsS7Observer",
  "V1 validation-trace script-sources StageSevenReceive semantic":
    "V1VtSsS7Receive",
  "V1 validation-trace script-sources StageSevenFinish semantic":
    "V1VtSsS7Finish",
  "V1 validation-trace redeemer item traversal normalizer":
    "V1VtRiTraversalNorm",
  "V1 validation-trace redeemer item outer normalizer": "V1VtRiOuterNorm",
  "V1 validation-trace redeemer item source authenticator": "V1VtRiSource",
  "V1 validation-trace redeemer item fold map executor": "V1VtRiFoldMap",
  "V1 validation-trace redeemer item finalize frame executor":
    "V1VtRiFinalizeFrame",
  "V1 validation-trace redeemer item open header executor": "V1VtRiOpenHeader",
  "V1 validation-trace redeemer item open tail executor": "V1VtRiOpenTail",
  "V1 validation-trace redeemer item head scalar executor": "V1VtRiHeadScalar",
  "V1 validation-trace redeemer item head sequence executor":
    "V1VtRiHeadSequence",
  "V1 validation-trace redeemer item head map executor": "V1VtRiHeadMap",
  "V1 validation-trace redeemer item head large constructor executor":
    "V1VtRiHeadLargeCtor",
  "V1 validation-trace redeemer item attach integer executor":
    "V1VtRiAttachInteger",
  "V1 validation-trace redeemer item attach bytes executor":
    "V1VtRiAttachBytes",
  "V1 validation-trace redeemer item fold list executor": "V1VtRiFoldList",
  "V1 validation-trace redeemer item advance integer executor":
    "V1VtRiAdvanceInteger",
  "V1 validation-trace redeemer item advance bytes executor":
    "V1VtRiAdvanceBytes",
  "V1 validation-trace redeemer item advance large constructor executor":
    "V1VtRiAdvanceLargeCtor",
  "V1 validation-trace redeemer item advance large fields executor":
    "V1VtRiAdvanceLargeFields",
  "V1 validation-trace redeemer item close executor": "V1VtRiClose",
  "V1 validation-trace redeemer item finish data executor": "V1VtRiFinishData",
  "V1 validation-trace redeemer item invalid header executor":
    "V1VtRiInvalidHeader",
  "V1 validation-trace redeemer item invalid tail executor":
    "V1VtRiInvalidTail",
  "V1 validation-trace redeemer item invalid data executor":
    "V1VtRiInvalidData",
  "V1 validation-trace redeemer item invalid scalar data executor":
    "V1VtRiInvalidScalarData",
  "V1 validation-trace redeemer item settlement": "V1VtRiSettlement",
  "V1 validation-trace script-sources RedeemerNormalization semantic":
    "V1VtSsRedeemerNorm",
  "V1 validation-trace script-sources StageTwoAdvance yield":
    "V1VtSsStage2AdvanceYield",
  "V1 validation-trace script-sources StageThreeReplay yield":
    "V1VtSsStage3ReplayYield",
  "V1 validation-trace script-sources StageThreeFinish yield":
    "V1VtSsStage3FinishYield",
  "V1 validation-trace script-sources StageFourBegin yield":
    "V1VtSsStage4BeginYield",
  "V1 validation-trace script-sources StageFourFinish yield":
    "V1VtSsStage4FinishYield",
  "V1 validation-trace script-sources StageSixBeginPolicy yield":
    "V1VtSsStage6BeginPolicyYield",
  "V1 validation-trace script-sources StageSixFoldAsset yield":
    "V1VtSsStage6FoldAssetYield",
  "V1 validation-trace script-sources StageSixFinish yield":
    "V1VtSsStage6FinishYield",
  "V1 validation-trace script-sources observer item yield":
    "V1VtSsS07ObserverItemYield",
  "V1 validation-trace script-sources observer bound yield":
    "V1VtSsS07ObserverBoundYield",
  "V1 validation-trace script-sources redeemer descriptor yield":
    "V1VtSsRedeemerItemStepYield",
  "V1 validation-trace ledger-output-proof structure yield":
    "V1VtLopStructureYield",
  "V1 validation-trace ledger-output-proof value yield": "V1VtLopValueYield",
  "V1 validation-trace ledger-output-proof datum fold-map yield":
    "V1VtLopDatumFoldMapYield",
  "V1 validation-trace ledger-output-proof datum finalize-frame yield":
    "V1VtLopDatumFinalizeFrameYield",
  "V1 validation-trace ledger-output-proof datum head-scalar yield":
    "V1VtLopDatumHeadScalarYield",
  "V1 validation-trace ledger-output-proof datum attach-integer yield":
    "V1VtLopDatumAttachIntegerYield",
  "V1 validation-trace ledger-output-proof datum fold-list yield":
    "V1VtLopDatumFoldListYield",
  "V1 validation-trace ledger-output-proof datum advance-integer yield":
    "V1VtLopDatumAdvanceIntegerYield",
  "V1 validation-trace ledger-output-proof reference-script yield":
    "V1VtLopRefScriptYield",
  "V1 validation-trace ledger-output-proof script-hash yield":
    "V1VtLopScriptHashYield",
  "V1 validation-trace ledger-output-proof native-script yield":
    "V1VtLopNativeScriptYield",
  "V1 validation-trace ledger-output-proof structure assets yield":
    "V1VtLopStructureAssetsYield",
  "V1 validation-trace ledger-output-proof structure optional yield":
    "V1VtLopStructureOptionalYield",
  "V1 validation-trace ledger-output-proof structure finish yield":
    "V1VtLopStructureFinishYield",
  "V1 validation-trace ledger-output-proof datum head-sequence yield":
    "V1VtLopDatumHeadSequenceYield",
  "V1 validation-trace ledger-output-proof datum head-map yield":
    "V1VtLopDatumHeadMapYield",
  "V1 validation-trace ledger-output-proof datum head-large-constructor yield":
    "V1VtLopDatumHeadLargeCtorYield",
  "V1 validation-trace ledger-output-proof datum attach-bytes yield":
    "V1VtLopDatumAttachBytesYield",
  "V1 validation-trace ledger-output-proof datum advance-bytes yield":
    "V1VtLopDatumAdvanceBytesYield",
  "V1 validation-trace ledger-output-proof datum finish yield":
    "V1VtLopDatumFinishYield",
  "V1 validation-trace ledger-output-proof datum large-constructor yield":
    "V1VtLopDatumLargeCtorYield",
  "V1 validation-trace ledger-output-proof datum large-fields yield":
    "V1VtLopDatumLargeFieldsYield",
  "V1 validation-trace ledger-output-proof datum close yield":
    "V1VtLopDatumCloseYield",
  "V1 validation-trace ledger-output-proof span yield": "V1VtLopSpanYield",
  "V1 validation-trace ledger-output-proof scalar-integer yield":
    "V1VtLopScalarIntegerYield",
  "V1 validation-trace ledger-output-proof scalar-bytes yield":
    "V1VtLopScalarBytesYield",
  "V1 validation-trace ledger-output-descriptor scan-facts yield":
    "V1VtLopDescScanFactsYield",
  "V1 validation-trace ledger-output-descriptor reference-script yield":
    "V1VtLopDescRefScriptYield",
  "V1 validation-trace ledger-output-descriptor datum-summary yield":
    "V1VtLopDescDatumSummaryYield",
  "V1 validation-trace ledger-output-descriptor value-summary yield":
    "V1VtLopDescValueSummaryYield",
  "V1 validation-trace phase-A native Advance semantic": "V1VtPaNsAdvance",
  "V1 validation-trace phase-A native Item semantic": "V1VtPaNsItem",
  "V1 validation-trace phase-A native TokenHead semantic": "V1VtPaNsHead",
  "V1 validation-trace phase-A native AllOrAnyContainerFramePayload semantic":
    "V1VtPaNsAllAnyFrame",
  "V1 validation-trace phase-A native AllOrAnyEmptyContainerPayload semantic":
    "V1VtPaNsAllAnyEmpty",
  "V1 validation-trace phase-A native AtLeastContainerFramePayload semantic":
    "V1VtPaNsAtLeastFrame",
  "V1 validation-trace phase-A native AtLeastEmptyContainerPayload semantic":
    "V1VtPaNsAtLeastEmpty",
  "V1 validation-trace phase-A native TimelockPayload semantic": "V1VtPaNsTime",
  "V1 validation-trace phase-A native SignatureMembershipPayload semantic":
    "V1VtPaNsSigMember",
  "V1 validation-trace phase-A native SignatureEmptyPayload semantic":
    "V1VtPaNsSigEmpty",
  "V1 validation-trace phase-A native SignatureBelowFirstPayload semantic":
    "V1VtPaNsSigBelow",
  "V1 validation-trace phase-A native SignatureAboveLastPayload semantic":
    "V1VtPaNsSigAbove",
  "V1 validation-trace phase-A native SignatureBetweenPayload semantic":
    "V1VtPaNsSigBetween",
  "V1 validation-trace phase-A native Frame semantic": "V1VtPaNsFrame",
  "V1 validation-trace phase-A preconditions Finalize semantic":
    "V1VtPaSpFinalize",
  "V1 validation-trace phase-A preconditions Item semantic": "V1VtPaSpItem",

  "reference-script-auth minting": "ReferenceScriptAuthMint",
  "hub-oracle minting": "HubOracleMint",
  "da-params-governor spending": "DaParamsGovernorSpend",
  "da-params-governor minting": "DaParamsGovernorMint",
  "da-bond-pool spending": "DaBondPoolSpend",
  "da-bond-pool minting": "DaBondPoolMint",
  "da-attestation spending": "DaAttestationSpend",
  "da-attestation minting": "DaAttestationMint",
  "scheduler spending": "SchedulerSpend",
  "scheduler minting": "SchedulerMint",
  "state-queue spending": "StateQueueSpend",
  "state-queue minting": "StateQueueMint",
  "state-queue commit withdrawal": "StateQueueCommitYield",
  "state-queue unattested-timeout withdrawal": "StateQueueUnattestedYield",
  "state-queue unavailable-timeout withdrawal": "StateQueueUnavailableYield",
  "state-queue fraud-removal withdrawal": "StateQueueFraudRemovalYield",
  "state-queue merge withdrawal": "StateQueueMergeYield",
  "registered-operators spending": "RegisteredOperatorsSpend",
  "registered-operators minting": "RegisteredOperatorsMint",
  "active-operators spending": "ActiveOperatorsSpend",
  "active-operators minting": "ActiveOperatorsMint",
  "retired-operators spending": "RetiredOperatorsSpend",
  "retired-operators minting": "RetiredOperatorsMint",
  "fraud-proof-catalogue minting": "FraudProofCatalogueMint",
  "deposit minting": "DepositMint",
  "deposit history retention": "DepositHistoryRetention",
  "deposit history retirement": "DepositHistoryRetirement",
  "withdrawal history retention": "WithdrawalHistoryRetention",
  "withdrawal history retirement": "WithdrawalHistoryRetirement",
  "deposit spending": "DepositSpend",
  "withdrawal minting": "WithdrawalMint",
  "withdrawal spending": "WithdrawalSpend",
  "settlement minting": "SettlementMint",
  "membership proof withdrawal": "MembershipProofWithdraw",
  "reserve spending": "ReserveSpend",
  "reserve observer": "ReserveObserver",
  "payout spending": "PayoutSpend",
  "payout minting": "PayoutMint",
  "V1 field-preimage certificate": "V1FieldPreimageCertSpend",
  "V1 field-preimage certificate minting": "V1FieldPreimageCertMint",
  "V1 script decode": "V1ScriptDecodeSpend",
  "V1 script decode minting": "V1ScriptDecodeMint",
  "V1 script decode term staker": "V1ScriptDecodeTermStake",
  "V1 script decode value staker": "V1ScriptDecodeValueStake",
  "V1 script decode list staker": "V1ScriptDecodeListStake",
  "V1 script decode fold staker": "V1ScriptDecodeFoldStake",
  "V1 script decode bytes staker": "V1ScriptDecodeBytesStake",
  "V1 script decode data staker": "V1ScriptDecodeDataStake",
  "V1 validation-trace dispute": "V1ValidationTraceDispute",
  "V1 validation-trace source": "V1ValidationTraceSource",
  "V1 validation-trace game": "V1ValidationTraceGame",
  "V1 validation-trace boundary": "V1ValidationTraceBoundary",
  "V1 validation-trace timeout": "V1ValidationTraceTimeout",
  "V1 validation-trace award": "V1ValidationTraceAward",
  "V1 validation-trace award minting": "V1ValidationTraceAwardMint",
  "V1 validation-trace case": "V1ValidationTraceCase",
  "V1 validation-trace case minting": "V1ValidationTraceCaseMint",
  "V1 validation-trace case configuration minting": "V1VtdCaseConfigMint",
  "V1 validation-trace thread expiry": "V1VtdThreadExpiry",
  "V1 validation-trace dispute bond": "V1VtdDisputeBond",
  "V1 validation-trace canonical-decode prepare": "V1VtdCanonicalPrepare",
  "V1 validation-trace canonical-decode empty semantic": "V1VtdCanonicalEmpty",
  "V1 validation-trace canonical-decode item semantic": "V1VtdCanonicalItem",
  "V1 validation-trace canonical-decode item source": "V1VtdCanonicalSource",
  "V1 validation-trace canonical-decode item observe": "V1VtdCanonicalObserve",
  "V1 validation-trace canonical-decode item proof": "V1VtdCanonicalProof",
  "V1 validation-trace canonical-decode item settlement":
    "V1VtdCanonicalSettle",
  "V1 validation-trace proof-item publication": "V1VtdProofItem",
  "V1 validation-trace CEK direct resolver": "V1ValidationTraceCekResolver0",
  "V1 fraud-proof transition-trace route": "V1FpTransitionTraceRoute",
  "V1 fraud-proof transition-trace final-0": "V1FpTransitionTraceFinal0",
  "V1 fraud-proof transition-trace final-1": "V1FpTransitionTraceFinal1",
  "V1 fraud-proof transition-trace final-2": "V1FpTransitionTraceFinal2",
  "V1 fraud-proof transition-trace final-3": "V1FpTransitionTraceFinal3",
  "V1 fraud-proof transition-trace final-4": "V1FpTransitionTraceFinal4",
  "V1 fraud-proof transition-trace final-5": "V1FpTransitionTraceFinal5",
  "V1 fraud-proof transition-trace final-6": "V1FpTransitionTraceFinal6",
  "V1 fraud-proof transition-trace final-7": "V1FpTransitionTraceFinal7",
  "V1 fraud-proof fabricated-deposit step-01": "V1FpFabricatedDepositS01",
  "V1 fraud-proof fabricated-deposit step-02": "V1FpFabricatedDepositS02",
  "V1 fraud-proof fabricated-deposit step-03": "V1FpFabricatedDepositS03",
  "V1 fraud-proof fabricated-deposit step-04": "V1FpFabricatedDepositS04",
  "V1 fraud-proof fabricated-withdrawal step-01": "V1FpFabricatedWithdrawalS01",
  "V1 fraud-proof fabricated-withdrawal step-02": "V1FpFabricatedWithdrawalS02",
  "V1 fraud-proof fabricated-withdrawal step-03": "V1FpFabricatedWithdrawalS03",
  "V1 fraud-proof fabricated-withdrawal step-04": "V1FpFabricatedWithdrawalS04",
  "V1 fraud-proof native-script-decoding step-01":
    "V1FpNativeScriptDecodingS01",
  "V1 fraud-proof native-script-decoding step-02":
    "V1FpNativeScriptDecodingS02",
  "V1 fraud-proof native-script-decoding step-03 open-subject":
    "V1FpNativeScriptDecodingS03Open",
  "V1 fraud-proof native-script-decoding step-03 bind-descriptor":
    "V1FpNativeScriptDecodingS03Bind",
  "V1 fraud-proof native-script-decoding step-03 advance-or-close":
    "V1FpNativeScriptDecodingS03Scan",
  "V1 fraud-proof native-script-decoding step-04":
    "V1FpNativeScriptDecodingS04",
  "V1 fraud-proof missing-signature step-01": "V1FpMissingSignatureS01",
  "V1 fraud-proof missing-signature step-02": "V1FpMissingSignatureS02",
  "V1 fraud-proof missing-signature step-03": "V1FpMissingSignatureS03",
  "V1 fraud-proof missing-signature step-04": "V1FpMissingSignatureS04",
  "V1 fraud-proof withdrawn-reference-input step-01":
    "V1FpWithdrawnReferenceInputS01",
  "V1 fraud-proof withdrawn-reference-input step-02":
    "V1FpWithdrawnReferenceInputS02",
  "V1 fraud-proof withdrawn-reference-input step-03":
    "V1FpWithdrawnReferenceInputS03",
  "V1 fraud-proof canonical-decodability step-01":
    "V1FpCanonicalDecodabilityS01",
  "V1 fraud-proof canonical-decodability step-02":
    "V1FpCanonicalDecodabilityS02",
  "V1 fraud-proof committed-field-shape step-01": "V1FpCommittedFieldShapeS01",
  "V1 fraud-proof committed-field-shape step-02": "V1FpCommittedFieldShapeS02",
  "V1 fraud-proof min-fee step-01": "V1FpMinFeeS01",
  "V1 fraud-proof min-fee step-02": "V1FpMinFeeS02",
  "V1 fraud-proof withdrawal-mistag step-01": "V1FpWithdrawalMistagS01",
  "V1 fraud-proof withdrawal-mistag step-02": "V1FpWithdrawalMistagS02",
  "V1 fraud-proof withdrawal-mistag step-03": "V1FpWithdrawalMistagS03",
  "V1 fraud-proof withdrawal-mistag step-04": "V1FpWithdrawalMistagS04",
  "V1 fraud-proof withdrawal-mistag step-05": "V1FpWithdrawalMistagS05",
  "V1 fraud-proof double-withdraw step-01": "V1FpDoubleWithdrawS01",
  "V1 fraud-proof double-withdraw step-02": "V1FpDoubleWithdrawS02",
  "V1 fraud-proof l2-tx-mistag step-01": "V1FpL2TxMistagS01",
  "V1 fraud-proof l2-tx-mistag step-02": "V1FpL2TxMistagS02",
  "V1 fraud-proof withdrawn-input step-01": "V1FpWithdrawnInputS01",
  "V1 fraud-proof withdrawn-input step-02": "V1FpWithdrawnInputS02",
  "V1 fraud-proof withdrawn-input step-03": "V1FpWithdrawnInputS03",
  "V1 fraud-proof value-not-preserved step-01": "V1FpValueNotPreservedS01",
  "V1 fraud-proof value-not-preserved step-02": "V1FpValueNotPreservedS02",
  "V1 fraud-proof value-not-preserved step-03": "V1FpValueNotPreservedS03",
  "V1 fraud-proof value-not-preserved step-04": "V1FpValueNotPreservedS04",
  "value conservation accepted-source": "ValueConservationAcceptedSource",
  "value conservation forced-source": "ValueConservationForcedSource",
  "value conservation event": "ValueConservationEvent",
  "value conservation pre-state": "ValueConservationPreState",
  "value conservation inputs": "ValueConservationInputs",
  "value conservation input-value": "ValueConservationInputValue",
  "value conservation assets": "ValueConservationAssets",
  "value conservation field-grammar": "ValueConservationFieldGrammar",
  "value conservation outputs": "ValueConservationOutputs",
  "value conservation output-scan": "ValueConservationOutputScan",
  "value conservation mint": "ValueConservationMint",
  "value conservation update": "ValueConservationUpdate",
  "value conservation terminal": "ValueConservationTerminal",

  "V1 fraud-proof input-set-uniqueness step-01": "V1FpInputSetUniquenessS01",
  "V1 fraud-proof input-set-uniqueness step-02": "V1FpInputSetUniquenessS02",
  "V1 fraud-proof input-set-uniqueness step-03": "V1FpInputSetUniquenessS03",
  "V1 fraud-proof input-set-uniqueness step-04": "V1FpInputSetUniquenessS04",
  "V1 fraud-proof mint-authorization step-01": "V1FpMintAuthorizationS01",
  "V1 fraud-proof mint-authorization step-02": "V1FpMintAuthorizationS02",
  "V1 fraud-proof mint-authorization step-03": "V1FpMintAuthorizationS03",
  "V1 fraud-proof mint-authorization step-04": "V1FpMintAuthorizationS04",
  "V1 fraud-proof mint-authorization step-05": "V1FpMintAuthorizationS05",
  "V1 fraud-proof mint-authorization step-06": "V1FpMintAuthorizationS06",
  "V1 fraud-proof mint-authorization step-07": "V1FpMintAuthorizationS07",
  "V1 fraud-proof network-id step-01": "V1FpNetworkIdS01",
  "V1 fraud-proof network-id step-02": "V1FpNetworkIdS02",
  "V1 fraud-proof missing-signature forced step": "V1FpMissingSigForcedStep",
  "V1 fraud-proof missing-signature forced signer":
    "V1FpMissingSigForcedSigner",
  "V1 fraud-proof missing-signature forced witness":
    "V1FpMissingSigForcedWitness",
  "V1 fraud-proof network-id forced step": "V1FpNetworkIdForced",
  "V1 fraud-proof network-id forced scan": "V1FpNetworkIdForcedScan",
  "V1 fraud-proof computation-thread minting": "V1FpComputationThreadMint",
  "V1 fraud-proof token minting": "V1FpTokenMint",
  "V1 MPF chunked-verify withdrawal": "V1MpfChunkedVerifyWithdraw",
  "V1 MPF pexcludes withdrawal": "V1MpfPexcludesWithdraw",
  "V1 fraud-proof double-spend step-01": "V1FpDoubleSpendS01",
  "V1 fraud-proof double-spend step-02": "V1FpDoubleSpendS02",
  "V1 fraud-proof double-spend step-03": "V1FpDoubleSpendS03",
  "V1 fraud-proof double-spend step-04": "V1FpDoubleSpendS04",
  "V1 fraud-proof non-existent-input step-01": "V1FpNonExistentInputS01",
  "V1 fraud-proof non-existent-input step-02": "V1FpNonExistentInputS02",
  "V1 fraud-proof non-existent-input step-03": "V1FpNonExistentInputS03",
  "V1 fraud-proof non-existent-input step-04": "V1FpNonExistentInputS04",
  "V1 fraud-proof non-existent-input-no-index step-01":
    "V1FpNonExistentInputNoIdxS01",
  "V1 fraud-proof non-existent-input-no-index step-02":
    "V1FpNonExistentInputNoIdxS02",
  "V1 fraud-proof non-existent-input-no-index step-03":
    "V1FpNonExistentInputNoIdxS03",
  "V1 fraud-proof non-existent-input-no-index step-04":
    "V1FpNonExistentInputNoIdxS04",
  "V1 fraud-proof invalid-range step-01": "V1FpInvalidRangeS01",
  "V1 fraud-proof invalid-range step-02": "V1FpInvalidRangeS02",
  "V1 fraud-proof zero-input step-01": "V1FpZeroInputS01",
  "V1 fraud-proof zero-input step-02": "V1FpZeroInputS02",
  "V1 fraud-proof da-hash-preimage step-01": "V1FpDaHashPreimageS01",
  "V1 fraud-proof da-hash-preimage step-02": "V1FpDaHashPreimageS02",
  "V1 fraud-proof no-reference-input step-01": "V1FpNoReferenceInputS01",
  "V1 fraud-proof no-reference-input step-02": "V1FpNoReferenceInputS02",
  "V1 fraud-proof no-reference-input step-03": "V1FpNoReferenceInputS03",
  "V1 fraud-proof no-reference-input step-04": "V1FpNoReferenceInputS04",
  "V1 fraud-proof reference-input-no-idx step-01": "V1FpReferenceInputNoIdxS01",
  "V1 fraud-proof reference-input-no-idx step-02": "V1FpReferenceInputNoIdxS02",
  "V1 fraud-proof reference-input-no-idx step-03": "V1FpReferenceInputNoIdxS03",
  "V1 fraud-proof reference-input-no-idx step-04": "V1FpReferenceInputNoIdxS04",
  "V1 fraud-proof invalid-signature step-01": "V1FpInvalidSignatureS01",
  "V1 fraud-proof invalid-signature step-02": "V1FpInvalidSignatureS02",
  "V1 fraud-proof native-script-invalid step-01": "V1FpNativeScriptInvalidS01",
  "V1 fraud-proof native-script-invalid step-02": "V1FpNativeScriptInvalidS02",
  "V1 fraud-proof native-script-invalid step-03": "V1FpNativeScriptInvalidS03",
  "V1 fraud-proof native-script-invalid step-04": "V1FpNativeScriptInvalidS04",
  "V1 fraud-proof native-script-invalid step-05": "V1FpNativeScriptInvalidS05",
  "V1 fraud-proof min-ada step-01": "V1FpMinAdaS01",
  "V1 fraud-proof min-ada step-02": "V1FpMinAdaS02",
  "V1 validation-trace value-and-mint asset-fold yield":
    "V1VtVamAssetFoldYield",
  "V1 validation-trace phase-A native item native yield":
    "V1VtPhaseANativeItemNativeYield",
  "V1 validation-trace phase-A native item foreign yield":
    "V1VtPhaseANativeItemForeignYield",
  "V1 validation-trace CEK context settle": "V1VtCekCtxSet",
  "V1 validation-trace CEK context control": "V1VtCekCtxCon",
  "V1 validation-trace CEK context reference": "V1VtCekCtxRef",
  "V1 validation-trace CEK context spend": "V1VtCekCtxSpe",
  "V1 validation-trace CEK context output": "V1VtCekCtxOut",
  "V1 validation-trace CEK context signer": "V1VtCekCtxSig",
  "V1 validation-trace CEK context observer authenticate": "V1VtCekCtxObsAut",
  "V1 validation-trace CEK context observer fold": "V1VtCekCtxObsFol",
  "V1 validation-trace CEK context mint init": "V1VtCekCtxMinIni",
  "V1 validation-trace CEK context mint item": "V1VtCekCtxMinIte",
  "V1 validation-trace CEK context assemble": "V1VtCekCtxAss",
  "V1 validation-trace CEK context tx info": "V1VtCekCtxTxInf",
  "V1 validation-trace CEK context seed": "V1VtCekCtxSee",
  "V1 validation-trace CEK context finalize authenticate": "V1VtCekCtxFinAut",
  "V1 validation-trace CEK context finalize spend": "V1VtCekCtxFinSpe",
  "V1 validation-trace CEK context finalize mint": "V1VtCekCtxFinMin",
  "V1 validation-trace CEK context finalize withdraw": "V1VtCekCtxFinWit",
  "V1 validation-trace CEK context finalize observe": "V1VtCekCtxFinObs",
  "V1 validation-trace CEK context finalize midgard": "V1VtCekCtxFinMid",
  "V1 validation-trace CEK context redeemer begin": "V1VtCekCtxRedBeg",
  "V1 validation-trace CEK context redeemer select authenticate":
    "V1VtCekCtxRedSelAut",
  "V1 validation-trace CEK context redeemer select initialize":
    "V1VtCekCtxRedSelIni",
  "V1 validation-trace CEK context redeemer select hash": "V1VtCekCtxRedSelHas",
  "V1 validation-trace CEK context redeemer select finish":
    "V1VtCekCtxRedSelFin",
  "V1 validation-trace CEK context item bind": "V1VtCekCtxIteBin",
  "V1 validation-trace CEK context item return": "V1VtCekCtxIteRet",
  "V1 validation-trace CEK context item selection hash": "V1VtCekCtxIteSelHas",
  "V1 validation-trace CEK context item data hash": "V1VtCekCtxIteDatHas",
  "V1 validation-trace CEK context item finalize": "V1VtCekCtxIteFin",
  "V1 validation-trace CEK context item selection continue":
    "V1VtCekCtxIteSelCon",
  "V1 validation-trace CEK context item selection finish":
    "V1VtCekCtxIteSelFin",
  "V1 validation-trace CEK context item data continue": "V1VtCekCtxIteDatCon",
  "V1 validation-trace CEK context item data finish descriptor":
    "V1VtCekCtxIteDatFinDes",
  "V1 validation-trace CEK context item data finish value":
    "V1VtCekCtxIteDatFinVal",
  "V1 validation-trace CEK context item entry": "V1VtCekCtxIteEnt",
  "V1 validation-trace CEK context item settlement": "V1VtCekCtxIteSet",
  "V1 validation-trace CEK core settle": "V1VtCekCoreSet",
  "V1 validation-trace CEK core compute": "V1VtCekCoreCom",
  "V1 validation-trace CEK core machine": "V1VtCekCoreMac",
  "V1 validation-trace CEK core case constant": "V1VtCekCoreCasCon",
  "V1 validation-trace CEK core map conversion": "V1VtCekCoreMapCon",
  "V1 validation-trace CEK core direct scalar": "V1VtCekCoreDirSca",
  "V1 validation-trace CEK core direct structured": "V1VtCekCoreDirStr",
  "V1 validation-trace CEK core direct scalar budget": "V1VtCekCoreDirScaBud",
  "V1 validation-trace CEK core direct structured budget":
    "V1VtCekCoreDirStrBud",
  "V1 validation-trace CEK core direct scalar roots": "V1VtCekCoreDirScaRoo",
  "V1 validation-trace CEK core direct structured roots":
    "V1VtCekCoreDirStrRoo",
  "V1 validation-trace CEK core semantic pair": "V1VtCekCoreSemPai",
  "V1 validation-trace CEK core semantic list construct":
    "V1VtCekCoreSemLisCon",
  "V1 validation-trace CEK core semantic list select": "V1VtCekCoreSemLisSel",
  "V1 validation-trace CEK core semantic choose": "V1VtCekCoreSemCho",
  "V1 validation-trace CEK core semantic data construct":
    "V1VtCekCoreSemDatCon",
  "V1 validation-trace CEK core semantic data scalar": "V1VtCekCoreSemDatSca",
  "V1 validation-trace CEK core semantic data misc": "V1VtCekCoreSemDatMis",
  "V1 validation-trace CEK core semantic budget": "V1VtCekCoreSemBud",
  "V1 validation-trace CEK core semantic roots": "V1VtCekCoreSemRoo",
  "V1 validation-trace CEK core semantic result": "V1VtCekCoreSemRes",
  "V1 validation-trace CEK core map start nodes": "V1VtCekCoreMapStaNod",
  "V1 validation-trace CEK core map start budget": "V1VtCekCoreMapStaBud",
  "V1 validation-trace CEK core map start roots": "V1VtCekCoreMapStaRoo",
  "V1 validation-trace CEK core semantic failure material":
    "V1VtCekCoreSemFaiMat",
  "V1 validation-trace CEK core semantic failure roots": "V1VtCekCoreSemFaiRoo",
  "V1 validation-trace CEK core bls final": "V1VtCekCoreBlsFin",
  "V1 validation-trace CEK core bls budget": "V1VtCekCoreBlsBud",
  "V1 validation-trace CEK core bls roots": "V1VtCekCoreBlsRoo",
  "V1 validation-trace CEK core failure budget": "V1VtCekCoreFaiBud",
  "V1 validation-trace CEK core failure known": "V1VtCekCoreFaiKno",
  "V1 validation-trace CEK core type failure kinds": "V1VtCekCoreTypFaiKin",
  "V1 validation-trace CEK core type failure roots": "V1VtCekCoreTypFaiRoo",

  "V1 validation-trace CEK selection authenticate yield": "V1VtCekSelAuthYield",
  "V1 validation-trace CEK selection successor yield": "V1VtCekSelSuccYield",

  "V1 fraud-proof transition-trace final-5 replay yield": "V1FpTtF5ReplayYield",
  "V1 fraud-proof min-ada step-02 tx yield": "V1FpMinAdaS02TxYield",
  "V1 fraud-proof transition-trace final-4 L2 assembly yield":
    "V1FpTtF4L2AssemblyYield",
  "V1 fraud-proof transition-trace final-4 L2 scan yield": "V1FpTtF4ScanYield",
  "V1 fraud-proof transition-trace final-4 L2 value yield":
    "V1FpTtF4ValueYield",
  "V1 fraud-proof transition-trace final-5 assembly yield":
    "V1FpTtF5AssemblyYield",
  "V1 fraud-proof transition-trace final-5 scan yield": "V1FpTtF5ScanYield",
  "V1 fraud-proof transition-trace final-5 value yield": "V1FpTtF5ValueYield",

  "V1 fraud-proof transition-trace final-4 L2 open yield":
    "V1FpTtF4L2OpenYield",
  "V1 fraud-proof transition-trace final-4 L2 delete yield":
    "V1FpTtF4L2DeleteYield",
  "V1 fraud-proof transition-trace final-4 L2 outputs yield":
    "V1FpTtF4L2OutputsYield",
  "V1 fraud-proof transition-trace final-4 L2 output step yield":
    "V1FpTtF4L2OutputStepYield",
  "V1 fraud-proof transition-trace final-4 L2 output descriptor yield":
    "V1FpTtF4OutputDescriptorYield",
  "V1 fraud-proof transition-trace final-4 L2 output summary yield":
    "V1FpTtF4OutputSummaryYield",
  "V1 fraud-proof transition-trace final-4 L2 summaries yield":
    "V1FpTtF4L2SummariesYield",
  "V1 fraud-proof transition-trace final-4 L2 replay yield":
    "V1FpTtF4L2ReplayYield",
  "V1 fraud-proof transition-trace final-5 projection yield":
    "V1FpTtF5ProjectionYield",
  "V1 fraud-proof transition-trace final-6 L1 event yield":
    "V1FpTtF6L1EventYield",
  "V1 fraud-proof transition-trace final-6 forced timing yield":
    "V1FpTtF6ForcedTimeYield",
  "V1 fraud-proof transition-trace final-5 summaries yield":
    "V1FpTtF5SummariesYield",

  "correction-lock spending": "CorrectionLockSpend",
  "V1 fraud-proof min-ada step-03": "V1FpMinAdaS03",
  "V1 fraud-proof min-ada step-04": "V1FpMinAdaS04",
  "V1 fraud-proof field-preimage-length-mismatch step-01": "V1FpFieldLenS01",
  "V1 fraud-proof field-preimage-length-mismatch step-02 accepted":
    "V1FpFieldLenS02Accepted",
  "V1 fraud-proof field-preimage-length-mismatch step-02 forced":
    "V1FpFieldLenS02Forced",
  "V1 fraud-proof field-preimage-length-mismatch step-03": "V1FpFieldLenS03",
  "V1 fraud-proof field-item-width-illegal step-01": "V1FpItemWidthS01",
  "V1 fraud-proof field-item-width-illegal step-02": "V1FpItemWidthS02",
  "V1 fraud-proof field-item-width-illegal step-03": "V1FpItemWidthS03",
  "V1 fraud-proof witness-script-decoding step-01": "V1FpWitnessDecodeS01",
  "V1 fraud-proof witness-script-decoding step-02": "V1FpWitnessDecodeS02",
  "V1 fraud-proof witness-script-decoding step-03": "V1FpWitnessDecodeS03",
  "V1 fraud-proof witness-script-decoding step-04": "V1FpWitnessDecodeS04",
  "V1 fraud-proof script-integrity-hash-missing step-01":
    "V1FpIntegrityMissingS01",
  "V1 fraud-proof script-integrity-hash-missing step-02":
    "V1FpIntegrityMissingS02",
  "V1 fraud-proof script-integrity-hash-missing step-03":
    "V1FpIntegrityMissingS03",
  "V1 fraud-proof script-integrity-hash-missing script-grammar":
    "V1FpIntegrityMissingGrammar",
  "V1 fraud-proof script-integrity-hash-missing script-scan":
    "V1FpIntegrityMissingScan",
  "V1 fraud-proof script-integrity-hash-missing redeemer-grammar":
    "V1FpIntegrityMissingRedeemer",
  "V1 fraud-proof script-integrity-hash-missing step-04":
    "V1FpIntegrityMissingS04",
  "V1 fraud-proof transaction-output-non-canonical step-01":
    "V1FpTxOutputCanonicalS01",
  "V1 fraud-proof mint-item-non-canonical step-01":
    "V1FpMintItemNonCanonicalS01",
  "V1 fraud-proof transaction-output-non-canonical step-02":
    "V1FpTxOutputCanonicalS02",
  "V1 fraud-proof mint-item-non-canonical step-02":
    "V1FpMintItemNonCanonicalS02",
  "V1 fraud-proof transaction-output-non-canonical step-03":
    "V1FpTxOutputCanonicalS03",
  "V1 fraud-proof transaction-output-non-canonical step-03 datum":
    "V1FpTxOutputCanonicalS03Datum",
  "V1 fraud-proof mint-item-non-canonical step-03":
    "V1FpMintItemNonCanonicalS03",
  "V1 fraud-proof transaction-output-non-canonical step-04":
    "V1FpTxOutputCanonicalS04",
  "V1 fraud-proof mint-item-non-canonical step-04":
    "V1FpMintItemNonCanonicalS04",
  "V1 fraud-proof resolved-output-non-canonical step-01":
    "V1FpResolvedOutputS01",
  "V1 fraud-proof resolved-output-non-canonical step-02":
    "V1FpResolvedOutputS02",
  "V1 fraud-proof resolved-output-non-canonical step-03":
    "V1FpResolvedOutputS03",
  "V1 fraud-proof resolved-output-non-canonical step-04":
    "V1FpResolvedOutputS04",
  "V1 fraud-proof resolved-output-non-canonical step-04 datum":
    "V1FpResolvedOutputS04Datum",
  "V1 fraud-proof resolved-output-non-canonical step-05":
    "V1FpResolvedOutputS05",
  "V1 fraud-proof mint-declared-asset-limit step-01": "V1FpMintAssetLimitS01",
  "V1 fraud-proof mint-declared-asset-limit step-02": "V1FpMintAssetLimitS02",
  "V1 fraud-proof mint-declared-asset-limit step-03": "V1FpMintAssetLimitS03",
  "V1 fraud-proof mint-declared-asset-limit step-04": "V1FpMintAssetLimitS04",
  "V1 fraud-proof spend-input-signer-missing step-01": "V1FpSpendSignerS01",
  "V1 fraud-proof spend-input-signer-missing step-02": "V1FpSpendSignerS02",
  "V1 fraud-proof spend-input-signer-missing step-03": "V1FpSpendSignerS03",
  "V1 fraud-proof spend-input-signer-missing step-04": "V1FpSpendSignerS04",
  "V1 fraud-proof spend-input-signer-missing step-05": "V1FpSpendSignerS05",
  "V1 fraud-proof protected-output-signer-missing step-01":
    "V1FpProtectedSignerS01",
  "V1 fraud-proof protected-output-signer-missing step-02":
    "V1FpProtectedSignerS02",
  "V1 fraud-proof protected-output-signer-missing step-03":
    "V1FpProtectedSignerS03",
  "V1 fraud-proof protected-output-signer-missing step-04":
    "V1FpProtectedSignerS04",
  "V1 fraud-proof protected-output-signer-missing step-05":
    "V1FpProtectedSignerS05",
  "V1 fraud-proof observers-forbidden-on-untagged-network step-01":
    "V1FpObserversForbiddenS01",
  "V1 fraud-proof observers-forbidden-on-untagged-network step-02":
    "V1FpObserversForbiddenS02",
  "V1 fraud-proof output-reference-script-decoding step-01":
    "V1FpOutputRefDecodeS01",
  "V1 fraud-proof output-reference-script-decoding step-02":
    "V1FpOutputRefDecodeS02",
  "V1 fraud-proof output-reference-script-decoding step-03":
    "V1FpOutputRefDecodeS03",
  "V1 fraud-proof output-reference-script-decoding step-04":
    "V1FpOutputRefDecodeS04",
  "V1 fraud-proof output-reference-script-decoding step-05":
    "V1FpOutputRefDecodeS05",
  "V1 fraud-proof output-reference-script-decoding step-06":
    "V1FpOutputRefDecodeS06",
  "V1 fraud-proof execution-source-script-decoding step-01":
    "V1FpExecSourceDecodeS01",
  "V1 fraud-proof execution-source-script-decoding step-02":
    "V1FpExecSourceDecodeS02",
  "V1 fraud-proof execution-source-script-decoding step-03":
    "V1FpExecSourceDecodeS03",
  "V1 fraud-proof execution-source-script-decoding step-04":
    "V1FpExecSourceDecodeS04",
  "V1 fraud-proof execution-source-script-decoding step-05":
    "V1FpExecSourceDecodeS05",
  "V1 fraud-proof observer-order-invalid step-01": "V1FpObserverOrderS01",
  "V1 fraud-proof observer-order-invalid step-02": "V1FpObserverOrderS02",
  "V1 fraud-proof observer-order-invalid step-03": "V1FpObserverOrderS03",
  "V1 fraud-proof observer-order-invalid step-04": "V1FpObserverOrderS04",
  "V1 fraud-proof redeemer-canonicity step-01": "V1FpRedeemerCanonS01",
  "V1 fraud-proof redeemer-canonicity step-02": "V1FpRedeemerCanonS02",
  "V1 fraud-proof redeemer-canonicity step-02 data": "V1FpRedeemerCanonS02Data",
  "V1 fraud-proof redeemer-canonicity step-03": "V1FpRedeemerCanonS03",
  "V1 fraud-proof receive-purpose-language step-01": "V1FpRecvPurposeS01",
  "V1 fraud-proof receive-purpose-language bind-event":
    "V1FpRecvPurposeBindEvent",
  "V1 fraud-proof receive-purpose-language bind-pre-state":
    "V1FpRecvPurposeBindPreState",
  "V1 fraud-proof receive-purpose-language source-start":
    "V1FpRecvPurposeSrcStart",
  "V1 fraud-proof receive-purpose-language source-resolved":
    "V1FpRecvPurposeSrcResolved",
  "V1 fraud-proof receive-purpose-language source-authenticate":
    "V1FpRecvPurposeSrcAuth",
  "V1 fraud-proof receive-purpose-language source-map": "V1FpRecvPurposeSrcMap",
  "V1 fraud-proof receive-purpose-language source-input-finish":
    "V1FpRecvPurposeSrcInputFinish",
  "V1 fraud-proof receive-purpose-language source-grammar":
    "V1FpRecvPurposeSrcGrammar",
  "V1 fraud-proof receive-purpose-language source-witnesses":
    "V1FpRecvPurposeSrcWitnesses",
  "V1 fraud-proof receive-purpose-language source-finish":
    "V1FpRecvPurposeSrcFinish",
  "V1 fraud-proof receive-purpose-language family-entry":
    "V1FpRecvPurposeEntry",
  "V1 fraud-proof receive-purpose-language receive-start":
    "V1FpRecvPurposeRecvStart",
  "V1 fraud-proof receive-purpose-language receive-grammar":
    "V1FpRecvPurposeRecvGrammar",
  "V1 fraud-proof receive-purpose-language receive-outputs":
    "V1FpRecvPurposeRecvOutputs",
  "V1 fraud-proof receive-purpose-language receive-finish":
    "V1FpRecvPurposeRecvFinish",
  "V1 fraud-proof receive-purpose-language receive-declaration":
    "V1FpRecvPurposeRecvDecl",
  "V1 fraud-proof receive-purpose-language source-lookup":
    "V1FpRecvPurposeSrcLookup",
  "V1 fraud-proof receive-purpose-language claimed-redeemer-grammar":
    "V1FpRecvPurposeClaimedGrammar",
  "V1 fraud-proof receive-purpose-language claimed-redeemer-scan":
    "V1FpRecvPurposeClaimedScan",
  "V1 fraud-proof receive-purpose-language terminal": "V1FpRecvPurposeTerminal",
  "V1 fraud-proof unused-script-witness step-01": "V1FpUnusedScriptWitnessS01",
  "V1 fraud-proof unused-script-witness step-02": "V1FpUnusedScriptWitnessS02",
  "V1 fraud-proof unused-script-witness step-03": "V1FpUnusedScriptWitnessS03",
  "V1 fraud-proof unused-script-witness step-04": "V1FpUnusedScriptWitnessS04",
  "V1 fraud-proof unused-script-witness step-05": "V1FpUnusedScriptWitnessS05",
  "V1 fraud-proof unused-script-witness step-06": "V1FpUnusedScriptWitnessS06",
  "V1 fraud-proof missing-script-source step-01": "V1FpMissingSrcS01",
  "V1 fraud-proof missing-script-source bind-event": "V1FpMissingSrcBindEvent",
  "V1 fraud-proof missing-script-source bind-pre-state":
    "V1FpMissingSrcBindPreState",
  "V1 fraud-proof missing-script-source source-start": "V1FpMissingSrcSrcStart",
  "V1 fraud-proof missing-script-source source-resolved":
    "V1FpMissingSrcSrcResolved",
  "V1 fraud-proof missing-script-source source-authenticate":
    "V1FpMissingSrcSrcAuth",
  "V1 fraud-proof missing-script-source source-map": "V1FpMissingSrcSrcMap",
  "V1 fraud-proof missing-script-source source-input-finish":
    "V1FpMissingSrcSrcInputFinish",
  "V1 fraud-proof missing-script-source source-grammar":
    "V1FpMissingSrcSrcGrammar",
  "V1 fraud-proof missing-script-source source-witnesses":
    "V1FpMissingSrcSrcWitnesses",
  "V1 fraud-proof missing-script-source source-finish":
    "V1FpMissingSrcSrcFinish",
  "V1 fraud-proof missing-script-source family-entry": "V1FpMissingSrcEntry",
  "V1 fraud-proof missing-script-source purpose-select":
    "V1FpMissingSrcPurposeSelect",
  "V1 fraud-proof missing-script-source spend-select":
    "V1FpMissingSrcSpendSelect",
  "V1 fraud-proof missing-script-source spend-scan": "V1FpMissingSrcSpendScan",
  "V1 fraud-proof missing-script-source mint-grammar":
    "V1FpMissingSrcMintGrammar",
  "V1 fraud-proof missing-script-source mint-scan": "V1FpMissingSrcMintScan",
  "V1 fraud-proof missing-script-source receive-start":
    "V1FpMissingSrcRecvStart",
  "V1 fraud-proof missing-script-source receive-grammar":
    "V1FpMissingSrcRecvGrammar",
  "V1 fraud-proof missing-script-source receive-outputs":
    "V1FpMissingSrcRecvOutputs",
  "V1 fraud-proof missing-script-source receive-finish":
    "V1FpMissingSrcRecvFinish",
  "V1 fraud-proof missing-script-source receive-declaration":
    "V1FpMissingSrcRecvDecl",
  "V1 fraud-proof missing-script-source source-lookup":
    "V1FpMissingSrcSrcLookup",
  "V1 fraud-proof missing-script-source observed-decision":
    "V1FpMissingSrcObserved",
  "V1 fraud-proof missing-script-source claim-start":
    "V1FpMissingSrcClaimStart",
  "V1 fraud-proof missing-script-source claim-spend-start":
    "V1FpMissingSrcClaimSpendStart",
  "V1 fraud-proof missing-script-source claim-spend-scan":
    "V1FpMissingSrcClaimSpend",
  "V1 fraud-proof missing-script-source claim-mint-grammar":
    "V1FpMissingSrcClaimMintGrammar",
  "V1 fraud-proof missing-script-source claim-mint-scan":
    "V1FpMissingSrcClaimMintScan",
  "V1 fraud-proof missing-script-source claim-receive-grammar":
    "V1FpMissingSrcClaimRecvGrammar",
  "V1 fraud-proof missing-script-source claim-receive-outputs":
    "V1FpMissingSrcClaimRecvOutputs",
  "V1 fraud-proof missing-script-source claim-receive-finish":
    "V1FpMissingSrcClaimRecvFinish",
  "V1 fraud-proof missing-script-source claim-receive-declaration":
    "V1FpMissingSrcClaimRecvDecl",
  "V1 fraud-proof missing-script-source claim-adjudicate":
    "V1FpMissingSrcAdjudicate",
  "V1 fraud-proof missing-script-source claimed-redeemer-grammar":
    "V1FpMissingSrcClaimedGrammar",
  "V1 fraud-proof missing-script-source claimed-redeemer-scan":
    "V1FpMissingSrcClaimedScan",
  "V1 fraud-proof missing-script-source terminal": "V1FpMissingSrcTerminal",
  "V1 fraud-proof missing-redeemer step-01": "V1FpMissingRdmrS01",
  "V1 fraud-proof missing-redeemer bind-event": "V1FpMissingRdmrBindEvent",
  "V1 fraud-proof missing-redeemer bind-pre-state":
    "V1FpMissingRdmrBindPreState",
  "V1 fraud-proof missing-redeemer source-start": "V1FpMissingRdmrSrcStart",
  "V1 fraud-proof missing-redeemer source-resolved":
    "V1FpMissingRdmrSrcResolved",
  "V1 fraud-proof missing-redeemer source-authenticate":
    "V1FpMissingRdmrSrcAuth",
  "V1 fraud-proof missing-redeemer source-map": "V1FpMissingRdmrSrcMap",
  "V1 fraud-proof missing-redeemer source-input-finish":
    "V1FpMissingRdmrSrcInputFinish",
  "V1 fraud-proof missing-redeemer source-grammar": "V1FpMissingRdmrSrcGrammar",
  "V1 fraud-proof missing-redeemer source-witnesses":
    "V1FpMissingRdmrSrcWitnesses",
  "V1 fraud-proof missing-redeemer source-finish": "V1FpMissingRdmrSrcFinish",
  "V1 fraud-proof missing-redeemer family-entry": "V1FpMissingRdmrEntry",
  "V1 fraud-proof missing-redeemer purpose-select":
    "V1FpMissingRdmrPurposeSelect",
  "V1 fraud-proof missing-redeemer spend-select": "V1FpMissingRdmrSpendSelect",
  "V1 fraud-proof missing-redeemer spend-scan": "V1FpMissingRdmrSpendScan",
  "V1 fraud-proof missing-redeemer mint-grammar": "V1FpMissingRdmrMintGrammar",
  "V1 fraud-proof missing-redeemer mint-scan": "V1FpMissingRdmrMintScan",
  "V1 fraud-proof missing-redeemer receive-start": "V1FpMissingRdmrRecvStart",
  "V1 fraud-proof missing-redeemer receive-grammar":
    "V1FpMissingRdmrRecvGrammar",
  "V1 fraud-proof missing-redeemer receive-outputs":
    "V1FpMissingRdmrRecvOutputs",
  "V1 fraud-proof missing-redeemer receive-finish": "V1FpMissingRdmrRecvFinish",
  "V1 fraud-proof missing-redeemer receive-declaration":
    "V1FpMissingRdmrRecvDecl",
  "V1 fraud-proof missing-redeemer source-lookup": "V1FpMissingRdmrSrcLookup",
  "V1 fraud-proof missing-redeemer step-02": "V1FpMissingRdmrS02",
  "V1 fraud-proof missing-redeemer step-03": "V1FpMissingRdmrS03",
  "V1 fraud-proof missing-redeemer step-04": "V1FpMissingRdmrS04",
  "V1 fraud-proof missing-redeemer claim-start": "V1FpMissingRdmrClaimStart",
  "V1 fraud-proof missing-redeemer claim-spend-start":
    "V1FpMissingRdmrClaimSpendStart",
  "V1 fraud-proof missing-redeemer claim-spend-scan":
    "V1FpMissingRdmrClaimSpend",
  "V1 fraud-proof missing-redeemer claim-mint-grammar":
    "V1FpMissingRdmrClaimMintGrammar",
  "V1 fraud-proof missing-redeemer claim-mint-scan":
    "V1FpMissingRdmrClaimMintScan",
  "V1 fraud-proof missing-redeemer claim-receive-grammar":
    "V1FpMissingRdmrClaimRecvGrammar",
  "V1 fraud-proof missing-redeemer claim-receive-outputs":
    "V1FpMissingRdmrClaimRecvOutputs",
  "V1 fraud-proof missing-redeemer claim-receive-finish":
    "V1FpMissingRdmrClaimRecvFinish",
  "V1 fraud-proof missing-redeemer claim-receive-declaration":
    "V1FpMissingRdmrClaimRecvDecl",
  "V1 fraud-proof missing-redeemer claim-adjudicate":
    "V1FpMissingRdmrAdjudicate",
  "V1 fraud-proof missing-redeemer claimed-redeemer-grammar":
    "V1FpMissingRdmrClaimedGrammar",
  "V1 fraud-proof missing-redeemer claimed-redeemer-scan":
    "V1FpMissingRdmrClaimedScan",
  "V1 fraud-proof missing-redeemer step-05": "V1FpMissingRdmrS05",
  "V1 fraud-proof unused-redeemer step-01": "V1FpUnusedRdmrS01",
  "V1 fraud-proof unused-redeemer bind-event": "V1FpUnusedRdmrBindEvent",
  "V1 fraud-proof unused-redeemer bind-pre-state": "V1FpUnusedRdmrBindPreState",
  "V1 fraud-proof unused-redeemer source-start": "V1FpUnusedRdmrSrcStart",
  "V1 fraud-proof unused-redeemer source-resolved": "V1FpUnusedRdmrSrcResolved",
  "V1 fraud-proof unused-redeemer source-authenticate": "V1FpUnusedRdmrSrcAuth",
  "V1 fraud-proof unused-redeemer source-map": "V1FpUnusedRdmrSrcMap",
  "V1 fraud-proof unused-redeemer source-input-finish":
    "V1FpUnusedRdmrSrcInputFinish",
  "V1 fraud-proof unused-redeemer source-grammar": "V1FpUnusedRdmrSrcGrammar",
  "V1 fraud-proof unused-redeemer source-witnesses":
    "V1FpUnusedRdmrSrcWitnesses",
  "V1 fraud-proof unused-redeemer source-finish": "V1FpUnusedRdmrSrcFinish",
  "V1 fraud-proof unused-redeemer family-entry": "V1FpUnusedRdmrEntry",
  "V1 fraud-proof unused-redeemer redeemer-grammar":
    "V1FpUnusedRdmrRdmrGrammar",
  "V1 fraud-proof unused-redeemer redeemer-scan": "V1FpUnusedRdmrRdmrScan",
  "V1 fraud-proof unused-redeemer claim-start": "V1FpUnusedRdmrClaimStart",
  "V1 fraud-proof unused-redeemer claim-spend-start":
    "V1FpUnusedRdmrClaimSpendStart",
  "V1 fraud-proof unused-redeemer claim-spend-scan": "V1FpUnusedRdmrClaimSpend",
  "V1 fraud-proof unused-redeemer claim-mint-grammar":
    "V1FpUnusedRdmrClaimMintGrammar",
  "V1 fraud-proof unused-redeemer claim-mint-scan":
    "V1FpUnusedRdmrClaimMintScan",
  "V1 fraud-proof unused-redeemer claim-receive-grammar":
    "V1FpUnusedRdmrClaimRecvGrammar",
  "V1 fraud-proof unused-redeemer claim-receive-outputs":
    "V1FpUnusedRdmrClaimRecvOutputs",
  "V1 fraud-proof unused-redeemer claim-receive-finish":
    "V1FpUnusedRdmrClaimRecvFinish",
  "V1 fraud-proof unused-redeemer claim-receive-declaration":
    "V1FpUnusedRdmrClaimRecvDecl",
  "V1 fraud-proof unused-redeemer claim-adjudicate": "V1FpUnusedRdmrAdjudicate",
  "V1 fraud-proof unused-redeemer terminal": "V1FpUnusedRdmrTerminal",
  "V1 fraud-proof execution-native-script-invalid step-01":
    "V1FpExecNativeInvalidS01",
  "V1 fraud-proof execution-native-script-invalid step-02":
    "V1FpExecNativeInvalidS02",
  "V1 fraud-proof execution-native-script-invalid step-03":
    "V1FpExecNativeInvalidS03",
  "V1 fraud-proof execution-native-script-invalid step-04":
    "V1FpExecNativeInvalidS04",
  "V1 fraud-proof execution-native-script-invalid step-05":
    "V1FpExecNativeInvalidS05",
  "V1 fraud-proof execution-native-script-invalid step-06":
    "V1FpExecNativeInvalidS06",
  "V1 fraud-proof execution-native-script-invalid accepted-reconstruction-init":
    "V1FpExecNativeInvAccInit",
  "V1 fraud-proof execution-native-script-invalid accepted-spend-prefix":
    "V1FpExecNativeInvAccSpend",
  "V1 fraud-proof execution-native-script-invalid accepted-mint-prefix":
    "V1FpExecNativeInvAccMint",
  "V1 fraud-proof execution-native-script-invalid accepted-observer-prefix":
    "V1FpExecNativeInvAccObserver",
  "V1 fraud-proof execution-native-script-invalid accepted-receive-prefix":
    "V1FpExecNativeInvAccReceive",
  "V1 fraud-proof execution-native-script-invalid accepted-inline-source":
    "V1FpExecNativeInvAccInline",
  "V1 fraud-proof execution-native-script-invalid accepted-reference-source":
    "V1FpExecNativeInvAccRef",
  "V1 fraud-proof script-integrity-hash-mismatch step-01":
    "V1FpIntegrityMismatchS01",
  "V1 fraud-proof script-integrity-hash-mismatch bind-event":
    "V1FpIntegrityBindEvent",
  "V1 fraud-proof script-integrity-hash-mismatch bind-pre-state":
    "V1FpIntegrityBindPreState",
  "V1 fraud-proof script-integrity-hash-mismatch source-start":
    "V1FpIntegritySrcStart",
  "V1 fraud-proof script-integrity-hash-mismatch source-resolved":
    "V1FpIntegritySrcResolved",
  "V1 fraud-proof script-integrity-hash-mismatch source-authenticate":
    "V1FpIntegritySrcAuth",
  "V1 fraud-proof script-integrity-hash-mismatch source-map":
    "V1FpIntegritySrcMap",
  "V1 fraud-proof script-integrity-hash-mismatch source-input-finish":
    "V1FpIntegritySrcInputFinish",
  "V1 fraud-proof script-integrity-hash-mismatch source-grammar":
    "V1FpIntegritySrcGrammar",
  "V1 fraud-proof script-integrity-hash-mismatch source-witnesses":
    "V1FpIntegritySrcWitnesses",
  "V1 fraud-proof script-integrity-hash-mismatch source-finish":
    "V1FpIntegritySrcFinish",
  "V1 fraud-proof script-integrity-hash-mismatch family-entry":
    "V1FpIntegrityEntry",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend":
    "V1FpIntegrityApSpend",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend-resolve":
    "V1FpIntegrityApSpendResolve",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint-grammar":
    "V1FpIntegrityApMintGrammar",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint":
    "V1FpIntegrityApMint",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe-open":
    "V1FpIntegrityApObserveOpen",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe":
    "V1FpIntegrityApObserve",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive-grammar":
    "V1FpIntegrityApRecvGrammar",
  "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive":
    "V1FpIntegrityApRecv",
  "V1 fraud-proof script-integrity-hash-mismatch terminal":
    "V1FpIntegrityTerminal",
  "V1 fraud-proof distinct-asset-accumulation-limit step-01":
    "V1FpDistinctAssetLimitS01",
  "V1 fraud-proof distinct-asset-accumulation-limit step-02":
    "V1FpDistinctAssetLimitS02",
  "V1 fraud-proof distinct-asset-accumulation-limit step-03":
    "V1FpDistinctAssetLimitS03",
  "V1 fraud-proof distinct-asset-accumulation-limit step-04":
    "V1FpDistinctAssetLimitS04",
  "V1 fraud-proof distinct-asset-accumulation-limit step-05":
    "V1FpDistinctAssetLimitS05",
  "V1 fraud-proof distinct-asset-accumulation-limit step-06":
    "V1FpDistinctAssetLimitS06",
  "availability-challenge spending": "AvailabilityChallengeSpend",
  "availability-challenge minting": "AvailabilityChallengeMint",
  "availability-challenge open withdrawal": "AvailabilityChallengeOpenYield",
  "availability-challenge settle withdrawal":
    "AvailabilityChallengeSettleYield",
  "availability-challenge close withdrawal": "AvailabilityChallengeCloseYield",
  "availability-challenge timeout withdrawal":
    "AvailabilityChallengeExpiryYield",
  "V1 validation-trace CEK prepare": "V1VtCekPrepare",
  "V1 validation-trace CEK finish semantic": "V1VtCekFinishSem",
  "V1 validation-trace CEK execution selection semantic": "V1VtCekSelectionSem",
  "V1 validation-trace CEK context step semantic": "V1VtCekContextStepSem",
  "V1 validation-trace CEK core step semantic": "V1VtCekCoreStepSem",
  "V1 validation-trace compact-binding prepare": "V1VtCompactBindingPrepare",
  "V1 validation-trace compact-binding semantic": "V1VtCompactBindingSem",
  "V1 validation-trace static-ledger-rules prepare": "V1VtStaticLedgerPrepare",
  "V1 validation-trace static-ledger-rules semantic": "V1VtStaticLedgerSem",
  "V1 validation-trace input-sets prepare": "V1VtInputSetsPrepare",
  "V1 validation-trace input-sets empty semantic": "V1VtInputSetsEmptySem",
  "V1 validation-trace input-sets item semantic": "V1VtInputSetsItemSem",
  "V1 validation-trace signatures prepare": "V1VtSignaturesPrepare",
  "V1 validation-trace signatures advance semantic": "V1VtSigAdvanceSem",
  "V1 validation-trace signatures address item semantic":
    "V1VtSigAddressItemSem",
  "V1 validation-trace signatures required item semantic":
    "V1VtSigRequiredItemSem",
  "V1 validation-trace signatures handoff semantic": "V1VtSigHandoffSem",
  "V1 validation-trace phase-A native scripts prepare":
    "V1VtPhaseANativePrepare",
  "V1 validation-trace phase-A script preconditions prepare":
    "V1VtPhaseAPrecondPrepare",
  "V1 validation-trace resolve-inputs prepare": "V1VtResolveInputsPrepare",
  "V1 validation-trace script-sources prepare": "V1VtScriptSourcesPrepare",
} as const);
