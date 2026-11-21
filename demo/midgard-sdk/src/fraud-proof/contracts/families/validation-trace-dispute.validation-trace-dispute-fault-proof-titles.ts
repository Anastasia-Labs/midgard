import "./validation-trace-dispute.validation-trace-semantic-resolver-group-sizes.js";

import { deploymentValidatorTitle as title } from "@al-ft/midgard-core/deployment-manifest-identity";

export const VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES = {
  proofItem: title("validationTraceDisputeProofItem"),
  dispute: title("validationTraceDispute"),
  source: title("validationTraceDisputeSource"),
  game: title("validationTraceDisputeGame"),
  boundary: title("validationTraceDisputeBoundary"),
  timeout: title("validationTraceDisputeTimeout"),
  award: title("validationTraceDisputeAward"),
  awardMint: title("validationTraceDisputeAwardMint"),
  disputeBond: title("validationTraceDisputeBond"),
  caseSpend: title("validationTraceDisputeCase"),
  caseMint: title("validationTraceDisputeCaseMint"),
  caseConfig: title("validationTraceDisputeCaseConfigMint"),
  threadExpiry: title("validationTraceDisputeThreadExpiryWithdraw"),
  canonicalDecodeItemStages: {
    source: title("validationTraceDisputeCanonicalDecodeItemSource"),
    observe: title("validationTraceDisputeCanonicalDecodeItemObserve"),
    proof: title("validationTraceDisputeCanonicalDecodeItemProof"),
    settlement: title("validationTraceDisputeCanonicalDecodeItemSettlement"),
  },
  scriptSourcesStageOneRedeemerStages: {
    sourceAuthenticator: title(
      "validationTraceDisputeRedeemerItemSourceAuthenticator",
    ),
    openHeaderExecutor: title(
      "validationTraceDisputeRedeemerItemOpenHeaderExecutor",
    ),
    openTailExecutor: title(
      "validationTraceDisputeRedeemerItemOpenTailExecutor",
    ),
    headScalarExecutor: title(
      "validationTraceDisputeRedeemerItemHeadScalarExecutor",
    ),
    headSequenceExecutor: title(
      "validationTraceDisputeRedeemerItemHeadSequenceExecutor",
    ),
    headMapExecutor: title("validationTraceDisputeRedeemerItemHeadMapExecutor"),
    headLargeConstructorExecutor: title(
      "validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor",
    ),
    attachIntegerExecutor: title(
      "validationTraceDisputeRedeemerItemAttachIntegerExecutor",
    ),
    attachBytesExecutor: title(
      "validationTraceDisputeRedeemerItemAttachBytesExecutor",
    ),
    foldListExecutor: title(
      "validationTraceDisputeRedeemerItemFoldListExecutor",
    ),
    advanceIntegerExecutor: title(
      "validationTraceDisputeRedeemerItemAdvanceIntegerExecutor",
    ),
    advanceBytesExecutor: title(
      "validationTraceDisputeRedeemerItemAdvanceBytesExecutor",
    ),
    advanceLargeConstructorExecutor: title(
      "validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor",
    ),
    advanceLargeFieldsExecutor: title(
      "validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor",
    ),
    closeExecutor: title("validationTraceDisputeRedeemerItemCloseExecutor"),
    finishDataExecutor: title(
      "validationTraceDisputeRedeemerItemFinishDataExecutor",
    ),
    invalidHeaderExecutor: title(
      "validationTraceDisputeRedeemerItemInvalidHeaderExecutor",
    ),
    invalidTailExecutor: title(
      "validationTraceDisputeRedeemerItemInvalidTailExecutor",
    ),
    invalidDataExecutor: title(
      "validationTraceDisputeRedeemerItemInvalidDataExecutor",
    ),
    invalidScalarDataExecutor: title(
      "validationTraceDisputeRedeemerItemInvalidScalarDataExecutor",
    ),
    envelope: title(
      "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic",
    ),
    traversalNormalizer: title(
      "validationTraceDisputeRedeemerItemTraversalNormalizer",
    ),
    outerNormalizer: title("validationTraceDisputeRedeemerItemOuterNormalizer"),
    foldMapExecutor: title("validationTraceDisputeRedeemerItemFoldMapExecutor"),
    finalizeFrameExecutor: title(
      "validationTraceDisputeRedeemerItemFinalizeFrameExecutor",
    ),
    settlement: title("validationTraceDisputeRedeemerItemSettlement"),
  },
  prepares: {
    canonicalDecode: title("validationTraceDisputeCanonicalDecodePrepare"),
    compactBinding: title("validationTraceDisputeCompactBindingPrepare"),
    staticLedgerRules: title("validationTraceDisputeStaticLedgerRulesPrepare"),
    inputSets: title("validationTraceDisputeInputSetsPrepare"),
    signatures: title("validationTraceDisputeSignaturesPrepare"),
    phaseANativeScripts: title(
      "validationTraceDisputePhaseANativeScriptsPrepare",
    ),
    phaseAScriptPreconditions: title(
      "validationTraceDisputePhaseAScriptPreconditionsPrepare",
    ),
    resolveInputs: title("validationTraceDisputeResolveInputsPrepare"),
    scriptSources: title("validationTraceDisputeScriptSourcesPrepare"),
    nativeScripts: title("validationTraceDisputeNativeScriptsPrepare"),
    scriptIntegrity: title("validationTraceDisputeScriptIntegrityPrepare"),
    cek: title("validationTraceDisputeCekPrepare"),
    valueAndMint: title("validationTraceDisputeValueAndMintPrepare"),
  },
  semantics: {
    canonicalDecodeEmpty: title(
      "validationTraceDisputeCanonicalDecodeEmptySemantic",
    ),
    canonicalDecodeItem: title(
      "validationTraceDisputeCanonicalDecodeItemSemantic",
    ),
    compactBinding: title("validationTraceDisputeCompactBindingSemantic"),
    staticLedgerRules: title("validationTraceDisputeStaticLedgerRulesSemantic"),
    inputSetsEmpty: title("validationTraceDisputeInputSetsEmptySemantic"),
    inputSetsItem: title("validationTraceDisputeInputSetsItemSemantic"),
    signaturesAdvance: title("validationTraceDisputeSignaturesAdvanceSemantic"),
    signaturesAddressItem: title(
      "validationTraceDisputeSignaturesAddressItemSemantic",
    ),
    signaturesRequiredItem: title(
      "validationTraceDisputeSignaturesRequiredItemSemantic",
    ),
    signaturesHandoff: title("validationTraceDisputeSignaturesHandoffSemantic"),
    phaseANativeScriptsAdvance: title(
      "validationTraceDisputePhaseANativeScriptsAdvanceSemantic",
    ),
    phaseANativeScriptsItem: title(
      "validationTraceDisputePhaseANativeScriptsItemSemantic",
    ),
    phaseANativeScriptsTokenHead: title(
      "validationTraceDisputePhaseANativeScriptsTokenHeadSemantic",
    ),
    phaseANativeScriptsAllOrAnyContainerFramePayload: title(
      "validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic",
    ),
    phaseANativeScriptsAllOrAnyEmptyContainerPayload: title(
      "validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic",
    ),
    phaseANativeScriptsAtLeastContainerFramePayload: title(
      "validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic",
    ),
    phaseANativeScriptsAtLeastEmptyContainerPayload: title(
      "validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic",
    ),
    phaseANativeScriptsTimelockPayload: title(
      "validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic",
    ),
    phaseANativeScriptsSignatureMembershipPayload: title(
      "validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic",
    ),
    phaseANativeScriptsSignatureEmptyPayload: title(
      "validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic",
    ),
    phaseANativeScriptsSignatureBelowFirstPayload: title(
      "validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic",
    ),
    phaseANativeScriptsSignatureAboveLastPayload: title(
      "validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic",
    ),
    phaseANativeScriptsSignatureBetweenPayload: title(
      "validationTraceDisputePhaseANativeScriptsSignatureBetweenPayloadSemantic",
    ),
    phaseANativeScriptsFrame: title(
      "validationTraceDisputePhaseANativeScriptsFrameSemantic",
    ),
    phaseAScriptPreconditions: title(
      "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
    ),
    phaseAScriptPreconditionsItem: title(
      "validationTraceDisputePhaseAScriptPreconditionsItemSemantic",
    ),
    resolveInputsInitial: title(
      "validationTraceDisputeResolveInputsInitialSemantic",
    ),
    resolveInputsFinish: title(
      "validationTraceDisputeResolveInputsFinishSemantic",
    ),
    resolveInputsMembershipBegin: title(
      "validationTraceDisputeResolveInputsMembershipBeginSemantic",
    ),
    resolveInputsMembershipStep: title(
      "validationTraceDisputeResolveInputsMembershipStepSemantic",
    ),
    resolveInputsMembershipFinalize: title(
      "validationTraceDisputeResolveInputsMembershipFinalizeSemantic",
    ),
    resolveInputsNonMembership: title(
      "validationTraceDisputeResolveInputsNonMembershipSemantic",
    ),
    scriptSourcesNonOutput: title(
      "validationTraceDisputeScriptSourcesNonOutputSemantic",
    ),
    scriptSourcesOutputProofBegin: title(
      "validationTraceDisputeScriptSourcesOutputProofBeginSemantic",
    ),
    scriptSourcesOutputProofStep: title(
      "validationTraceDisputeScriptSourcesOutputProofStepSemantic",
    ),
    scriptSourcesOutputProofFinalize: title(
      "validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic",
    ),
    scriptSourcesOutputProofFinish: title(
      "validationTraceDisputeScriptSourcesOutputProofFinishSemantic",
    ),
    scriptSourcesStageZeroBegin: title(
      "validationTraceDisputeScriptSourcesStageZeroBeginSemantic",
    ),
    scriptSourcesStageZeroFinish: title(
      "validationTraceDisputeScriptSourcesStageZeroFinishSemantic",
    ),
    scriptSourcesStageZeroHashBlock: title(
      "validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic",
    ),
    scriptSourcesStageZeroHashAdvance: title(
      "validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic",
    ),
    scriptSourcesStageZeroHashTerminal: title(
      "validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic",
    ),
    scriptSourcesStageNineMismatch: title(
      "validationTraceDisputeScriptSourcesStageNineMismatchSemantic",
    ),
    scriptSourcesStageNineNativeMatch: title(
      "validationTraceDisputeScriptSourcesStageNineNativeMatchSemantic",
    ),
    scriptSourcesStageNineEffectfulMatch: title(
      "validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic",
    ),
    scriptSourcesStageNineMissing: title(
      "validationTraceDisputeScriptSourcesStageNineMissingSemantic",
    ),
    scriptSourcesStageOneFinish: title(
      "validationTraceDisputeScriptSourcesStageOneFinishSemantic",
    ),
    scriptSourcesStageOneRedeemer: title(
      "validationTraceDisputeScriptSourcesStageOneRedeemerSemantic",
    ),
    scriptSourcesStageElevenFinish: title(
      "validationTraceDisputeScriptSourcesStageElevenFinishSemantic",
    ),
    scriptSourcesStageElevenSource: title(
      "validationTraceDisputeScriptSourcesStageElevenSourceSemantic",
    ),
    scriptSourcesStageTwelveFinish: title(
      "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic",
    ),
    scriptSourcesStageTwelveRedeemer: title(
      "validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic",
    ),
    scriptSourcesStageTenMissing: title(
      "validationTraceDisputeScriptSourcesStageTenMissingSemantic",
    ),
    scriptSourcesStageTenMismatch: title(
      "validationTraceDisputeScriptSourcesStageTenMismatchSemantic",
    ),
    scriptSourcesStageTenMatch: title(
      "validationTraceDisputeScriptSourcesStageTenMatchSemantic",
    ),
    scriptSourcesStageEightFinish: title(
      "validationTraceDisputeScriptSourcesStageEightFinishSemantic",
    ),
    scriptSourcesStageEightPurpose: title(
      "validationTraceDisputeScriptSourcesStageEightPurposeSemantic",
    ),
    scriptSourcesStageSevenObserver: title(
      "validationTraceDisputeScriptSourcesStageSevenObserverSemantic",
    ),
    scriptSourcesStageSevenReceive: title(
      "validationTraceDisputeScriptSourcesStageSevenReceiveSemantic",
    ),
    scriptSourcesStageSevenFinish: title(
      "validationTraceDisputeScriptSourcesStageSevenFinishSemantic",
    ),
    nativeScriptsTerminal: title(
      "validationTraceDisputeNativeScriptsTerminalSemantic",
    ),
    nativeScriptsNative: title(
      "validationTraceDisputeNativeScriptsNativeSemantic",
    ),
    nativeScriptsEffectful: title(
      "validationTraceDisputeNativeScriptsEffectfulSemantic",
    ),
    scriptIntegrityAuthentication: title(
      "validationTraceDisputeScriptIntegrityAuthenticationSemantic",
    ),
    scriptIntegrityCompact: title(
      "validationTraceDisputeScriptIntegrityCompactSemantic",
    ),
    scriptIntegrityWitnessSet: title(
      "validationTraceDisputeScriptIntegrityWitnessSetSemantic",
    ),
    scriptIntegrityFinalize: title(
      "validationTraceDisputeScriptIntegrityFinalizeSemantic",
    ),
    cekFinish: title("validationTraceDisputeCekFinishSemantic"),
    cekExecutionSelection: title(
      "validationTraceDisputeCekExecutionSelectionSemantic",
    ),
    cekContextStep: title("validationTraceDisputeCekContextStepSemantic"),
    cekCoreStep: title("validationTraceDisputeCekCoreStepSemantic"),
    valueAndMintBegin: title("validationTraceDisputeValueAndMintBeginSemantic"),
    valueAndMintReplayBegin: title(
      "validationTraceDisputeValueAndMintReplayBeginSemantic",
    ),
    valueAndMintReplayInput: title(
      "validationTraceDisputeValueAndMintReplayInputSemantic",
    ),
    valueAndMintReplayAsset: title(
      "validationTraceDisputeValueAndMintReplayAssetSemantic",
    ),
    valueAndMintReplayFinish: title(
      "validationTraceDisputeValueAndMintReplayFinishSemantic",
    ),
    valueAndMintOutputDescriptor: title(
      "validationTraceDisputeValueAndMintOutputDescriptorSemantic",
    ),
    valueAndMintOutputAsset: title(
      "validationTraceDisputeValueAndMintOutputAssetSemantic",
    ),
    valueAndMintOutputFinish: title(
      "validationTraceDisputeValueAndMintOutputFinishSemantic",
    ),
    valueAndMintMintAsset: title(
      "validationTraceDisputeValueAndMintMintAssetSemantic",
    ),
    valueAndMintMintFinish: title(
      "validationTraceDisputeValueAndMintMintFinishSemantic",
    ),
    valueAndMintFinalize: title(
      "validationTraceDisputeValueAndMintFinalizeSemantic",
    ),
  },
  yields: {
    scriptSourcesStageTwoAdvance: title(
      "validationTraceDisputeScriptSourcesStageTwoAdvanceWithdraw",
    ),
    scriptSourcesStageThreeReplay: title(
      "validationTraceDisputeScriptSourcesStageThreeReplayWithdraw",
    ),
    scriptSourcesStageThreeFinish: title(
      "validationTraceDisputeScriptSourcesStageThreeFinishWithdraw",
    ),
    scriptSourcesStageFourBegin: title(
      "validationTraceDisputeScriptSourcesStageFourBeginWithdraw",
    ),
    scriptSourcesStageFourFinish: title(
      "validationTraceDisputeScriptSourcesStageFourFinishWithdraw",
    ),
    scriptSourcesStageSixBeginPolicy: title(
      "validationTraceDisputeScriptSourcesStageSixBeginPolicyWithdraw",
    ),
    scriptSourcesStageSixFoldAsset: title(
      "validationTraceDisputeScriptSourcesStageSixFoldAssetWithdraw",
    ),
    scriptSourcesStageSixFinish: title(
      "validationTraceDisputeScriptSourcesStageSixFinishWithdraw",
    ),
    scriptSourcesObserverItem: title(
      "validationTraceDisputeScriptSourcesObserverItemWithdraw",
    ),
    scriptSourcesObserverBound: title(
      "validationTraceDisputeScriptSourcesObserverBoundWithdraw",
    ),
    scriptSourcesRedeemerDescriptor: title(
      "validationTraceDisputeScriptSourcesRedeemerDescriptorWithdraw",
    ),
    ledgerOutputProofStructure: title(
      "validationTraceDisputeLedgerOutputProofStructureWithdraw",
    ),
    ledgerOutputProofValue: title(
      "validationTraceDisputeLedgerOutputProofValueWithdraw",
    ),
    ledgerOutputProofDatumFoldMap: title(
      "validationTraceDisputeLedgerOutputProofDatumFoldMapWithdraw",
    ),
    ledgerOutputProofDatumFinalizeFrame: title(
      "validationTraceDisputeLedgerOutputProofDatumFinalizeFrameWithdraw",
    ),
    ledgerOutputProofDatumHeadScalar: title(
      "validationTraceDisputeLedgerOutputProofDatumHeadScalarWithdraw",
    ),
    ledgerOutputProofDatumAttachInteger: title(
      "validationTraceDisputeLedgerOutputProofDatumAttachIntegerWithdraw",
    ),
    ledgerOutputProofDatumFoldList: title(
      "validationTraceDisputeLedgerOutputProofDatumFoldListWithdraw",
    ),
    ledgerOutputProofDatumAdvanceInteger: title(
      "validationTraceDisputeLedgerOutputProofDatumAdvanceIntegerWithdraw",
    ),
    ledgerOutputProofReferenceScript: title(
      "validationTraceDisputeLedgerOutputProofReferenceScriptWithdraw",
    ),
    ledgerOutputProofScriptHash: title(
      "validationTraceDisputeLedgerOutputProofScriptHashWithdraw",
    ),
    ledgerOutputProofNativeScript: title(
      "validationTraceDisputeLedgerOutputProofNativeScriptWithdraw",
    ),
    ledgerOutputProofStructureAssets: title(
      "validationTraceDisputeLedgerOutputProofStructureAssetsWithdraw",
    ),
    ledgerOutputProofStructureOptional: title(
      "validationTraceDisputeLedgerOutputProofStructureOptionalWithdraw",
    ),
    ledgerOutputProofStructureFinish: title(
      "validationTraceDisputeLedgerOutputProofStructureFinishWithdraw",
    ),
    ledgerOutputProofDatumHeadSequence: title(
      "validationTraceDisputeLedgerOutputProofDatumHeadSequenceWithdraw",
    ),
    ledgerOutputProofDatumHeadMap: title(
      "validationTraceDisputeLedgerOutputProofDatumHeadMapWithdraw",
    ),
    ledgerOutputProofDatumHeadLargeConstructor: title(
      "validationTraceDisputeLedgerOutputProofDatumHeadLargeConstructorWithdraw",
    ),
    ledgerOutputProofDatumAttachBytes: title(
      "validationTraceDisputeLedgerOutputProofDatumAttachBytesWithdraw",
    ),
    ledgerOutputProofDatumAdvanceBytes: title(
      "validationTraceDisputeLedgerOutputProofDatumAdvanceBytesWithdraw",
    ),
    ledgerOutputProofDatumFinish: title(
      "validationTraceDisputeLedgerOutputProofDatumFinishWithdraw",
    ),
    ledgerOutputProofDatumLargeConstructor: title(
      "validationTraceDisputeLedgerOutputProofDatumLargeConstructorWithdraw",
    ),
    ledgerOutputProofDatumLargeFields: title(
      "validationTraceDisputeLedgerOutputProofDatumLargeFieldsWithdraw",
    ),
    ledgerOutputProofDatumClose: title(
      "validationTraceDisputeLedgerOutputProofDatumCloseWithdraw",
    ),
    ledgerOutputProofSpan: title(
      "validationTraceDisputeLedgerOutputProofSpanWithdraw",
    ),
    ledgerOutputProofScalarInteger: title(
      "validationTraceDisputeLedgerOutputProofScalarIntegerWithdraw",
    ),
    ledgerOutputProofScalarBytes: title(
      "validationTraceDisputeLedgerOutputProofScalarBytesWithdraw",
    ),
    ledgerOutputDescriptorScanFacts: title(
      "validationTraceDisputeLedgerOutputDescriptorScanFactsWithdraw",
    ),
    ledgerOutputDescriptorReferenceScript: title(
      "validationTraceDisputeLedgerOutputDescriptorReferenceScriptWithdraw",
    ),
    ledgerOutputDescriptorDatumSummary: title(
      "validationTraceDisputeLedgerOutputDescriptorDatumSummaryWithdraw",
    ),
    ledgerOutputDescriptorValueSummary: title(
      "validationTraceDisputeLedgerOutputDescriptorValueSummaryWithdraw",
    ),
    phaseANativeItemNative: title(
      "validationTraceDisputePhaseANativeItemNativeWithdraw",
    ),
    phaseANativeItemForeign: title(
      "validationTraceDisputePhaseANativeItemForeignWithdraw",
    ),
    cekSelectionAuthenticate: title(
      "validationTraceDisputeCekSelectionAuthenticateWithdraw",
    ),
    cekSelectionSuccessor: title(
      "validationTraceDisputeCekSelectionSuccessorWithdraw",
    ),
    valueAndMintAssetFold: title(
      "validationTraceDisputeValueAndMintAssetFoldWithdraw",
    ),
  },
} as const;
