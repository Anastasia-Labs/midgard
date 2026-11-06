/** The holders of each validation phase's resolution, with the deployment
 * manifest's name for each. */
import { SCRIPT_SOURCES_REDEEMER_SEMANTIC } from "./one-step-script-sources-redeemer.js";

/** The holders of each phase's resolution, with the deployment manifest's
 * name for each. */
export const PHASE_CONTRACT_NAMES = Object.freeze({
  compactBindingPrepare: "validationTraceDisputeCompactBindingPrepare",
  compactBindingSemantic: "validationTraceDisputeCompactBindingSemantic",
  staticLedgerRulesPrepare: "validationTraceDisputeStaticLedgerRulesPrepare",
  staticLedgerRulesSemantic: "validationTraceDisputeStaticLedgerRulesSemantic",
  inputSetsPrepare: "validationTraceDisputeInputSetsPrepare",
  inputSetsEmptySemantic: "validationTraceDisputeInputSetsEmptySemantic",
  inputSetsItemSemantic: "validationTraceDisputeInputSetsItemSemantic",
  signaturesPrepare: "validationTraceDisputeSignaturesPrepare",
  signaturesAdvanceSemantic: "validationTraceDisputeSignaturesAdvanceSemantic",
  signaturesAddressItemSemantic:
    "validationTraceDisputeSignaturesAddressItemSemantic",
  signaturesRequiredItemSemantic:
    "validationTraceDisputeSignaturesRequiredItemSemantic",
  signaturesHandoffSemantic: "validationTraceDisputeSignaturesHandoffSemantic",
  phaseANativeScriptsPrepare:
    "validationTraceDisputePhaseANativeScriptsPrepare",
  phaseANativeScriptsAdvanceSemantic:
    "validationTraceDisputePhaseANativeScriptsAdvanceSemantic",
  phaseANativeScriptsItemSemantic:
    "validationTraceDisputePhaseANativeScriptsItemSemantic",
  phaseANativeScriptsTokenHeadSemantic:
    "validationTraceDisputePhaseANativeScriptsTokenHeadSemantic",
  phaseANativeScriptsAllOrAnyContainerFrameSemantic:
    "validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic",
  phaseANativeScriptsAllOrAnyEmptySemantic:
    "validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic",
  phaseANativeScriptsAtLeastContainerFrameSemantic:
    "validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic",
  phaseANativeScriptsAtLeastEmptySemantic:
    "validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic",
  phaseANativeScriptsTimelockSemantic:
    "validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic",
  phaseANativeScriptsSignatureMembershipSemantic:
    "validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic",
  phaseANativeScriptsSignatureEmptySemantic:
    "validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic",
  phaseANativeScriptsSignatureBelowFirstSemantic:
    "validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic",
  phaseANativeScriptsSignatureAboveLastSemantic:
    "validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic",
  phaseANativeScriptsSignatureBetweenSemantic:
    "validationTraceDisputePhaseANativeScriptsSignatureBetweenPayloadSemantic",
  phaseANativeScriptsFrameSemantic:
    "validationTraceDisputePhaseANativeScriptsFrameSemantic",
  phaseAScriptPreconditionsPrepare:
    "validationTraceDisputePhaseAScriptPreconditionsPrepare",
  phaseAScriptPreconditionsFinalizeSemantic:
    "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
  phaseAScriptPreconditionsItemSemantic:
    "validationTraceDisputePhaseAScriptPreconditionsItemSemantic",
  resolveInputsPrepare: "validationTraceDisputeResolveInputsPrepare",
  resolveInputsInitialSemantic:
    "validationTraceDisputeResolveInputsInitialSemantic",
  resolveInputsFinishSemantic:
    "validationTraceDisputeResolveInputsFinishSemantic",
  resolveInputsMembershipBeginSemantic:
    "validationTraceDisputeResolveInputsMembershipBeginSemantic",
  resolveInputsMembershipStepSemantic:
    "validationTraceDisputeResolveInputsMembershipStepSemantic",
  resolveInputsMembershipFinalizeSemantic:
    "validationTraceDisputeResolveInputsMembershipFinalizeSemantic",
  scriptSourcesPrepare: "validationTraceDisputeScriptSourcesPrepare",
  scriptSourcesNonOutputSemantic:
    "validationTraceDisputeScriptSourcesNonOutputSemantic",
  scriptSourcesOutputProofBeginSemantic:
    "validationTraceDisputeScriptSourcesOutputProofBeginSemantic",
  scriptSourcesOutputProofStepSemantic:
    "validationTraceDisputeScriptSourcesOutputProofStepSemantic",
  scriptSourcesOutputProofFinalizeSemantic:
    "validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic",
  scriptSourcesOutputProofFinishSemantic:
    "validationTraceDisputeScriptSourcesOutputProofFinishSemantic",
  scriptSourcesStageZeroBeginSemantic:
    "validationTraceDisputeScriptSourcesStageZeroBeginSemantic",
  scriptSourcesStageZeroFinishSemantic:
    "validationTraceDisputeScriptSourcesStageZeroFinishSemantic",
  scriptSourcesStageZeroHashBlockSemantic:
    "validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic",
  scriptSourcesStageZeroHashAdvanceSemantic:
    "validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic",
  scriptSourcesStageZeroHashTerminalSemantic:
    "validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic",
  scriptSourcesStageNineMismatchSemantic:
    "validationTraceDisputeScriptSourcesStageNineMismatchSemantic",
  scriptSourcesStageNineNativeMatchSemantic:
    "validationTraceDisputeScriptSourcesStageNineNativeMatchSemantic",
  scriptSourcesStageNineEffectfulMatchSemantic:
    "validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic",
  scriptSourcesStageOneFinishSemantic:
    "validationTraceDisputeScriptSourcesStageOneFinishSemantic",
  scriptSourcesStageOneRedeemerSemantic:
    "validationTraceDisputeScriptSourcesStageOneRedeemerSemantic",
  scriptSourcesStageElevenFinishSemantic:
    "validationTraceDisputeScriptSourcesStageElevenFinishSemantic",
  scriptSourcesStageElevenSourceSemantic:
    "validationTraceDisputeScriptSourcesStageElevenSourceSemantic",
  scriptSourcesStageTwelveFinishSemantic:
    "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic",
  scriptSourcesStageTwelveRedeemerSemantic:
    "validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic",
  scriptSourcesStageTenMismatchSemantic:
    "validationTraceDisputeScriptSourcesStageTenMismatchSemantic",
  scriptSourcesStageTenMatchSemantic:
    "validationTraceDisputeScriptSourcesStageTenMatchSemantic",
  scriptSourcesStageEightFinishSemantic:
    "validationTraceDisputeScriptSourcesStageEightFinishSemantic",
  scriptSourcesStageEightPurposeSemantic:
    "validationTraceDisputeScriptSourcesStageEightPurposeSemantic",
  scriptSourcesStageSevenObserverSemantic:
    "validationTraceDisputeScriptSourcesStageSevenObserverSemantic",
  scriptSourcesStageSevenReceiveSemantic:
    "validationTraceDisputeScriptSourcesStageSevenReceiveSemantic",
  scriptSourcesStageSevenFinishSemantic:
    "validationTraceDisputeScriptSourcesStageSevenFinishSemantic",
  [SCRIPT_SOURCES_REDEEMER_SEMANTIC]:
    "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic",
  nativeScriptsPrepare: "validationTraceDisputeNativeScriptsPrepare",
  nativeScriptsTerminalSemantic:
    "validationTraceDisputeNativeScriptsTerminalSemantic",
  nativeScriptsNativeSemantic:
    "validationTraceDisputeNativeScriptsNativeSemantic",
  nativeScriptsEffectfulSemantic:
    "validationTraceDisputeNativeScriptsEffectfulSemantic",
  scriptIntegrityPrepare: "validationTraceDisputeScriptIntegrityPrepare",
  scriptIntegrityAuthenticationSemantic:
    "validationTraceDisputeScriptIntegrityAuthenticationSemantic",
  scriptIntegrityCompactSemantic:
    "validationTraceDisputeScriptIntegrityCompactSemantic",
  scriptIntegrityWitnessSetSemantic:
    "validationTraceDisputeScriptIntegrityWitnessSetSemantic",
  scriptIntegrityFinalizeSemantic:
    "validationTraceDisputeScriptIntegrityFinalizeSemantic",
} as const);

export type ValidationTracePhaseHolder = keyof typeof PHASE_CONTRACT_NAMES;

export const PHASE_HOLDERS = Object.freeze(
  Object.keys(PHASE_CONTRACT_NAMES),
) as readonly ValidationTracePhaseHolder[];

export const isValidationTracePhaseHolder = (
  holder: string,
): holder is ValidationTracePhaseHolder =>
  Object.hasOwn(PHASE_CONTRACT_NAMES, holder);
