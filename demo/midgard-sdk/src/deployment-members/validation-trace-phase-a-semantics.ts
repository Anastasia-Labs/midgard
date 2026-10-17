import { spend } from "./recipes.types.js";

export const validationTracePhaseASemanticsRecipes = {
  validationTraceDisputePhaseANativeScriptsAdvanceSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[10],
  ),
  validationTraceDisputePhaseANativeScriptsItemSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[11],
  ),
  validationTraceDisputePhaseANativeScriptsTokenHeadSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[12],
  ),
  validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[13],
    ),
  validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[14],
    ),
  validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[15],
    ),
  validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[16],
    ),
  validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[17],
  ),
  validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[18],
    ),
  validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[19],
  ),
  validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[20],
    ),
  validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[21],
    ),
  validationTraceDisputePhaseANativeScriptsSignatureBetweenPayloadSemantic:
    spend(
      (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[22],
    ),
  validationTraceDisputePhaseANativeScriptsFrameSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[23],
  ),
  validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[24],
  ),
  validationTraceDisputePhaseAScriptPreconditionsItemSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[25],
  ),
} as const;
