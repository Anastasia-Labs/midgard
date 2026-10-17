import { spend } from "./recipes.types.js";

export const validationTraceScriptSourcesSemanticsRecipes = {
  validationTraceDisputeScriptSourcesNonOutputSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[32],
  ),
  validationTraceDisputeScriptSourcesOutputProofBeginSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[33],
  ),
  validationTraceDisputeScriptSourcesOutputProofStepSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[34],
  ),
  validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[35],
  ),
  validationTraceDisputeScriptSourcesOutputProofFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[36],
  ),
  validationTraceDisputeScriptSourcesStageZeroBeginSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[37],
  ),
  validationTraceDisputeScriptSourcesStageZeroFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[38],
  ),
  validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[39],
  ),
  validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[40],
  ),
  validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[41],
  ),
  validationTraceDisputeScriptSourcesStageNineMismatchSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[42],
  ),
  validationTraceDisputeScriptSourcesStageNineNativeMatchSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[43],
  ),
  validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[44],
  ),
  validationTraceDisputeScriptSourcesStageNineMissingSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[45],
  ),
  validationTraceDisputeScriptSourcesStageOneFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[46],
  ),
  validationTraceDisputeScriptSourcesStageOneRedeemerSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[47],
  ),
  validationTraceDisputeScriptSourcesStageElevenFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[48],
  ),
  validationTraceDisputeScriptSourcesStageElevenSourceSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[49],
  ),
  validationTraceDisputeScriptSourcesStageTwelveFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[50],
  ),
  validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[51],
  ),
  validationTraceDisputeScriptSourcesStageTenMissingSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[52],
  ),
  validationTraceDisputeScriptSourcesStageTenMismatchSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[53],
  ),
  validationTraceDisputeScriptSourcesStageTenMatchSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[54],
  ),
  validationTraceDisputeScriptSourcesStageEightFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[55],
  ),
  validationTraceDisputeScriptSourcesStageEightPurposeSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[56],
  ),
  validationTraceDisputeScriptSourcesStageSevenObserverSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[57],
  ),
  validationTraceDisputeScriptSourcesStageSevenReceiveSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[58],
  ),
  validationTraceDisputeScriptSourcesStageSevenFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[59],
  ),
} as const;
