import { spend } from "./recipes.types.js";

export const validationTraceRedeemerItemRecipes = {
  validationTraceDisputeRedeemerItemTraversalNormalizer: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.traversalNormalizer,
  ),
  validationTraceDisputeRedeemerItemOuterNormalizer: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.outerNormalizer,
  ),
  validationTraceDisputeRedeemerItemSourceAuthenticator: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.sourceAuthenticator,
  ),
  validationTraceDisputeRedeemerItemFoldMapExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.foldMapExecutor,
  ),
  validationTraceDisputeRedeemerItemFinalizeFrameExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.finalizeFrameExecutor,
  ),
  validationTraceDisputeRedeemerItemOpenHeaderExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[2],
  ),
  validationTraceDisputeRedeemerItemOpenTailExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[3],
  ),
  validationTraceDisputeRedeemerItemHeadScalarExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[4],
  ),
  validationTraceDisputeRedeemerItemHeadSequenceExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[5],
  ),
  validationTraceDisputeRedeemerItemHeadMapExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[6],
  ),
  validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[7],
  ),
  validationTraceDisputeRedeemerItemAttachIntegerExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[8],
  ),
  validationTraceDisputeRedeemerItemAttachBytesExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[9],
  ),
  validationTraceDisputeRedeemerItemFoldListExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[10],
  ),
  validationTraceDisputeRedeemerItemAdvanceIntegerExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[11],
  ),
  validationTraceDisputeRedeemerItemAdvanceBytesExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[12],
  ),
  validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[13],
  ),
  validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[14],
  ),
  validationTraceDisputeRedeemerItemCloseExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[15],
  ),
  validationTraceDisputeRedeemerItemFinishDataExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[16],
  ),
  validationTraceDisputeRedeemerItemInvalidHeaderExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[17],
  ),
  validationTraceDisputeRedeemerItemInvalidTailExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[18],
  ),
  validationTraceDisputeRedeemerItemInvalidDataExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[19],
  ),
  validationTraceDisputeRedeemerItemInvalidScalarDataExecutor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.executors[20],
  ),
  validationTraceDisputeRedeemerItemSettlement: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.settlement,
  ),
  validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute
        .scriptSourcesStageOneRedeemerStages.envelope,
  ),
} as const;
