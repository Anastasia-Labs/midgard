import { withdraw } from "./recipes.types.js";

export const validationTraceScriptSourcesYieldsRecipes = {
  validationTraceDisputeScriptSourcesStageTwoAdvanceWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageTwoAdvance,
  ),
  validationTraceDisputeScriptSourcesStageThreeReplayWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageThreeReplay,
  ),
  validationTraceDisputeScriptSourcesStageThreeFinishWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageThreeFinish,
  ),
  validationTraceDisputeScriptSourcesStageFourBeginWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageFourBegin,
  ),
  validationTraceDisputeScriptSourcesStageFourFinishWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageFourFinish,
  ),
  validationTraceDisputeScriptSourcesStageSixBeginPolicyWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageSixBeginPolicy,
  ),
  validationTraceDisputeScriptSourcesStageSixFoldAssetWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageSixFoldAsset,
  ),
  validationTraceDisputeScriptSourcesStageSixFinishWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesStageSixFinish,
  ),
  validationTraceDisputeScriptSourcesObserverItemWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesObserverItem,
  ),
  validationTraceDisputeScriptSourcesObserverBoundWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesObserverBound,
  ),
  validationTraceDisputeScriptSourcesRedeemerDescriptorWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .scriptSourcesRedeemerDescriptor,
  ),
} as const;
