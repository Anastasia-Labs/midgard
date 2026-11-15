import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceScriptSourcesYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeScriptSourcesStageTwoAdvanceWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace script-sources StageTwoAdvance yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_two_advance.withdraw",
      familyPaths: [["yields", "scriptSourcesStageTwoAdvance"]],
    },
    validationTraceDisputeScriptSourcesStageThreeReplayWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 validation-trace script-sources StageThreeReplay yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_replay.withdraw",
      familyPaths: [["yields", "scriptSourcesStageThreeReplay"]],
    },
    validationTraceDisputeScriptSourcesStageThreeFinishWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 validation-trace script-sources StageThreeFinish yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_finish.withdraw",
      familyPaths: [["yields", "scriptSourcesStageThreeFinish"]],
    },
    validationTraceDisputeScriptSourcesStageFourBeginWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace script-sources StageFourBegin yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_begin.withdraw",
      familyPaths: [["yields", "scriptSourcesStageFourBegin"]],
    },
    validationTraceDisputeScriptSourcesStageFourFinishWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace script-sources StageFourFinish yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_finish.withdraw",
      familyPaths: [["yields", "scriptSourcesStageFourFinish"]],
    },
    validationTraceDisputeScriptSourcesStageSixBeginPolicyWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 validation-trace script-sources StageSixBeginPolicy yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_begin_policy.withdraw",
      familyPaths: [["yields", "scriptSourcesStageSixBeginPolicy"]],
    },
    validationTraceDisputeScriptSourcesStageSixFoldAssetWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 validation-trace script-sources StageSixFoldAsset yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_fold_asset.withdraw",
      familyPaths: [["yields", "scriptSourcesStageSixFoldAsset"]],
    },
    validationTraceDisputeScriptSourcesStageSixFinishWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace script-sources StageSixFinish yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_finish.withdraw",
      familyPaths: [["yields", "scriptSourcesStageSixFinish"]],
    },
    validationTraceDisputeScriptSourcesObserverItemWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace script-sources observer item yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_seven_observer_item_yield_v1.main.withdraw",
      familyPaths: [["yields", "scriptSourcesObserverItem"]],
    },
    validationTraceDisputeScriptSourcesObserverBoundWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace script-sources observer bound yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_seven_observer_bound_yield_v1.main.withdraw",
      familyPaths: [["yields", "scriptSourcesObserverBound"]],
    },
    validationTraceDisputeScriptSourcesRedeemerDescriptorWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 validation-trace script-sources redeemer descriptor yield",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_redeemer_item_step_yield_v1.main.withdraw",
      familyPaths: [["yields", "scriptSourcesRedeemerDescriptor"]],
    },
  },
);
