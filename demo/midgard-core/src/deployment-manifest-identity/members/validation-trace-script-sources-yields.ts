import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceScriptSourcesYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeScriptSourcesStageTwoAdvanceWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageTwoAdvance yield",
        token: "V1VtSsStage2AdvanceYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_two_advance.withdraw",
      familyPaths: [["yields", "scriptSourcesStageTwoAdvance"]],
    },
    validationTraceDisputeScriptSourcesStageThreeReplayWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageThreeReplay yield",
        token: "V1VtSsStage3ReplayYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_replay.withdraw",
      familyPaths: [["yields", "scriptSourcesStageThreeReplay"]],
    },
    validationTraceDisputeScriptSourcesStageThreeFinishWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageThreeFinish yield",
        token: "V1VtSsStage3FinishYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_finish.withdraw",
      familyPaths: [["yields", "scriptSourcesStageThreeFinish"]],
    },
    validationTraceDisputeScriptSourcesStageFourBeginWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageFourBegin yield",
        token: "V1VtSsStage4BeginYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_begin.withdraw",
      familyPaths: [["yields", "scriptSourcesStageFourBegin"]],
    },
    validationTraceDisputeScriptSourcesStageFourFinishWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageFourFinish yield",
        token: "V1VtSsStage4FinishYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_finish.withdraw",
      familyPaths: [["yields", "scriptSourcesStageFourFinish"]],
    },
    validationTraceDisputeScriptSourcesStageSixBeginPolicyWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageSixBeginPolicy yield",
        token: "V1VtSsStage6BeginPolicyYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_begin_policy.withdraw",
      familyPaths: [["yields", "scriptSourcesStageSixBeginPolicy"]],
    },
    validationTraceDisputeScriptSourcesStageSixFoldAssetWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageSixFoldAsset yield",
        token: "V1VtSsStage6FoldAssetYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_fold_asset.withdraw",
      familyPaths: [["yields", "scriptSourcesStageSixFoldAsset"]],
    },
    validationTraceDisputeScriptSourcesStageSixFinishWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources StageSixFinish yield",
        token: "V1VtSsStage6FinishYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_finish.withdraw",
      familyPaths: [["yields", "scriptSourcesStageSixFinish"]],
    },
    validationTraceDisputeScriptSourcesObserverItemWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources observer item yield",
        token: "V1VtSsS07ObserverItemYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_seven_observer_item_yield_v1.main.withdraw",
      familyPaths: [["yields", "scriptSourcesObserverItem"]],
    },
    validationTraceDisputeScriptSourcesObserverBoundWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources observer bound yield",
        token: "V1VtSsS07ObserverBoundYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_seven_observer_bound_yield_v1.main.withdraw",
      familyPaths: [["yields", "scriptSourcesObserverBound"]],
    },
    validationTraceDisputeScriptSourcesRedeemerDescriptorWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace script-sources redeemer descriptor yield",
        token: "V1VtSsRedeemerItemStepYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_redeemer_item_step_yield_v1.main.withdraw",
      familyPaths: [["yields", "scriptSourcesRedeemerDescriptor"]],
    },
  },
);
