import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceScriptSourcesSemanticsMembers =
  defineDeploymentFamily("validationTraceDispute", {
    validationTraceDisputeScriptSourcesNonOutputSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources NonOutput semantic",
        token: "V1VtSsNonOutput",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "32"],
        ["steps", "39"],
      ],
    },
    validationTraceDisputeScriptSourcesOutputProofBeginSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources OutputProofBegin semantic",
        token: "V1VtSsOutputProofBegin",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "33"],
        ["steps", "40"],
      ],
    },
    validationTraceDisputeScriptSourcesOutputProofStepSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources OutputProofStep semantic",
        token: "V1VtSsOutputProofStep",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "34"],
        ["steps", "41"],
      ],
    },
    validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources OutputProofFinalize semantic",
        token: "V1VtSsOutputProofFinalize",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "35"],
        ["steps", "42"],
      ],
    },
    validationTraceDisputeScriptSourcesOutputProofFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources OutputProofFinish semantic",
        token: "V1VtSsOutputProofFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "36"],
        ["steps", "43"],
      ],
    },
    validationTraceDisputeScriptSourcesStageZeroBeginSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageZeroBegin semantic",
        token: "V1VtSsS0Begin",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "37"],
        ["steps", "44"],
      ],
    },
    validationTraceDisputeScriptSourcesStageZeroFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageZeroFinish semantic",
        token: "V1VtSsS0Finish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_zero_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "38"],
        ["steps", "45"],
      ],
    },
    validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageZeroHashBlock semantic",
        token: "V1VtSsS0HashBlock",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_zero_hash_block_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "39"],
        ["steps", "46"],
      ],
    },
    validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageZeroHashAdvance semantic",
        token: "V1VtSsS0HashAdvance",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_zero_hash_advance_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "40"],
        ["steps", "47"],
      ],
    },
    validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageZeroHashTerminal semantic",
        token: "V1VtSsS0HashTerminal",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_zero_hash_terminal_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "41"],
        ["steps", "48"],
      ],
    },
    validationTraceDisputeScriptSourcesStageNineMismatchSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageNineMismatch semantic",
        token: "V1VtSsS9Mismatch",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_nine_mismatch_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "42"],
        ["steps", "49"],
      ],
    },
    validationTraceDisputeScriptSourcesStageNineNativeMatchSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageNineNativeMatch semantic",
        token: "V1VtSsS9NativeMatch",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_nine_native_match_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "43"],
        ["steps", "50"],
      ],
    },
    validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageNineEffectfulMatch semantic",
        token: "V1VtSsS9EffectfulMatch",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_nine_effectful_match_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "44"],
        ["steps", "51"],
      ],
    },
    validationTraceDisputeScriptSourcesStageNineMissingSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageNineMissing semantic",
        token: "V1VtSsS9Missing",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_nine_missing_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "45"],
        ["steps", "52"],
      ],
    },
    validationTraceDisputeScriptSourcesStageOneFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageOneFinish semantic",
        token: "V1VtSsS1Finish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "46"],
        ["steps", "53"],
      ],
    },
    validationTraceDisputeScriptSourcesStageOneRedeemerSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageOneRedeemer semantic",
        token: "V1VtSsS1Redeemer",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "47"],
        ["steps", "54"],
      ],
    },
    validationTraceDisputeScriptSourcesStageElevenFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageElevenFinish semantic",
        token: "V1VtSsS11Finish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_eleven_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "48"],
        ["steps", "55"],
      ],
    },
    validationTraceDisputeScriptSourcesStageElevenSourceSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageElevenSource semantic",
        token: "V1VtSsS11Source",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_eleven_source_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "49"],
        ["steps", "56"],
      ],
    },
    validationTraceDisputeScriptSourcesStageTwelveFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageTwelveFinish semantic",
        token: "V1VtSsS12Finish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_twelve_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "50"],
        ["steps", "57"],
      ],
    },
    validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageTwelveRedeemer semantic",
        token: "V1VtSsS12Redeemer",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_twelve_redeemer_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "51"],
        ["steps", "58"],
      ],
    },
    validationTraceDisputeScriptSourcesStageTenMissingSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageTenMissing semantic",
        token: "V1VtSsS10Missing",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_ten_missing_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "52"],
        ["steps", "59"],
      ],
    },
    validationTraceDisputeScriptSourcesStageTenMismatchSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageTenMismatch semantic",
        token: "V1VtSsS10Mismatch",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_ten_mismatch_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "53"],
        ["steps", "60"],
      ],
    },
    validationTraceDisputeScriptSourcesStageTenMatchSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageTenMatch semantic",
        token: "V1VtSsS10Match",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_ten_match_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "54"],
        ["steps", "61"],
      ],
    },
    validationTraceDisputeScriptSourcesStageEightFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageEightFinish semantic",
        token: "V1VtSsS8Finish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_eight_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "55"],
        ["steps", "62"],
      ],
    },
    validationTraceDisputeScriptSourcesStageEightPurposeSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageEightPurpose semantic",
        token: "V1VtSsS8Purpose",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_eight_purpose_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "56"],
        ["steps", "63"],
      ],
    },
    validationTraceDisputeScriptSourcesStageSevenObserverSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageSevenObserver semantic",
        token: "V1VtSsS7Observer",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_seven_observer_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "57"],
        ["steps", "64"],
      ],
    },
    validationTraceDisputeScriptSourcesStageSevenReceiveSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageSevenReceive semantic",
        token: "V1VtSsS7Receive",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_seven_receive_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "58"],
        ["steps", "65"],
      ],
    },
    validationTraceDisputeScriptSourcesStageSevenFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources StageSevenFinish semantic",
        token: "V1VtSsS7Finish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_seven_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "59"],
        ["steps", "66"],
      ],
    },
  });
