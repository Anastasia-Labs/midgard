import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceRedeemerItemMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeRedeemerItemTraversalNormalizer: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item traversal normalizer",
        token: "V1VtRiTraversalNorm",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "traversalNormalizer"],
        ["steps", "90"],
      ],
    },
    validationTraceDisputeRedeemerItemOuterNormalizer: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item outer normalizer",
        token: "V1VtRiOuterNorm",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "outerNormalizer"],
        ["steps", "91"],
      ],
    },
    validationTraceDisputeRedeemerItemSourceAuthenticator: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item source authenticator",
        token: "V1VtRiSource",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_source_authenticator.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "sourceAuthenticator"],
        ["steps", "92"],
      ],
    },
    validationTraceDisputeRedeemerItemFoldMapExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item fold map executor",
        token: "V1VtRiFoldMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_map_executor_v1.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "0"],
        ["steps", "93"],
        ["scriptSourcesStageOneRedeemerStages", "foldMapExecutor"],
      ],
    },
    validationTraceDisputeRedeemerItemFinalizeFrameExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item finalize frame executor",
        token: "V1VtRiFinalizeFrame",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finalize_frame_executor_v1.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "1"],
        ["steps", "94"],
        ["scriptSourcesStageOneRedeemerStages", "finalizeFrameExecutor"],
      ],
    },
    validationTraceDisputeRedeemerItemOpenHeaderExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item open header executor",
        token: "V1VtRiOpenHeader",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_header_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "2"],
        ["steps", "95"],
      ],
    },
    validationTraceDisputeRedeemerItemOpenTailExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item open tail executor",
        token: "V1VtRiOpenTail",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_tail_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "3"],
        ["steps", "96"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadScalarExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item head scalar executor",
        token: "V1VtRiHeadScalar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_scalar_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "4"],
        ["steps", "97"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadSequenceExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item head sequence executor",
        token: "V1VtRiHeadSequence",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_sequence_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "5"],
        ["steps", "98"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadMapExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item head map executor",
        token: "V1VtRiHeadMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_map_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "6"],
        ["steps", "99"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item head large constructor executor",
        token: "V1VtRiHeadLargeCtor",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_large_constructor_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "7"],
        ["steps", "100"],
      ],
    },
    validationTraceDisputeRedeemerItemAttachIntegerExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item attach integer executor",
        token: "V1VtRiAttachInteger",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_integer_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "8"],
        ["steps", "101"],
      ],
    },
    validationTraceDisputeRedeemerItemAttachBytesExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item attach bytes executor",
        token: "V1VtRiAttachBytes",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_bytes_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "9"],
        ["steps", "102"],
      ],
    },
    validationTraceDisputeRedeemerItemFoldListExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item fold list executor",
        token: "V1VtRiFoldList",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_list_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "10"],
        ["steps", "103"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceIntegerExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item advance integer executor",
        token: "V1VtRiAdvanceInteger",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_integer_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "11"],
        ["steps", "104"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceBytesExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item advance bytes executor",
        token: "V1VtRiAdvanceBytes",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_bytes_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "12"],
        ["steps", "105"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item advance large constructor executor",
        token: "V1VtRiAdvanceLargeCtor",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_constructor_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "13"],
        ["steps", "106"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item advance large fields executor",
        token: "V1VtRiAdvanceLargeFields",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_fields_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "14"],
        ["steps", "107"],
      ],
    },
    validationTraceDisputeRedeemerItemCloseExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item close executor",
        token: "V1VtRiClose",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_close_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "15"],
        ["steps", "108"],
      ],
    },
    validationTraceDisputeRedeemerItemFinishDataExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item finish data executor",
        token: "V1VtRiFinishData",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finish_data_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "16"],
        ["steps", "109"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidHeaderExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item invalid header executor",
        token: "V1VtRiInvalidHeader",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_header_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "17"],
        ["steps", "110"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidTailExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item invalid tail executor",
        token: "V1VtRiInvalidTail",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_tail_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "18"],
        ["steps", "111"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidDataExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item invalid data executor",
        token: "V1VtRiInvalidData",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_data_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "19"],
        ["steps", "112"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidScalarDataExecutor: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item invalid scalar data executor",
        token: "V1VtRiInvalidScalarData",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_scalar_data_executor.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "executors", "20"],
        ["steps", "113"],
      ],
    },
    validationTraceDisputeRedeemerItemSettlement: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace redeemer item settlement",
        token: "V1VtRiSettlement",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "settlement"],
        ["steps", "114"],
      ],
    },
    validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace script-sources RedeemerNormalization semantic",
        token: "V1VtSsRedeemerNorm",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1.main.spend",
      familyPaths: [
        ["scriptSourcesStageOneRedeemerStages", "envelope"],
        ["steps", "89"],
        ["semanticResolvers", "82"],
      ],
    },
  },
);
