import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceRedeemerItemMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeRedeemerItemTraversalNormalizer: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item traversal normalizer",
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
      referenceRole: "V1 validation-trace redeemer item outer normalizer",
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
      referenceRole: "V1 validation-trace redeemer item source authenticator",
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
      referenceRole: "V1 validation-trace redeemer item fold map executor",
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
      referenceRole:
        "V1 validation-trace redeemer item finalize frame executor",
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
      referenceRole: "V1 validation-trace redeemer item open header executor",
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
      referenceRole: "V1 validation-trace redeemer item open tail executor",
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
      referenceRole: "V1 validation-trace redeemer item head scalar executor",
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
      referenceRole: "V1 validation-trace redeemer item head sequence executor",
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
      referenceRole: "V1 validation-trace redeemer item head map executor",
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
      referenceRole:
        "V1 validation-trace redeemer item head large constructor executor",
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
      referenceRole:
        "V1 validation-trace redeemer item attach integer executor",
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
      referenceRole: "V1 validation-trace redeemer item attach bytes executor",
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
      referenceRole: "V1 validation-trace redeemer item fold list executor",
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
      referenceRole:
        "V1 validation-trace redeemer item advance integer executor",
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
      referenceRole: "V1 validation-trace redeemer item advance bytes executor",
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
      referenceRole:
        "V1 validation-trace redeemer item advance large constructor executor",
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
      referenceRole:
        "V1 validation-trace redeemer item advance large fields executor",
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
      referenceRole: "V1 validation-trace redeemer item close executor",
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
      referenceRole: "V1 validation-trace redeemer item finish data executor",
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
      referenceRole:
        "V1 validation-trace redeemer item invalid header executor",
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
      referenceRole: "V1 validation-trace redeemer item invalid tail executor",
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
      referenceRole: "V1 validation-trace redeemer item invalid data executor",
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
      referenceRole:
        "V1 validation-trace redeemer item invalid scalar data executor",
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
      referenceRole: "V1 validation-trace redeemer item settlement",
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
      referenceRole:
        "V1 validation-trace script-sources RedeemerNormalization semantic",
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
