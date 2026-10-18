import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceRedeemerItemMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeRedeemerItemTraversalNormalizer: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item traversal normalizer",
      manifestPosition: 102,
      publicationPosition: 412,
      referenceRolePosition: 217,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1.main.spend",
      familyPaths: [
        ["steps", "90"],
        ["scriptSourcesStageOneRedeemerStages", "traversalNormalizer"],
      ],
    },
    validationTraceDisputeRedeemerItemOuterNormalizer: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item outer normalizer",
      manifestPosition: 103,
      publicationPosition: 413,
      referenceRolePosition: 218,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1.main.spend",
      familyPaths: [
        ["steps", "91"],
        ["scriptSourcesStageOneRedeemerStages", "outerNormalizer"],
      ],
    },
    validationTraceDisputeRedeemerItemSourceAuthenticator: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item source authenticator",
      manifestPosition: 104,
      publicationPosition: 414,
      referenceRolePosition: 219,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_source_authenticator.main.spend",
      familyPaths: [
        ["steps", "92"],
        ["scriptSourcesStageOneRedeemerStages", "sourceAuthenticator"],
      ],
    },
    validationTraceDisputeRedeemerItemFoldMapExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item fold map executor",
      manifestPosition: 105,
      publicationPosition: 415,
      referenceRolePosition: 220,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_map_executor_v1.main.spend",
      familyPaths: [
        ["steps", "93"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "0"],
        ["scriptSourcesStageOneRedeemerStages", "foldMapExecutor"],
      ],
    },
    validationTraceDisputeRedeemerItemFinalizeFrameExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item finalize frame executor",
      manifestPosition: 106,
      publicationPosition: 416,
      referenceRolePosition: 221,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finalize_frame_executor_v1.main.spend",
      familyPaths: [
        ["steps", "94"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "1"],
        ["scriptSourcesStageOneRedeemerStages", "finalizeFrameExecutor"],
      ],
    },
    validationTraceDisputeRedeemerItemOpenHeaderExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item open header executor",
      manifestPosition: 107,
      publicationPosition: 417,
      referenceRolePosition: 222,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_header_executor.main.spend",
      familyPaths: [
        ["steps", "95"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "2"],
      ],
    },
    validationTraceDisputeRedeemerItemOpenTailExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item open tail executor",
      manifestPosition: 108,
      publicationPosition: 418,
      referenceRolePosition: 223,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_tail_executor.main.spend",
      familyPaths: [
        ["steps", "96"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "3"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadScalarExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item head scalar executor",
      manifestPosition: 109,
      publicationPosition: 419,
      referenceRolePosition: 224,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_scalar_executor.main.spend",
      familyPaths: [
        ["steps", "97"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "4"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadSequenceExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item head sequence executor",
      manifestPosition: 110,
      publicationPosition: 420,
      referenceRolePosition: 225,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_sequence_executor.main.spend",
      familyPaths: [
        ["steps", "98"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "5"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadMapExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item head map executor",
      manifestPosition: 111,
      publicationPosition: 421,
      referenceRolePosition: 226,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_map_executor.main.spend",
      familyPaths: [
        ["steps", "99"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "6"],
      ],
    },
    validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item head large constructor executor",
      manifestPosition: 112,
      publicationPosition: 422,
      referenceRolePosition: 227,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_large_constructor_executor.main.spend",
      familyPaths: [
        ["steps", "100"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "7"],
      ],
    },
    validationTraceDisputeRedeemerItemAttachIntegerExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item attach integer executor",
      manifestPosition: 113,
      publicationPosition: 423,
      referenceRolePosition: 228,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_integer_executor.main.spend",
      familyPaths: [
        ["steps", "101"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "8"],
      ],
    },
    validationTraceDisputeRedeemerItemAttachBytesExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item attach bytes executor",
      manifestPosition: 114,
      publicationPosition: 424,
      referenceRolePosition: 229,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_bytes_executor.main.spend",
      familyPaths: [
        ["steps", "102"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "9"],
      ],
    },
    validationTraceDisputeRedeemerItemFoldListExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item fold list executor",
      manifestPosition: 115,
      publicationPosition: 425,
      referenceRolePosition: 230,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_list_executor.main.spend",
      familyPaths: [
        ["steps", "103"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "10"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceIntegerExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item advance integer executor",
      manifestPosition: 116,
      publicationPosition: 426,
      referenceRolePosition: 231,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_integer_executor.main.spend",
      familyPaths: [
        ["steps", "104"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "11"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceBytesExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item advance bytes executor",
      manifestPosition: 117,
      publicationPosition: 427,
      referenceRolePosition: 232,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_bytes_executor.main.spend",
      familyPaths: [
        ["steps", "105"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "12"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item advance large constructor executor",
      manifestPosition: 118,
      publicationPosition: 428,
      referenceRolePosition: 233,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_constructor_executor.main.spend",
      familyPaths: [
        ["steps", "106"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "13"],
      ],
    },
    validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item advance large fields executor",
      manifestPosition: 119,
      publicationPosition: 429,
      referenceRolePosition: 234,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_fields_executor.main.spend",
      familyPaths: [
        ["steps", "107"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "14"],
      ],
    },
    validationTraceDisputeRedeemerItemCloseExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item close executor",
      manifestPosition: 120,
      publicationPosition: 430,
      referenceRolePosition: 235,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_close_executor.main.spend",
      familyPaths: [
        ["steps", "108"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "15"],
      ],
    },
    validationTraceDisputeRedeemerItemFinishDataExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item finish data executor",
      manifestPosition: 121,
      publicationPosition: 431,
      referenceRolePosition: 236,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finish_data_executor.main.spend",
      familyPaths: [
        ["steps", "109"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "16"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidHeaderExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item invalid header executor",
      manifestPosition: 122,
      publicationPosition: 432,
      referenceRolePosition: 237,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_header_executor.main.spend",
      familyPaths: [
        ["steps", "110"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "17"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidTailExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item invalid tail executor",
      manifestPosition: 123,
      publicationPosition: 433,
      referenceRolePosition: 238,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_tail_executor.main.spend",
      familyPaths: [
        ["steps", "111"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "18"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidDataExecutor: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item invalid data executor",
      manifestPosition: 124,
      publicationPosition: 434,
      referenceRolePosition: 239,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_data_executor.main.spend",
      familyPaths: [
        ["steps", "112"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "19"],
      ],
    },
    validationTraceDisputeRedeemerItemInvalidScalarDataExecutor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace redeemer item invalid scalar data executor",
      manifestPosition: 125,
      publicationPosition: 435,
      referenceRolePosition: 240,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_scalar_data_executor.main.spend",
      familyPaths: [
        ["steps", "113"],
        ["scriptSourcesStageOneRedeemerStages", "executors", "20"],
      ],
    },
    validationTraceDisputeRedeemerItemSettlement: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item settlement",
      manifestPosition: 126,
      publicationPosition: 436,
      referenceRolePosition: 241,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1.main.spend",
      familyPaths: [
        ["steps", "114"],
        ["scriptSourcesStageOneRedeemerStages", "settlement"],
      ],
    },
    validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace script-sources RedeemerNormalization semantic",
      manifestPosition: 127,
      publicationPosition: 437,
      referenceRolePosition: 242,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1.main.spend",
      familyPaths: [
        ["steps", "89"],
        ["scriptSourcesStageOneRedeemerStages", "envelope"],
        ["semanticResolvers", "82"],
      ],
    },
  },
);
