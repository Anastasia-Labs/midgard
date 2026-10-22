import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceRedeemerItemMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeRedeemerItemTraversalNormalizer: {
      purpose: "spend",
      referenceRole: "V1 validation-trace redeemer item traversal normalizer",
      manifestPosition: 103,
      publicationPosition: 382,
      referenceRolePosition: 218,
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
      manifestPosition: 104,
      publicationPosition: 383,
      referenceRolePosition: 219,
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
      manifestPosition: 105,
      publicationPosition: 384,
      referenceRolePosition: 220,
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
      manifestPosition: 106,
      publicationPosition: 385,
      referenceRolePosition: 221,
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
      manifestPosition: 107,
      publicationPosition: 386,
      referenceRolePosition: 222,
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
      manifestPosition: 108,
      publicationPosition: 387,
      referenceRolePosition: 223,
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
      manifestPosition: 109,
      publicationPosition: 388,
      referenceRolePosition: 224,
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
      manifestPosition: 110,
      publicationPosition: 389,
      referenceRolePosition: 225,
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
      manifestPosition: 111,
      publicationPosition: 390,
      referenceRolePosition: 226,
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
      manifestPosition: 112,
      publicationPosition: 391,
      referenceRolePosition: 227,
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
      manifestPosition: 113,
      publicationPosition: 392,
      referenceRolePosition: 228,
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
      manifestPosition: 114,
      publicationPosition: 393,
      referenceRolePosition: 229,
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
      manifestPosition: 115,
      publicationPosition: 394,
      referenceRolePosition: 230,
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
      manifestPosition: 116,
      publicationPosition: 395,
      referenceRolePosition: 231,
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
      manifestPosition: 117,
      publicationPosition: 396,
      referenceRolePosition: 232,
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
      manifestPosition: 118,
      publicationPosition: 397,
      referenceRolePosition: 233,
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
      manifestPosition: 119,
      publicationPosition: 398,
      referenceRolePosition: 234,
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
      manifestPosition: 120,
      publicationPosition: 399,
      referenceRolePosition: 235,
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
      manifestPosition: 121,
      publicationPosition: 400,
      referenceRolePosition: 236,
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
      manifestPosition: 122,
      publicationPosition: 401,
      referenceRolePosition: 237,
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
      manifestPosition: 123,
      publicationPosition: 402,
      referenceRolePosition: 238,
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
      manifestPosition: 124,
      publicationPosition: 403,
      referenceRolePosition: 239,
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
      manifestPosition: 125,
      publicationPosition: 404,
      referenceRolePosition: 240,
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
      manifestPosition: 126,
      publicationPosition: 405,
      referenceRolePosition: 241,
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
      manifestPosition: 127,
      publicationPosition: 406,
      referenceRolePosition: 242,
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
      manifestPosition: 128,
      publicationPosition: 407,
      referenceRolePosition: 243,
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
