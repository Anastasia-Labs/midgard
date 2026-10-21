import { defineDeploymentFamily } from "../members.types.js";

/** The validation phases' one-step resolvers and their semantic resolvers,
 * which a thread visits between the boundary and the award. */
export const validationTracePhaseResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCompactBindingPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace compact-binding prepare",
      manifestPosition: 543,
      publicationPosition: 538,
      referenceRolePosition: 538,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/compact_binding_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "1"],
        ["resolvers", "1"],
        ["steps", "156"],
      ],
    },
    validationTraceDisputeCompactBindingSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace compact-binding semantic",
      manifestPosition: 544,
      publicationPosition: 539,
      referenceRolePosition: 539,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/compact_binding_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "2"],
        ["steps", "9"],
      ],
    },
    validationTraceDisputeStaticLedgerRulesPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace static-ledger-rules prepare",
      manifestPosition: 545,
      publicationPosition: 540,
      referenceRolePosition: 540,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/static_ledger_rules_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "2"],
        ["resolvers", "2"],
        ["steps", "157"],
      ],
    },
    validationTraceDisputeStaticLedgerRulesSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace static-ledger-rules semantic",
      manifestPosition: 546,
      publicationPosition: 541,
      referenceRolePosition: 541,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/static_ledger_rules_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "3"],
        ["steps", "10"],
      ],
    },
    validationTraceDisputeInputSetsPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace input-sets prepare",
      manifestPosition: 547,
      publicationPosition: 542,
      referenceRolePosition: 542,
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/input_sets_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "3"],
        ["resolvers", "3"],
        ["steps", "158"],
      ],
    },
    validationTraceDisputeInputSetsEmptySemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace input-sets empty semantic",
      manifestPosition: 548,
      publicationPosition: 543,
      referenceRolePosition: 543,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/input_sets_empty_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "4"],
        ["steps", "11"],
      ],
    },
    validationTraceDisputeInputSetsItemSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace input-sets item semantic",
      manifestPosition: 549,
      publicationPosition: 544,
      referenceRolePosition: 544,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/input_sets_item_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "5"],
        ["steps", "12"],
      ],
    },
    validationTraceDisputeSignaturesPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace signatures prepare",
      manifestPosition: 550,
      publicationPosition: 545,
      referenceRolePosition: 545,
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/signatures_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "4"],
        ["resolvers", "4"],
        ["steps", "159"],
      ],
    },
    validationTraceDisputeSignaturesAdvanceSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace signatures advance semantic",
      manifestPosition: 551,
      publicationPosition: 546,
      referenceRolePosition: 546,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/signatures_advance_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "6"],
        ["steps", "13"],
      ],
    },
    validationTraceDisputeSignaturesAddressItemSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace signatures address item semantic",
      manifestPosition: 552,
      publicationPosition: 547,
      referenceRolePosition: 547,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/signatures_address_item_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "7"],
        ["steps", "14"],
      ],
    },
    validationTraceDisputeSignaturesRequiredItemSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace signatures required item semantic",
      manifestPosition: 553,
      publicationPosition: 548,
      referenceRolePosition: 548,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/signatures_required_item_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "8"],
        ["steps", "15"],
      ],
    },
    validationTraceDisputeSignaturesHandoffSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace signatures handoff semantic",
      manifestPosition: 554,
      publicationPosition: 549,
      referenceRolePosition: 549,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/signatures_handoff_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "9"],
        ["steps", "16"],
      ],
    },
    validationTraceDisputePhaseANativeScriptsPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace phase-A native scripts prepare",
      manifestPosition: 555,
      publicationPosition: 550,
      referenceRolePosition: 550,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "5"],
        ["resolvers", "5"],
        ["steps", "160"],
      ],
    },
    validationTraceDisputePhaseAScriptPreconditionsPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace phase-A script preconditions prepare",
      manifestPosition: 556,
      publicationPosition: 551,
      referenceRolePosition: 551,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_script_preconditions_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "6"],
        ["resolvers", "6"],
        ["steps", "161"],
      ],
    },
    validationTraceDisputeResolveInputsPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace resolve-inputs prepare",
      manifestPosition: 557,
      publicationPosition: 552,
      referenceRolePosition: 552,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "7"],
        ["resolvers", "7"],
        ["steps", "162"],
      ],
    },
    validationTraceDisputeScriptSourcesPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace script-sources prepare",
      manifestPosition: 558,
      publicationPosition: 553,
      referenceRolePosition: 553,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "8"],
        ["resolvers", "8"],
        ["steps", "163"],
      ],
    },
  },
);
