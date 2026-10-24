import { defineDeploymentFamily } from "../members.types.js";

/** The validation phases' one-step resolvers and their semantic resolvers,
 * which a thread visits between the boundary and the award. */
export const validationTracePhaseResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCompactBindingPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace compact-binding prepare",
      manifestPosition: 575,
      publicationPosition: 570,
      referenceRolePosition: 570,
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
      manifestPosition: 576,
      publicationPosition: 571,
      referenceRolePosition: 571,
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
      manifestPosition: 577,
      publicationPosition: 572,
      referenceRolePosition: 572,
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
      manifestPosition: 578,
      publicationPosition: 573,
      referenceRolePosition: 573,
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
      manifestPosition: 579,
      publicationPosition: 574,
      referenceRolePosition: 574,
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
      manifestPosition: 580,
      publicationPosition: 575,
      referenceRolePosition: 575,
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
      manifestPosition: 581,
      publicationPosition: 576,
      referenceRolePosition: 576,
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
      manifestPosition: 582,
      publicationPosition: 577,
      referenceRolePosition: 577,
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
      manifestPosition: 583,
      publicationPosition: 578,
      referenceRolePosition: 578,
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
      manifestPosition: 584,
      publicationPosition: 579,
      referenceRolePosition: 579,
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
      manifestPosition: 585,
      publicationPosition: 580,
      referenceRolePosition: 580,
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
      manifestPosition: 586,
      publicationPosition: 581,
      referenceRolePosition: 581,
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
      manifestPosition: 587,
      publicationPosition: 582,
      referenceRolePosition: 582,
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
      manifestPosition: 588,
      publicationPosition: 583,
      referenceRolePosition: 583,
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
      manifestPosition: 589,
      publicationPosition: 584,
      referenceRolePosition: 584,
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
      manifestPosition: 590,
      publicationPosition: 585,
      referenceRolePosition: 585,
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
