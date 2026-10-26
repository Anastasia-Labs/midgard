import { defineDeploymentFamily } from "../members.types.js";

/** The validation phases' one-step resolvers and their semantic resolvers,
 * which a thread visits between the boundary and the award. */
export const validationTracePhaseResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCompactBindingPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace compact-binding prepare",
      manifestPosition: 622,
      publicationPosition: 617,
      referenceRolePosition: 617,
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
      manifestPosition: 623,
      publicationPosition: 618,
      referenceRolePosition: 618,
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
      manifestPosition: 624,
      publicationPosition: 619,
      referenceRolePosition: 619,
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
      manifestPosition: 625,
      publicationPosition: 620,
      referenceRolePosition: 620,
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
      manifestPosition: 626,
      publicationPosition: 621,
      referenceRolePosition: 621,
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
      manifestPosition: 627,
      publicationPosition: 622,
      referenceRolePosition: 622,
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
      manifestPosition: 628,
      publicationPosition: 623,
      referenceRolePosition: 623,
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
      manifestPosition: 629,
      publicationPosition: 624,
      referenceRolePosition: 624,
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
      manifestPosition: 630,
      publicationPosition: 625,
      referenceRolePosition: 625,
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
      manifestPosition: 631,
      publicationPosition: 626,
      referenceRolePosition: 626,
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
      manifestPosition: 632,
      publicationPosition: 627,
      referenceRolePosition: 627,
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
      manifestPosition: 633,
      publicationPosition: 628,
      referenceRolePosition: 628,
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
      manifestPosition: 634,
      publicationPosition: 629,
      referenceRolePosition: 629,
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
      manifestPosition: 635,
      publicationPosition: 630,
      referenceRolePosition: 630,
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
      manifestPosition: 636,
      publicationPosition: 631,
      referenceRolePosition: 631,
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
      manifestPosition: 637,
      publicationPosition: 632,
      referenceRolePosition: 632,
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
