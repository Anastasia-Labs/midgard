import { defineDeploymentFamily } from "../members.types.js";

/** The validation phases' one-step resolvers and their semantic resolvers,
 * which a thread visits between the boundary and the award. */
export const validationTracePhaseResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCompactBindingPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace compact-binding prepare",
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
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "8"],
        ["resolvers", "8"],
        ["steps", "163"],
      ],
    },
    validationTraceDisputeNativeScriptsPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace native-scripts prepare",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/native_scripts_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "9"],
        ["resolvers", "9"],
        ["steps", "164"],
      ],
    },
    validationTraceDisputeScriptIntegrityPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace script-integrity prepare",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_integrity_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "10"],
        ["resolvers", "10"],
        ["steps", "165"],
      ],
    },
    validationTraceDisputeValueAndMintPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint prepare",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "12"],
        ["resolvers", "12"],
        ["steps", "167"],
      ],
    },
  },
);
