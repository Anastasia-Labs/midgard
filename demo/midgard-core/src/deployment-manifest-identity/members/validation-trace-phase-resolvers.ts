import { defineDeploymentFamily } from "../members.types.js";

/** The validation phases' one-step resolvers and their semantic resolvers,
 * which a thread visits between the boundary and the award. */
export const validationTracePhaseResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCompactBindingPrepare: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace compact-binding prepare",
        token: "V1VtCompactBindingPrepare",
      },
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
      reference: {
        role: "V1 validation-trace compact-binding semantic",
        token: "V1VtCompactBindingSem",
      },
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
      reference: {
        role: "V1 validation-trace static-ledger-rules prepare",
        token: "V1VtStaticLedgerPrepare",
      },
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
      reference: {
        role: "V1 validation-trace static-ledger-rules semantic",
        token: "V1VtStaticLedgerSem",
      },
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
      reference: {
        role: "V1 validation-trace input-sets prepare",
        token: "V1VtInputSetsPrepare",
      },
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
      reference: {
        role: "V1 validation-trace input-sets empty semantic",
        token: "V1VtInputSetsEmptySem",
      },
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
      reference: {
        role: "V1 validation-trace input-sets item semantic",
        token: "V1VtInputSetsItemSem",
      },
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
      reference: {
        role: "V1 validation-trace signatures prepare",
        token: "V1VtSignaturesPrepare",
      },
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
      reference: {
        role: "V1 validation-trace signatures advance semantic",
        token: "V1VtSigAdvanceSem",
      },
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
      reference: {
        role: "V1 validation-trace signatures address item semantic",
        token: "V1VtSigAddressItemSem",
      },
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
      reference: {
        role: "V1 validation-trace signatures required item semantic",
        token: "V1VtSigRequiredItemSem",
      },
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
      reference: {
        role: "V1 validation-trace signatures handoff semantic",
        token: "V1VtSigHandoffSem",
      },
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
      reference: {
        role: "V1 validation-trace phase-A native scripts prepare",
        token: "V1VtPhaseANativePrepare",
      },
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
      reference: {
        role: "V1 validation-trace phase-A script preconditions prepare",
        token: "V1VtPhaseAPrecondPrepare",
      },
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
      reference: {
        role: "V1 validation-trace resolve-inputs prepare",
        token: "V1VtResolveInputsPrepare",
      },
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
      reference: {
        role: "V1 validation-trace script-sources prepare",
        token: "V1VtScriptSourcesPrepare",
      },
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
      reference: {
        role: "V1 validation-trace native-scripts prepare",
        token: "V1VtNativeScriptsPrepare",
      },
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
      reference: {
        role: "V1 validation-trace script-integrity prepare",
        token: "V1VtScriptIntegrityPrepare",
      },
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
      reference: {
        role: "V1 validation-trace value-and-mint prepare",
        token: "V1VtValueAndMintPrepare",
      },
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
