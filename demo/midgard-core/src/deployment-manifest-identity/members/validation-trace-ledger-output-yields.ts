import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceLedgerOutputYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeLedgerOutputProofStructureWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof structure yield",
        token: "V1VtLopStructureYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_structure_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofStructure"]],
    },
    validationTraceDisputeLedgerOutputProofValueWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof value yield",
        token: "V1VtLopValueYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_value_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofValue"]],
    },
    validationTraceDisputeLedgerOutputProofDatumFoldMapWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum fold-map yield",
        token: "V1VtLopDatumFoldMapYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_map_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumFoldMap"]],
    },
    validationTraceDisputeLedgerOutputProofDatumFinalizeFrameWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum finalize-frame yield",
        token: "V1VtLopDatumFinalizeFrameYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_finalize_frame_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumFinalizeFrame"]],
    },
    validationTraceDisputeLedgerOutputProofDatumHeadScalarWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum head-scalar yield",
        token: "V1VtLopDatumHeadScalarYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_head_scalar_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumHeadScalar"]],
    },
    validationTraceDisputeLedgerOutputProofDatumAttachIntegerWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum attach-integer yield",
        token: "V1VtLopDatumAttachIntegerYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_integer_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumAttachInteger"]],
    },
    validationTraceDisputeLedgerOutputProofDatumFoldListWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum fold-list yield",
        token: "V1VtLopDatumFoldListYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_list_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumFoldList"]],
    },
    validationTraceDisputeLedgerOutputProofDatumAdvanceIntegerWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum advance-integer yield",
        token: "V1VtLopDatumAdvanceIntegerYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_integer_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumAdvanceInteger"]],
    },
    validationTraceDisputeLedgerOutputProofReferenceScriptWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof reference-script yield",
        token: "V1VtLopRefScriptYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_reference_script_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofReferenceScript"]],
    },
    validationTraceDisputeLedgerOutputProofScriptHashWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof script-hash yield",
        token: "V1VtLopScriptHashYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_script_hash_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofScriptHash"]],
    },
    validationTraceDisputeLedgerOutputProofNativeScriptWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof native-script yield",
        token: "V1VtLopNativeScriptYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_native_script_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofNativeScript"]],
    },
    validationTraceDisputeLedgerOutputProofStructureAssetsWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof structure assets yield",
        token: "V1VtLopStructureAssetsYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_structure_assets_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofStructureAssets"]],
    },
    validationTraceDisputeLedgerOutputProofStructureOptionalWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof structure optional yield",
        token: "V1VtLopStructureOptionalYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_structure_optional_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofStructureOptional"]],
    },
    validationTraceDisputeLedgerOutputProofStructureFinishWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof structure finish yield",
        token: "V1VtLopStructureFinishYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_structure_finish_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofStructureFinish"]],
    },
    validationTraceDisputeLedgerOutputProofDatumHeadSequenceWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum head-sequence yield",
        token: "V1VtLopDatumHeadSequenceYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_head_sequence_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumHeadSequence"]],
    },
    validationTraceDisputeLedgerOutputProofDatumHeadMapWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum head-map yield",
        token: "V1VtLopDatumHeadMapYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_head_map_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumHeadMap"]],
    },
    validationTraceDisputeLedgerOutputProofDatumHeadLargeConstructorWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum head-large-constructor yield",
        token: "V1VtLopDatumHeadLargeCtorYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_head_large_constructor_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumHeadLargeConstructor"]],
    },
    validationTraceDisputeLedgerOutputProofDatumAttachBytesWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum attach-bytes yield",
        token: "V1VtLopDatumAttachBytesYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_bytes_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumAttachBytes"]],
    },
    validationTraceDisputeLedgerOutputProofDatumAdvanceBytesWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum advance-bytes yield",
        token: "V1VtLopDatumAdvanceBytesYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_bytes_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumAdvanceBytes"]],
    },
    validationTraceDisputeLedgerOutputProofDatumFinishWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum finish yield",
        token: "V1VtLopDatumFinishYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_finish_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumFinish"]],
    },
    validationTraceDisputeLedgerOutputProofDatumLargeConstructorWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum large-constructor yield",
        token: "V1VtLopDatumLargeCtorYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_large_constructor_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumLargeConstructor"]],
    },
    validationTraceDisputeLedgerOutputProofDatumLargeFieldsWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum large-fields yield",
        token: "V1VtLopDatumLargeFieldsYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_large_fields_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumLargeFields"]],
    },
    validationTraceDisputeLedgerOutputProofDatumCloseWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof datum close yield",
        token: "V1VtLopDatumCloseYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_datum_close_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofDatumClose"]],
    },
    validationTraceDisputeLedgerOutputProofSpanWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof span yield",
        token: "V1VtLopSpanYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_span_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofSpan"]],
    },
    validationTraceDisputeLedgerOutputProofScalarIntegerWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof scalar-integer yield",
        token: "V1VtLopScalarIntegerYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_scalar_integer_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofScalarInteger"]],
    },
    validationTraceDisputeLedgerOutputProofScalarBytesWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-proof scalar-bytes yield",
        token: "V1VtLopScalarBytesYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_proof_scalar_bytes_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputProofScalarBytes"]],
    },
    validationTraceDisputeLedgerOutputDescriptorScanFactsWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-descriptor scan-facts yield",
        token: "V1VtLopDescScanFactsYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_descriptor_scan_facts_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputDescriptorScanFacts"]],
    },
    validationTraceDisputeLedgerOutputDescriptorReferenceScriptWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-descriptor reference-script yield",
        token: "V1VtLopDescRefScriptYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_descriptor_reference_script_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputDescriptorReferenceScript"]],
    },
    validationTraceDisputeLedgerOutputDescriptorDatumSummaryWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-descriptor datum-summary yield",
        token: "V1VtLopDescDatumSummaryYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_descriptor_datum_summary_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputDescriptorDatumSummary"]],
    },
    validationTraceDisputeLedgerOutputDescriptorValueSummaryWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace ledger-output-descriptor value-summary yield",
        token: "V1VtLopDescValueSummaryYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/ledger_output_descriptor_value_summary_yield.main.withdraw",
      familyPaths: [["yields", "ledgerOutputDescriptorValueSummary"]],
    },
  },
);
