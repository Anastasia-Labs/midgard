import type { LedgerOutputProofAttestation } from "../ledger-output-proof-plan.js";

/**
 * The shared ledger-output-proof stage yields, indexed exactly by
 * `ledger_output_proof_roles.stage_role`. A step transaction references the
 * stage yield of the plan's role index first, then the attestation yields of
 * `ledgerOutputProofAttestationRoles` in order.
 */
export const LEDGER_OUTPUT_PROOF_STAGE_YIELD_ROLES = [
  {
    contract: "ledgerOutputProofStructure",
    deployment: "validationTraceDisputeLedgerOutputProofStructureWithdraw",
    role: "V1 validation-trace ledger-output-proof structure yield",
  },
  {
    contract: "ledgerOutputProofValue",
    deployment: "validationTraceDisputeLedgerOutputProofValueWithdraw",
    role: "V1 validation-trace ledger-output-proof value yield",
  },
  {
    contract: "ledgerOutputProofDatumFoldMap",
    deployment: "validationTraceDisputeLedgerOutputProofDatumFoldMapWithdraw",
    role: "V1 validation-trace ledger-output-proof datum fold-map yield",
  },
  {
    contract: "ledgerOutputProofDatumFinalizeFrame",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumFinalizeFrameWithdraw",
    role: "V1 validation-trace ledger-output-proof datum finalize-frame yield",
  },
  {
    contract: "ledgerOutputProofDatumHeadScalar",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumHeadScalarWithdraw",
    role: "V1 validation-trace ledger-output-proof datum head-scalar yield",
  },
  {
    contract: "ledgerOutputProofDatumAttachInteger",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumAttachIntegerWithdraw",
    role: "V1 validation-trace ledger-output-proof datum attach-integer yield",
  },
  {
    contract: "ledgerOutputProofDatumFoldList",
    deployment: "validationTraceDisputeLedgerOutputProofDatumFoldListWithdraw",
    role: "V1 validation-trace ledger-output-proof datum fold-list yield",
  },
  {
    contract: "ledgerOutputProofDatumAdvanceInteger",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumAdvanceIntegerWithdraw",
    role: "V1 validation-trace ledger-output-proof datum advance-integer yield",
  },
  {
    contract: "ledgerOutputProofReferenceScript",
    deployment:
      "validationTraceDisputeLedgerOutputProofReferenceScriptWithdraw",
    role: "V1 validation-trace ledger-output-proof reference-script yield",
  },
  {
    contract: "ledgerOutputProofScriptHash",
    deployment: "validationTraceDisputeLedgerOutputProofScriptHashWithdraw",
    role: "V1 validation-trace ledger-output-proof script-hash yield",
  },
  {
    contract: "ledgerOutputProofNativeScript",
    deployment: "validationTraceDisputeLedgerOutputProofNativeScriptWithdraw",
    role: "V1 validation-trace ledger-output-proof native-script yield",
  },
  {
    contract: "ledgerOutputProofStructureAssets",
    deployment:
      "validationTraceDisputeLedgerOutputProofStructureAssetsWithdraw",
    role: "V1 validation-trace ledger-output-proof structure assets yield",
  },
  {
    contract: "ledgerOutputProofStructureOptional",
    deployment:
      "validationTraceDisputeLedgerOutputProofStructureOptionalWithdraw",
    role: "V1 validation-trace ledger-output-proof structure optional yield",
  },
  {
    contract: "ledgerOutputProofStructureFinish",
    deployment:
      "validationTraceDisputeLedgerOutputProofStructureFinishWithdraw",
    role: "V1 validation-trace ledger-output-proof structure finish yield",
  },
  {
    contract: "ledgerOutputProofDatumHeadSequence",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumHeadSequenceWithdraw",
    role: "V1 validation-trace ledger-output-proof datum head-sequence yield",
  },
  {
    contract: "ledgerOutputProofDatumHeadMap",
    deployment: "validationTraceDisputeLedgerOutputProofDatumHeadMapWithdraw",
    role: "V1 validation-trace ledger-output-proof datum head-map yield",
  },
  {
    contract: "ledgerOutputProofDatumHeadLargeConstructor",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumHeadLargeConstructorWithdraw",
    role: "V1 validation-trace ledger-output-proof datum head-large-constructor yield",
  },
  {
    contract: "ledgerOutputProofDatumAttachBytes",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumAttachBytesWithdraw",
    role: "V1 validation-trace ledger-output-proof datum attach-bytes yield",
  },
  {
    contract: "ledgerOutputProofDatumAdvanceBytes",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumAdvanceBytesWithdraw",
    role: "V1 validation-trace ledger-output-proof datum advance-bytes yield",
  },
  {
    contract: "ledgerOutputProofDatumFinish",
    deployment: "validationTraceDisputeLedgerOutputProofDatumFinishWithdraw",
    role: "V1 validation-trace ledger-output-proof datum finish yield",
  },
  {
    contract: "ledgerOutputProofDatumLargeConstructor",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumLargeConstructorWithdraw",
    role: "V1 validation-trace ledger-output-proof datum large-constructor yield",
  },
  {
    contract: "ledgerOutputProofDatumLargeFields",
    deployment:
      "validationTraceDisputeLedgerOutputProofDatumLargeFieldsWithdraw",
    role: "V1 validation-trace ledger-output-proof datum large-fields yield",
  },
  {
    contract: "ledgerOutputProofDatumClose",
    deployment: "validationTraceDisputeLedgerOutputProofDatumCloseWithdraw",
    role: "V1 validation-trace ledger-output-proof datum close yield",
  },
  {
    contract: "ledgerOutputProofSpan",
    deployment: "validationTraceDisputeLedgerOutputProofSpanWithdraw",
    role: "V1 validation-trace ledger-output-proof span yield",
  },
] as const;

/**
 * The two shared scalar attestation yields, keyed by the plan's
 * attestation-role names. The span yield is no longer an attestation: it is
 * the stage yield of the span-attach step (stage role 23) above.
 */
export const LEDGER_OUTPUT_PROOF_ATTESTATION_YIELD_ROLES = {
  scalarInteger: {
    contract: "ledgerOutputProofScalarInteger",
    deployment: "validationTraceDisputeLedgerOutputProofScalarIntegerWithdraw",
    role: "V1 validation-trace ledger-output-proof scalar-integer yield",
  },
  scalarBytes: {
    contract: "ledgerOutputProofScalarBytes",
    deployment: "validationTraceDisputeLedgerOutputProofScalarBytesWithdraw",
    role: "V1 validation-trace ledger-output-proof scalar-bytes yield",
  },
} as const satisfies Record<
  LedgerOutputProofAttestation,
  {
    readonly contract: string;
    readonly deployment: string;
    readonly role: string;
  }
>;

/**
 * The four descriptor yields of a finalize transaction, indexed exactly by
 * `ledger_output_proof_roles.descriptor_role`; the finalize dispatcher's
 * `yield_ref_input_indices` lists them in this order.
 */
export const LEDGER_OUTPUT_DESCRIPTOR_YIELD_ROLES = [
  {
    contract: "ledgerOutputDescriptorScanFacts",
    deployment: "validationTraceDisputeLedgerOutputDescriptorScanFactsWithdraw",
    role: "V1 validation-trace ledger-output-descriptor scan-facts yield",
  },
  {
    contract: "ledgerOutputDescriptorReferenceScript",
    deployment:
      "validationTraceDisputeLedgerOutputDescriptorReferenceScriptWithdraw",
    role: "V1 validation-trace ledger-output-descriptor reference-script yield",
  },
  {
    contract: "ledgerOutputDescriptorDatumSummary",
    deployment:
      "validationTraceDisputeLedgerOutputDescriptorDatumSummaryWithdraw",
    role: "V1 validation-trace ledger-output-descriptor datum-summary yield",
  },
  {
    contract: "ledgerOutputDescriptorValueSummary",
    deployment:
      "validationTraceDisputeLedgerOutputDescriptorValueSummaryWithdraw",
    role: "V1 validation-trace ledger-output-descriptor value-summary yield",
  },
] as const;
