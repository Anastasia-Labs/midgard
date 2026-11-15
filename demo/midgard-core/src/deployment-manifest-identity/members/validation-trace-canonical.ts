import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCanonicalMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCanonicalDecodePrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode prepare",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_v1.main.spend",
      familyPaths: [
        ["canonicalDecodePrepare"],
        ["steps", "155"],
        ["prepareResolvers", "0"],
        ["resolvers", "0"],
      ],
    },
    validationTraceDisputeCanonicalDecodeEmptySemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode empty semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_empty_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "0"],
        ["steps", "7"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "1"],
        ["steps", "8"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemSource: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item source",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_source_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "source"],
        ["steps", "151"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemObserve: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item observe",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "observe"],
        ["steps", "152"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemProof: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item proof",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_proof_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "proof"],
        ["steps", "153"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemSettlement: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item settlement",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_settlement_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "settlement"],
        ["steps", "154"],
      ],
    },
    validationTraceDisputeProofItem: {
      purpose: "spend",
      referenceRole: "V1 validation-trace proof-item publication",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/proof_item_v1.main.else",
      familyPaths: [["proofItem"], ["steps", "6"]],
    },
  },
);
