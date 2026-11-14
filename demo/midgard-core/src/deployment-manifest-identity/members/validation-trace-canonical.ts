import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCanonicalMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCanonicalDecodePrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode prepare",
      manifestPosition: 170,
      publicationPosition: 64,
      referenceRolePosition: 54,
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
      manifestPosition: 171,
      publicationPosition: 65,
      referenceRolePosition: 55,
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
      manifestPosition: 172,
      publicationPosition: 66,
      referenceRolePosition: 56,
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
      manifestPosition: 173,
      publicationPosition: 67,
      referenceRolePosition: 57,
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
      manifestPosition: 174,
      publicationPosition: 68,
      referenceRolePosition: 58,
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
      manifestPosition: 175,
      publicationPosition: 69,
      referenceRolePosition: 59,
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
      manifestPosition: 176,
      publicationPosition: 70,
      referenceRolePosition: 60,
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
      manifestPosition: 177,
      publicationPosition: 71,
      referenceRolePosition: 61,
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/proof_item_v1.main.else",
      familyPaths: [["proofItem"], ["steps", "6"]],
    },
  },
);
