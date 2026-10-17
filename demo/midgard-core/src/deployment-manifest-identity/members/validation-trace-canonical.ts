import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCanonicalMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCanonicalDecodePrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode prepare",
      manifestPosition: 169,
      publicationPosition: 63,
      referenceRolePosition: 53,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_v1.main.spend",
      familyPaths: [
        ["steps", "155"],
        ["canonicalDecodePrepare"],
        ["prepareResolvers", "0"],
        ["resolvers", "0"],
      ],
    },
    validationTraceDisputeCanonicalDecodeEmptySemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode empty semantic",
      manifestPosition: 170,
      publicationPosition: 64,
      referenceRolePosition: 54,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_empty_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "7"],
        ["semanticResolvers", "0"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item semantic",
      manifestPosition: 171,
      publicationPosition: 65,
      referenceRolePosition: 55,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "8"],
        ["semanticResolvers", "1"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemSource: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item source",
      manifestPosition: 172,
      publicationPosition: 66,
      referenceRolePosition: 56,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_source_v1.main.spend",
      familyPaths: [
        ["steps", "151"],
        ["canonicalDecodeItemStages", "source"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemObserve: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item observe",
      manifestPosition: 173,
      publicationPosition: 67,
      referenceRolePosition: 57,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main.spend",
      familyPaths: [
        ["steps", "152"],
        ["canonicalDecodeItemStages", "observe"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemProof: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item proof",
      manifestPosition: 174,
      publicationPosition: 68,
      referenceRolePosition: 58,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_proof_v1.main.spend",
      familyPaths: [
        ["steps", "153"],
        ["canonicalDecodeItemStages", "proof"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemSettlement: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item settlement",
      manifestPosition: 175,
      publicationPosition: 69,
      referenceRolePosition: 59,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_settlement_v1.main.spend",
      familyPaths: [
        ["steps", "154"],
        ["canonicalDecodeItemStages", "settlement"],
      ],
    },
    validationTraceDisputeProofItem: {
      purpose: "spend",
      referenceRole: "V1 validation-trace proof-item publication",
      manifestPosition: 176,
      publicationPosition: 70,
      referenceRolePosition: 60,
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/proof_item_v1.main.else",
      familyPaths: [["steps", "6"], ["proofItem"]],
    },
  },
);
