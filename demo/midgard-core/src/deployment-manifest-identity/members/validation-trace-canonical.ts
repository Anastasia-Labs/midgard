import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCanonicalMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCanonicalDecodePrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode prepare",
      manifestPosition: 163,
      publicationPosition: 57,
      referenceRolePosition: 47,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_v1.main.spend",
      familyPaths: [
        ["canonicalDecodePrepare"],
        ["prepareResolvers", "0"],
        ["resolvers", "0"],
        ["steps", "163"],
      ],
    },
    validationTraceDisputeCanonicalDecodeEmptySemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode empty semantic",
      manifestPosition: 164,
      publicationPosition: 58,
      referenceRolePosition: 48,
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
      manifestPosition: 165,
      publicationPosition: 59,
      referenceRolePosition: 49,
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
      manifestPosition: 166,
      publicationPosition: 60,
      referenceRolePosition: 50,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_source_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "source"],
        ["steps", "159"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemObserve: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item observe",
      manifestPosition: 167,
      publicationPosition: 61,
      referenceRolePosition: 51,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "observe"],
        ["steps", "160"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemProof: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item proof",
      manifestPosition: 168,
      publicationPosition: 62,
      referenceRolePosition: 52,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_proof_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "proof"],
        ["steps", "161"],
      ],
    },
    validationTraceDisputeCanonicalDecodeItemSettlement: {
      purpose: "spend",
      referenceRole: "V1 validation-trace canonical-decode item settlement",
      manifestPosition: 169,
      publicationPosition: 63,
      referenceRolePosition: 53,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/canonical_decode_item_settlement_v1.main.spend",
      familyPaths: [
        ["canonicalDecodeItemStages", "settlement"],
        ["steps", "162"],
      ],
    },
    validationTraceDisputeProofItem: {
      purpose: "spend",
      referenceRole: "V1 validation-trace proof-item publication",
      manifestPosition: 170,
      publicationPosition: 64,
      referenceRolePosition: 54,
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/proof_item_v1.main.else",
      familyPaths: [["proofItem"], ["steps", "6"]],
    },
  },
);
