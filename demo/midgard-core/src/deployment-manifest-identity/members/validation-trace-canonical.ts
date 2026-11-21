import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCanonicalMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCanonicalDecodePrepare: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace canonical-decode prepare",
        token: "V1VtdCanonicalPrepare",
      },
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
      reference: {
        role: "V1 validation-trace canonical-decode empty semantic",
        token: "V1VtdCanonicalEmpty",
      },
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
      reference: {
        role: "V1 validation-trace canonical-decode item semantic",
        token: "V1VtdCanonicalItem",
      },
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
      reference: {
        role: "V1 validation-trace canonical-decode item source",
        token: "V1VtdCanonicalSource",
      },
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
      reference: {
        role: "V1 validation-trace canonical-decode item observe",
        token: "V1VtdCanonicalObserve",
      },
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
      reference: {
        role: "V1 validation-trace canonical-decode item proof",
        token: "V1VtdCanonicalProof",
      },
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
      reference: {
        role: "V1 validation-trace canonical-decode item settlement",
        token: "V1VtdCanonicalSettle",
      },
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
      reference: {
        role: "V1 validation-trace proof-item publication",
        token: "V1VtdProofItem",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/proof_item_v1.main.else",
      familyPaths: [["proofItem"], ["steps", "6"]],
    },
  },
);
