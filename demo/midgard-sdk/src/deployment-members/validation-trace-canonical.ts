import { spend } from "./recipes.types.js";

export const validationTraceCanonicalRecipes = {
  validationTraceDisputeCanonicalDecodePrepare: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.canonicalDecodePrepare,
  ),
  validationTraceDisputeCanonicalDecodeEmptySemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[0],
  ),
  validationTraceDisputeCanonicalDecodeItemSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[1],
  ),
  validationTraceDisputeCanonicalDecodeItemSource: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.canonicalDecodeItemStages
        .source,
  ),
  validationTraceDisputeCanonicalDecodeItemObserve: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.canonicalDecodeItemStages
        .observe,
  ),
  validationTraceDisputeCanonicalDecodeItemProof: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.canonicalDecodeItemStages
        .proof,
  ),
  validationTraceDisputeCanonicalDecodeItemSettlement: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.canonicalDecodeItemStages
        .settlement,
  ),
  validationTraceDisputeProofItem: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.proofItem,
  ),
} as const;
