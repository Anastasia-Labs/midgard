import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxProofCommitment,
  deriveMidgardForcedTxFaultEvidenceMaterial,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec";
import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core/consensus-validation";

/** Exact identity and fields from the authenticated canonical event source. */
export const retainedValidationTransactionSource = (
  canonicalTransactionCbor: Uint8Array,
  sourceKind: "normal" | "forced",
) => {
  const material = (
    sourceKind === "forced"
      ? deriveMidgardForcedTxFaultEvidenceMaterial
      : deriveMidgardNativeTxFaultEvidenceMaterial
  )(canonicalTransactionCbor);
  return {
    transactionId: material.transactionId,
    transactionCommitment: (sourceKind === "forced"
      ? computeMidgardForcedTxProofCommitment
      : computeMidgardNativeTxProofCommitment)(material.proofSource),
    fields:
      sourceKind === "forced"
        ? material.fieldPreimages
        : deriveMidgardTxFieldPreimages(canonicalTransactionCbor).map(
            (field) => field.preimageCbor,
          ),
  };
};

export const validateRetainedValidationTransactionIdentity = (
  source: ReturnType<typeof retainedValidationTransactionSource>,
  transactionId: Uint8Array,
  transactionCommitment: Uint8Array,
): void => {
  if (
    !source.transactionId.equals(Buffer.from(transactionId)) ||
    !source.transactionCommitment.equals(Buffer.from(transactionCommitment))
  )
    throw new Error(
      "Retained field source transaction identity differs from its authenticated transaction",
    );
};
