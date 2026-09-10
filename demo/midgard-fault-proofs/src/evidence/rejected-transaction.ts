import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";

/**
 * Retained preimages preserve the submitted TxIsValid claim. A rejected forced
 * leaf commits the operator's TxIsInvalid scalar, so its source comparison and
 * proof state must derive from that adjudicated envelope. Body identity and all
 * nine field preimages remain unchanged; callers still authenticate every
 * source component against the committed leaf and check its typed reason.
 */
export const deriveRejectedTransactionFaultEvidenceMaterial = (
  submittedTransactionCbor: Uint8Array,
): ReturnType<typeof deriveMidgardNativeTxFaultEvidenceMaterial> => {
  const submitted = deriveMidgardNativeTxFaultEvidenceMaterial(
    submittedTransactionCbor,
  );
  return deriveMidgardNativeTxFaultEvidenceMaterial(
    encodeMidgardNativeTxCanonical({
      ...submitted.canonical,
      validity: "TxIsInvalid",
    }),
  );
};
