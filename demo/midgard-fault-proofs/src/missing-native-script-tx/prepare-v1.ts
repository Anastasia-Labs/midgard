import {
  buildMissingNativeScriptTxEvidence,
  type MissingNativeScriptTxEvidence,
} from "./evidence-v1.js";

/**
 * Preparation is intentionally an in-memory strict classifier in this
 * pre-registration wave. CLI/file serialization belongs to registration.
 */
export const prepareMissingNativeScriptTx = (
  args: Parameters<typeof buildMissingNativeScriptTxEvidence>[0],
): MissingNativeScriptTxEvidence => buildMissingNativeScriptTxEvidence(args);
