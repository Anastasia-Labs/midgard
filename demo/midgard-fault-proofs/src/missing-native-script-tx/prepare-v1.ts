import {
  buildMissingNativeScriptTxEvidenceV1,
  type MissingNativeScriptTxEvidenceV1,
} from "./evidence-v1.js";

/**
 * Preparation is intentionally an in-memory strict classifier in this
 * pre-registration wave. CLI/file serialization belongs to registration.
 */
export const prepareMissingNativeScriptTxV1 = (
  args: Parameters<typeof buildMissingNativeScriptTxEvidenceV1>[0],
): MissingNativeScriptTxEvidenceV1 =>
  buildMissingNativeScriptTxEvidenceV1(args);
