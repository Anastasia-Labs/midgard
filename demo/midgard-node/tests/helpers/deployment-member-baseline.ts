import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "@al-ft/midgard-sdk";

/** Pre-migration admission rule, kept independent of the new member projections. */
export const VALIDATION_TRACE_SEMANTIC_KEYS = Object.keys(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
);
export const isRecordedValidationTraceSemantic = (key: string): boolean =>
  [
    "resolveInputs",
    "scriptSources",
    "phaseANativeScripts",
    "phaseAScriptPreconditions",
  ].some((prefix) => key.startsWith(prefix)) ||
  key === "canonicalDecodeEmpty" ||
  key === "canonicalDecodeItem";
