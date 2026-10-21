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
    "cek",
  ].some((prefix) => key.startsWith(prefix)) ||
  key === "canonicalDecodeEmpty" ||
  key === "canonicalDecodeItem" ||
  key === "compactBinding" ||
  key === "staticLedgerRules";
export const VALIDATION_TRACE_PREPARE_KEYS = Object.keys(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
);
export const isRecordedValidationTracePrepare = (key: string): boolean =>
  key === "canonicalDecode" ||
  key === "cek" ||
  key === "compactBinding" ||
  key === "staticLedgerRules";

/** The validation-trace chain slots under `vtd` the manifest records no
 * bytes for: unrecorded semantic and prepare resolvers, and the steps. */
export const unrecordedValidationTraceSlots = (vtd: string): string[] => [
  ...VALIDATION_TRACE_SEMANTIC_KEYS.flatMap((key, index) =>
    isRecordedValidationTraceSemantic(key)
      ? []
      : [`${vtd}.semanticResolvers.${index.toString()}`],
  ),
  `${vtd}.steps`,
  ...["prepareResolvers", "resolvers"].flatMap((list) =>
    VALIDATION_TRACE_PREPARE_KEYS.flatMap((key, index) =>
      isRecordedValidationTracePrepare(key)
        ? []
        : [`${vtd}.${list}.${index.toString()}`],
    ),
  ),
];
