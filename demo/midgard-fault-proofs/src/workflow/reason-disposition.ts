import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

/**
 * Disposition of every typed `RejectionReason` constructor onto the fault-proof
 * catalogue, as fixed by the non-interactive proof-thread program
 * (`docs/fault-proofs/non-interactive-proof-thread-program-v1.md`, §3).
 *
 * A non-interactive reason has exactly one direct family per source kind and
 * must be provable in both directions: an operator that accepted a
 * transaction the reason rejects (wrongful acceptance) and an operator that
 * forced a rejection the reason does not justify (wrongful rejection).
 * `PlutusExecutionFailed` is the sole interactive reason; it routes to the
 * challenge-response validation dispute. Production classification may never
 * send a non-interactive reason to the interactive route because a family,
 * runner, or deployment identity is missing (§3.3).
 */
export type TypedReasonProving = "non_interactive" | "interactive";

export type TypedReasonDisposition = {
  /** Categories that may serve the arm; one per source kind at most. */
  readonly categories: readonly FraudProofCatalogueCategoryName[];
  readonly proving: TypedReasonProving;
};

export const INTERACTIVE_TYPED_REASON_ARMS = Object.freeze([
  "PlutusExecutionFailed",
] as const);

/** `InputNotFound.source_kind` for a spend input (field 0). */
export const INPUT_NOT_FOUND_SOURCE_KIND_SPEND = 0n;
/** `InputNotFound.source_kind` for a reference input (field 1). */
export const INPUT_NOT_FOUND_SOURCE_KIND_REFERENCE = 1n;

const direct = (
  ...categories: readonly FraudProofCatalogueCategoryName[]
): TypedReasonDisposition => ({ categories, proving: "non_interactive" });

export const TYPED_REASON_DISPOSITIONS = Object.freeze({
  // CanonicalDecode
  FieldPreimageLengthMismatch: direct("fieldPreimageLengthMismatch"),
  FieldItemWidthIllegal: direct("fieldItemWidthIllegal"),
  // InputSets
  EmptyInputs: direct("zeroInput"),
  DuplicateInput: direct("inputSetUniqueness"),
  ValidityIntervalMalformed: direct("invalidRange"),
  // StaticLedgerRules
  NetworkIdMismatch: direct("networkId"),
  FeeBelowMinimum: direct("minFee"),
  // Signatures
  AddressWitnessSignatureInvalid: direct("invalidSignature"),
  RequiredSignerUnsigned: direct("missingSignature"),
  // WitnessScripts
  WitnessScriptHeaderMalformed: direct("witnessScriptDecoding"),
  WitnessNativeScriptMalformed: direct("witnessScriptDecoding"),
  WitnessNativeScriptNodeLimit: direct("witnessScriptDecoding"),
  WitnessNativeScriptDepthLimit: direct("witnessScriptDecoding"),
  WitnessNativeScriptFalse: direct("nativeScriptInvalid"),
  // PhaseAScriptPreconditions
  ScriptIntegrityHashMissing: direct("scriptIntegrityHashMissing"),
  ObserversForbiddenOnUntaggedNetwork: direct(
    "observersForbiddenOnUntaggedNetwork",
  ),
  ObserverOrderInvalid: direct("observerOrderInvalid"),
  // ResolveInputs
  ValidityIntervalExcludesBlockSlot: direct("invalidRange"),
  InputNotFound: direct("nonExistentInput", "noReferenceInput"),
  InputSpentOutputNonCanonical: direct("resolvedOutputNonCanonical"),
  ResolvedReferenceScriptMalformed: direct("nativeScriptDecoding"),
  ResolvedReferenceScriptNodeLimit: direct("nativeScriptDecoding"),
  ResolvedReferenceScriptDepthLimit: direct("nativeScriptDecoding"),
  SpendInputSignerMissing: direct("spendInputSignerMissing"),
  // ScriptSources
  RedeemerMalformed: direct("redeemerCanonicity"),
  OutputNonCanonical: direct("transactionOutputNonCanonical"),
  OutputReferenceScriptMalformed: direct("outputReferenceScriptDecoding"),
  OutputReferenceScriptNodeLimit: direct("outputReferenceScriptDecoding"),
  OutputReferenceScriptDepthLimit: direct("outputReferenceScriptDecoding"),
  ProtectedOutputSignerMissing: direct("protectedOutputSignerMissing"),
  MintDeclaredAssetLimit: direct("mintDeclaredAssetLimit"),
  ScriptSourceMissing: direct("missingScriptSource"),
  RedeemerMissing: direct("missingRedeemer"),
  UnusedScriptWitness: direct("unusedScriptWitness"),
  UnusedRedeemer: direct("unusedRedeemer"),
  // Execution
  ExecutionNativeScriptMalformed: direct("executionSourceScriptDecoding"),
  ExecutionNativeScriptNodeLimit: direct("executionSourceScriptDecoding"),
  ExecutionNativeScriptDepthLimit: direct("executionSourceScriptDecoding"),
  ExecutionNativeScriptFalse: direct("executionNativeScriptInvalid"),
  ScriptIntegrityHashMismatch: direct("scriptIntegrityHashMismatch"),
  ReceivePurposePlutusV3Forbidden: direct("receivePurposeLanguage"),
  PlutusExecutionFailed: {
    categories: ["validationTraceDispute"],
    proving: "interactive",
  },
  // ValueAndMint
  InputAssetAccumulationLimit: direct("distinctAssetAccumulationLimit"),
  OutputAssetAccumulationLimit: direct("distinctAssetAccumulationLimit"),
  MintAssetAccumulationLimit: direct("distinctAssetAccumulationLimit"),
  OutputBelowMinAda: direct("minAda"),
  ValueNotPreserved: direct("valueNotPreserved"),
}) satisfies Readonly<Record<string, TypedReasonDisposition>>;

export type TypedReasonArm = keyof typeof TYPED_REASON_DISPOSITIONS;

export const TYPED_REASON_ARMS = Object.freeze(
  Object.keys(TYPED_REASON_DISPOSITIONS) as readonly TypedReasonArm[],
);

export const isTypedReasonArm = (arm: string): arm is TypedReasonArm =>
  Object.prototype.hasOwnProperty.call(TYPED_REASON_DISPOSITIONS, arm);

/**
 * The single direct category for a typed reason at a concrete coordinate.
 * `InputNotFound` is the only arm whose family depends on the coordinate: the
 * spend source belongs to `nonExistentInput`, the reference source to
 * `noReferenceInput`.
 */
export const directCategoryOfTypedReason = (
  arm: TypedReasonArm,
  coordinate: { readonly source_kind?: bigint } = {},
): FraudProofCatalogueCategoryName => {
  if (arm === "InputNotFound") {
    if (coordinate.source_kind === INPUT_NOT_FOUND_SOURCE_KIND_SPEND) {
      return "nonExistentInput";
    }
    if (coordinate.source_kind === INPUT_NOT_FOUND_SOURCE_KIND_REFERENCE) {
      return "noReferenceInput";
    }
    throw new Error(
      `directCategoryOfTypedReason: InputNotFound needs source_kind 0 or 1, got ${String(coordinate.source_kind)}`,
    );
  }
  const [category] = TYPED_REASON_DISPOSITIONS[arm].categories;
  return category;
};

/**
 * Every category that must be production-runnable for the non-interactive
 * dispositions to hold: no typed reason other than the interactive residue may
 * fall back to `validationTraceDispute`.
 */
export const NON_INTERACTIVE_DIRECT_CATEGORIES = Object.freeze(
  [
    ...new Set(
      TYPED_REASON_ARMS.filter(
        (arm) => TYPED_REASON_DISPOSITIONS[arm].proving === "non_interactive",
      ).flatMap((arm) => TYPED_REASON_DISPOSITIONS[arm].categories),
    ),
  ].sort(),
);

/**
 * Every `arm -> category` pair whose non-interactive direct category is not
 * installed in the given surface (a runner registry, a watcher installation
 * list, or a deployment manifest's category set), in disposition order.
 */
export const missingNonInteractiveInstallations = (
  installedCategories: Iterable<string>,
): readonly string[] => {
  const installed = new Set(installedCategories);
  return TYPED_REASON_ARMS.flatMap((arm) => {
    const disposition = TYPED_REASON_DISPOSITIONS[arm];
    if (disposition.proving !== "non_interactive") return [];
    return disposition.categories
      .filter((category) => !installed.has(category))
      .map((category) => `${arm} -> ${category}`);
  });
};

/**
 * Production-readiness check (§3.3): throws with one actionable list naming
 * every non-interactive typed reason whose direct category is not installed
 * in the given surface.
 */
export const assertNonInteractiveReasonsInstalled = ({
  surface,
  installedCategories,
}: {
  readonly surface: string;
  readonly installedCategories: Iterable<string>;
}): void => {
  const missing = missingNonInteractiveInstallations(installedCategories);
  if (missing.length > 0) {
    throw new Error(
      `${surface} would route non-interactive typed reasons to validationTraceDispute: ${missing.join(", ")}`,
    );
  }
};
