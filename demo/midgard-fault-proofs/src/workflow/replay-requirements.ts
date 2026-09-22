import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

/**
 * Families whose proof opens the challenged header's `prev_utxos_root`: a
 * predecessor membership or non-membership proof is part of the artifact
 * (nonExistentInput, noReferenceInput, minAda: non-membership;
 * missingNativeScriptUtxo: membership). The classifier must hold the
 * authenticated predecessor before it issues a fault decision for a header
 * that commits a non-empty previous ledger; otherwise it decides `unprovable`
 * with `predecessor_context_unavailable`. The watcher's decision-time gate
 * re-checks that invariant on the decision it receives.
 *
 * This is not the registry's `requires.replayContext` set: that flag names
 * the families whose artifact re-derives from the admitted context object,
 * and three of those tolerate an absent predecessor.
 */
export const PREDECESSOR_LEDGER_PROOF_CATEGORIES = Object.freeze([
  "nonExistentInput",
  "noReferenceInput",
  "missingNativeScriptUtxo",
  "minAda",
] as const satisfies readonly FraudProofCatalogueCategoryName[]);

/**
 * Families whose complete replay reads the historical native-script corpus
 * (`requireReplayHistoricalCorpus`). A classifier whose launch scope names one
 * of them requires an admitted historical replay authority at construction
 * and resolves the corpus for every challenged header. The registry's
 * `requires.historicalNativeScriptAuthority` set is a superset: its other
 * members need the authority for the artifact, not for detection.
 */
export const HISTORICAL_CORPUS_REPLAY_CATEGORIES = Object.freeze([
  "resolvedOutputNonCanonical",
  "spendInputSignerMissing",
  "executionNativeScriptInvalid",
  "missingNativeScriptUtxo",
  "transitionTrace",
] as const satisfies readonly FraudProofCatalogueCategoryName[]);

/** Whether a launch scope names any category of the given replay requirement. */
export const launchScopeRequires = (
  launchScope: readonly FraudProofCatalogueCategoryName[],
  categories: readonly FraudProofCatalogueCategoryName[],
): boolean => launchScope.some((category) => categories.includes(category));
