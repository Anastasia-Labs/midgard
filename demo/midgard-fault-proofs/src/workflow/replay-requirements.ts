import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

/**
 * Families whose proof replays the challenged block against the ledger its
 * header commits as `prev_utxos_root`: a predecessor membership or
 * non-membership proof is part of the artifact (nonExistentInput,
 * noReferenceInput, minAda), or the replay resolves spent and referenced
 * outputs from that ledger (resolvedOutputNonCanonical, spendInputSignerMissing,
 * executionNativeScriptInvalid, transitionTrace). The classifier must hold the
 * authenticated predecessor before it issues a fault decision for a header
 * that commits a non-empty previous ledger; otherwise it decides `unprovable`
 * with `predecessor_context_unavailable`. The watcher's decision-time gate
 * re-checks that invariant on the decision it receives.
 *
 * This is not the registry's `requires.replayContext` set: that flag names
 * the families whose artifact re-derives from the admitted context object,
 * and some of those tolerate an absent predecessor.
 */
export const PREDECESSOR_LEDGER_PROOF_CATEGORIES = Object.freeze([
  "nonExistentInput",
  "noReferenceInput",
  "minAda",
  "resolvedOutputNonCanonical",
  "spendInputSignerMissing",
  "executionNativeScriptInvalid",
  "transitionTrace",
] as const satisfies readonly FraudProofCatalogueCategoryName[]);

/** Whether a launch scope names any category of the given replay requirement. */
export const launchScopeRequires = (
  launchScope: readonly FraudProofCatalogueCategoryName[],
  categories: readonly FraudProofCatalogueCategoryName[],
): boolean => launchScope.some((category) => categories.includes(category));
