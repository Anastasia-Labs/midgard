import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

/**
 * Catalogue categories with no installed off-chain workflow. Each category's
 * workflow registration row records why it is missing. Their on-chain
 * validators are unchanged.
 *
 * `validationTraceDispute` needs no trace evidence from the DA payload: the
 * challenger re-executes the event from DA replay and commits its own trace
 * root when it opens the dispute. What is missing is the challenger runner.
 */
export const UNINSTALLED_FRAUD_PROOF_CATEGORIES = Object.freeze([
  "validationTraceDispute",
  "unusedRedeemer",
  "missingRedeemer",
  "missingScriptSource",
  "unusedScriptWitness",
  "scriptIntegrityHashMismatch",
  "receivePurposeLanguage",
  "executionNativeScriptInvalid",
  "executionSourceScriptDecoding",
  "distinctAssetAccumulationLimit",
] as const satisfies readonly FraudProofCatalogueCategoryName[]);

export type UninstalledFraudProofCategory =
  (typeof UNINSTALLED_FRAUD_PROOF_CATEGORIES)[number];

/** A catalogue category the fault-proof workflows install. */
export type InstalledFraudProofCategory = Exclude<
  FraudProofCatalogueCategoryName,
  UninstalledFraudProofCategory
>;

const uninstalled: ReadonlySet<FraudProofCatalogueCategoryName> = new Set(
  UNINSTALLED_FRAUD_PROOF_CATEGORIES,
);

export const isInstalledFraudProofCategory = (
  category: FraudProofCatalogueCategoryName,
): category is InstalledFraudProofCategory => !uninstalled.has(category);

/** The installed categories, in catalogue order. */
export const INSTALLED_FRAUD_PROOF_CATEGORY_ORDER: readonly InstalledFraudProofCategory[] =
  Object.freeze(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(isInstalledFraudProofCategory),
  );
