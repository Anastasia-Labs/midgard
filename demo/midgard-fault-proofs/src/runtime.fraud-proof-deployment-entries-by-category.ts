import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentContract,
  deploymentFamilyStepNames,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { type SupportedFaultProofCategoryName } from "./runtime.require-fault-proof-step-reference-script.js";

/**
 * The older families whose deployment resolution checks only their first
 * step. Checking their later steps is a separate owner decision (2026-11-04).
 */
export const FIRST_STEP_ONLY_DEPLOYMENT_CATEGORIES: ReadonlySet<SupportedFaultProofCategoryName> =
  new Set([
    "doubleSpend",
    "nonExistentInput",
    "nonExistentInputNoIndex",
    "noReferenceInput",
    "referenceInputNoIdx",
    "invalidRange",
    "zeroInput",
    "daHashPreimage",
    "invalidSignature",
    "validationTraceDispute",
  ]);

/**
 * Canonical manifest entries in the same order as each SDK chain's `steps`:
 * the core deployment members' step names, or only the first of them for a
 * first-step-only category. Every other category names every step, so
 * production resolution cannot silently fall back to an inline validator for
 * a later transition.
 */
export const FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY = Object.freeze(
  Object.fromEntries(
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => {
      const steps = deploymentFamilyStepNames(category);
      return [
        category,
        Object.freeze(
          FIRST_STEP_ONLY_DEPLOYMENT_CATEGORIES.has(category)
            ? steps.slice(0, 1)
            : [...steps],
        ),
      ];
    }),
  ),
) as Readonly<
  Record<
    SupportedFaultProofCategoryName,
    readonly [DeploymentContract, ...DeploymentContract[]]
  >
>;
