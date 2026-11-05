import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentContract,
  deploymentFamilyStepNames,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { type SupportedFaultProofCategoryName } from "./runtime.require-fault-proof-step-reference-script.js";

/**
 * Categories whose deployment resolution still checks only their first step.
 * Every other category checks every step (owner ruling 2026-11-05).
 *
 * validationTraceDispute stays here pending a ruling: core's 147 step names
 * are an ordered subsequence of the SDK chain's 168 `steps`, not a prefix of
 * it. They diverge at index 67, where the chain carries the nativeScripts and
 * scriptIntegrity semantic resolvers that have no core deployment member, so
 * an index-for-index comparison would check names from index 67 on against
 * the wrong validators and refuse a deployment whose named steps all match.
 */
export const FIRST_STEP_ONLY_DEPLOYMENT_CATEGORIES: ReadonlySet<SupportedFaultProofCategoryName> =
  new Set(["validationTraceDispute"]);

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
