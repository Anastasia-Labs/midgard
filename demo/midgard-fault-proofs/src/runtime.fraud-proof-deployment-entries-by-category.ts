import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentContract,
  deploymentFamilyStepNames,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { type SupportedFaultProofCategoryName } from "./runtime.require-fault-proof-step-reference-script.js";

/**
 * Canonical manifest entries in the same order as each SDK chain's `steps`:
 * the core deployment members' step names. Every category names every step,
 * so production resolution cannot silently fall back to an inline validator
 * for a later transition.
 */
export const FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY = Object.freeze(
  Object.fromEntries(
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => [
      category,
      Object.freeze([...deploymentFamilyStepNames(category)]),
    ]),
  ),
) as Readonly<
  Record<
    SupportedFaultProofCategoryName,
    readonly [DeploymentContract, ...DeploymentContract[]]
  >
>;
