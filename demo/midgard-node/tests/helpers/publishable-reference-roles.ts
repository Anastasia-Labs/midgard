import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE as CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";

import { NOT_YET_PUBLISHABLE_CONTRACTS } from "../../src/deployable-scripts.js";

/** The contract a manifest reference-script role names. */
export const contractOfRole = (role: string) =>
  CONTRACT_BY_ROLE[role as keyof typeof CONTRACT_BY_ROLE];

/**
 * `roles` without those whose scripts initialization cannot publish yet, read
 * from the node's own exception set, so the expectation tightens as that set
 * empties.
 */
export const publishableRoles = (roles: readonly string[]): string[] =>
  roles.filter(
    (role) => !NOT_YET_PUBLISHABLE_CONTRACTS.has(contractOfRole(role)),
  );
