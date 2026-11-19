/** Pre-registration canonical-decodability thread initialization. */
import type {
  ResolvedInitBuild,
  SubmitResolvedInitParams,
} from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { buildResolvedInit } from "../submit-init.submit-resolved-init.js";
import {
  CANONICAL_DECODABILITY_CATEGORY_LABEL,
  type CanonicalDecodabilityContracts,
} from "./contracts.js";
import {
  type CanonicalDecodabilityCatalogueCategory,
  canonicalDecodabilitySubmitError,
} from "./submit-common.js";

export type CanonicalDecodabilityInitBuild = ResolvedInitBuild;

export const buildCanonicalDecodabilityInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: CanonicalDecodabilityContracts;
    readonly category: CanonicalDecodabilityCatalogueCategory;
  },
): Promise<CanonicalDecodabilityInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw canonicalDecodabilitySubmitError(
      `catalogue category registers ${category.scriptHash}, but deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  if (!/^[0-9a-f]{8}$/u.test(category.categoryId)) {
    throw canonicalDecodabilitySubmitError(
      "catalogue category id must be exactly four lowercase hexadecimal bytes.",
    );
  }
  return await buildResolvedInit({
    ...params,
    label: CANONICAL_DECODABILITY_CATEGORY_LABEL,
    // Journal roles predate the shared init; kept so recorded preflights
    // are unchanged.
    referenceScriptRoles: {
      computationThreadMint:
        "canonical-decodability-init-computation-thread-mint",
      phasMembershipWithdraw: "canonical-decodability-init-phas-membership",
    },
  });
};
