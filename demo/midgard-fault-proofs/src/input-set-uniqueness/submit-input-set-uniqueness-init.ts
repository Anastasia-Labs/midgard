/**
 * `input-set-uniqueness` thread init.
 *
 * Delegates to the shared `buildResolvedInit` in `src/submit-init.ts` with
 * the family's explicit contracts record and catalogue category: the
 * `inputSetUniqueness` entry in the production `buildInit` category union is
 * parent-owned and lands at registration.
 */
import type {
  ResolvedInitBuild,
  SubmitResolvedInitParams,
} from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { buildResolvedInit } from "../submit-init.submit-resolved-init.js";
import {
  INPUT_SET_UNIQUENESS_CATEGORY_LABEL,
  type InputSetUniquenessContracts,
} from "./contracts.js";
import {
  type InputSetUniquenessCatalogueCategory,
  inputSetUniquenessSubmitError,
} from "./submit-common.js";

export type InputSetUniquenessInitBuild = ResolvedInitBuild;

export const buildInputSetUniquenessInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: InputSetUniquenessContracts;
    readonly category: InputSetUniquenessCatalogueCategory;
  },
): Promise<InputSetUniquenessInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw inputSetUniquenessSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: INPUT_SET_UNIQUENESS_CATEGORY_LABEL,
  });
};
