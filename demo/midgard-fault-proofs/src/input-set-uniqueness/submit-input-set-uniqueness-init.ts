/**
 * `input-set-uniqueness` thread init.
 *
 * Delegates to the shared `submitResolvedInit` in `src/submit-init.ts` with
 * the family's explicit contracts record and catalogue category: the
 * `inputSetUniqueness` entry in the production `submitInit` category union is
 * parent-owned and lands at registration.
 */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  INPUT_SET_UNIQUENESS_CATEGORY_LABEL,
  type InputSetUniquenessContracts,
} from "./contracts.js";
import {
  type InputSetUniquenessCatalogueCategory,
  inputSetUniquenessSubmitError,
} from "./submit-common.js";

export type SubmitInputSetUniquenessInitResult = SubmitResolvedInitResult;

export const submitInputSetUniquenessInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: InputSetUniquenessContracts;
    readonly category: InputSetUniquenessCatalogueCategory;
  },
): Promise<SubmitInputSetUniquenessInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw inputSetUniquenessSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await submitResolvedInit({
    ...params,
    label: INPUT_SET_UNIQUENESS_CATEGORY_LABEL,
  });
};
