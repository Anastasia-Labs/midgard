/** Pre-registration init for the standalone min-fee computation thread. */
import {
  MIN_FEE_CATEGORY_LABEL,
  type MinFeeContracts,
} from "./min-fee-contracts.js";
import {
  type MinFeeCatalogueCategory,
  minFeeSubmitError,
} from "./min-fee-submit-common.js";
import {
  buildResolvedInit,
  type ResolvedInitBuild,
  type SubmitResolvedInitParams,
} from "./submit-init.js";

export type MinFeeInitBuild = ResolvedInitBuild;

export const buildMinFeeInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: MinFeeContracts;
    readonly category: MinFeeCatalogueCategory;
  },
): Promise<MinFeeInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw minFeeSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: MIN_FEE_CATEGORY_LABEL,
  });
};
