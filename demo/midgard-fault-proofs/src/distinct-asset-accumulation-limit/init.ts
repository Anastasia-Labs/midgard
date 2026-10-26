/** Computation-thread init for the `distinctAssetAccumulationLimit` family. */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL,
  type DistinctAssetAccumulationLimitContracts,
} from "./contracts.js";

export const submitDistinctAssetAccumulationLimitInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts"> & {
    readonly contracts: DistinctAssetAccumulationLimitContracts;
  },
): Promise<SubmitResolvedInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash)
    throw new Error(
      `${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL}: catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  return await submitResolvedInit({
    ...params,
    label: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL,
  });
};
