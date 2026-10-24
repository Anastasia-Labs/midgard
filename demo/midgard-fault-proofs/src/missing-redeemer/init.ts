/** Computation-thread init for the `missingRedeemer` family. */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  MISSING_REDEEMER_CATEGORY_LABEL,
  type MissingRedeemerContracts,
} from "./contracts.js";

export const submitMissingRedeemerInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts"> & {
    readonly contracts: MissingRedeemerContracts;
  },
): Promise<SubmitResolvedInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash)
    throw new Error(
      `${MISSING_REDEEMER_CATEGORY_LABEL}: catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  return await submitResolvedInit({
    ...params,
    label: MISSING_REDEEMER_CATEGORY_LABEL,
  });
};
