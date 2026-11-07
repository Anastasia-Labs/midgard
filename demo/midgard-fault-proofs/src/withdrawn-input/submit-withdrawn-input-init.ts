import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContracts,
} from "./contracts.js";
import {
  type WithdrawnInputCatalogueCategory,
  withdrawnInputSubmitError,
} from "./submit-common.js";

export type SubmitWithdrawnInputInitResult = SubmitResolvedInitResult;

export const submitWithdrawnInputInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: WithdrawnInputContracts;
    readonly category: WithdrawnInputCatalogueCategory;
  },
): Promise<SubmitWithdrawnInputInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw withdrawnInputSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await submitResolvedInit({
    ...params,
    label: WITHDRAWN_INPUT_CATEGORY_LABEL,
  });
};
