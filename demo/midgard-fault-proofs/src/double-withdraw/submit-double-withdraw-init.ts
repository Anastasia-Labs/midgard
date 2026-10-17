import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  DOUBLE_WITHDRAW_CATEGORY_LABEL,
  type DoubleWithdrawContracts,
} from "./contracts.js";
import {
  type DoubleWithdrawCatalogueCategory,
  doubleWithdrawSubmitError,
} from "./submit-common.js";

export type SubmitDoubleWithdrawInitResult = SubmitResolvedInitResult;

export const submitDoubleWithdrawInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: DoubleWithdrawContracts;
    readonly category: DoubleWithdrawCatalogueCategory;
  },
): Promise<SubmitDoubleWithdrawInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw doubleWithdrawSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await submitResolvedInit({
    ...params,
    label: DOUBLE_WITHDRAW_CATEGORY_LABEL,
  });
};
