import {
  buildResolvedInit,
  type ResolvedInitBuild,
  type SubmitResolvedInitParams,
} from "../submit-init.js";
import {
  DOUBLE_WITHDRAW_CATEGORY_LABEL,
  type DoubleWithdrawContracts,
} from "./contracts.js";
import {
  type DoubleWithdrawCatalogueCategory,
  doubleWithdrawSubmitError,
} from "./submit-common.js";

export type DoubleWithdrawInitBuild = ResolvedInitBuild;

export const buildDoubleWithdrawInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: DoubleWithdrawContracts;
    readonly category: DoubleWithdrawCatalogueCategory;
  },
): Promise<DoubleWithdrawInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw doubleWithdrawSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: DOUBLE_WITHDRAW_CATEGORY_LABEL,
  });
};
