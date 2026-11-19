import {
  buildResolvedInit,
  type ResolvedInitBuild,
  type SubmitResolvedInitParams,
} from "../submit-init.js";
import {
  L2_TX_MISTAG_CATEGORY_LABEL,
  type L2TxMistagContracts,
} from "./contracts.js";
import {
  type L2TxMistagCatalogueCategory,
  l2TxMistagSubmitError,
} from "./submit-common.js";

export type L2TxMistagInitBuild = ResolvedInitBuild;

export const buildL2TxMistagInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: L2TxMistagContracts;
    readonly category: L2TxMistagCatalogueCategory;
  },
): Promise<L2TxMistagInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw l2TxMistagSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  if (!/^[0-9a-f]{8}$/u.test(category.categoryId)) {
    throw l2TxMistagSubmitError(
      "category id must be four bytes of lowercase hex.",
    );
  }
  return await buildResolvedInit({
    ...params,
    label: L2_TX_MISTAG_CATEGORY_LABEL,
  });
};
