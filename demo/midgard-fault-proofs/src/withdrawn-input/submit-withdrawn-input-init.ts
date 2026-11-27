import type {
  ResolvedInitBuild,
  SubmitResolvedInitParams,
} from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { buildResolvedInit } from "../submit-init.submit-resolved-init.js";
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContracts,
} from "./contracts.js";
import {
  type WithdrawnInputCatalogueCategory,
  withdrawnInputSubmitError,
} from "./submit-common.js";

export type WithdrawnInputInitBuild = ResolvedInitBuild;

export const buildWithdrawnInputInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: WithdrawnInputContracts;
    readonly category: WithdrawnInputCatalogueCategory;
  },
): Promise<WithdrawnInputInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw withdrawnInputSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: WITHDRAWN_INPUT_CATEGORY_LABEL,
  });
};
