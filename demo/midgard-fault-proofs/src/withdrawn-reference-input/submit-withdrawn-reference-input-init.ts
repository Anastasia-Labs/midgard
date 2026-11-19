/**
 * `withdrawn-reference-input` thread init (offchain plan §4.2).
 *
 * Delegates to the shared `buildResolvedInit` in `src/submit-init.ts` with
 * the family's explicit contracts record and catalogue category: the
 * `withdrawnReferenceInput` entry in the production `buildInit` category
 * union is deliberately deferred to the registration wave (plan §2.2).
 */ import type {
  ResolvedInitBuild,
  SubmitResolvedInitParams,
} from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { buildResolvedInit } from "../submit-init.submit-resolved-init.js";
import {
  WITHDRAWN_REFERENCE_INPUT_CATEGORY_LABEL,
  type WithdrawnReferenceInputContracts,
} from "./contracts.js";
import {
  type WithdrawnReferenceInputCatalogueCategory,
  withdrawnReferenceInputSubmitError,
} from "./submit-common.js";

export type WithdrawnReferenceInputInitBuild = ResolvedInitBuild;

export const buildWithdrawnReferenceInputInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: WithdrawnReferenceInputContracts;
    readonly category: WithdrawnReferenceInputCatalogueCategory;
  },
): Promise<WithdrawnReferenceInputInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw withdrawnReferenceInputSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: WITHDRAWN_REFERENCE_INPUT_CATEGORY_LABEL,
  });
};
