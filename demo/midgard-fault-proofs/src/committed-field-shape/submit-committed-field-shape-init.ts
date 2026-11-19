import {
  buildResolvedInit,
  type ResolvedInitBuild,
  type SubmitResolvedInitParams,
} from "../submit-init.js";
import {
  COMMITTED_FIELD_SHAPE_CATEGORY_LABEL,
  type CommittedFieldShapeContracts,
} from "./contracts.js";
import {
  type CommittedFieldShapeCatalogueCategory,
  committedFieldShapeSubmitError,
} from "./submit-common.js";

export type CommittedFieldShapeInitBuild = ResolvedInitBuild;

/** Pre-registration init using an explicit category membership record. */
export const buildCommittedFieldShapeInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: CommittedFieldShapeContracts;
    readonly category: CommittedFieldShapeCatalogueCategory;
  },
): Promise<CommittedFieldShapeInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw committedFieldShapeSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: COMMITTED_FIELD_SHAPE_CATEGORY_LABEL,
  });
};
