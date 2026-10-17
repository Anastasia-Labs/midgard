import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  COMMITTED_FIELD_SHAPE_CATEGORY_LABEL,
  type CommittedFieldShapeContracts,
} from "./contracts.js";
import {
  type CommittedFieldShapeCatalogueCategory,
  committedFieldShapeSubmitError,
} from "./submit-common.js";

export type SubmitCommittedFieldShapeInitResult = SubmitResolvedInitResult;

/** Pre-registration init using an explicit category membership record. */
export const submitCommittedFieldShapeInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: CommittedFieldShapeContracts;
    readonly category: CommittedFieldShapeCatalogueCategory;
  },
): Promise<SubmitCommittedFieldShapeInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw committedFieldShapeSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await submitResolvedInit({
    ...params,
    label: COMMITTED_FIELD_SHAPE_CATEGORY_LABEL,
  });
};
