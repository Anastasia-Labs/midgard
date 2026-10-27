/** Computation-thread init for a direct purpose family. */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import type { DirectPurposeContracts, DirectPurposeFamily } from "./family.js";

export const submitDirectPurposeInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts"> & {
    readonly family: DirectPurposeFamily<unknown>;
    readonly contracts: DirectPurposeContracts;
  },
): Promise<SubmitResolvedInitResult> => {
  const { family, contracts, category, ...rest } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash)
    throw new Error(
      `${family.label}: catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  return await submitResolvedInit({
    ...rest,
    contracts,
    category,
    label: family.label,
  });
};
