/** Computation-thread init for a direct purpose family. */
import type {
  ResolvedInitBuild,
  SubmitResolvedInitParams,
} from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { buildResolvedInit } from "../submit-init.submit-resolved-init.js";
import type {
  DirectPurposeContracts,
  DirectPurposeCustodyFamily,
} from "./family.js";

export const buildDirectPurposeInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts"> & {
    readonly family: DirectPurposeCustodyFamily;
    readonly contracts: DirectPurposeContracts;
  },
): Promise<ResolvedInitBuild> => {
  const { family, contracts, category, ...rest } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash)
    throw new Error(
      `${family.label}: catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  return await buildResolvedInit({
    ...rest,
    contracts,
    category,
    label: family.label,
  });
};
