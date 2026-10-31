/** Computation-thread init for the `scriptIntegrityHashMismatch` family. */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL,
  type ScriptIntegrityHashMismatchContracts,
} from "./contracts.js";

export const submitScriptIntegrityHashMismatchInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts"> & {
    readonly contracts: ScriptIntegrityHashMismatchContracts;
  },
): Promise<SubmitResolvedInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash)
    throw new Error(
      `${SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL}: catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  return await submitResolvedInit({
    ...params,
    label: SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL,
  });
};
