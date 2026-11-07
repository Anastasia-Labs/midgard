/** Computation-thread init for the `unusedScriptWitness` family. */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
  type UnusedScriptWitnessContracts,
} from "./contracts.js";

export const submitUnusedScriptWitnessInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts"> & {
    readonly contracts: UnusedScriptWitnessContracts;
  },
): Promise<SubmitResolvedInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash)
    throw new Error(
      `${UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL}: catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  return await submitResolvedInit({
    ...params,
    label: UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
  });
};
