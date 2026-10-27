/** Computation-thread init for the `executionNativeScriptInvalid` family. */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  type ExecutionNativeScriptInvalidContracts,
} from "./contracts.js";

export const submitExecutionNativeScriptInvalidInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts"> & {
    readonly contracts: ExecutionNativeScriptInvalidContracts;
  },
): Promise<SubmitResolvedInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash)
    throw new Error(
      `${EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL}: catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  return await submitResolvedInit({
    ...params,
    label: EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  });
};
