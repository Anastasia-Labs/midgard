import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  type ExecutionNativeScriptInvalidContracts,
} from "./contracts.js";

type Base = Parameters<typeof submitLinearFaultCancel>[0];
export const submitExecutionNativeScriptInvalidCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: ExecutionNativeScriptInvalidContracts;
  },
) =>
  await submitLinearFaultCancel({
    ...params,
    family: EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });

export const submitExecutionNativeScriptInvalidAcceptedCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: ExecutionNativeScriptInvalidContracts;
  },
) => {
  if (params.contracts.acceptedPrelude?.length !== 7)
    throw new Error(
      "execution-native-script-invalid: accepted cancellation requires seven physical scripts",
    );
  return await submitLinearFaultCancel({
    ...params,
    family: EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
    steps: params.contracts.acceptedPrelude,
    computationThread: params.contracts.computationThread,
  });
};
