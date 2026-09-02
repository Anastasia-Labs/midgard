import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  type ExecutionNativeScriptInvalidContractsV1,
} from "./contracts-v1.js";

type Base = Parameters<typeof submitLinearFaultCancelV1>[0];
export const submitExecutionNativeScriptInvalidCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: ExecutionNativeScriptInvalidContractsV1;
  },
) =>
  await submitLinearFaultCancelV1({
    ...params,
    family: EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });

export const submitExecutionNativeScriptInvalidAcceptedCancelV1 = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: ExecutionNativeScriptInvalidContractsV1;
  },
) => {
  if (params.contracts.acceptedPrelude?.length !== 7)
    throw new Error(
      "execution-native-script-invalid: accepted cancellation requires seven physical scripts",
    );
  return await submitLinearFaultCancelV1({
    ...params,
    family: EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
    steps: params.contracts.acceptedPrelude,
    computationThread: params.contracts.computationThread,
  });
};
