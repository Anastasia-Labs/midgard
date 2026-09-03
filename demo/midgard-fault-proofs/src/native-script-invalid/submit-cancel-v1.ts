import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import {
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  type NativeScriptInvalidContracts,
} from "./contracts-v1.js";

type Base = Parameters<typeof submitLinearFaultCancel>[0];
export const submitNativeScriptInvalidCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: NativeScriptInvalidContracts;
  },
) =>
  await submitLinearFaultCancel({
    ...params,
    family: NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });
