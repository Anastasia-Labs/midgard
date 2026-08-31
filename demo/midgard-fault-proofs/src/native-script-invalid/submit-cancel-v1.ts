import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import {
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  type NativeScriptInvalidContractsV1,
} from "./contracts-v1.js";

type Base = Parameters<typeof submitLinearFaultCancelV1>[0];
export const submitNativeScriptInvalidCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: NativeScriptInvalidContractsV1;
  },
) =>
  await submitLinearFaultCancelV1({
    ...params,
    family: NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });
