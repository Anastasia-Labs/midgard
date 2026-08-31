import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import {
  MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL,
  type MissingNativeScriptUtxoContractsV1,
} from "./contracts-v1.js";

type Base = Parameters<typeof submitLinearFaultCancelV1>[0];
export const submitMissingNativeScriptUtxoCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: MissingNativeScriptUtxoContractsV1;
  },
) =>
  await submitLinearFaultCancelV1({
    ...params,
    family: MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });
