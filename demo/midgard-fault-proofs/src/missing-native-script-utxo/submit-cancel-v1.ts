import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import {
  MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL,
  type MissingNativeScriptUtxoContracts,
} from "./contracts-v1.js";

type Base = Parameters<typeof submitLinearFaultCancel>[0];
export const submitMissingNativeScriptUtxoCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: MissingNativeScriptUtxoContracts;
  },
) =>
  await submitLinearFaultCancel({
    ...params,
    family: MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });
