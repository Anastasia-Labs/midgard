import { buildLinearFaultCancel } from "../linear-fault-cancel.js";
import {
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContracts,
} from "./contracts.js";

export const buildWitnessScriptDecodingCancel = async (
  args: Omit<
    Parameters<typeof buildLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: WitnessScriptDecodingContracts },
) => {
  const { contracts, ...rest } = args;
  return await buildLinearFaultCancel({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
