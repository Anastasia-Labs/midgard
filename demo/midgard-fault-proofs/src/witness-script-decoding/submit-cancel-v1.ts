import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import {
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContractsV1,
} from "./contracts-v1.js";

export const submitWitnessScriptDecodingCancelV1 = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancelV1>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: WitnessScriptDecodingContractsV1 },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancelV1({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
