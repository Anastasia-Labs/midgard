import { buildLinearFaultCancel } from "../linear-fault-cancel.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContracts,
} from "./contracts.js";

export const buildOutputReferenceScriptDecodingCancel = async (
  args: Omit<
    Parameters<typeof buildLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & {
    readonly contracts: OutputReferenceScriptDecodingContracts;
  },
) => {
  const { contracts, ...rest } = args;
  return await buildLinearFaultCancel({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
