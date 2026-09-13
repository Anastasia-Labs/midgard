import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import type { ExecutionSourceScriptDecodingContracts } from "./contracts.js";

const FAMILY = "execution-source-script-decoding";
export const submitExecutionSourceScriptDecodingCancel = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & {
    readonly contracts: ExecutionSourceScriptDecodingContracts;
  },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancel({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
