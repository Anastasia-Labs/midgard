import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { ExecutionSourceScriptDecodingContractsV1 } from "./contracts-v1.js";

const FAMILY = "execution-source-script-decoding";
export const submitExecutionSourceScriptDecodingCancelV1 = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancelV1>[0],
    "family" | "steps" | "computationThread"
  > & {
    readonly contracts: ExecutionSourceScriptDecodingContractsV1;
  },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancelV1({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
