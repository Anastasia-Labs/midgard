import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { MissingScriptSourceContractsV1 } from "./contracts-v1.js";

const FAMILY = "missing-script-source";
export const submitMissingScriptSourceCancelV1 = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancelV1>[0],
    "family" | "steps" | "computationThread"
  > & {
    readonly contracts: MissingScriptSourceContractsV1;
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
