import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import type { MissingScriptSourceContracts } from "./contracts-v1.js";

const FAMILY = "missing-script-source";
export const submitMissingScriptSourceCancel = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & {
    readonly contracts: MissingScriptSourceContracts;
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
