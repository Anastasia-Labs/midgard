import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import type { UnusedScriptWitnessContracts } from "./contracts-v1.js";

const FAMILY = "unused-script-witness";
export const submitUnusedScriptWitnessCancel = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: UnusedScriptWitnessContracts },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancel({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
