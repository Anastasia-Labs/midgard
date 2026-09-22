import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import type { UnusedRedeemerContracts } from "./contracts.js";

const FAMILY = "unused-redeemer";
export const submitUnusedRedeemerCancel = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: UnusedRedeemerContracts },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancel({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
