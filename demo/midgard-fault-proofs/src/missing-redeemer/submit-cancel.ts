import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import type { MissingRedeemerContracts } from "./contracts.js";

/** Burns the computation thread through the shared cancellation protocol. */
export const submitMissingRedeemerCancel = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: MissingRedeemerContracts },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancel({
    ...rest,
    family: "missing-redeemer",
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
