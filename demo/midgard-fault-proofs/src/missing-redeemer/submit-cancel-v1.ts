import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { MissingRedeemerContractsV1 } from "./contracts-v1.js";

/** Burns the computation thread through the shared cancellation protocol. */
export const submitMissingRedeemerCancelV1 = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancelV1>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: MissingRedeemerContractsV1 },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancelV1({
    ...rest,
    family: "missing-redeemer",
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
