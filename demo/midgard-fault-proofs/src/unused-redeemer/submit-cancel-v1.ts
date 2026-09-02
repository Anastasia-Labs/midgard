import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";

const FAMILY = "unused-redeemer";
export const submitUnusedRedeemerCancelV1 = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancelV1>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: UnusedRedeemerContractsV1 },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancelV1({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
