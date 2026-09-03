import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import type { ReceivePurposeLanguageContracts } from "./contracts-v1.js";
export const submitReceivePurposeLanguageCancel = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: ReceivePurposeLanguageContracts },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancel({
    ...rest,
    family: "receive-purpose-language",
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
