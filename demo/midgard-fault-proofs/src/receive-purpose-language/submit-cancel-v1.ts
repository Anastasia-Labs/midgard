import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { ReceivePurposeLanguageContractsV1 } from "./contracts-v1.js";
export const submitReceivePurposeLanguageCancelV1 = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancelV1>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: ReceivePurposeLanguageContractsV1 },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancelV1({
    ...rest,
    family: "receive-purpose-language",
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
