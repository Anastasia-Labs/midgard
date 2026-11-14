import { submitLinearFaultCancel } from "../../src/linear-fault-cancel.js";

type LinearFaultCancelInput = Parameters<typeof submitLinearFaultCancel>[0];

/** Cancels an owned thread at any step of the named family's linear chain. */
export const submitFamilyCancel = (
  family: string,
  {
    contracts,
    ...input
  }: Omit<LinearFaultCancelInput, "family" | "steps" | "computationThread"> & {
    readonly contracts: Pick<
      LinearFaultCancelInput,
      "steps" | "computationThread"
    >;
  },
) =>
  submitLinearFaultCancel({
    ...input,
    family,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
