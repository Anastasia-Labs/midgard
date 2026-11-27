import { buildLinearFaultCancel } from "../../src/linear-fault-cancel.js";
import { buildAndSubmit } from "./submit-steps.js";

type LinearFaultCancelInput = Parameters<typeof buildLinearFaultCancel>[0];

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
  buildAndSubmit(buildLinearFaultCancel, {
    ...input,
    family,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
