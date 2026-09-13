import type { TransitionTraceFaultProofContracts } from "@al-ft/midgard-sdk";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";

type Base = Parameters<typeof submitLinearFaultCancel>[0];
/** Cancels route or any persisted terminal-family phase with the owner's key. */
export const submitTransitionTraceCancel = (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: TransitionTraceFaultProofContracts;
  },
) =>
  submitLinearFaultCancel({
    ...params,
    family: "transition-trace",
    steps: params.contracts.transitionTrace.steps,
    computationThread: params.contracts.computationThread,
  });
