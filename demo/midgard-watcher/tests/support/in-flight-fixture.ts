import {
  createWorkflowInFlight,
  type WorkflowInFlight,
} from "@al-ft/midgard-fault-proofs/workflow/in-flight";

/** What the progress authority reads of the process's in-flight set. */
export const inFlightOf = (
  inFlight: WorkflowInFlight = createWorkflowInFlight(),
) => ({ sending: inFlight.sending, forgetInFlight: inFlight.forget });
