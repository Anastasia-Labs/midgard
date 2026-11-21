/**
 * One watcher process as the fault-proof workflow sees it: the process's
 * in-flight set, a fresh run of an objective over it at each invocation,
 * and every transaction the process recorded as sent, oldest first. A
 * restart is a new process: it starts with nothing in flight and nothing
 * sent.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import {
  createWorkflowInFlight,
  type InFlightTransaction,
  type WorkflowInFlight,
  type WorkflowSends,
} from "../../src/workflow/in-flight.js";
import {
  createWorkflowRun,
  type WorkflowRun,
} from "../../src/workflow/workflow-run.js";

export type WorkflowObjective = Readonly<{
  category: FraudProofCatalogueCategoryName;
  headerHash: string;
}>;

export type TestWorkflowProcess = Readonly<{
  inFlight: WorkflowInFlight;
  /** The objective's part of the in-flight set, the same object each call. */
  sends: (objective: WorkflowObjective) => WorkflowSends;
  /** A fresh invocation of the objective over the process's in-flight set. */
  run: (objective: WorkflowObjective) => WorkflowRun;
  /** Every transaction the process recorded as sent, oldest first, settled
   * or not. */
  sent: () => readonly InFlightTransaction[];
}>;

export const createTestWorkflowProcess = (
  options: { readonly nowMs?: () => number } = {},
): TestWorkflowProcess => {
  const inFlight = createWorkflowInFlight(
    options.nowMs === undefined ? {} : { nowMs: options.nowMs },
  );
  const sent: InFlightTransaction[] = [];
  const observed = new WeakMap<WorkflowSends, WorkflowSends>();
  const sends = (objective: WorkflowObjective): WorkflowSends => {
    const part = inFlight.forObjective(objective);
    let known = observed.get(part);
    if (known === undefined) {
      known = Object.freeze({
        ...part,
        record: (transaction: InFlightTransaction) => {
          part.record(transaction);
          sent.push(transaction);
        },
      });
      observed.set(part, known);
    }
    return known;
  };
  return Object.freeze({
    inFlight,
    sends,
    run: (objective) => createWorkflowRun(sends(objective)),
    sent: () => Object.freeze([...sent]),
  });
};
