import type { WorkflowSends } from "./in-flight.js";

/**
 * One invocation of a workflow: the objective's part of the process's
 * in-flight set. A fresh object per invocation, so the actuation permit the
 * invocation holds binds to it alone (`actuation-permit.ts`).
 */
export type WorkflowRun = Readonly<{ sends: WorkflowSends }>;

export const createWorkflowRun = (sends: WorkflowSends): WorkflowRun =>
  Object.freeze({ sends });
