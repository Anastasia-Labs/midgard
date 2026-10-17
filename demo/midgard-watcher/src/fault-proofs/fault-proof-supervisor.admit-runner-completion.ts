import {
  assertWorkflowActuationPermitIdentity,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherProofExecution } from "./fault-proof-objective-journal.js";
import type {
  SupervisorDependencies,
  WatcherFaultProofJob,
} from "./fault-proof-supervisor.validate-job.js";

/** A fresh runner result uses the same canonical terminal door as recovery. */
export const admitWatcherProofRunnerCompletion = async (input: {
  readonly job: WatcherFaultProofJob;
  readonly execution: WatcherProofExecution | undefined;
  readonly actuationPermit: WorkflowActuationPermit | null;
  readonly verifyCompleted: SupervisorDependencies["verifyCompleted"];
  readonly outcome: unknown;
  readonly onApplicable: (
    verified: Readonly<{
      execution: WatcherProofExecution;
      confirmationDepth: number;
    }>,
  ) => Promise<void>;
}): Promise<unknown> => {
  const execution = input.execution;
  if (execution?.entries.at(-1)?.event.kind !== "completed")
    throw new Error(
      "proof runner reported completion without a completed journal",
    );
  const assertPermit = () => {
    if (input.actuationPermit !== null)
      assertWorkflowActuationPermitIdentity({
        permit: input.actuationPermit,
        category: input.job.category,
        rollbackGeneration: input.job.rollbackGeneration,
      });
  };
  assertPermit();
  const verification = await input.verifyCompleted({
    job: input.job,
    execution,
    actuationPermit: input.actuationPermit,
  });
  assertPermit();
  if (verification.kind === "applicable") {
    await input.onApplicable({
      execution,
      confirmationDepth: verification.confirmationDepth,
    });
    return input.outcome;
  }
  return verification.kind === "retryable"
    ? verification
    : {
        kind: "pending",
        resume: "await_observation",
        reason: verification.reason ?? "completion_pending",
      };
};
