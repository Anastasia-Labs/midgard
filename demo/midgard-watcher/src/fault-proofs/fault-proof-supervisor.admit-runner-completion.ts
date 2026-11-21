import {
  assertWorkflowActuationPermitIdentity,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { FraudProofWorkflowTerminal } from "@al-ft/midgard-fault-proofs/workflow/journal";

import type { WatcherProofVerifiedCompletion } from "./fault-proof-objective-table.js";
import type {
  SupervisorDependencies,
  WatcherFaultProofJob,
} from "./fault-proof-supervisor.validate-job.js";

/** The `kind` a runner outcome names, if it names one. */
export const runnerOutcomeKind = (outcome: unknown): unknown =>
  typeof outcome === "object" && outcome !== null && "kind" in outcome
    ? outcome.kind
    : undefined;

/** The workflow and terminal a completed run reports
 * (`{ kind: "completed", result: { workflowId, terminal } }`). */
const completedRun = (
  outcome: unknown,
): Readonly<{ workflowId: string; terminal: FraudProofWorkflowTerminal }> => {
  const result =
    typeof outcome === "object" && outcome !== null && "result" in outcome
      ? (outcome.result as {
          readonly kind?: unknown;
          readonly workflowId?: unknown;
          readonly terminal?: unknown;
        } | null)
      : null;
  if (
    result?.kind !== "completed" ||
    typeof result.workflowId !== "string" ||
    typeof result.terminal !== "object" ||
    result.terminal === null
  )
    throw new Error("proof runner reported completion without its terminal");
  return {
    workflowId: result.workflowId,
    terminal: result.terminal as FraudProofWorkflowTerminal,
  };
};

/**
 * A fresh runner completion is checked against the chain before it counts:
 * the terminal the run reports must be the one raw L1 shows.
 */
export const admitWatcherProofRunnerCompletion = async (input: {
  readonly job: WatcherFaultProofJob;
  readonly actuationPermit: WorkflowActuationPermit | null;
  readonly verifyCompleted: SupervisorDependencies["verifyCompleted"];
  readonly outcome: unknown;
  readonly onApplicable: (
    verified: WatcherProofVerifiedCompletion,
  ) => Promise<void>;
}): Promise<unknown> => {
  const { workflowId, terminal } = completedRun(input.outcome);
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
    actuationPermit: input.actuationPermit,
    terminal,
  });
  assertPermit();
  if (verification.kind === "applicable") {
    await input.onApplicable({
      workflowId,
      terminalTxHash: verification.terminalTxHash,
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
