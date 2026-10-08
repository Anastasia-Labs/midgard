import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  isWorkflowActuationRevokedError,
  type WorkflowActuationRevokedError,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "./fault-proof-application.js";
import type { WatcherFaultProofExecution } from "./fault-proof-execution.js";
import { createSupervisor } from "./fault-proof-supervisor.create-supervisor.js";
import {
  type SupervisorDependencies,
  type UnsafeWatcherFaultProofSupervisorForTest,
  type WatcherFaultProofJob,
  type WatcherFaultProofSupervisor,
  type WatcherJournalBusyRequeue,
} from "./fault-proof-supervisor.validate-job.js";
import type { WatcherDecisionHold } from "./watcher-decision-hold.js";

export const createWatcherFaultProofSupervisor = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  readonly execution: WatcherFaultProofExecution;
  /** The follower-store pins that hold open objectives' L1 history. */
  readonly proofRetention: WatcherProofRetention;
  /** Funding reservations held because their recorded decision is missing. */
  readonly reservationDecisionHolds: () => readonly WatcherDecisionHold[];
  /** The startup funding sweep and a decision-driver pass, around the
   * in-process requeue after journal_busy; the runtime always passes it. */
  readonly journalBusyRequeue?: WatcherJournalBusyRequeue;
}): WatcherFaultProofSupervisor =>
  createSupervisor({
    journalRoot: input.journalRoot,
    deploymentFingerprint: input.deploymentFingerprint,
    deadlineAlertHeadroomMs: input.deadlineAlertHeadroomMs,
    queueAuthenticationKey: input.queueAuthenticationKey,
    nowMs: Date.now,
    exposeUnsafeRunnerForTest: false,
    proofRetention: input.proofRetention,
    reservationDecisionHolds: input.reservationDecisionHolds,
    ...(input.journalBusyRequeue === undefined
      ? {}
      : { journalBusyRequeue: input.journalBusyRequeue }),
    dependencies: Object.freeze({
      categories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      run: async ({ job, actuationPermit, admission }) => {
        if (actuationPermit === null) {
          throw new Error(
            "production fault-proof runner omitted actuation authority",
          );
        }
        return await input.execution.execute({
          job,
          actuationPermit,
          admission,
        });
      },
      verifyCompleted: async (request) => {
        if (request.actuationPermit === null)
          throw new Error("completion validation omitted authority");
        const event = request.execution.entries.at(-1)?.event;
        if (event?.kind !== "completed")
          throw new Error("completion admission omitted its durable terminal");
        const verification = await input.execution.verifyCompleted({
          job: request.job,
          entries: request.execution.entries,
          terminal: event.terminal,
          actuationPermit: request.actuationPermit,
        });
        // The canonical depth of the correction decides whether the verified
        // completion is beyond rollback recovery and may be marked durably.
        return verification.kind === "applicable"
          ? {
              kind: "applicable",
              confirmationDepth:
                verification.terminal.observedAt.confirmationDepth,
            }
          : verification;
      },
      isActuationRevokedError: isWorkflowActuationRevokedError,
    }),
  }) as WatcherFaultProofSupervisor;

/** Test-only dependency seam; production category admission cannot be changed. */
export const unsafeCreateWatcherFaultProofSupervisorForTest = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs?: number;
  readonly unsafeNowMsForTest?: () => number;
  readonly unsafeQueueAuthenticationKeyForTest?: Uint8Array;
  readonly run: (job: WatcherFaultProofJob) => Promise<unknown>;
  readonly unsafeIsActuationRevokedErrorForTest?: (
    error: unknown,
  ) => error is WorkflowActuationRevokedError;
  readonly unsafeVerifyCompletedForTest?: SupervisorDependencies["verifyCompleted"];
  readonly unsafeProofRetentionForTest?: WatcherProofRetention;
}): UnsafeWatcherFaultProofSupervisorForTest =>
  createSupervisor({
    ...(input.unsafeProofRetentionForTest === undefined
      ? {}
      : { proofRetention: input.unsafeProofRetentionForTest }),
    journalRoot: input.journalRoot,
    deploymentFingerprint: input.deploymentFingerprint,
    deadlineAlertHeadroomMs:
      input.deadlineAlertHeadroomMs ??
      Math.min(3_600_000, MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs),
    queueAuthenticationKey:
      input.unsafeQueueAuthenticationKeyForTest ??
      Uint8Array.from({ length: 32 }, () => 0xa5),
    nowMs: input.unsafeNowMsForTest ?? (() => 0),
    exposeUnsafeRunnerForTest: true,
    dependencies: Object.freeze({
      categories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      run: async ({ job }) => await input.run(job),
      // Within rollback recovery by default, so nothing is marked.
      verifyCompleted:
        input.unsafeVerifyCompletedForTest ??
        (async () => ({ kind: "applicable", confirmationDepth: 1 }) as const),
      isActuationRevokedError:
        input.unsafeIsActuationRevokedErrorForTest ??
        isWorkflowActuationRevokedError,
    }),
  }) as UnsafeWatcherFaultProofSupervisorForTest;
