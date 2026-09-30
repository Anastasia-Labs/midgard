import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  isWorkflowActuationRevokedError,
  type WorkflowActuationRevokedError,
} from "@al-ft/midgard-fault-proofs";

import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "./fault-proof-application.js";
import type { WatcherFaultProofExecution } from "./fault-proof-execution.js";
import { createSupervisor } from "./fault-proof-supervisor.create-supervisor.js";
import {
  type UnsafeWatcherFaultProofSupervisorForTest,
  type WatcherFaultProofJob,
  type WatcherFaultProofSupervisor,
} from "./fault-proof-supervisor.validate-job.js";

export const createWatcherFaultProofSupervisor = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  readonly execution: WatcherFaultProofExecution;
}): WatcherFaultProofSupervisor =>
  createSupervisor({
    journalRoot: input.journalRoot,
    deploymentFingerprint: input.deploymentFingerprint,
    deadlineAlertHeadroomMs: input.deadlineAlertHeadroomMs,
    queueAuthenticationKey: input.queueAuthenticationKey,
    nowMs: Date.now,
    exposeUnsafeRunnerForTest: false,
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
        return await input.execution.verifyCompleted({
          job: request.job,
          entries: request.execution.entries,
          terminal: event.terminal,
          actuationPermit: request.actuationPermit,
        });
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
}): UnsafeWatcherFaultProofSupervisorForTest =>
  createSupervisor({
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
      verifyCompleted: async () => ({ kind: "applicable" as const }),
      isActuationRevokedError:
        input.unsafeIsActuationRevokedErrorForTest ??
        isWorkflowActuationRevokedError,
    }),
  }) as UnsafeWatcherFaultProofSupervisorForTest;
