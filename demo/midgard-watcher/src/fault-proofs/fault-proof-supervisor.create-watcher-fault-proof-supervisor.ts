import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  isWorkflowActuationRevokedError,
  type WorkflowActuationRevokedError,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import {
  createWorkflowInFlight,
  type WorkflowInFlight,
} from "@al-ft/midgard-fault-proofs/workflow/in-flight";

import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "./fault-proof-application.js";
import type {
  WatcherFaultProofExecution,
  WatcherFaultProofExecutionAdmission,
} from "./fault-proof-execution.js";
import { createSupervisor } from "./fault-proof-supervisor.create-supervisor.js";
import type {
  SupervisorDependencies,
  UnsafeWatcherFaultProofSupervisorForTest,
  WatcherFaultProofJob,
  WatcherFaultProofSupervisor,
  WatcherJournalBusyRequeue,
} from "./fault-proof-supervisor.validate-job.js";
import type { WatcherL1Clock } from "./wallet-only-send-open.js";

/** A deadline alert's headroom: the longest single-stage deadline. */
export const watcherDeadlineAlertHeadroomMs = (
  deadlines: Readonly<
    Record<
      "daFetchMs" | "daPublishMs" | "proofConstructMs" | "proofSubmitMs",
      number
    >
  >,
): number =>
  Math.max(
    deadlines.daFetchMs,
    deadlines.daPublishMs,
    deadlines.proofConstructMs,
    deadlines.proofSubmitMs,
  );

export const createWatcherFaultProofSupervisor = (input: {
  readonly journalRoot: string;
  /** The process's in-flight set, which the execution's runs record into. */
  readonly inFlight: WorkflowInFlight;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  /** The L1 chain the restart wait reads. */
  readonly l1Clock: WatcherL1Clock;
  readonly execution: Pick<
    WatcherFaultProofExecution,
    "verifyCompleted" | "execute"
  > &
    Partial<
      Pick<WatcherFaultProofExecution, "objectiveSettled" | "departureStep">
    >;
  /** The follower-store pins that hold open objectives' L1 history. */
  readonly proofRetention: WatcherProofRetention;
  /** A decision-driver pass after the in-process requeue after
   * journal_busy; the runtime always passes it. */
  readonly journalBusyRequeue?: WatcherJournalBusyRequeue;
  /** The pass over our own threads each admitted observation is followed by. */
  readonly ownThreadSweep?: () => Promise<void>;
  /** The process's proof-materials roster. */
  readonly proofMaterials?: SupervisorDependencies["proofMaterials"];
  /** The sweep-only run after a departure completes an objective. */
  readonly sweepProofMaterials?: SupervisorDependencies["sweepProofMaterials"];
  /** Whether the views hold sweepable materials (a failed sweep's retry). */
  readonly proofMaterialsSweepable?: SupervisorDependencies["proofMaterialsSweepable"];
  /** Follows the sends of objectives no run drives at each pass at tip. */
  readonly observeSignedTransaction?: SupervisorDependencies["observeSignedTransaction"];
}): WatcherFaultProofSupervisor =>
  createSupervisor({
    journalRoot: input.journalRoot,
    inFlight: input.inFlight,
    deploymentFingerprint: input.deploymentFingerprint,
    deadlineAlertHeadroomMs: input.deadlineAlertHeadroomMs,
    queueAuthenticationKey: input.queueAuthenticationKey,
    nowMs: Date.now,
    l1Clock: input.l1Clock,
    exposeUnsafeRunnerForTest: false,
    proofRetention: input.proofRetention,
    ...(input.journalBusyRequeue === undefined
      ? {}
      : { journalBusyRequeue: input.journalBusyRequeue }),
    ...(input.ownThreadSweep === undefined
      ? {}
      : { ownThreadSweep: input.ownThreadSweep }),
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
        const verification = await input.execution.verifyCompleted({
          job: request.job,
          actuationPermit: request.actuationPermit,
          ...(request.terminal === undefined
            ? {}
            : { terminal: request.terminal }),
        });
        // The canonical depth of the correction decides whether the verified
        // completion is beyond rollback recovery and may be marked durably.
        return verification.kind === "applicable"
          ? {
              kind: "applicable",
              confirmationDepth:
                verification.terminal.observedAt.confirmationDepth,
              terminalTxHash: verification.terminal.correction.removalTxHash,
              terminal: verification.terminal,
            }
          : verification;
      },
      isActuationRevokedError: isWorkflowActuationRevokedError,
      ...(input.execution.objectiveSettled === undefined
        ? {}
        : { objectiveSettled: input.execution.objectiveSettled }),
      ...(input.execution.departureStep === undefined
        ? {}
        : { departureStep: input.execution.departureStep }),
      ...(input.proofMaterials === undefined
        ? {}
        : { proofMaterials: input.proofMaterials }),
      ...(input.sweepProofMaterials === undefined
        ? {}
        : { sweepProofMaterials: input.sweepProofMaterials }),
      ...(input.proofMaterialsSweepable === undefined
        ? {}
        : { proofMaterialsSweepable: input.proofMaterialsSweepable }),
      ...(input.observeSignedTransaction === undefined
        ? {}
        : { observeSignedTransaction: input.observeSignedTransaction }),
    }),
  }) as WatcherFaultProofSupervisor;

/** Test-only dependency seam; production category admission cannot be changed. */
export const unsafeCreateWatcherFaultProofSupervisorForTest = (input: {
  readonly journalRoot: string;
  /** Defaults to an empty in-flight set. */
  readonly inFlight?: WorkflowInFlight;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs?: number;
  readonly unsafeNowMsForTest?: () => number;
  /** Defaults to no applied block, so the restart wait never opens. */
  readonly unsafeL1ClockForTest?: WatcherL1Clock;
  readonly unsafeQueueAuthenticationKeyForTest?: Uint8Array;
  readonly run: (
    job: WatcherFaultProofJob,
    admission: WatcherFaultProofExecutionAdmission,
  ) => Promise<unknown>;
  readonly unsafeIsActuationRevokedErrorForTest?: (
    error: unknown,
  ) => error is WorkflowActuationRevokedError;
  readonly unsafeVerifyCompletedForTest?: SupervisorDependencies["verifyCompleted"];
  readonly unsafeProofRetentionForTest?: WatcherProofRetention;
  readonly unsafeOwnThreadSweepForTest?: () => Promise<void>;
}): UnsafeWatcherFaultProofSupervisorForTest =>
  createSupervisor({
    ...(input.unsafeOwnThreadSweepForTest === undefined
      ? {}
      : { ownThreadSweep: input.unsafeOwnThreadSweepForTest }),
    ...(input.unsafeProofRetentionForTest === undefined
      ? {}
      : { proofRetention: input.unsafeProofRetentionForTest }),
    journalRoot: input.journalRoot,
    inFlight: input.inFlight ?? createWorkflowInFlight(),
    deploymentFingerprint: input.deploymentFingerprint,
    deadlineAlertHeadroomMs:
      input.deadlineAlertHeadroomMs ??
      Math.min(3_600_000, MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs),
    queueAuthenticationKey:
      input.unsafeQueueAuthenticationKeyForTest ??
      Uint8Array.from({ length: 32 }, () => 0xa5),
    nowMs: input.unsafeNowMsForTest ?? (() => 0),
    l1Clock: input.unsafeL1ClockForTest ?? {
      slotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preview,
      appliedSlot: () => null,
    },
    exposeUnsafeRunnerForTest: true,
    dependencies: Object.freeze({
      categories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      run: async ({ job, admission }) => await input.run(job, admission),
      // Within rollback recovery by default, so nothing is marked.
      verifyCompleted:
        input.unsafeVerifyCompletedForTest ??
        (async () =>
          ({
            kind: "applicable",
            confirmationDepth: 1,
            terminalTxHash: "00".repeat(32),
          }) as const),
      isActuationRevokedError:
        input.unsafeIsActuationRevokedErrorForTest ??
        isWorkflowActuationRevokedError,
    }),
  }) as UnsafeWatcherFaultProofSupervisorForTest;
