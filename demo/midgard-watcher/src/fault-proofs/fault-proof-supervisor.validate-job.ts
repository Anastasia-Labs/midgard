import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  assertWorkflowActuationPermitIdentity,
  type HeaderDecision,
  type WorkflowActuationPermit,
  type WorkflowActuationRevokedError,
} from "@al-ft/midgard-fault-proofs";
import { Header } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { type WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import type { WatcherFaultProofExecutionAdmission } from "./fault-proof-execution.js";
import { type WatcherProofExecution } from "./fault-proof-objective-journal.js";
import { listWatcherProofObjectives } from "./fault-proof-objective-table.js";
import { type WatcherFaultProofProgressRequest } from "./fault-proof-progress-authority.js";
import type { WatcherJournalDatabase } from "./watcher-journal-database.js";

export const WATCHER_FAULT_PROOF_SUPERVISOR_SCHEMA_VERSION =
  "midgard-watcher-production-fault-proof-supervisor-v1" as const;

const HEADER_HASH = /^[0-9a-f]{56}$/u;

export const DEPLOYMENT_FINGERPRINT = /^[0-9a-f]{64}$/u;

export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const MAX_RECOVERABLE_WORKFLOWS = 2_048;

export type WatcherFaultProofDeadline = Readonly<{
  headerHash: string;
  headerEndTimeMs: string;
  maturityAtMs: string;
  latestSafeStartAtMs: string;
}>;

const admittedDeadlines = new WeakSet<object>();

/**
 * Derives W04/W34 timing authority from an authenticated Header. The
 * complete correction path owns the canonical half-maturity budget, so the
 * latest safe start is exactly maturity minus that bound.
 */
export const watcherFaultProofDeadline = (
  header: WatcherStateQueueHeaderObservation,
): WatcherFaultProofDeadline => {
  assertWatcherStateQueueHeaderObservation(header);
  const decoded = Data.from(header.headerCborHex, Header);
  if (Data.to(decoded, Header) !== header.headerCborHex) {
    throw new Error("fault-proof deadline HeaderV1 CBOR is noncanonical");
  }
  const headerEndTimeMs = decoded.endTime;
  const maturityAtMs =
    headerEndTimeMs + BigInt(MIDGARD_RETENTION_WINDOW.maturityMs);
  const latestSafeStartAtMs =
    maturityAtMs - BigInt(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs);
  const deadline = Object.freeze({
    headerHash: header.headerHash,
    headerEndTimeMs: headerEndTimeMs.toString(),
    maturityAtMs: maturityAtMs.toString(),
    latestSafeStartAtMs: latestSafeStartAtMs.toString(),
  });
  admittedDeadlines.add(deadline);
  return deadline;
};

export type WatcherFaultProofJob = Readonly<{
  mode: "run" | "resume";
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
  decisionDigest: string;
  rollbackGeneration: string;
  deadline: WatcherFaultProofDeadline | null;
  /** Wake-up revision, not part of the durable execution or queue identity. */
  observationRevision?: string;
}>;

export type WatcherFaultProofReconciliation = Readonly<{
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
  decisionDigest: string;
  actuationPermit: WorkflowActuationPermit;
}>;

export type UnsafeWatcherFaultProofJobForTest = Omit<
  WatcherFaultProofJob,
  "deadline"
> &
  Readonly<{ deadline?: WatcherFaultProofDeadline }>;

export type WatcherFaultProofSupervisorStatus = Readonly<{
  phase: "accepting" | "blocked" | "closing" | "closed";
  recovered: boolean;
  unfinishedObjectiveCount: number;
  queuedJobCount: number;
  activeJob: WatcherFaultProofJob | null;
  blockedJob: WatcherFaultProofJob | null;
  deadlineHealth: "safe" | "at_risk" | "unsafe";
  earliestDeadlineJob: WatcherFaultProofJob | null;
  remainingSafeStartMs: string | null;
  /** The journals' first integrity failure in this process, or null:
   * readiness reports journal_integrity until an operator repairs them. */
  journalIntegrity: string | null;
  /** Why the journals could not be opened, or null: readiness reports
   * journal_unavailable while the open is retried. */
  journalUnavailable: string | null;
  /** A fault-proof journal holds its cap of live rows: readiness reports
   * journal_capacity until rows complete or are pruned. */
  journalCapacity: boolean;
}>;

export type WatcherFaultProofSupervisor = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_SUPERVISOR_SCHEMA_VERSION;
  done: Promise<void>;
  requestProgress(request: WatcherFaultProofProgressRequest): Promise<void>;
  revokeAuthority(reason: string): void;
  status(): WatcherFaultProofSupervisorStatus;
  durableQueueStatus(): Readonly<{
    queuedJobCount: number;
    oldestQueuedAtMs: string | null;
  }>;
  close(): Promise<void>;
}>;

export type UnsafeWatcherFaultProofSupervisorForTest =
  WatcherFaultProofSupervisor &
    Readonly<{
      recoverExisting(
        decision: HeaderDecision | null,
        actuationPermit?: WorkflowActuationPermit,
        deadline?: WatcherFaultProofDeadline,
        rollbackGeneration?: string,
        reconciliations?: readonly WatcherFaultProofReconciliation[],
      ): Promise<number>;
      unsafeRunOrResumeForTest(
        job: UnsafeWatcherFaultProofJobForTest,
      ): Promise<unknown>;
      unsafeScheduleForTest(
        job: UnsafeWatcherFaultProofJobForTest,
      ): Promise<void>;
    }>;

export type SupervisorDependencies = Readonly<{
  categories: readonly WatcherInstalledWorkflowCategory[];
  run(
    input: Readonly<{
      job: WatcherFaultProofJob;
      actuationPermit: WorkflowActuationPermit | null;
      admission: WatcherFaultProofExecutionAdmission;
    }>,
  ): Promise<unknown>;
  verifyCompleted(
    input: Readonly<{
      job: WatcherFaultProofJob;
      execution: WatcherProofExecution;
      actuationPermit: WorkflowActuationPermit | null;
    }>,
  ): Promise<
    | Readonly<{ kind: "applicable"; confirmationDepth: number }>
    | Readonly<{ kind: "pending" | "retryable"; reason?: string }>
  >;
  isActuationRevokedError(
    error: unknown,
  ): error is WorkflowActuationRevokedError;
}>;

/** The recorded objectives that still hold work, in category order. */
export const recordedWorkflowObjectives = (
  database: WatcherJournalDatabase,
  categories: readonly WatcherInstalledWorkflowCategory[],
): readonly Readonly<{
  mode: "resume";
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
}>[] =>
  Object.freeze(
    listWatcherProofObjectives(database, categories)
      .filter(({ state }) => state !== "marked")
      .map(({ objective }) =>
        Object.freeze({ mode: "resume" as const, ...objective }),
      )
      .sort(
        (left, right) =>
          categories.indexOf(left.category) -
            categories.indexOf(right.category) ||
          (left.headerHash < right.headerHash
            ? -1
            : left.headerHash > right.headerHash
              ? 1
              : 0),
      ),
  );

export const validateJob = (
  job: WatcherFaultProofJob | UnsafeWatcherFaultProofJobForTest,
  categories: readonly WatcherInstalledWorkflowCategory[],
  requireAdmittedDeadline: boolean,
  actuationPermit: WorkflowActuationPermit | null,
): WatcherFaultProofJob => {
  if (job.deadline === null) {
    if (
      actuationPermit === null ||
      job.mode !== "resume" ||
      !categories.includes(job.category)
    )
      throw new Error("reconciliation job omitted its admitted authority");
    const authority = assertWorkflowActuationPermitIdentity({
      permit: actuationPermit,
      category: job.category,
      rollbackGeneration: job.rollbackGeneration,
    });
    if (
      authority.authority !== "reconciliation" ||
      authority.headerHash !== job.headerHash ||
      authority.decisionDigest !== job.decisionDigest
    )
      throw new Error(
        "reconciliation job changed its existing execution identity",
      );
    return Object.freeze({ ...job, deadline: null });
  }
  const deadline =
    job.deadline ??
    Object.freeze({
      headerHash: job.headerHash,
      headerEndTimeMs: "0",
      maturityAtMs: MIDGARD_RETENTION_WINDOW.maturityMs.toString(),
      latestSafeStartAtMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs.toString(),
    });
  if (
    (job.mode !== "run" && job.mode !== "resume") ||
    !categories.includes(job.category) ||
    !HEADER_HASH.test(job.headerHash) ||
    !DEPLOYMENT_FINGERPRINT.test(job.decisionDigest) ||
    !CANONICAL_NATURAL.test(job.rollbackGeneration) ||
    deadline.headerHash !== job.headerHash ||
    !CANONICAL_NATURAL.test(deadline.headerEndTimeMs) ||
    !CANONICAL_NATURAL.test(deadline.maturityAtMs) ||
    !CANONICAL_NATURAL.test(deadline.latestSafeStartAtMs) ||
    (requireAdmittedDeadline && !admittedDeadlines.has(deadline))
  ) {
    throw new Error("watcher fault-proof supervisor job is invalid");
  }
  const headerEndTimeMs = BigInt(deadline.headerEndTimeMs);
  const maturityAtMs = BigInt(deadline.maturityAtMs);
  const latestSafeStartAtMs = BigInt(deadline.latestSafeStartAtMs);
  if (
    maturityAtMs !==
      headerEndTimeMs + BigInt(MIDGARD_RETENTION_WINDOW.maturityMs) ||
    latestSafeStartAtMs !==
      maturityAtMs - BigInt(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs)
  ) {
    throw new Error("watcher fault-proof supervisor job is invalid");
  }
  return Object.freeze({
    mode: job.mode,
    category: job.category,
    headerHash: job.headerHash,
    decisionDigest: job.decisionDigest,
    rollbackGeneration: job.rollbackGeneration,
    deadline,
    observationRevision: job.observationRevision,
  });
};
