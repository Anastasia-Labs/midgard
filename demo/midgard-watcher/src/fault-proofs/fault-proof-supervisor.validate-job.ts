import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import type {
  WorkflowActuationPermit,
  WorkflowActuationRevokedError,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { HeaderDecision } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import type { FraudProofWorkflowTerminal } from "@al-ft/midgard-fault-proofs/workflow/identity";
import { Header, StateQueueNode } from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import { assertWatcherStateQueueHeaderObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherStateQueueHeaderObservation } from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import type { WatcherFaultProofExecutionAdmission } from "./fault-proof-execution.js";
import type { WatcherProofCleanupFailure } from "./fault-proof-objective-cleanup.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import { listWatcherProofObjectives } from "./fault-proof-objective-table.js";
import type {
  WatcherDecisionSupersedeRequest,
  WatcherDecisionSupersession,
  WatcherFaultProofProgressRequest,
  WatcherProofMaterialsRoster,
} from "./fault-proof-progress-authority.js";
import type { WatcherFaultProofDepartureStep } from "./fault-proof-progress-authority.types.js";
import type {
  WatcherProofMaterialsSweep,
  WatcherProofMaterialsSweepable,
} from "./fault-proof-supervisor.proof-materials-sweep.js";
import type { WatcherDecisionHold } from "./watcher-decision-hold.js";
import type { WatcherJournalDatabase } from "./watcher-journal-database.js";

export const WATCHER_FAULT_PROOF_SUPERVISOR_SCHEMA_VERSION =
  "midgard-watcher-production-fault-proof-supervisor-v1" as const;

const HEADER_HASH = /^[0-9a-f]{56}$/u;

const DIGEST = /^[0-9a-f]{64}$/u;

export const POLICY_ID = /^[0-9a-f]{56}$/u;

export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const MAX_RECOVERABLE_WORKFLOWS = 2_048;

export type WatcherFaultProofDeadline = Readonly<{
  headerHash: string;
  headerBlockTimeMs: string;
  maturityAtMs: string;
  latestSafeStartAtMs: string;
}>;

const admittedDeadlines = new WeakSet<object>();

/**
 * Derives W04/W34 timing authority from the authenticated queue node. The
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
  const node = Data.from(header.stateQueueNodeCborHex, StateQueueNode);
  if (Data.to(node.header, Header) !== header.headerCborHex) {
    throw new Error(
      "fault-proof deadline node does not carry its authenticated HeaderV1",
    );
  }
  const headerBlockTimeMs = node.block_time;
  const maturityAtMs =
    headerBlockTimeMs + BigInt(MIDGARD_RETENTION_WINDOW.maturityMs);
  const latestSafeStartAtMs =
    maturityAtMs - BigInt(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs);
  const deadline = Object.freeze({
    headerHash: header.headerHash,
    headerBlockTimeMs: headerBlockTimeMs.toString(),
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
  /** Objectives held by name, each with the readiness reason it names until
   * its header leaves the finalized queue. */
  journalDecisionMissing: readonly WatcherDecisionHold[];
  /** Released or final objectives whose rows could not be deleted
   * (their rows stay): every admission retries the removal, and readiness
   * reports fault_proof_objective_cleanup_failed until it succeeds. */
  objectiveCleanupFailures: readonly WatcherProofCleanupFailure[];
  /** A busy or locked database the supervisor holds on, or null: readiness
   * reports journal_busy until the in-process requeue after its backoff. */
  journalBusy: string | null;
}>;

/** What the runtime does after the in-process requeue after journal_busy. */
export type WatcherJournalBusyRequeue = Readonly<{
  /** Asks the decision driver for a pass, which dispatches the work again. */
  wake(): void;
}>;

export type WatcherFaultProofSupervisor = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_SUPERVISOR_SCHEMA_VERSION;
  done: Promise<void>;
  requestProgress(request: WatcherFaultProofProgressRequest): Promise<void>;
  /** The progress authority's supersede, serialized with progress. */
  supersedeDecision(
    request: WatcherDecisionSupersedeRequest,
  ): Promise<WatcherDecisionSupersession>;
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
  /** Checks the objective's completion against the chain: `terminal` is a
   * run's fresh result, absent when only the chain says what completed. An
   * applicable completion names the removal transaction the chain shows and
   * its confirmation depth. */
  verifyCompleted(
    input: Readonly<{
      job: WatcherFaultProofJob;
      actuationPermit: WorkflowActuationPermit | null;
      terminal?: FraudProofWorkflowTerminal;
    }>,
  ): Promise<
    | Readonly<{
        kind: "applicable";
        confirmationDepth: number;
        terminalTxHash: string;
        /** The completion the chain shows, when the check read it whole. */
        terminal?: FraudProofWorkflowTerminal;
      }>
    | Readonly<{ kind: "pending" | "retryable"; reason?: string }>
  >;
  isActuationRevokedError(
    error: unknown,
  ): error is WorkflowActuationRevokedError;
  /** The objective is no longer driven: held, completed or its header gone. */
  objectiveSettled?(objective: WatcherProofObjective): void;
  /** The proof step an objective whose header left the queue takes from the
   * chain alone, with no run (`WatcherProofDepartureStep`). */
  departureStep?(step: WatcherFaultProofDepartureStep): void;
  /** The process's proof-materials roster, told by the progress authority. */
  proofMaterials?: WatcherProofMaterialsRoster;
  /** The sweep-only run after a departure completes an objective
   * (`fault-proof-supervisor.proof-materials-sweep.ts`). */
  sweepProofMaterials?: WatcherProofMaterialsSweep;
  /** Whether the views hold sweepable materials: a failed or lost sweep is
   * run again at a pass at tip only while they do. */
  proofMaterialsSweepable?: WatcherProofMaterialsSweepable;
}>;

/** The supervisor clock: a safe non-negative integer of milliseconds. */
export const supervisorClock = (nowMs: () => number) => (): number => {
  const value = nowMs();
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("watcher fault-proof supervisor clock is invalid");
  }
  return value;
};

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
): WatcherFaultProofJob => {
  const deadline =
    job.deadline ??
    Object.freeze({
      headerHash: job.headerHash,
      headerBlockTimeMs: "0",
      maturityAtMs: MIDGARD_RETENTION_WINDOW.maturityMs.toString(),
      latestSafeStartAtMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs.toString(),
    });
  if (
    (job.mode !== "run" && job.mode !== "resume") ||
    !categories.includes(job.category) ||
    !HEADER_HASH.test(job.headerHash) ||
    !DIGEST.test(job.decisionDigest) ||
    !CANONICAL_NATURAL.test(job.rollbackGeneration) ||
    deadline.headerHash !== job.headerHash ||
    !CANONICAL_NATURAL.test(deadline.headerBlockTimeMs) ||
    !CANONICAL_NATURAL.test(deadline.maturityAtMs) ||
    !CANONICAL_NATURAL.test(deadline.latestSafeStartAtMs) ||
    (requireAdmittedDeadline && !admittedDeadlines.has(deadline))
  ) {
    throw new Error("watcher fault-proof supervisor job is invalid");
  }
  const headerBlockTimeMs = BigInt(deadline.headerBlockTimeMs);
  const maturityAtMs = BigInt(deadline.maturityAtMs);
  const latestSafeStartAtMs = BigInt(deadline.latestSafeStartAtMs);
  if (
    maturityAtMs !==
      headerBlockTimeMs + BigInt(MIDGARD_RETENTION_WINDOW.maturityMs) ||
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
