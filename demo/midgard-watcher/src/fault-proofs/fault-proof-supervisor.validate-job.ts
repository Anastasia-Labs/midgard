import { mkdir, readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

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
import { type WatcherFaultProofProgressRequest } from "./fault-proof-progress-authority.js";

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

export const exactWorkflowDirectories = async (
  journalRoot: string,
  categories: readonly WatcherInstalledWorkflowCategory[],
): Promise<
  readonly Readonly<{
    mode: "resume";
    category: WatcherInstalledWorkflowCategory;
    headerHash: string;
  }>[]
> => {
  const root = join(journalRoot, "fault-proofs");
  await mkdir(root, { recursive: true, mode: 0o700 });
  if ((await realpath(root)) !== root) {
    throw new Error("watcher fault-proof journal root traverses a symlink");
  }
  const allowed = new Set<string>(categories);
  const categoryEntries = await readdir(root, { withFileTypes: true });
  for (const entry of categoryEntries) {
    if (!allowed.has(entry.name) || !entry.isDirectory()) {
      throw new Error(
        `watcher fault-proof journal contains unknown category ${entry.name}`,
      );
    }
  }
  const jobs: Readonly<{
    mode: "resume";
    category: WatcherInstalledWorkflowCategory;
    headerHash: string;
  }>[] = [];
  for (const category of categories) {
    const categoryPath = join(root, category);
    const categoryEntry = categoryEntries.find(
      (entry) => entry.name === category,
    );
    if (categoryEntry === undefined) continue;
    if ((await realpath(categoryPath)) !== categoryPath) {
      throw new Error(
        `watcher fault-proof category ${category} traverses a symlink`,
      );
    }
    const headerEntries = await readdir(categoryPath, {
      withFileTypes: true,
    });
    headerEntries.sort((left, right) => left.name.localeCompare(right.name));
    for (const entry of headerEntries) {
      if (!entry.isDirectory() || !HEADER_HASH.test(entry.name)) {
        throw new Error(
          `watcher fault-proof journal contains invalid ${category} target ${entry.name}`,
        );
      }
      const headerPath = join(categoryPath, entry.name);
      if ((await realpath(headerPath)) !== headerPath) {
        throw new Error(
          `watcher fault-proof target ${category}/${entry.name} traverses a symlink`,
        );
      }
      jobs.push(
        Object.freeze({ mode: "resume", category, headerHash: entry.name }),
      );
      if (jobs.length > MAX_RECOVERABLE_WORKFLOWS) {
        throw new Error(
          "watcher fault-proof journal exceeds the recovery bound",
        );
      }
    }
  }
  return Object.freeze(jobs);
};

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
