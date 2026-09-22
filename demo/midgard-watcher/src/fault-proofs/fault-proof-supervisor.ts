import { mkdir, readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  assertWorkflowActuationPermitIdentity,
  type HeaderDecision,
  isWorkflowActuationRevokedError,
  requireRunnableHeaderFault,
  revokeWorkflowActuationPermit,
  type WorkflowActuationPermit,
  type WorkflowActuationRevokedError,
} from "@al-ft/midgard-fault-proofs";
import { Header } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherInstalledWorkflowCategory,
} from "./fault-proof-application.js";
import type {
  WatcherFaultProofExecution,
  WatcherFaultProofExecutionAdmission,
} from "./fault-proof-execution.js";
import {
  readWatcherProofExecution,
  type WatcherProofExecution,
} from "./fault-proof-objective-journal.js";
import {
  createWatcherFaultProofProgressAuthority,
  type WatcherFaultProofProgressRequest,
} from "./fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
  type WatcherFaultProofQueueJournal,
} from "./fault-proof-queue-journal.js";
export type { WatcherFaultProofProgressRequest } from "./fault-proof-progress-authority.js";

export const WATCHER_FAULT_PROOF_SUPERVISOR_SCHEMA_VERSION =
  "midgard-watcher-production-fault-proof-supervisor-v1" as const;

const HEADER_HASH = /^[0-9a-f]{56}$/u;
const DEPLOYMENT_FINGERPRINT = /^[0-9a-f]{64}$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAX_RECOVERABLE_WORKFLOWS = 2_048;

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

type UnsafeWatcherFaultProofJobForTest = Omit<
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

type SupervisorDependencies = Readonly<{
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
    Readonly<{ kind: "applicable" | "pending" | "retryable"; reason?: string }>
  >;
  isActuationRevokedError(
    error: unknown,
  ): error is WorkflowActuationRevokedError;
}>;

const exactWorkflowDirectories = async (
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

const validateJob = (
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

const createSupervisor = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  readonly nowMs: () => number;
  readonly dependencies: SupervisorDependencies;
  readonly exposeUnsafeRunnerForTest: boolean;
}): WatcherFaultProofSupervisor | UnsafeWatcherFaultProofSupervisorForTest => {
  if (!DEPLOYMENT_FINGERPRINT.test(input.deploymentFingerprint)) {
    throw new Error(
      "watcher fault-proof supervisor deployment fingerprint is invalid",
    );
  }
  if (
    !Number.isSafeInteger(input.deadlineAlertHeadroomMs) ||
    input.deadlineAlertHeadroomMs < 1 ||
    input.deadlineAlertHeadroomMs >
      MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs
  ) {
    throw new Error(
      "watcher fault-proof supervisor deadline alert headroom is invalid",
    );
  }
  if (input.queueAuthenticationKey.byteLength !== 32) {
    throw new Error(
      "watcher fault-proof supervisor queue authentication key is invalid",
    );
  }
  let openedQueueJournal: WatcherFaultProofQueueJournal | null = null;
  const queueJournal: Promise<WatcherFaultProofQueueJournal> =
    openWatcherFaultProofQueueJournal({
      journalRoot: input.journalRoot,
      deploymentFingerprint: input.deploymentFingerprint,
      authenticationKey: input.queueAuthenticationKey,
    }).then((journal) => {
      openedQueueJournal = journal;
      return journal;
    });
  const categories = Object.freeze([...input.dependencies.categories]);
  const progressAuthority = createWatcherFaultProofProgressAuthority({
    journalRoot: input.journalRoot,
    deploymentFingerprint: input.deploymentFingerprint,
    categories,
  });
  let progressSerial = Promise.resolve();
  let authorityEpoch = 0;
  if (
    categories.length !== WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length ||
    categories.some(
      (category, index) =>
        category !== WATCHER_INSTALLED_WORKFLOW_CATEGORIES[index],
    )
  ) {
    throw new Error(
      "watcher fault-proof supervisor categories differ from the installed application",
    );
  }
  let phase: WatcherFaultProofSupervisorStatus["phase"] = "accepting";
  let recovered = false;
  let recovery: Promise<number> | undefined;
  let queuedJobCount = 0;
  let activeJob: WatcherFaultProofJob | null = null;
  let activeInvocationPermit: WorkflowActuationPermit | null = null;
  let blockedJob: WatcherFaultProofJob | null = null;
  type PendingJob = Readonly<{
    job: WatcherFaultProofJob;
    jobIdentityDigest: string;
    queuedAtMs: string;
    actuationPermit: WorkflowActuationPermit | null;
    completion: Promise<unknown>;
    resolve(value: unknown): void;
    reject(error: Error): void;
  }>;
  const queue: PendingJob[] = [];
  let pump: Promise<void> = Promise.resolve();
  let pumping = false;
  const jobs = new Map<string, PendingJob>();
  // Authority generations are context for one objective, never another owner.
  const pendingUpdates = new Map<
    string,
    Readonly<{
      job: WatcherFaultProofJob;
      actuationPermit: WorkflowActuationPermit | null;
    }>
  >();
  const selectedExecutions = new Map<string, string>();
  const processedContexts = new Map<string, string>();
  const completedValidations = new Map<string, string>();
  const rememberCompletion = (key: string, validation: string): void => {
    completedValidations.delete(key);
    completedValidations.set(key, validation);
    if (completedValidations.size > MAX_RECOVERABLE_WORKFLOWS) {
      const oldest = completedValidations.keys().next().value!;
      completedValidations.delete(oldest);
      selectedExecutions.delete(oldest);
      processedContexts.delete(oldest);
    }
  };
  type Update = Readonly<{
    job: WatcherFaultProofJob;
    actuationPermit: WorkflowActuationPermit | null;
  }>;
  const transportRetries = new Map<
    string,
    {
      update: Update;
      attempts: number;
      timer: ReturnType<typeof setTimeout> | undefined;
    }
  >();
  const contextKey = (job: WatcherFaultProofJob) =>
    `${job.rollbackGeneration}:${job.observationRevision ?? job.decisionDigest}`;

  const objectiveKey = (job: WatcherFaultProofJob) =>
    `${job.category}\u0000${job.headerHash}`;
  let resolveDone!: () => void;
  let rejectDone!: (reason: Error) => void;
  const done = new Promise<void>((resolve, reject) => {
    resolveDone = resolve;
    rejectDone = reject;
  });
  // A production process observes `done`; this handler prevents a startup
  // validation failure from becoming an unhandled rejection before mounting.
  void done.catch(() => undefined);

  const block = (error: unknown, job: WatcherFaultProofJob | null): Error => {
    const normalized =
      error instanceof Error ? error : new Error(String(error));
    if (phase !== "blocked" && phase !== "closed") {
      phase = "blocked";
      blockedJob = job;
      rejectDone(normalized);
    }
    return normalized;
  };

  const now = (): number => {
    const value = input.nowMs();
    if (!Number.isSafeInteger(value) || value < 0) {
      throw new Error("watcher fault-proof supervisor clock is invalid");
    }
    return value;
  };

  const remainingSafeStartMs = (job: WatcherFaultProofJob): bigint =>
    BigInt(job.deadline!.latestSafeStartAtMs) - BigInt(now());

  const compareJobs = (
    left: WatcherFaultProofJob,
    right: WatcherFaultProofJob,
  ): number => {
    if (left.deadline === null || right.deadline === null)
      return left.deadline === right.deadline
        ? 0
        : left.deadline === null
          ? -1
          : 1;
    const leftDeadline = BigInt(left.deadline.latestSafeStartAtMs);
    const rightDeadline = BigInt(right.deadline.latestSafeStartAtMs);
    if (leftDeadline !== rightDeadline) {
      return leftDeadline < rightDeadline ? -1 : 1;
    }
    const leftMaturity = BigInt(left.deadline.maturityAtMs);
    const rightMaturity = BigInt(right.deadline.maturityAtMs);
    if (leftMaturity !== rightMaturity) {
      return leftMaturity < rightMaturity ? -1 : 1;
    }
    const leftCategory = categories.indexOf(left.category);
    const rightCategory = categories.indexOf(right.category);
    if (leftCategory !== rightCategory) return leftCategory - rightCategory;
    return left.headerHash.localeCompare(right.headerHash);
  };

  const comparePending = (left: PendingJob, right: PendingJob): number =>
    compareJobs(left.job, right.job);

  const runPending = async (entry: PendingJob): Promise<void> => {
    const { job, actuationPermit } = entry;
    let invocationJob = job;
    let invocationPermit = actuationPermit;
    if (phase === "blocked") {
      entry.reject(new Error("watcher fault-proof supervisor is blocked"));
      return;
    }
    try {
      await (
        await queueJournal
      ).markStarted(entry.jobIdentityDigest, now().toString());
      queuedJobCount -= 1;
    } catch (error) {
      entry.reject(block(error, job));
      return;
    }
    activeJob = job;
    activeInvocationPermit = actuationPermit;
    let outcome: unknown;
    let failure: Error | undefined;
    try {
      // The queue may have waited while another observation completed the
      // objective. Admission belongs immediately before funding and invocation.
      if (actuationPermit !== null) {
        const authority = assertWorkflowActuationPermitIdentity({
          permit: actuationPermit,
          category: job.category,
          rollbackGeneration: job.rollbackGeneration,
        });
        if (
          authority.deploymentFingerprint !== input.deploymentFingerprint ||
          authority.headerHash !== job.headerHash ||
          authority.decisionDigest !== job.decisionDigest
        )
          throw new Error(
            "proof objective changed its admitted execution authority",
          );
      }
      const key = objectiveKey(job);
      const execution = await readWatcherProofExecution({
        journalRoot: input.journalRoot,
        deploymentFingerprint: input.deploymentFingerprint,
        objective: job,
        selectedWorkflowId: selectedExecutions.get(key),
      });
      if (execution !== undefined) {
        selectedExecutions.set(key, execution.workflowId);
        if (!input.exposeUnsafeRunnerForTest)
          await progressAuthority.updateExecution({
            objective: job,
            execution,
          });
      }
      const completed = execution?.entries.find(
        ({ event }) => event.kind === "completed",
      );
      if (completed !== undefined && execution !== undefined) {
        const validationKey = `${job.rollbackGeneration}:${watcherSha256CanonicalJson(execution.entries)}`;
        const verification =
          completedValidations.get(key) === validationKey
            ? { kind: "applicable" as const }
            : await input.dependencies.verifyCompleted({
                job,
                execution,
                actuationPermit,
              });
        if (actuationPermit !== null)
          assertWorkflowActuationPermitIdentity({
            permit: actuationPermit,
            category: job.category,
            rollbackGeneration: job.rollbackGeneration,
          });
        if (verification.kind === "applicable") {
          rememberCompletion(key, validationKey);
          progressAuthority.markCompleted(job);
          outcome = { kind: "completed", terminal: completed.event };
        } else {
          completedValidations.delete(key);
          outcome =
            verification.kind === "retryable"
              ? verification
              : {
                  kind: "pending",
                  resume: "await_observation",
                  reason: verification.reason,
                };
        }
      } else {
        if (job.deadline !== null && remainingSafeStartMs(job) <= 0n) {
          if (
            input.exposeUnsafeRunnerForTest ||
            execution === undefined ||
            !execution.entries.some(
              ({ event }) => event.kind === "submission_intent",
            )
          )
            throw new Error(
              `watcher fault-proof deadline is unsafe for ${job.category}/${job.headerHash}`,
            );
          invocationPermit = await progressAuthority.reconcileExecution({
            objective: job,
            execution,
            rollbackGeneration: job.rollbackGeneration,
          });
          activeInvocationPermit = invocationPermit;
          const authority = assertWorkflowActuationPermitIdentity({
            permit: invocationPermit,
            category: job.category,
            rollbackGeneration: job.rollbackGeneration,
          });
          invocationJob = {
            ...job,
            mode: "resume",
            deadline: null,
            decisionDigest: authority.decisionDigest,
          };
        }
        const admission: WatcherFaultProofExecutionAdmission = {
          mode: execution === undefined ? "run" : "resume",
          funding:
            execution?.entries.some(
              ({ event }) => event.kind === "submission_intent",
            ) === true
              ? "resume_only"
              : "create_or_resume",
        };
        outcome = await input.dependencies.run({
          job: { ...invocationJob, mode: admission.mode },
          actuationPermit: invocationPermit,
          admission,
        });
        if (!input.exposeUnsafeRunnerForTest) {
          const updated = await readWatcherProofExecution({
            journalRoot: input.journalRoot,
            deploymentFingerprint: input.deploymentFingerprint,
            objective: job,
            selectedWorkflowId: selectedExecutions.get(key),
          });
          if (updated !== undefined) {
            selectedExecutions.set(key, updated.workflowId);
            await progressAuthority.updateExecution({
              objective: job,
              execution: updated,
            });
          }
          if (
            typeof outcome === "object" &&
            outcome !== null &&
            "kind" in outcome &&
            outcome.kind === "completed"
          ) {
            if (updated?.entries.at(-1)?.event.kind !== "completed")
              throw new Error(
                "proof runner reported completion without a completed journal",
              );
            if (actuationPermit !== null)
              assertWorkflowActuationPermitIdentity({
                permit: actuationPermit,
                category: job.category,
                rollbackGeneration: job.rollbackGeneration,
              });
            rememberCompletion(
              key,
              `${job.rollbackGeneration}:${watcherSha256CanonicalJson(updated.entries)}`,
            );
            progressAuthority.markCompleted(job);
          }
        }
      }
    } catch (error) {
      if (input.dependencies.isActuationRevokedError(error)) {
        if (
          error.decisionDigest !== invocationJob.decisionDigest ||
          error.rollbackGeneration !== invocationJob.rollbackGeneration
        ) {
          failure = block(
            new Error(
              "revoked actuation outcome differs from the scheduled workflow identity",
            ),
            job,
          );
        } else {
          outcome = Object.freeze({
            kind: "actuation_revoked" as const,
            decisionDigest: error.decisionDigest,
            rollbackGeneration: error.rollbackGeneration,
            checkpoint: error.checkpoint,
          });
        }
      } else {
        failure = block(error, job);
      }
    } finally {
      // A failed run blocks the supervisor and the process exits failed
      // closed. Leave its queue registration active so the next process
      // requeues the job instead of reading a durable finish that never
      // reached the workflow journal.
      if (failure === undefined) {
        try {
          await (
            await queueJournal
          ).markFinished(entry.jobIdentityDigest, now().toString());
        } catch (error) {
          failure = block(error, job);
        }
      }
    }
    // Completion releases the in-memory deduplication entry. Keep it owned
    // until the durable finish is committed so an unchanged fault cannot
    // restart a completed runner while its reservation has already closed.
    await serializeSchedule(async () => {
      activeJob = null;
      activeInvocationPermit = null;
      const key = objectiveKey(job);
      jobs.delete(key);
      const pending = pendingUpdates.get(key);
      pendingUpdates.delete(key);
      if (failure !== undefined) {
        entry.reject(failure);
        return;
      }
      processedContexts.set(key, contextKey(job));
      entry.resolve(outcome);
      if (
        typeof outcome === "object" &&
        outcome !== null &&
        "kind" in outcome &&
        outcome.kind === "retryable"
      ) {
        const previous = transportRetries.get(key);
        const retry = {
          update: pending ?? { job, actuationPermit },
          attempts: (previous?.attempts ?? 0) + 1,
          timer: undefined as ReturnType<typeof setTimeout> | undefined,
        };
        transportRetries.set(key, retry);
        if (phase === "accepting") {
          const delay = Math.min(
            30_000,
            1_000 * 2 ** Math.min(retry.attempts - 1, 5),
          );
          retry.timer = setTimeout(() => {
            void serializeSchedule(async () => {
              retry.timer = undefined;
              if (phase !== "accepting") return;
              const latest = retry.update;
              processedContexts.delete(key);
              await handover(latest);
            }).catch((error: unknown) => block(error, job));
          }, delay);
        }
      } else {
        const retry = transportRetries.get(key);
        if (retry?.timer !== undefined) clearTimeout(retry.timer);
        transportRetries.delete(key);
        if (
          pending !== undefined &&
          (phase === "accepting" || phase === "closing")
        )
          await handover(pending);
      }
    });
  };

  const ensurePump = (): void => {
    if (pumping) return;
    pumping = true;
    pump = Promise.resolve().then(async () => {
      try {
        while (queue.length > 0) {
          queue.sort(comparePending);
          const entry = queue.shift()!;
          await runPending(entry);
        }
      } catch (error) {
        block(error, activeJob);
      } finally {
        pumping = false;
      }
    });
  };

  const scheduleImpl = async (
    rawJob: WatcherFaultProofJob | UnsafeWatcherFaultProofJobForTest,
    actuationPermit: WorkflowActuationPermit | null,
  ): Promise<Readonly<{ completion: Promise<unknown> }>> => {
    if (phase !== "accepting" && phase !== "closing") {
      throw new Error(`watcher fault-proof supervisor is ${phase}`);
    }
    const job = validateJob(
      rawJob,
      categories,
      !input.exposeUnsafeRunnerForTest,
      actuationPermit,
    );
    const key = objectiveKey(job);
    const existing = jobs.get(key);
    if (existing !== undefined) {
      const latest = pendingUpdates.get(key)?.job ?? existing.job;
      if (BigInt(job.rollbackGeneration) < BigInt(latest.rollbackGeneration))
        return { completion: existing.completion };
      if (
        existing.job.deadline !== null &&
        job.deadline !== null &&
        JSON.stringify(existing.job.deadline) !== JSON.stringify(job.deadline)
      ) {
        throw new Error(
          "duplicate watcher fault-proof job changed its authenticated deadline",
        );
      }
      if (
        existing.job.decisionDigest !== job.decisionDigest ||
        existing.job.rollbackGeneration !== job.rollbackGeneration ||
        existing.job.observationRevision !== job.observationRevision
      )
        pendingUpdates.set(key, { job, actuationPermit });
      return Object.freeze({ completion: existing.completion });
    }
    if (processedContexts.get(key) === contextKey(job))
      return { completion: Promise.resolve(undefined) };
    const retry = transportRetries.get(key);
    if (retry?.timer !== undefined) {
      retry.update = { job, actuationPermit };
      return { completion: Promise.resolve(undefined) };
    }
    const identity = Object.freeze({
      category: job.category,
      headerHash: job.headerHash,
      decisionDigest: job.decisionDigest,
      rollbackGeneration: job.rollbackGeneration,
    });
    const jobIdentityDigest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint: input.deploymentFingerprint,
      identity,
    });
    // Queue completion records scheduling only. The execution journal is
    // admitted inside the worker, after it acquires ownership of this objective.
    const registration = await (
      await queueJournal
    ).register(identity, now().toString(), { reopenFinished: true });
    queuedJobCount += 1;
    let resolve!: (value: unknown) => void;
    let reject!: (error: Error) => void;
    const completion = new Promise<unknown>((resolvePromise, rejectPromise) => {
      resolve = resolvePromise;
      reject = rejectPromise;
    });
    const entry: PendingJob = Object.freeze({
      job,
      jobIdentityDigest,
      queuedAtMs: registration.queuedAtMs,
      actuationPermit,
      completion,
      resolve,
      reject,
    });
    jobs.set(key, entry);
    queue.push(entry);
    ensurePump();
    return Object.freeze({ completion });
  };

  const handover = async (update: Update): Promise<void> => {
    try {
      const scheduled = await scheduleImpl(update.job, update.actuationPermit);
      void scheduled.completion.catch(() => undefined);
    } catch (error) {
      if (!input.dependencies.isActuationRevokedError(error)) throw error;
      // A queued notification is not authority. Revocation while waiting is
      // expected; the next admitted canonical observation can wake this slot.
    }
  };

  let scheduleSerial = Promise.resolve();
  const serializeSchedule = <T>(operation: () => Promise<T>): Promise<T> => {
    const result = scheduleSerial.then(operation);
    scheduleSerial = result.then(
      () => undefined,
      () => undefined,
    );
    return result;
  };
  const schedule = (
    rawJob: WatcherFaultProofJob | UnsafeWatcherFaultProofJobForTest,
    actuationPermit: WorkflowActuationPermit | null,
  ): Promise<Readonly<{ completion: Promise<unknown> }>> => {
    const operation = serializeSchedule(async () => {
      if (phase !== "accepting")
        throw new Error(`watcher fault-proof supervisor is ${phase}`);
      return await scheduleImpl(rawJob, actuationPermit);
    });
    return operation;
  };

  const runOrResume = (
    rawJob: UnsafeWatcherFaultProofJobForTest,
  ): Promise<unknown> =>
    schedule(rawJob, null).then(({ completion }) => completion);

  const recoverExisting: UnsafeWatcherFaultProofSupervisorForTest["recoverExisting"] =
    async (
      decision,
      actuationPermit,
      deadline,
      rollbackGeneration = "0",
      reconciliations = [],
    ) => {
      if (recovery !== undefined) return await recovery;
      recovery = (async () => {
        try {
          const existing = await exactWorkflowDirectories(
            input.journalRoot,
            categories,
          );
          if (!CANONICAL_NATURAL.test(rollbackGeneration)) {
            throw new Error(
              "fault-proof recovery rollback generation is malformed",
            );
          }
          const authorized = (() => {
            if (decision === null) {
              const unsafeDeadline = Object.freeze({
                headerHash: "",
                headerEndTimeMs: "0",
                maturityAtMs: MIDGARD_RETENTION_WINDOW.maturityMs.toString(),
                latestSafeStartAtMs:
                  MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs.toString(),
              });
              return input.exposeUnsafeRunnerForTest
                ? existing.map((job) => ({
                    ...job,
                    decisionDigest: "00".repeat(32),
                    rollbackGeneration,
                    deadline: Object.freeze({
                      ...unsafeDeadline,
                      headerHash: job.headerHash,
                    }),
                  }))
                : [];
            }
            if (actuationPermit === undefined || deadline === undefined) {
              throw new Error(
                "fault-proof recovery omitted live actuation or deadline authority",
              );
            }
            const fault = requireRunnableHeaderFault(decision);
            if (
              fault.deploymentFingerprint !== input.deploymentFingerprint ||
              fault.launchScope.length !== categories.length ||
              fault.launchScope.some(
                (category, index) => category !== categories[index],
              )
            ) {
              throw new Error(
                "recovery decision differs from the installed application identity",
              );
            }
            return existing
              .filter(
                ({ category, headerHash }) =>
                  category === fault.category &&
                  headerHash === fault.headerHash,
              )
              .map((job) => ({
                ...job,
                decisionDigest: fault.decisionDigest,
                rollbackGeneration,
                deadline,
              }));
          })();
          for (const job of authorized) {
            const scheduled = await schedule(
              job,
              decision === null ? null : actuationPermit!,
            );
            void scheduled.completion.catch(() => undefined);
          }
          for (const reconciliation of reconciliations) {
            if (
              !existing.some(
                (job) =>
                  job.category === reconciliation.category &&
                  job.headerHash === reconciliation.headerHash,
              )
            )
              throw new Error(
                "reconciliation intake has no existing workflow directory",
              );
            const scheduled = await schedule(
              {
                mode: "resume",
                category: reconciliation.category,
                headerHash: reconciliation.headerHash,
                decisionDigest: reconciliation.decisionDigest,
                rollbackGeneration,
                deadline: null,
              },
              reconciliation.actuationPermit,
            );
            void scheduled.completion.catch(() => undefined);
          }
          recovered = true;
          return authorized.length + reconciliations.length;
        } catch (error) {
          throw block(error, null);
        }
      })();
      try {
        return await recovery;
      } finally {
        recovery = undefined;
      }
    };

  const supervisor: WatcherFaultProofSupervisor = {
    schemaVersion: WATCHER_FAULT_PROOF_SUPERVISOR_SCHEMA_VERSION,
    done,
    requestProgress: (request) => {
      if (phase !== "accepting")
        return Promise.reject(
          new Error(`watcher fault-proof supervisor is ${phase}`),
        );
      const epoch = authorityEpoch;
      const operation = progressSerial.then(async () => {
        if (phase === "blocked" || phase === "closed")
          throw new Error(`watcher fault-proof supervisor is ${phase}`);
        if (epoch !== authorityEpoch) return;
        try {
          const contexts = await progressAuthority.admit(request);
          if (epoch !== authorityEpoch) return;
          recovered = true;
          for (const context of contexts) {
            const { decision, actuationPermit, deadline, rollbackGeneration } =
              context;
            const scheduled = await serializeSchedule(() =>
              scheduleImpl(
                {
                  mode: deadline === null ? "resume" : "run",
                  category:
                    decision.category as WatcherInstalledWorkflowCategory,
                  headerHash: decision.headerHash,
                  decisionDigest: decision.decisionDigest,
                  rollbackGeneration,
                  deadline,
                  observationRevision: context.observationRevision,
                },
                actuationPermit,
              ),
            );
            void scheduled.completion.catch(() => undefined);
          }
        } catch (error) {
          if (input.dependencies.isActuationRevokedError(error)) return;
          throw block(error, null);
        }
      });
      progressSerial = operation.catch(() => undefined);
      return operation;
    },
    revokeAuthority: (reason) => {
      authorityEpoch += 1;
      if (activeInvocationPermit !== null)
        revokeWorkflowActuationPermit(activeInvocationPermit, reason);
      for (const { actuationPermit } of jobs.values())
        if (actuationPermit !== null)
          revokeWorkflowActuationPermit(actuationPermit, reason);
      for (const { actuationPermit } of pendingUpdates.values())
        if (actuationPermit !== null)
          revokeWorkflowActuationPermit(actuationPermit, reason);
      for (const { update } of transportRetries.values())
        if (update.actuationPermit !== null)
          revokeWorkflowActuationPermit(update.actuationPermit, reason);
      progressAuthority.revokeAuthority(reason);
      for (const key of completedValidations.keys())
        selectedExecutions.delete(key);
      completedValidations.clear();
      processedContexts.clear();
      for (const retry of transportRetries.values())
        if (retry.timer !== undefined) clearTimeout(retry.timer);
      transportRetries.clear();
    },
    status: () => {
      const earliestQueuedJob = queue.slice().sort(comparePending)[0]?.job;
      const earliestDeadlineJob = (() => {
        if (activeJob === null) return earliestQueuedJob ?? null;
        if (earliestQueuedJob === undefined) return activeJob;
        return compareJobs(activeJob, earliestQueuedJob) <= 0
          ? activeJob
          : earliestQueuedJob;
      })();
      const remaining =
        earliestDeadlineJob === null
          ? null
          : earliestDeadlineJob.deadline === null
            ? null
            : remainingSafeStartMs(earliestDeadlineJob);
      const deadlineHealth =
        phase === "blocked" || (remaining !== null && remaining <= 0n)
          ? ("unsafe" as const)
          : remaining !== null &&
              remaining <= BigInt(input.deadlineAlertHeadroomMs)
            ? ("at_risk" as const)
            : ("safe" as const);
      return Object.freeze({
        phase,
        recovered,
        unfinishedObjectiveCount: progressAuthority.unfinishedCount(),
        queuedJobCount,
        activeJob,
        blockedJob,
        deadlineHealth,
        earliestDeadlineJob,
        remainingSafeStartMs: remaining?.toString() ?? null,
      });
    },
    durableQueueStatus: () => {
      if (openedQueueJournal === null) {
        throw new Error("fault-proof durable queue recovery is incomplete");
      }
      return openedQueueJournal.status();
    },
    close: async () => {
      if (phase === "closed") return;
      if (phase === "accepting") phase = "closing";
      for (const retry of transportRetries.values())
        if (retry.timer !== undefined) clearTimeout(retry.timer);
      await progressSerial;
      await scheduleSerial;
      await pump;
      if (phase !== "blocked") {
        phase = "closed";
        resolveDone();
      }
    },
  };
  const exposed = input.exposeUnsafeRunnerForTest
    ? Object.freeze({
        ...supervisor,
        recoverExisting,
        unsafeRunOrResumeForTest: runOrResume,
        unsafeScheduleForTest: async (
          job: UnsafeWatcherFaultProofJobForTest,
        ) => {
          await schedule(job, null);
        },
      })
    : Object.freeze(supervisor);
  return exposed;
};

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
    deadlineAlertHeadroomMs: input.deadlineAlertHeadroomMs ?? 3_600_000,
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
