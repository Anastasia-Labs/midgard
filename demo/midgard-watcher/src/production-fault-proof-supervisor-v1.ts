import { mkdir, readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_RETENTION_WINDOW_V1 } from "@al-ft/midgard-core";
import {
  isProductionWorkflowActuationRevokedErrorV1,
  type ProductionHeaderDecisionV1,
  type ProductionWorkflowActuationPermitV1,
  type ProductionWorkflowActuationRevokedErrorV1,
  requireRunnableProductionHeaderFaultV1,
} from "@al-ft/midgard-fault-proofs";
import { HeaderV1 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
  type WatcherInstalledProductionWorkflowCategoryV1,
} from "./production-fault-proof-application-v1.js";
import {
  openWatcherProductionFaultProofQueueJournalV1,
  watcherProductionFaultProofQueueIdentityDigestV1,
  type WatcherProductionFaultProofQueueJournalV1,
} from "./production-fault-proof-queue-journal-v1.js";
import {
  assertWatcherProductionStateQueueHeaderObservationV1,
  type WatcherProductionStateQueueHeaderObservationV1,
} from "./production-state-queue-observation-v1.js";

export const WATCHER_PRODUCTION_FAULT_PROOF_SUPERVISOR_V1_SCHEMA_VERSION =
  "midgard-watcher-production-fault-proof-supervisor-v1" as const;

const HEADER_HASH = /^[0-9a-f]{56}$/u;
const DEPLOYMENT_FINGERPRINT = /^[0-9a-f]{64}$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAX_RECOVERABLE_WORKFLOWS = 2_048;

export type WatcherProductionFaultProofDeadlineV1 = Readonly<{
  headerHash: string;
  headerEndTimeMs: string;
  maturityAtMs: string;
  latestSafeStartAtMs: string;
}>;

const admittedDeadlinesV1 = new WeakSet<object>();

/**
 * Derives W04/W34 timing authority from an authenticated HeaderV1. The
 * complete correction path owns the canonical half-maturity budget, so the
 * latest safe start is exactly maturity minus that bound.
 */
export const watcherProductionFaultProofDeadlineV1 = (
  header: WatcherProductionStateQueueHeaderObservationV1,
): WatcherProductionFaultProofDeadlineV1 => {
  assertWatcherProductionStateQueueHeaderObservationV1(header);
  const decoded = Data.from(header.headerCborHex, HeaderV1);
  if (Data.to(decoded, HeaderV1) !== header.headerCborHex) {
    throw new Error("fault-proof deadline HeaderV1 CBOR is noncanonical");
  }
  const headerEndTimeMs = decoded.endTime;
  const maturityAtMs =
    headerEndTimeMs + BigInt(MIDGARD_RETENTION_WINDOW_V1.maturityMs);
  const latestSafeStartAtMs =
    maturityAtMs -
    BigInt(MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs);
  const deadline = Object.freeze({
    headerHash: header.headerHash,
    headerEndTimeMs: headerEndTimeMs.toString(),
    maturityAtMs: maturityAtMs.toString(),
    latestSafeStartAtMs: latestSafeStartAtMs.toString(),
  });
  admittedDeadlinesV1.add(deadline);
  return deadline;
};

export type WatcherProductionFaultProofJobV1 = Readonly<{
  mode: "run" | "resume";
  category: WatcherInstalledProductionWorkflowCategoryV1;
  headerHash: string;
  decisionDigest: string;
  rollbackGeneration: string;
  deadline: WatcherProductionFaultProofDeadlineV1;
}>;

type UnsafeWatcherProductionFaultProofJobForTestV1 = Omit<
  WatcherProductionFaultProofJobV1,
  "deadline"
> &
  Readonly<{ deadline?: WatcherProductionFaultProofDeadlineV1 }>;

export type WatcherProductionFaultProofSupervisorStatusV1 = Readonly<{
  phase: "accepting" | "blocked" | "closing" | "closed";
  recovered: boolean;
  queuedJobCount: number;
  activeJob: WatcherProductionFaultProofJobV1 | null;
  blockedJob: WatcherProductionFaultProofJobV1 | null;
  deadlineHealth: "safe" | "at_risk" | "unsafe";
  earliestDeadlineJob: WatcherProductionFaultProofJobV1 | null;
  remainingSafeStartMs: string | null;
}>;

export type WatcherProductionFaultProofSupervisorV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_FAULT_PROOF_SUPERVISOR_V1_SCHEMA_VERSION;
  done: Promise<void>;
  recoverExisting(
    decision: ProductionHeaderDecisionV1 | null,
    actuationPermit?: ProductionWorkflowActuationPermitV1,
    deadline?: WatcherProductionFaultProofDeadlineV1,
    rollbackGeneration?: string,
  ): Promise<number>;
  status(): WatcherProductionFaultProofSupervisorStatusV1;
  durableQueueStatus(): Readonly<{
    queuedJobCount: number;
    oldestQueuedAtMs: string | null;
  }>;
  close(): Promise<void>;
}>;

export type UnsafeWatcherProductionFaultProofSupervisorForTestV1 =
  WatcherProductionFaultProofSupervisorV1 &
    Readonly<{
      unsafeRunOrResumeForTest(
        job: UnsafeWatcherProductionFaultProofJobForTestV1,
      ): Promise<unknown>;
      unsafeScheduleForTest(
        job: UnsafeWatcherProductionFaultProofJobForTestV1,
      ): Promise<void>;
    }>;

type SupervisorDependenciesV1 = Readonly<{
  categories: readonly WatcherInstalledProductionWorkflowCategoryV1[];
  run(
    input: Readonly<{
      job: WatcherProductionFaultProofJobV1;
      actuationPermit: ProductionWorkflowActuationPermitV1 | null;
    }>,
  ): Promise<unknown>;
  isActuationRevokedError(
    error: unknown,
  ): error is ProductionWorkflowActuationRevokedErrorV1;
}>;

const admittedDecisionEnqueueBySupervisorV1 = new WeakMap<
  object,
  (
    decision: ProductionHeaderDecisionV1,
    actuationPermit: ProductionWorkflowActuationPermitV1,
    deadline: WatcherProductionFaultProofDeadlineV1,
    rollbackGeneration: string,
  ) => Promise<void>
>();

const exactWorkflowDirectories = async (
  journalRoot: string,
  categories: readonly WatcherInstalledProductionWorkflowCategoryV1[],
): Promise<
  readonly Readonly<{
    mode: "resume";
    category: WatcherInstalledProductionWorkflowCategoryV1;
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
    category: WatcherInstalledProductionWorkflowCategoryV1;
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
  job:
    | WatcherProductionFaultProofJobV1
    | UnsafeWatcherProductionFaultProofJobForTestV1,
  categories: readonly WatcherInstalledProductionWorkflowCategoryV1[],
  requireAdmittedDeadline: boolean,
): WatcherProductionFaultProofJobV1 => {
  const deadline =
    job.deadline ??
    Object.freeze({
      headerHash: job.headerHash,
      headerEndTimeMs: "0",
      maturityAtMs: MIDGARD_RETENTION_WINDOW_V1.maturityMs.toString(),
      latestSafeStartAtMs:
        MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs.toString(),
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
    (requireAdmittedDeadline && !admittedDeadlinesV1.has(deadline))
  ) {
    throw new Error("watcher fault-proof supervisor job is invalid");
  }
  const headerEndTimeMs = BigInt(deadline.headerEndTimeMs);
  const maturityAtMs = BigInt(deadline.maturityAtMs);
  const latestSafeStartAtMs = BigInt(deadline.latestSafeStartAtMs);
  if (
    maturityAtMs !==
      headerEndTimeMs + BigInt(MIDGARD_RETENTION_WINDOW_V1.maturityMs) ||
    latestSafeStartAtMs !==
      maturityAtMs -
        BigInt(MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs)
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
  });
};

const createSupervisor = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  readonly nowMs: () => number;
  readonly dependencies: SupervisorDependenciesV1;
  readonly exposeUnsafeRunnerForTest: boolean;
}):
  | WatcherProductionFaultProofSupervisorV1
  | UnsafeWatcherProductionFaultProofSupervisorForTestV1 => {
  if (!DEPLOYMENT_FINGERPRINT.test(input.deploymentFingerprint)) {
    throw new Error(
      "watcher fault-proof supervisor deployment fingerprint is invalid",
    );
  }
  if (
    !Number.isSafeInteger(input.deadlineAlertHeadroomMs) ||
    input.deadlineAlertHeadroomMs < 1 ||
    input.deadlineAlertHeadroomMs >
      MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs
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
  let openedQueueJournal: WatcherProductionFaultProofQueueJournalV1 | null =
    null;
  const queueJournal: Promise<WatcherProductionFaultProofQueueJournalV1> =
    openWatcherProductionFaultProofQueueJournalV1({
      journalRoot: input.journalRoot,
      deploymentFingerprint: input.deploymentFingerprint,
      authenticationKey: input.queueAuthenticationKey,
    }).then((journal) => {
      openedQueueJournal = journal;
      return journal;
    });
  const categories = Object.freeze([...input.dependencies.categories]);
  if (
    categories.length !==
      WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1.length ||
    categories.some(
      (category, index) =>
        category !== WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1[index],
    )
  ) {
    throw new Error(
      "watcher fault-proof supervisor categories differ from the installed application",
    );
  }
  let phase: WatcherProductionFaultProofSupervisorStatusV1["phase"] =
    "accepting";
  let recovered = false;
  let recovery: Promise<number> | undefined;
  let queuedJobCount = 0;
  let activeJob: WatcherProductionFaultProofJobV1 | null = null;
  let blockedJob: WatcherProductionFaultProofJobV1 | null = null;
  type PendingJobV1 = Readonly<{
    job: WatcherProductionFaultProofJobV1;
    jobIdentityDigest: string;
    queuedAtMs: string;
    actuationPermit: ProductionWorkflowActuationPermitV1 | null;
    completion: Promise<unknown>;
    resolve(value: unknown): void;
    reject(error: Error): void;
  }>;
  const queue: PendingJobV1[] = [];
  let pump: Promise<void> = Promise.resolve();
  let pumping = false;
  const jobs = new Map<string, PendingJobV1>();
  const seenTargets = new Set<string>();
  let resolveDone!: () => void;
  let rejectDone!: (reason: Error) => void;
  const done = new Promise<void>((resolve, reject) => {
    resolveDone = resolve;
    rejectDone = reject;
  });
  // A production process observes `done`; this handler prevents a startup
  // validation failure from becoming an unhandled rejection before mounting.
  void done.catch(() => undefined);

  const block = (
    error: unknown,
    job: WatcherProductionFaultProofJobV1 | null,
  ): Error => {
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

  const remainingSafeStartMs = (
    job: WatcherProductionFaultProofJobV1,
  ): bigint => BigInt(job.deadline.latestSafeStartAtMs) - BigInt(now());

  const compareJobs = (
    left: WatcherProductionFaultProofJobV1,
    right: WatcherProductionFaultProofJobV1,
  ): number => {
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

  const comparePending = (left: PendingJobV1, right: PendingJobV1): number =>
    compareJobs(left.job, right.job);

  const runPending = async (entry: PendingJobV1): Promise<void> => {
    const { job, actuationPermit } = entry;
    if (phase === "blocked") {
      entry.reject(new Error("watcher fault-proof supervisor is blocked"));
      return;
    }
    if (remainingSafeStartMs(job) <= 0n) {
      entry.reject(
        block(
          new Error(
            `watcher fault-proof deadline is unsafe for ${job.category}/${job.headerHash}`,
          ),
          job,
        ),
      );
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
    try {
      entry.resolve(await input.dependencies.run({ job, actuationPermit }));
    } catch (error) {
      if (input.dependencies.isActuationRevokedError(error)) {
        if (
          error.decisionDigest !== job.decisionDigest ||
          error.rollbackGeneration !== job.rollbackGeneration
        ) {
          entry.reject(
            block(
              new Error(
                "revoked actuation outcome differs from the scheduled workflow identity",
              ),
              job,
            ),
          );
        } else {
          entry.resolve(
            Object.freeze({
              kind: "actuation_revoked" as const,
              decisionDigest: error.decisionDigest,
              rollbackGeneration: error.rollbackGeneration,
              checkpoint: error.checkpoint,
            }),
          );
        }
      } else {
        entry.reject(block(error, job));
      }
    } finally {
      activeJob = null;
      try {
        await (
          await queueJournal
        ).markFinished(entry.jobIdentityDigest, now().toString());
      } catch (error) {
        block(error, job);
      }
    }
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
      } finally {
        pumping = false;
      }
    });
  };

  const scheduleImpl = async (
    rawJob:
      | WatcherProductionFaultProofJobV1
      | UnsafeWatcherProductionFaultProofJobForTestV1,
    actuationPermit: ProductionWorkflowActuationPermitV1 | null,
  ): Promise<Readonly<{ completion: Promise<unknown> }>> => {
    if (phase !== "accepting") {
      throw new Error(`watcher fault-proof supervisor is ${phase}`);
    }
    const job = validateJob(
      rawJob,
      categories,
      !input.exposeUnsafeRunnerForTest,
    );
    const key = `${job.category}\u0000${job.headerHash}\u0000${job.decisionDigest}\u0000${job.rollbackGeneration}`;
    const existing = jobs.get(key);
    if (existing !== undefined) {
      if (
        existing.job.deadline.headerEndTimeMs !==
          job.deadline.headerEndTimeMs ||
        existing.job.deadline.headerHash !== job.deadline.headerHash ||
        existing.job.deadline.maturityAtMs !== job.deadline.maturityAtMs ||
        existing.job.deadline.latestSafeStartAtMs !==
          job.deadline.latestSafeStartAtMs
      ) {
        throw new Error(
          "duplicate watcher fault-proof job changed its authenticated deadline",
        );
      }
      return Object.freeze({ completion: existing.completion });
    }
    if (remainingSafeStartMs(job) <= 0n) {
      throw block(
        new Error(
          `watcher fault-proof deadline is unsafe for ${job.category}/${job.headerHash}`,
        ),
        job,
      );
    }
    const identity = Object.freeze({
      category: job.category,
      headerHash: job.headerHash,
      decisionDigest: job.decisionDigest,
      rollbackGeneration: job.rollbackGeneration,
    });
    const jobIdentityDigest = watcherProductionFaultProofQueueIdentityDigestV1({
      deploymentFingerprint: input.deploymentFingerprint,
      identity,
    });
    const registration = await (
      await queueJournal
    ).register(identity, now().toString());
    if (registration.finished) {
      return Object.freeze({ completion: Promise.resolve(undefined) });
    }
    seenTargets.add(`${job.category}\u0000${job.headerHash}`);
    queuedJobCount += 1;
    let resolve!: (value: unknown) => void;
    let reject!: (error: Error) => void;
    const completion = new Promise<unknown>((resolvePromise, rejectPromise) => {
      resolve = resolvePromise;
      reject = rejectPromise;
    });
    const entry: PendingJobV1 = Object.freeze({
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
    void completion.finally(() => jobs.delete(key)).catch(() => undefined);
    ensurePump();
    return Object.freeze({ completion });
  };

  let scheduleSerial = Promise.resolve();
  const schedule = (
    rawJob:
      | WatcherProductionFaultProofJobV1
      | UnsafeWatcherProductionFaultProofJobForTestV1,
    actuationPermit: ProductionWorkflowActuationPermitV1 | null,
  ): Promise<Readonly<{ completion: Promise<unknown> }>> => {
    const operation = scheduleSerial.then(
      async () => await scheduleImpl(rawJob, actuationPermit),
    );
    scheduleSerial = operation.then(
      () => undefined,
      () => undefined,
    );
    return operation;
  };

  const runOrResume = (
    rawJob: UnsafeWatcherProductionFaultProofJobForTestV1,
  ): Promise<unknown> =>
    schedule(rawJob, null).then(({ completion }) => completion);

  const supervisor: WatcherProductionFaultProofSupervisorV1 = {
    schemaVersion: WATCHER_PRODUCTION_FAULT_PROOF_SUPERVISOR_V1_SCHEMA_VERSION,
    done,
    recoverExisting: async (
      decision,
      actuationPermit,
      deadline,
      rollbackGeneration = "0",
    ) => {
      if (recovery !== undefined) return await recovery;
      recovery = (async () => {
        try {
          const existing = await exactWorkflowDirectories(
            input.journalRoot,
            categories,
          );
          for (const job of existing) {
            seenTargets.add(`${job.category}\u0000${job.headerHash}`);
          }
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
                maturityAtMs: MIDGARD_RETENTION_WINDOW_V1.maturityMs.toString(),
                latestSafeStartAtMs:
                  MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs.toString(),
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
            const fault = requireRunnableProductionHeaderFaultV1(decision);
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
          recovered = true;
          return authorized.length;
        } catch (error) {
          throw block(error, null);
        }
      })();
      return await recovery;
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
        unsafeRunOrResumeForTest: runOrResume,
        unsafeScheduleForTest: async (
          job: UnsafeWatcherProductionFaultProofJobForTestV1,
        ) => {
          await schedule(job, null);
        },
      })
    : Object.freeze(supervisor);
  admittedDecisionEnqueueBySupervisorV1.set(
    exposed,
    async (decision, actuationPermit, deadline, rollbackGeneration) => {
      if (!recovered) {
        throw new Error(
          "watcher fault-proof supervisor has not completed journal recovery",
        );
      }
      if (!CANONICAL_NATURAL.test(rollbackGeneration)) {
        throw new Error("fault-proof enqueue rollback generation is malformed");
      }
      const fault = requireRunnableProductionHeaderFaultV1(decision);
      if (
        fault.deploymentFingerprint !== input.deploymentFingerprint ||
        fault.launchScope.length !== categories.length ||
        fault.launchScope.some(
          (category, index) => category !== categories[index],
        )
      ) {
        throw new Error(
          "runnable fault decision differs from the installed application identity",
        );
      }
      const targetKey = `${fault.category}\u0000${fault.headerHash}`;
      await schedule(
        {
          mode: seenTargets.has(targetKey) ? "resume" : "run",
          category:
            fault.category as WatcherInstalledProductionWorkflowCategoryV1,
          headerHash: fault.headerHash,
          decisionDigest: fault.decisionDigest,
          rollbackGeneration,
          deadline,
        },
        actuationPermit,
      );
    },
  );
  return exposed;
};

/**
 * The only production intake path for a newly classified fault. Category and
 * target are derived from the fault-proofs module's live opaque admission;
 * persisted envelopes and caller-authored jobs cannot pass this boundary.
 */
export const enqueueWatcherProductionFaultDecisionV1 = async (input: {
  readonly supervisor: WatcherProductionFaultProofSupervisorV1;
  readonly decision: ProductionHeaderDecisionV1;
  readonly actuationPermit: ProductionWorkflowActuationPermitV1;
  readonly deadline: WatcherProductionFaultProofDeadlineV1;
  readonly rollbackGeneration: string;
}): Promise<void> => {
  const enqueue = admittedDecisionEnqueueBySupervisorV1.get(input.supervisor);
  if (enqueue === undefined) {
    throw new Error("watcher fault-proof supervisor is not module-admitted");
  }
  await enqueue(
    input.decision,
    input.actuationPermit,
    input.deadline,
    input.rollbackGeneration,
  );
};

export const createWatcherProductionFaultProofSupervisorV1 = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  readonly run: (
    input: Readonly<{
      job: WatcherProductionFaultProofJobV1;
      actuationPermit: ProductionWorkflowActuationPermitV1;
    }>,
  ) => Promise<unknown>;
}): WatcherProductionFaultProofSupervisorV1 =>
  createSupervisor({
    journalRoot: input.journalRoot,
    deploymentFingerprint: input.deploymentFingerprint,
    deadlineAlertHeadroomMs: input.deadlineAlertHeadroomMs,
    queueAuthenticationKey: input.queueAuthenticationKey,
    nowMs: Date.now,
    exposeUnsafeRunnerForTest: false,
    dependencies: Object.freeze({
      categories: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
      run: async ({ job, actuationPermit }) => {
        if (actuationPermit === null) {
          throw new Error(
            "production fault-proof runner omitted actuation authority",
          );
        }
        return await input.run({ job, actuationPermit });
      },
      isActuationRevokedError: isProductionWorkflowActuationRevokedErrorV1,
    }),
  }) as WatcherProductionFaultProofSupervisorV1;

/** Test-only dependency seam; production category admission cannot be changed. */
export const unsafeCreateWatcherProductionFaultProofSupervisorForTestV1 =
  (input: {
    readonly journalRoot: string;
    readonly deploymentFingerprint: string;
    readonly deadlineAlertHeadroomMs?: number;
    readonly unsafeNowMsForTest?: () => number;
    readonly unsafeQueueAuthenticationKeyForTest?: Uint8Array;
    readonly run: (job: WatcherProductionFaultProofJobV1) => Promise<unknown>;
    readonly unsafeIsActuationRevokedErrorForTest?: (
      error: unknown,
    ) => error is ProductionWorkflowActuationRevokedErrorV1;
  }): UnsafeWatcherProductionFaultProofSupervisorForTestV1 =>
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
        categories: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
        run: async ({ job }) => await input.run(job),
        isActuationRevokedError:
          input.unsafeIsActuationRevokedErrorForTest ??
          isProductionWorkflowActuationRevokedErrorV1,
      }),
    }) as UnsafeWatcherProductionFaultProofSupervisorForTestV1;
