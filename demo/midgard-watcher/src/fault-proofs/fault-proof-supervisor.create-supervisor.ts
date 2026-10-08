import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  assertWorkflowActuationPermitIdentity,
  requireRunnableHeaderFault,
  revokeWorkflowActuationPermit,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherInstalledWorkflowCategory,
} from "./fault-proof-application.js";
import type { WatcherFaultProofExecutionAdmission } from "./fault-proof-execution.js";
import { readWatcherProofExecution } from "./fault-proof-objective-journal.js";
import { watcherJournalCapacityReached } from "./fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "./fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
  type WatcherFaultProofQueueJournal,
} from "./fault-proof-queue-journal.js";
import { admitWatcherProofRunnerCompletion } from "./fault-proof-supervisor.admit-runner-completion.js";
import {
  CANONICAL_NATURAL,
  DEPLOYMENT_FINGERPRINT,
  MAX_RECOVERABLE_WORKFLOWS,
  recordedWorkflowObjectives,
  type SupervisorDependencies,
  type UnsafeWatcherFaultProofJobForTest,
  type UnsafeWatcherFaultProofSupervisorForTest,
  validateJob,
  WATCHER_FAULT_PROOF_SUPERVISOR_SCHEMA_VERSION,
  type WatcherFaultProofJob,
  type WatcherFaultProofSupervisor,
  type WatcherFaultProofSupervisorStatus,
  type WatcherJournalBusyRequeue,
} from "./fault-proof-supervisor.validate-job.js";
import {
  isWatcherProofDecisionMissingError,
  type WatcherDecisionHold,
  WatcherProofDecisionMissingError,
} from "./watcher-decision-hold.js";
import {
  createWatcherJournalBusyHold,
  isWatcherSqliteBusyError,
} from "./watcher-journal-busy.js";
import {
  isWatcherJournalCapacityError,
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
  openWatcherJournalDatabase,
  watcherJournalIntegrityFailure,
  watcherJournalUnavailable,
} from "./watcher-journal-database.js";
import { watcherJournalOpener } from "./watcher-journal-database.opener.js";

export const createSupervisor = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  readonly nowMs: () => number;
  readonly dependencies: SupervisorDependencies;
  readonly exposeUnsafeRunnerForTest: boolean;
  /** Holds each open objective's L1 history past k (E1 ruling). */
  readonly proofRetention?: WatcherProofRetention;
  /** Funding reservations held because their recorded decision is missing. */
  readonly reservationDecisionHolds?: () => readonly WatcherDecisionHold[];
  /** What a fresh process does around requeueing work after journal_busy. */
  readonly journalBusyRequeue?: WatcherJournalBusyRequeue;
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
  // An open that could not complete (journal_unavailable) is retried by
  // every use and in the background; an integrity failure latches.
  let openedQueueJournal: WatcherFaultProofQueueJournal | null = null;
  const openQueue = () =>
    watcherJournalOpener(
      async () =>
        (openedQueueJournal = await openWatcherFaultProofQueueJournal({
          journalRoot: input.journalRoot,
          deploymentFingerprint: input.deploymentFingerprint,
          authenticationKey: input.queueAuthenticationKey,
        })),
      { retryInBackground: true },
    );
  let queueOpener = openQueue();
  const queueJournal = () => queueOpener.open();
  const categories = Object.freeze([...input.dependencies.categories]);
  const journals = () =>
    openWatcherJournalDatabase({
      journalRoot: input.journalRoot,
      authenticationKey: input.queueAuthenticationKey,
    });
  const createAuthority = () =>
    createWatcherFaultProofProgressAuthority({
      journalRoot: input.journalRoot,
      deploymentFingerprint: input.deploymentFingerprint,
      categories,
      authenticationKey: input.queueAuthenticationKey,
      ...(input.proofRetention === undefined
        ? {}
        : { retention: input.proofRetention }),
      ...(input.dependencies.objectiveSettled === undefined
        ? {}
        : { onObjectiveSettled: input.dependencies.objectiveSettled }),
    });
  let progressAuthority = createAuthority();
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
  void queueJournal().catch(() => undefined); // each use reports the refusal
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

  // A refused journal (`journal_integrity`), one that could not be opened
  // (`journal_unavailable`), work whose recorded decision is missing
  // (`journal_decision_missing`) or a busy database (`journal_busy`) holds
  // the watcher unready and never fails the process, as does an objective
  // past its latest safe start (`fault_proof_start_deadline_passed`); every
  // other failure blocks it.
  const block = (error: unknown, job: WatcherFaultProofJob | null): Error => {
    const normalized =
      error instanceof Error ? error : new Error(String(error));
    if (
      isWatcherJournalIntegrityError(error) ||
      isWatcherJournalUnavailableError(error) ||
      isWatcherProofDecisionMissingError(error)
    )
      return normalized;
    if (isWatcherSqliteBusyError(error)) {
      if (phase === "accepting") busy.hold(normalized);
      return normalized;
    }
    if (phase !== "blocked" && phase !== "closed") {
      phase = "blocked";
      blockedJob = job;
      rejectDone(normalized);
    }
    return normalized;
  };
  // The statement that met a busy database committed nothing. Rebuild the
  // work from durable state as a fresh process does: drop every in-memory
  // entry, open the queue and the progress authority again, run the startup
  // funding sweep while no job runs, then let the decision driver dispatch.
  const busy = createWatcherJournalBusyHold({
    requeue: async () => {
      const reset = progressSerial.then(async () => {
        await pump;
        await serializeSchedule(async () => {
          authorityEpoch += 1;
          for (const entry of queue.splice(0))
            entry.reject(
              new Error("fault-proof job requeued after journal_busy"),
            );
          for (const retry of transportRetries.values())
            if (retry.timer !== undefined) clearTimeout(retry.timer);
          transportRetries.clear();
          jobs.clear();
          pendingUpdates.clear();
          selectedExecutions.clear();
          processedContexts.clear();
          completedValidations.clear();
          queuedJobCount = 0;
          activeJob = null;
          activeInvocationPermit = null;
          recovered = false;
          queueOpener.close();
          openedQueueJournal = null;
          queueOpener = openQueue();
          void queueJournal().catch(() => undefined);
          progressAuthority = createAuthority();
        });
      });
      progressSerial = reset.catch(() => undefined);
      await reset;
      await input.journalBusyRequeue
        ?.releaseUnusedFunding()
        .catch((error: unknown) => {
          if (isWatcherSqliteBusyError(error)) throw error;
          // As at startup, a refused or unopened journal keeps them reserved.
          if (
            !isWatcherJournalIntegrityError(error) &&
            !isWatcherJournalUnavailableError(error)
          )
            block(error, null);
        });
    },
    resumed: () => input.journalBusyRequeue?.wake(),
  });

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
    // Deadline work first; deadline-free work keeps arrival order behind it.
    if (left.deadline === null || right.deadline === null)
      return left.deadline === right.deadline ? 0 : left.deadline ? -1 : 1;
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
        await queueJournal()
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
      const completed = execution?.entries.find(
        ({ event }) => event.kind === "completed",
      );
      // A completion marked beyond rollback recovery is final: a job queued
      // before the mark, at a later generation, neither holds its released
      // L1 history again nor verifies it again.
      const marker =
        completed !== undefined && execution !== undefined
          ? progressAuthority.completionMarker(job, execution)
          : null;
      if (execution !== undefined) {
        selectedExecutions.set(key, execution.workflowId);
        if (!input.exposeUnsafeRunnerForTest && marker === null)
          await progressAuthority.updateExecution({
            objective: job,
            execution,
          });
      }
      if (completed !== undefined && execution !== undefined) {
        const validationKey = `${job.rollbackGeneration}:${watcherSha256CanonicalJson(execution.entries)}`;
        const verification =
          marker !== null
            ? ({
                kind: "applicable",
                confirmationDepth: marker.confirmationDepth,
              } as const)
            : completedValidations.get(key) === validationKey
              ? ({ kind: "applicable", confirmationDepth: 0 } as const)
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
          await progressAuthority.markCompleted(job, {
            execution,
            confirmationDepth: verification.confirmationDepth,
          });
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
          // Past its latest safe start only signed work is reconciled. The
          // deadline is fixed by the header, so an objective with no signed
          // attempt can never start: it is held by name, never run again,
          // and clears once its header leaves the finalized queue.
          if (
            execution === undefined ||
            !execution.entries.some(
              ({ event }) => event.kind === "submission_intent",
            )
          )
            throw new WatcherProofDecisionMissingError({
              kind: "objective",
              category: job.category,
              headerHash: job.headerHash,
              decisionDigest: job.decisionDigest,
              detail: `${job.category}/${job.headerHash}`,
              readiness: "fault_proof_start_deadline_passed",
            });
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
            outcome = await admitWatcherProofRunnerCompletion({
              job,
              execution: updated,
              actuationPermit,
              verifyCompleted: input.dependencies.verifyCompleted,
              outcome,
              onApplicable: async (verified) => {
                rememberCompletion(
                  key,
                  `${job.rollbackGeneration}:${watcherSha256CanonicalJson(verified.execution.entries)}`,
                );
                await progressAuthority.markCompleted(job, verified);
              },
            });
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
      } else if (isWatcherProofDecisionMissingError(error)) {
        // Held until its header leaves the finalized queue; never run again.
        await progressAuthority.holdObjective(error.hold);
        outcome = Object.freeze({
          kind: "pending" as const,
          resume: "await_observation" as const,
          reason: error.message,
        });
      } else {
        failure = block(error, job);
      }
    } finally {
      // A failed run blocks the supervisor and the process exits failed
      // closed, or, on a busy database, holds it until the in-process
      // requeue. Leave its queue registration active so the next process,
      // or that requeue, requeues the job instead of reading a durable
      // finish that never reached the workflow journal.
      if (failure === undefined) {
        try {
          await (
            await queueJournal()
          ).markFinished(entry.jobIdentityDigest, now().toString());
          busy.committed();
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
        while (queue.length > 0 && busy.reason() === null) {
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
    // Held work is requeued from durable state once the hold clears.
    if (busy.reason() !== null)
      return { completion: Promise.resolve(undefined) };
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
    // At its cap of open objectives the journal refuses a new one: status
    // reports journal_capacity and a later observation retries.
    const registration = await (await queueJournal())
      .register(identity, now().toString())
      .catch((error: unknown) => {
        if (isWatcherJournalCapacityError(error)) return null;
        throw error;
      });
    if (registration === null)
      return { completion: Promise.resolve(undefined) };
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
          const existing = recordedWorkflowObjectives(journals(), categories);
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
                "reconciliation intake has no recorded proof objective",
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
        if (epoch !== authorityEpoch || busy.reason() !== null) return;
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
          const failure = block(error, null);
          if (busy.reason() !== null && isWatcherSqliteBusyError(error)) return;
          throw failure;
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
      // Capacity is read first: a read that meets corruption latches it, and
      // this same status reports journal_integrity.
      let journalCapacity = false;
      try {
        journalCapacity =
          openedQueueJournal !== null &&
          watcherJournalIntegrityFailure(input.journalRoot) === null &&
          watcherJournalCapacityReached(journals());
      } catch (error) {
        if (!isWatcherJournalIntegrityError(error)) throw error;
      }
      const journalIntegrity = watcherJournalIntegrityFailure(
        input.journalRoot,
      );
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
        journalIntegrity,
        journalUnavailable:
          journalIntegrity === null
            ? watcherJournalUnavailable(input.journalRoot)
            : null,
        journalCapacity: journalIntegrity === null && journalCapacity,
        journalDecisionMissing: Object.freeze([
          ...progressAuthority.decisionHolds(),
          ...(input.reservationDecisionHolds?.() ?? []),
        ]),
        journalBusy: busy.reason(),
      });
    },
    durableQueueStatus: () => {
      if (openedQueueJournal === null) {
        // A journal failure answers as itself, never as a bad request:
        // this throws the latched integrity failure or the failed open.
        journals();
        throw new Error("fault-proof durable queue recovery is incomplete");
      }
      return openedQueueJournal.status();
    },
    close: async () => {
      if (phase === "closed") return;
      if (phase === "accepting") phase = "closing";
      queueOpener.close();
      busy.close();
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
