import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  assertWorkflowActuationPermitIdentity,
  revokeWorkflowActuationPermit,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { requireRunnableHeaderFault } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import type { WorkflowInFlight } from "@al-ft/midgard-fault-proofs/workflow/in-flight";
import { followReleasedInFlight } from "@al-ft/midgard-fault-proofs/workflow/in-flight-released";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
} from "@al-ft/midgard-fault-proofs/workflow/journal";
import { isSqliteBusyError } from "@al-ft/midgard-fault-proofs/workflow/sqlite-busy";

import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "./fault-proof-application.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.production-dependencies.js";
import { isWatcherFaultProofContinuationStall } from "./fault-proof-continuation-stall.js";
import type { WatcherFaultProofExecutionAdmission } from "./fault-proof-execution.js";
import {
  isBeyondWatcherRollbackRecovery,
  watcherJournalCapacityReached,
  type WatcherProofVerifiedCompletion,
} from "./fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "./fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
  type WatcherFaultProofQueueJournal,
} from "./fault-proof-queue-journal.js";
import {
  admitWatcherProofRunnerCompletion,
  runnerOutcomeKind,
} from "./fault-proof-supervisor.admit-runner-completion.js";
import { watcherFaultProofJobOrder } from "./fault-proof-supervisor.compare-jobs.js";
import { createWatcherProofMaterialsSweeps } from "./fault-proof-supervisor.proof-materials-sweep.js";
import {
  createWatcherOpenProofDeadlines,
  watcherProofStartDeadlinePassed,
  watcherProofStartTiming,
} from "./fault-proof-supervisor.safe-start.js";
import {
  CANONICAL_NATURAL,
  DEPLOYMENT_FINGERPRINT,
  MAX_RECOVERABLE_WORKFLOWS,
  recordedWorkflowObjectives,
  supervisorClock,
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
  createWalletOnlySend,
  type WatcherL1Clock,
} from "./wallet-only-send-open.js";
import { isWatcherProofDecisionMissingError } from "./watcher-decision-hold.js";
import { createWatcherJournalBusyHold } from "./watcher-journal-busy.js";
import {
  isWatcherJournalCapacityError,
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
  openWatcherJournalDatabase,
  watcherJournalIntegrityFailure,
  watcherJournalUnavailable,
} from "./watcher-journal-database.js";
import { watcherJournalOpener } from "./watcher-journal-database.opener.js";

/** Completion checks that found our removal on the chain, short of the
 * depth that makes it final. */
const LANDED_AWAITING_FINALITY = new Set<string | undefined>([
  "completion_not_final",
  "release_finality",
]);

/** Completion checks that found no removal of ours on the chain: the target
 * still stands, or another record ended it. */
const NOT_LANDED = new Set<string | undefined>([
  "target_live",
  "terminal_not_ours",
]);

/** The completion the chain verified for `job`, bound to the removal it
 * shows and that removal's depth. */
const verifiedCompletion = (
  deploymentFingerprint: string,
  job: Pick<WatcherFaultProofJob, "category" | "headerHash" | "decisionDigest">,
  verification: Readonly<{ terminalTxHash: string; confirmationDepth: number }>,
): WatcherProofVerifiedCompletion => ({
  workflowId: computeFraudProofWorkflowId({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category: job.category,
    target: { kind: "state_queue_header", headerHash: job.headerHash },
    decisionDigest: job.decisionDigest,
  }),
  terminalTxHash: verification.terminalTxHash,
  confirmationDepth: verification.confirmationDepth,
});

type Objective = Readonly<{ category: string; headerHash: string }>;

export const createSupervisor = (input: {
  readonly journalRoot: string;
  /** What this process sent and has not seen settle, held in memory only:
   * whether an objective's run continues in-flight work reads it. */
  readonly inFlight: Pick<
    WorkflowInFlight,
    "sending" | "forget" | "forObjective" | "objectives"
  >;
  readonly deploymentFingerprint: string;
  readonly deadlineAlertHeadroomMs: number;
  readonly queueAuthenticationKey: Uint8Array;
  readonly nowMs: () => number;
  /** The L1 chain the restart wait reads. */
  readonly l1Clock: WatcherL1Clock;
  readonly dependencies: SupervisorDependencies;
  readonly exposeUnsafeRunnerForTest: boolean;
  /** Holds each open objective's L1 history past k (E1 ruling). */
  readonly proofRetention?: WatcherProofRetention;
  /** What a fresh process does after requeueing work after journal_busy. */
  readonly journalBusyRequeue?: WatcherJournalBusyRequeue;
  /** A bounded pass over our own threads, run by the pump once the jobs an
   * admitted observation queued are drained; its failure is its own. */
  readonly ownThreadSweep?: () => Promise<void>;
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
  // every use, a status read included, and in the background only while
  // transient (`isTransientJournalFailure`); an integrity failure latches.
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
  const openDeadlines = createWatcherOpenProofDeadlines();
  const sweeps = createWatcherProofMaterialsSweeps({
    sweep: input.dependencies.sweepProofMaterials,
    inFlight: input.inFlight,
    walletOnlySend: () => walletOnlySend,
    sweepable: input.dependencies.proofMaterialsSweepable,
    isRevoked: input.dependencies.isActuationRevokedError,
  });
  const jobInProcess = (objective: Objective): boolean =>
    [jobs, registering, transportRetries].some((held) =>
      held.has(objectiveKey(objective)),
    );
  const createAuthority = () =>
    createWatcherFaultProofProgressAuthority({
      journalRoot: input.journalRoot,
      deploymentFingerprint: input.deploymentFingerprint,
      categories,
      authenticationKey: input.queueAuthenticationKey,
      retention: input.proofRetention,
      sending: (objective) => input.inFlight.sending(objective),
      forgetInFlight: (objective) => input.inFlight.forget(objective),
      // Its header left the queue: whether our removal landed is the chain's
      // to say (the completion check of `observeAtMaturity`).
      departure: async ({ decision, actuationPermit, rollbackGeneration }) => {
        const job = {
          mode: "resume",
          category: decision.category as WatcherInstalledWorkflowCategory,
          headerHash: decision.headerHash,
          decisionDigest: decision.decisionDigest,
          rollbackGeneration,
          deadline: null,
        } as const;
        const verification = await verifyCompleted({ job, actuationPermit });
        if (verification.kind === "applicable") {
          const { terminal } = verification;
          if (terminal !== undefined)
            sweeps.verified({ job, actuationPermit, terminal });
          return {
            kind: "final",
            verified: verifiedCompletion(
              input.deploymentFingerprint,
              job,
              verification,
            ),
          };
        }
        if (verification.kind === "retryable") return { kind: "unknown" };
        if (LANDED_AWAITING_FINALITY.has(verification.reason))
          return { kind: "landed" };
        return {
          kind: NOT_LANDED.has(verification.reason) ? "absent" : "unknown",
        };
      },
      onDepartureStep: (decision, rollbackGeneration, step) => {
        if (step === "completed") sweeps.completed(decision);
        input.dependencies.departureStep?.({
          category: decision.category as WatcherInstalledWorkflowCategory,
          headerHash: decision.headerHash,
          decisionDigest: decision.decisionDigest,
          rollbackGeneration,
          step,
        });
      },
      onObjectiveSettled: (objective) => {
        openDeadlines.settled(objective, progressAuthority.decisionHolds());
        dropRetry(objectiveKey(objective)); // nothing drives it again
        input.dependencies.objectiveSettled?.(objective);
      },
      proofMaterials: input.dependencies.proofMaterials,
      jobInProcess,
    });
  let progressAuthority = createAuthority();
  let progressSerial: Promise<unknown> = Promise.resolve();
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
  // Objectives whose job is being registered, a handover included.
  const registering = new Set<string>();
  // Authority generations are context for one objective, never another owner.
  type Update = Readonly<{
    job: WatcherFaultProofJob;
    actuationPermit: WorkflowActuationPermit | null;
  }>;
  const pendingUpdates = new Map<string, Update>();
  // The context each objective last ran in, for the most recent objectives.
  const processedContexts = new Map<string, string>();
  const rememberContext = (key: string, context: string): void => {
    processedContexts.delete(key);
    processedContexts.set(key, context);
    if (processedContexts.size > MAX_RECOVERABLE_WORKFLOWS)
      processedContexts.delete(processedContexts.keys().next().value!);
  };
  // Objectives whose session stalled: each yields its place to other queued
  // objectives for the next pick, then competes by deadline again.
  const yielded = new Set<string>();
  // One finality threshold (plan section 9: depth > k is final): a completion
  // applies only beyond rollback recovery, the depth its marker needs; one
  // verified at exactly k waits a block rather than finish unmarked with its
  // L1 history pinned. Without a k nothing is marked.
  const securityParameter = input.proofRetention?.securityParameter;
  const verifyCompleted: SupervisorDependencies["verifyCompleted"] = async (
    request,
  ) => {
    const verification = await input.dependencies.verifyCompleted(request);
    return verification.kind === "applicable" &&
      securityParameter !== undefined &&
      !isBeyondWatcherRollbackRecovery(
        verification.confirmationDepth,
        securityParameter,
      )
      ? Object.freeze({ kind: "pending", reason: "completion_not_final" })
      : verification;
  };
  // At or past maturity with nothing in flight, as after a restart, no
  // removal of ours can land any more, but one may have landed already: a
  // landed removal completes the objective (or waits for finality), and
  // anything else holds it by name until its header leaves the queue.
  const observeAtMaturity = async (
    job: WatcherFaultProofJob,
    actuationPermit: WorkflowActuationPermit | null,
  ): Promise<unknown> => {
    const verification = await verifyCompleted({ job, actuationPermit });
    if (actuationPermit !== null)
      assertWorkflowActuationPermitIdentity({
        permit: actuationPermit,
        category: job.category,
        rollbackGeneration: job.rollbackGeneration,
      });
    if (verification.kind === "applicable") {
      await progressAuthority.markCompleted(
        job,
        verifiedCompletion(input.deploymentFingerprint, job, verification),
      );
      return Object.freeze({
        kind: "completed" as const,
        terminalTxHash: verification.terminalTxHash,
      });
    }
    if (verification.kind === "retryable") return verification;
    if (LANDED_AWAITING_FINALITY.has(verification.reason))
      return Object.freeze({
        kind: "pending" as const,
        resume: "await_observation" as const,
        reason: verification.reason,
      });
    throw watcherProofStartDeadlinePassed(job);
  };
  const transportRetries = new Map<
    string,
    {
      update: Update;
      attempts: number;
      timer: ReturnType<typeof setTimeout> | undefined;
    }
  >();
  const dropRetry = (key: string): void => {
    clearTimeout(transportRetries.get(key)?.timer);
    transportRetries.delete(key);
  };
  const contextKey = (job: WatcherFaultProofJob) =>
    `${job.rollbackGeneration}:${job.observationRevision ?? job.decisionDigest}`;

  const objectiveKey = ({ category, headerHash }: Objective) =>
    `${category}\u0000${headerHash}`;
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
  // (`journal_unavailable`) or a busy database (`journal_busy`) holds the
  // watcher unready and never fails the process, as does an objective past
  // its maturity with nothing landed (`fault_proof_start_deadline_passed`);
  // every other failure blocks it.
  const block = (error: unknown, job: WatcherFaultProofJob | null): Error => {
    const normalized =
      error instanceof Error ? error : new Error(String(error));
    if (
      isWatcherJournalIntegrityError(error) ||
      isWatcherJournalUnavailableError(error) ||
      isWatcherProofDecisionMissingError(error)
    )
      return normalized;
    if (isSqliteBusyError(error)) {
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
  // The statement that met a busy database committed nothing. Rebuild from
  // durable state as a fresh process does: drop in-memory entries, reopen the
  // queue and authority, sweep funding while no job runs, then dispatch.
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
          for (const key of transportRetries.keys()) dropRetry(key);
          jobs.clear();
          pendingUpdates.clear();
          processedContexts.clear();
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
    },
    resumed: () => input.journalBusyRequeue?.wake(),
  });

  const now = supervisorClock(input.nowMs);
  // Built once: no requeue or later admission moves the instant it opens at.
  // A run skips each wallet-only action it holds and makes the rest; a run
  // left with only those ends in the wait's named state, and the next
  // observation runs the objective again.
  const walletOnlySend = createWalletOnlySend({ ...input.l1Clock, nowMs: now });
  const remainingSafeStartMs = (job: WatcherFaultProofJob): bigint =>
    BigInt(job.deadline!.latestSafeStartAtMs) - BigInt(now());

  const compareJobs = watcherFaultProofJobOrder(categories);

  const comparePending = (left: PendingJob, right: PendingJob): number =>
    Number(yielded.has(objectiveKey(left.job))) -
      Number(yielded.has(objectiveKey(right.job))) ||
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
    openDeadlines.track(job);
    let outcome: unknown;
    let failure: Error | undefined;
    try {
      // The queue may have waited while another observation completed the
      // objective. Admission belongs immediately before invocation.
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
      // A completion marked beyond rollback recovery is final: a job queued
      // before the mark, at a later generation, neither holds its released
      // L1 history again nor verifies it again.
      const marker = progressAuthority.completionMarker(job);
      // Past the latest safe start work runs late until maturity; at
      // maturity only what is in flight is continued, and with nothing in
      // flight the chain is asked whether the removal landed
      // (fault-proof-supervisor.safe-start.ts).
      const sending = input.inFlight.sending(job);
      const timing =
        marker === null
          ? watcherProofStartTiming(job, sending, BigInt(now()))
          : undefined;
      if (marker !== null) {
        outcome = {
          kind: "completed",
          terminalTxHash:
            "terminalTxHash" in marker ? marker.terminalTxHash : null,
        };
      } else if (timing === "observe") {
        outcome = await observeAtMaturity(job, actuationPermit);
      } else {
        if (timing === "reconcile") {
          invocationPermit = await progressAuthority.reconcileExecution({
            objective: job,
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
          // A run continues what this process has in flight, or starts
          // from what the chain shows.
          mode: sending || invocationJob.deadline === null ? "resume" : "run",
          ...(timing === "on_time" ? {} : { startedLate: job.deadline! }),
          walletOnlySend,
        };
        outcome = await input.dependencies.run({
          job: { ...invocationJob, mode: admission.mode },
          actuationPermit: invocationPermit,
          admission,
        });
        if (!input.exposeUnsafeRunnerForTest) {
          const kind = runnerOutcomeKind(outcome);
          // Not our conviction: no completion marker, only the release.
          if (kind === "resolved_by_another_record")
            await progressAuthority.markResolved(job);
          if (kind === "completed")
            outcome = await admitWatcherProofRunnerCompletion({
              job,
              actuationPermit,
              verifyCompleted,
              outcome,
              onApplicable: async (verified) =>
                await progressAuthority.markCompleted(job, verified),
            });
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
      // A failed run blocks the supervisor (exit failed closed), or, on a
      // busy database, holds it until the in-process requeue. Its queue
      // registration stays active so the next process, or that requeue,
      // requeues the job rather than read a finish never journaled.
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
    // restart a completed runner before its finish is recorded.
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
      rememberContext(key, contextKey(job));
      openDeadlines.finished(job, runnerOutcomeKind(outcome));
      if (isWatcherFaultProofContinuationStall(outcome)) yielded.add(key);
      entry.resolve(outcome);
      if (runnerOutcomeKind(outcome) === "retryable") {
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
              if (phase !== "accepting" || transportRetries.get(key) !== retry)
                return;
              const latest = retry.update;
              processedContexts.delete(key);
              await handover(latest);
            }).catch((error: unknown) => block(error, job));
          }, delay);
        }
      } else {
        dropRetry(key);
        if (
          pending !== undefined &&
          (phase === "accepting" || phase === "closing")
        )
          await handover(pending);
      }
    });
  };

  // Set by each admitted observation; the pump runs one sweep for any number.
  let sweepDue = false;
  // Set by each admitted pass at tip: the in-flight parts of objectives no
  // run drives are followed, and a proof-materials sweep due runs again.
  let tipDue = false;
  const passAtTip = async (): Promise<void> => {
    const observe = input.dependencies.observeSignedTransaction;
    if (observe !== undefined)
      await followReleasedInFlight({
        inFlight: input.inFlight,
        driven: (objective) =>
          progressAuthority.drives(objective) ||
          jobInProcess(objective) ||
          sweeps.sweeping(objective),
        observeSignedTransaction: observe,
      });
    await sweeps.atTip();
  };
  const ensurePump = (): void => {
    if (pumping) return;
    pumping = true;
    pump = Promise.resolve().then(async () => {
      try {
        for (;;) {
          while (queue.length > 0 && busy.reason() === null) {
            queue.sort(comparePending);
            const entry = queue.shift()!;
            yielded.clear();
            await runPending(entry);
          }
          if (
            !(sweepDue || tipDue) ||
            queue.length > 0 ||
            busy.reason() !== null
          )
            break;
          const atTip = tipDue;
          sweepDue = tipDue = false;
          if (phase === "accepting")
            await input.ownThreadSweep?.().catch(() => undefined);
          if (atTip && phase === "accepting")
            await passAtTip().catch(() => undefined);
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
    if (phase !== "accepting" && phase !== "closing")
      throw new Error(`watcher fault-proof supervisor is ${phase}`);
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
    // Queue completion records scheduling only; the run is admitted inside
    // the worker, after it acquires ownership of this objective. At its cap of open objectives the journal refuses a new one: status
    // reports journal_capacity and a later observation retries.
    registering.add(key);
    const registration = await (async () => {
      try {
        return await (
          await queueJournal()
        ).register(identity, now().toString());
      } catch (error) {
        registering.delete(key);
        if (isWatcherJournalCapacityError(error)) return null;
        throw error;
      }
    })();
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
    registering.delete(key);
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
  ): Promise<Readonly<{ completion: Promise<unknown> }>> =>
    serializeSchedule(async () => {
      if (phase !== "accepting")
        throw new Error(`watcher fault-proof supervisor is ${phase}`);
      return await scheduleImpl(rawJob, actuationPermit);
    });

  const runOrResume = (
    rawJob: UnsafeWatcherFaultProofJobForTest,
  ): Promise<unknown> =>
    schedule(rawJob, null).then(({ completion }) => completion);

  const recoverExisting: UnsafeWatcherFaultProofSupervisorForTest["recoverExisting"] =
    async (decision, actuationPermit, deadline, rollbackGeneration = "0") => {
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
                headerBlockTimeMs: "0",
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
          recovered = true;
          return authorized.length;
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
          sweepDue ||= input.ownThreadSweep !== undefined;
          tipDue ||=
            (await input.proofRetention?.passAtTip(
              request.observation.nativePoint,
            )) === true;
          ensurePump();
        } catch (error) {
          if (input.dependencies.isActuationRevokedError(error)) return;
          const failure = block(error, null);
          if (busy.reason() !== null && isSqliteBusyError(error)) return;
          throw failure;
        }
      });
      progressSerial = operation.catch(() => undefined);
      return operation;
    },
    supersedeDecision: (request) => {
      const operation = progressSerial.then(async () => {
        if (phase === "blocked" || phase === "closed")
          throw new Error(`watcher fault-proof supervisor is ${phase}`);
        return await progressAuthority.supersede(request);
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
      processedContexts.clear();
      for (const key of transportRetries.keys()) dropRetry(key);
    },
    status: () => {
      // Held and waiting objectives count with queued and active jobs.
      const earliestDeadlineJob =
        [
          ...(activeJob === null ? [] : [activeJob]),
          ...queue.map((entry) => entry.job),
          ...openDeadlines.jobs(),
        ].sort(compareJobs)[0] ?? null;
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
        ]),
        objectiveCleanupFailures: progressAuthority.cleanupFailures(),
        journalBusy: busy.reason(),
      });
    },
    durableQueueStatus: () => {
      if (openedQueueJournal === null) {
        // A read is a use and opens the queue journal again. A journal
        // failure answers as itself, never as a bad request: this throws
        // the latched integrity failure or the failed open.
        void queueJournal().catch(() => undefined);
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
      await sweeps.idle();
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
