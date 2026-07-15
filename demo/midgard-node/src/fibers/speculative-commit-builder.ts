import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect, Metric, Option, Queue, Ref, Runtime } from "effect";
import { Worker } from "worker_threads";

import {
  ForeignTipReconciliationsDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { reachPipelinedCommitCrashCheckpoint } from "@/e2e/pipelined-commit-crash-checkpoint.js";
import {
  promoteOrRecoverNativeMpf,
  publishCommitMempoolLedgerMutation,
} from "@/fibers/block-commitment.js";
import {
  publishFinalizedDaPayloadBestEffort,
  runAfterL1ControlPlaneRelease,
} from "@/fibers/da-publication-trigger.js";
import { resolveWorkerEntry } from "@/fibers/resolve-worker-entry.js";
import {
  barrierWatermarksAreFresh,
  reduceSpeculativeCommitState,
  shouldRetrySpeculativeConfirmationWake,
  speculationOverlapEfficiency,
  type SpeculativeCandidateSummary,
  type SpeculativeCommitState,
  type SpeculativeInvalidationReason,
} from "@/fibers/speculative-commit-state.js";
import { makeAwaitedWorkerTerminator } from "@/fibers/worker-lifecycle.js";
import {
  type CommitSubmitWake,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  type NodeConfigDep,
  withL1ControlPlane,
} from "@/services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  refreshStateQueueGlobalsFromSnapshot,
} from "@/services/state-queue-topology.js";
import { stateQueueBaseHeaderHash } from "@/workers/commit-block-header/state-queue.js";
import {
  deserializeStateQueueUTxO,
  type SpeculativeCommitWorkerInstruction,
  type WorkerInput,
  type WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { WorkerError } from "@/workers/utils/common.js";

const speculativeBuildDuration = Metric.timer(
  "speculative_build_duration_ms",
  "Memory-only speculative candidate build duration",
);
const speculationHitCounter = Metric.counter("speculation_hit_total", {
  description: "Speculative candidates submitted without rebuilding",
});
const speculationInvalidationCounter = Metric.counter(
  "speculation_invalidations_total",
  { description: "Speculative candidates invalidated by T1-T7 reason" },
);
const speculationOverlapGauge = Metric.gauge("speculation_overlap_efficiency", {
  description: "Fraction of confirmation wait overlapped by candidate build",
});
const submitAfterConfirmTimer = Metric.timer(
  "submit_after_confirm_ms",
  "Confirmation wake to speculative candidate submission completion",
);
const commitCadenceTimer = Metric.timer(
  "commit_cadence_ms",
  "Time between consecutive block submissions",
);
const l1ConfirmationWaitTimer = Metric.timer(
  "l1_confirmation_wait_ms",
  "Previous block submission to confirmation observation latency",
);
const speculativeCommitBlockNumTxGauge = Metric.gauge(
  "commit_block_num_tx_count",
  {
    description:
      "Current number of L2 transactions in the submitted commit block",
    bigint: true,
  },
);
const speculativeCommitBlockCounter = Metric.counter("commit_block_count", {
  description: "Number of submitted commit blocks",
  bigint: true,
  incremental: true,
});
const speculativeCommitBlockTxCounter = Metric.counter(
  "commit_block_tx_count",
  {
    description: "Number of L2 transactions in submitted commit blocks",
    bigint: true,
    incremental: true,
  },
);

type ActiveSpeculativeWorkerSession = {
  readonly generation: number;
  readonly worker: SpeculativeCommitWorkerPort;
  readonly candidate: SpeculativeCandidateSummary;
  readonly finalOutput: Promise<WorkerOutput>;
  readonly terminate: () => Promise<number>;
  localFinalizationRecovery?: Extract<
    WorkerOutput,
    { readonly type: "SuccessfulLocalFinalizationRecoveryOutput" }
  >;
};

export type SpeculativeCommitWorkerPort = {
  on(event: "message", listener: (output: WorkerOutput) => void): void;
  on(event: "error", listener: (error: Error) => void): void;
  on(event: "exit", listener: (code: number) => void): void;
  postMessage(instruction: SpeculativeCommitWorkerInstruction): void;
  terminate(): Promise<number>;
};

type BuildingSpeculativeWorkerSession = {
  readonly generation: number;
  readonly worker: SpeculativeCommitWorkerPort;
  cancelled: boolean;
  readonly terminate: () => Promise<number>;
};

type FinishedSpeculativeWorkerSession = {
  readonly output: WorkerOutput;
  readonly candidate: SpeculativeCandidateSummary;
  readonly localFinalizationRecovery?: ActiveSpeculativeWorkerSession["localFinalizationRecovery"];
};

type SubmitSpeculativeCandidateInstruction = Extract<
  SpeculativeCommitWorkerInstruction,
  { readonly type: "SubmitSpeculativeCandidate" }
>;

const authenticateForeignTipEvidence = (
  liveTail: SubmitSpeculativeCandidateInstruction["confirmedBlock"],
) =>
  Effect.gen(function* () {
    const tail = yield* deserializeStateQueueUTxO(liveTail);
    const headerHash = yield* stateQueueBaseHeaderHash(tail);
    if (headerHash === undefined) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Foreign T2 tip has no committed header hash",
          cause: "missing_header_hash",
        }),
      );
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(tail.datum);
    const recomputedHeaderHash = yield* SDK.hashBlockHeader(header);
    if (recomputedHeaderHash !== headerHash) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Foreign T2 tip header evidence is not self-consistent",
          cause: `tip=${headerHash},recomputed=${recomputedHeaderHash}`,
        }),
      );
    }
    return { headerHash, header } as const;
  });

/**
 * Authenticates the canonical tip carried across the confirmation boundary and
 * persists its immutable T2 evidence when it replaced the candidate base.
 * Returns false only when the supplied tip is still the expected base.
 */
export const persistAuthenticatedForeignTipMismatch = ({
  expectedHeaderHash,
  liveTail,
  assertedForeignHeaderHash,
}: {
  readonly expectedHeaderHash: string;
  readonly liveTail: SubmitSpeculativeCandidateInstruction["confirmedBlock"];
  readonly assertedForeignHeaderHash?: string;
}) =>
  Effect.gen(function* () {
    const evidence = yield* authenticateForeignTipEvidence(liveTail);
    if (
      assertedForeignHeaderHash !== undefined &&
      evidence.headerHash !== assertedForeignHeaderHash
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message:
            "Confirmation wake does not match its canonical tip evidence",
          cause: `wake=${assertedForeignHeaderHash},tip=${evidence.headerHash}`,
        }),
      );
    }
    if (evidence.headerHash === expectedHeaderHash) return false;
    yield* ForeignTipReconciliationsDB.recordMismatch({
      foreignHeaderHash: evidence.headerHash,
      replacedBaseHeaderHash: expectedHeaderHash,
      foreignHeader: evidence.header,
    });
    return true;
  });

export const recordForeignTipMismatchBeforeInvalidation = <E, R>({
  expectedHeaderHash,
  confirmedHeaderHash,
  confirmedTip,
  invalidateCandidate,
}: {
  readonly expectedHeaderHash: string;
  readonly confirmedHeaderHash: string;
  readonly confirmedTip: SubmitSpeculativeCandidateInstruction["confirmedBlock"];
  readonly invalidateCandidate: Effect.Effect<void, E, R>;
}) =>
  Effect.gen(function* () {
    const recorded = yield* persistAuthenticatedForeignTipMismatch({
      expectedHeaderHash,
      liveTail: confirmedTip,
      assertedForeignHeaderHash: confirmedHeaderHash,
    });
    if (!recorded) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message:
            "Confirmation mismatch branch received the unchanged candidate base",
          cause: expectedHeaderHash,
        }),
      );
    }
    yield* invalidateCandidate;
  });

/**
 * Production pre-resume decision seam. A foreign/reorged live tail must be
 * rejected before the parked speculative MPFs are handed back to the worker.
 */
export const decideSpeculativeInstructionForLiveTip = ({
  expectedHeaderHash,
  liveTail,
  submitInstruction,
}: {
  readonly expectedHeaderHash: string;
  readonly liveTail: SubmitSpeculativeCandidateInstruction["confirmedBlock"];
  readonly submitInstruction: SubmitSpeculativeCandidateInstruction;
}) =>
  Effect.gen(function* () {
    const mismatchRecorded = yield* persistAuthenticatedForeignTipMismatch({
      expectedHeaderHash,
      liveTail,
    });
    if (!mismatchRecorded) return submitInstruction;
    return {
      type: "InvalidateSpeculativeCandidate",
      reason: "T2",
    } as const;
  });

let activeSession: ActiveSpeculativeWorkerSession | undefined;
let buildingSession: BuildingSpeculativeWorkerSession | undefined;
let nextWorkerGeneration = 0;
let lastSubmittedAtMs = 0;

const terminateWorkerSession = (
  session: BuildingSpeculativeWorkerSession | ActiveSpeculativeWorkerSession,
): Promise<number> => session.terminate();

const clearWorkerSessionGeneration = (generation: number): void => {
  if (buildingSession?.generation === generation) buildingSession = undefined;
  if (activeSession?.generation === generation) activeSession = undefined;
};

const terminateAndClearWorkerSession = (
  session: BuildingSpeculativeWorkerSession | ActiveSpeculativeWorkerSession,
): Effect.Effect<void, WorkerError> =>
  Effect.tryPromise({
    try: () => terminateWorkerSession(session),
    catch: (cause) =>
      new WorkerError({
        worker: "speculative-commit-builder",
        message:
          "Speculative worker termination was not confirmed; the session remains blocked",
        cause,
      }),
  }).pipe(
    Effect.tap(() =>
      Effect.sync(() => clearWorkerSessionGeneration(session.generation)),
    ),
    Effect.asVoid,
  );

export const hasActiveSpeculativeCommitSession = (): boolean =>
  activeSession !== undefined || buildingSession !== undefined;

const spawnSpeculativeSessionWithWorker = (
  createWorker: () => SpeculativeCommitWorkerPort,
  afterTermination: () => Promise<void> = () => Promise.resolve(),
): Effect.Effect<SpeculativeCandidateSummary, WorkerError> =>
  Effect.async((resume) => {
    if (activeSession !== undefined || buildingSession !== undefined) {
      resume(
        Effect.fail(
          new WorkerError({
            worker: "speculative-commit-builder",
            message: "A speculative commit worker session is already active",
            cause: activeSession?.candidate.candidateId ?? "candidate_building",
          }),
        ),
      );
      return Effect.void;
    }
    const worker = createWorker();
    const session: BuildingSpeculativeWorkerSession = {
      generation: ++nextWorkerGeneration,
      worker,
      cancelled: false,
      terminate: makeAwaitedWorkerTerminator(worker, afterTermination),
    };
    buildingSession = session;
    let ready = false;
    let finalSettled = false;
    let resolveFinal!: (output: WorkerOutput) => void;
    let rejectFinal!: (error: unknown) => void;
    const finalOutput = new Promise<WorkerOutput>((resolve, reject) => {
      resolveFinal = resolve;
      rejectFinal = reject;
    });
    // The completion is intentionally awaited later, after confirmation. Keep
    // Node from treating an early worker failure as an unhandled rejection.
    void finalOutput.catch(() => undefined);
    const fail = (cause: unknown): void => {
      if (finalSettled) return;
      finalSettled = true;
      const error =
        cause instanceof WorkerError
          ? cause
          : new WorkerError({
              worker: "speculative-commit-builder",
              message: "Speculative commit worker session failed",
              cause,
            });
      const settleFailure = (settledError: WorkerError): void => {
        if (!ready) resume(Effect.fail(settledError));
        rejectFinal(settledError);
      };
      // Do not expose a failed session as finished until its worker has
      // stopped and the parent-owned logical MPF lease cleanup has run.
      void terminateWorkerSession(session).then(
        () => {
          clearWorkerSessionGeneration(session.generation);
          settleFailure(error);
        },
        (terminationCause) =>
          settleFailure(
            new WorkerError({
              worker: "speculative-commit-builder",
              message:
                "Speculative commit worker session failed and termination cleanup did not complete",
              cause: { workerFailure: error, terminationCause },
            }),
          ),
      );
    };
    worker.on("message", (output: WorkerOutput) => {
      if (finalSettled) return;
      if (output.type === "SpeculativeCandidateReadyOutput") {
        if (ready) {
          fail(new Error("speculative worker emitted candidate-ready twice"));
          return;
        }
        if (session.cancelled || buildingSession !== session) {
          fail(
            new Error(
              `stale speculative worker generation ${session.generation.toString()} emitted candidate-ready after invalidation`,
            ),
          );
          return;
        }
        ready = true;
        buildingSession = undefined;
        activeSession = {
          generation: session.generation,
          worker,
          candidate: output.candidate,
          finalOutput,
          terminate: session.terminate,
        };
        resume(Effect.succeed(output.candidate));
        return;
      }
      if (
        ready &&
        output.type === "SuccessfulLocalFinalizationRecoveryOutput"
      ) {
        if (activeSession !== undefined) {
          activeSession.localFinalizationRecovery = output;
        }
        return;
      }
      if (!ready && output.type === "FailureOutput") {
        fail(new Error(output.error));
        return;
      }
      if (!ready) {
        fail(
          new Error(
            `speculative worker completed before candidate-ready (${output.type})`,
          ),
        );
        return;
      }
      if (finalSettled) return;
      finalSettled = true;
      resolveFinal(output);
    });
    worker.on("error", fail);
    worker.on("exit", (code) => {
      if (!finalSettled) {
        fail(new Error(`worker exited with code ${code.toString()}`));
      }
    });
    return Effect.suspend(() => {
      if (!ready) {
        session.cancelled = true;
        return terminateAndClearWorkerSession(session).pipe(
          Effect.catchAll(Effect.logError),
        );
      }
      return Effect.void;
    });
  });

const spawnSpeculativeSession = (
  input: WorkerInput,
): Effect.Effect<SpeculativeCandidateSummary, WorkerError, Database> =>
  Effect.gen(function* () {
    const databaseRuntime = yield* Effect.runtime<Database>();
    const releaseTerminatedWorkerLedgerLease = () =>
      Runtime.runPromise(databaseRuntime)(
        MpfEngineStateDB.releaseLedgerStoreLease(
          input.data.ledgerStoreLeaseOwner,
        ).pipe(
          Effect.catchAll((cause) =>
            Effect.logError(
              "Failed to release the terminated speculative worker ledger MPF lease; its bounded TTL remains the fallback.",
              cause,
            ),
          ),
        ),
      );
    return yield* spawnSpeculativeSessionWithWorker(
      () =>
        new Worker(
          resolveWorkerEntry(import.meta.url, "commit-block-header.js"),
          {
            workerData: input,
            transferList:
              input.nativeMpf === undefined ? [] : [input.nativeMpf.port],
          },
        ),
      releaseTerminatedWorkerLedgerLease,
    );
  });

export const spawnSpeculativeSessionForTest = (
  worker: SpeculativeCommitWorkerPort,
  afterTermination?: () => Promise<void>,
): Effect.Effect<SpeculativeCandidateSummary, WorkerError> =>
  spawnSpeculativeSessionWithWorker(() => worker, afterTermination);

const finishSpeculativeSession = (
  instruction: SpeculativeCommitWorkerInstruction,
): Effect.Effect<FinishedSpeculativeWorkerSession, WorkerError> =>
  Effect.gen(function* () {
    const session = activeSession;
    if (session === undefined) {
      return yield* Effect.fail(
        new WorkerError({
          worker: "speculative-commit-builder",
          message: "No speculative commit worker session is active",
          cause: instruction.type,
        }),
      );
    }
    session.worker.postMessage(instruction);
    const clearAndTerminate = terminateAndClearWorkerSession(session).pipe(
      Effect.catchAll(Effect.logError),
    );
    const result = yield* Effect.either(
      Effect.tryPromise({
        try: () => session.finalOutput,
        catch: (cause) =>
          cause instanceof WorkerError
            ? cause
            : new WorkerError({
                worker: "speculative-commit-builder",
                message:
                  "Speculative commit worker did not return a final output",
                cause,
              }),
      }).pipe(Effect.onInterrupt(() => clearAndTerminate)),
    );
    yield* terminateAndClearWorkerSession(session);
    if (result._tag === "Left") {
      return yield* Effect.fail(result.left);
    }
    return {
      output: result.right,
      candidate: session.candidate,
      localFinalizationRecovery: session.localFinalizationRecovery,
    };
  });

const invalidateSession = (
  reason: SpeculativeInvalidationReason,
): Effect.Effect<void, never> =>
  activeSession === undefined
    ? buildingSession === undefined
      ? Effect.void
      : Effect.suspend(() => {
          const session = buildingSession;
          if (session !== undefined) {
            session.cancelled = true;
          }
          return session === undefined
            ? Effect.void
            : terminateAndClearWorkerSession(session).pipe(
                Effect.catchAll(Effect.logError),
              );
        })
    : finishSpeculativeSession({
        type: "InvalidateSpeculativeCandidate",
        reason,
      }).pipe(
        Effect.asVoid,
        Effect.catchAll((error) =>
          Effect.suspend(() => {
            const session = activeSession ?? buildingSession;
            return session === undefined
              ? Effect.logError(error)
              : terminateAndClearWorkerSession(session).pipe(
                  Effect.catchAll(Effect.logError),
                );
          }),
        ),
      );

export const shutdownSpeculativeCommitSession = (): Effect.Effect<
  void,
  never
> =>
  Effect.suspend(() => {
    const session = activeSession ?? buildingSession;
    if (session === undefined) return Effect.void;
    if ("cancelled" in session) session.cancelled = true;
    return terminateAndClearWorkerSession(session).pipe(
      Effect.catchAll(Effect.logError),
    );
  });

export const invalidateSpeculativeSessionForTest = (
  reason: SpeculativeInvalidationReason,
): Effect.Effect<void, never> => invalidateSession(reason);

const acquirePipelinePhase = (
  globals: Globals,
  phase: "speculative_build" | "mutation_worker",
) =>
  Ref.modify(globals.COMMIT_PIPELINE_PHASE, (active) =>
    active === "idle" ? ([true, phase] as const) : ([false, active] as const),
  );

const releasePipelinePhase = (globals: Globals) =>
  Effect.all(
    [
      Ref.set(globals.COMMIT_PIPELINE_PHASE, "idle"),
      Ref.set(globals.COMMIT_WORKER_ACTIVE, false),
    ],
    { discard: true },
  );

export const invalidateSpeculativeCommitCandidate = (
  globals: Globals,
  config: NodeConfigDep,
  reason: SpeculativeInvalidationReason,
) =>
  Effect.gen(function* () {
    yield* invalidateSession(reason);
    const previousState = yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
    const state = yield* Ref.updateAndGet(
      globals.SPECULATIVE_COMMIT_STATE,
      (current) =>
        reduceSpeculativeCommitState(
          current,
          { _tag: "Invalidate", reason, atMs: Date.now() },
          config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
        ),
    );
    yield* Ref.set(globals.SPECULATIVE_COMMIT_SESSION_ACTIVE, false);
    if (state !== previousState) {
      yield* Metric.increment(
        Metric.tagged(speculationInvalidationCounter, "reason", reason),
      );
      yield* Effect.logWarning(
        `pipeline_trace phase=candidate_invalidated reason=${reason} state=${state._tag}`,
      );
    }
    const unconfirmedSubmittedTxHash = yield* Ref.get(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
    );
    if (
      state._tag === "Invalidated" &&
      previousState._tag !== "Invalidated" &&
      unconfirmedSubmittedTxHash !== ""
    ) {
      yield* Queue.offer(
        globals.SPECULATIVE_BUILD_WAKE_QUEUE,
        state.baseHeaderHash,
      );
    } else if (state._tag === "Invalidated") {
      yield* Ref.set(
        globals.SPECULATIVE_COMMIT_STATE,
        reduceSpeculativeCommitState(
          state,
          { _tag: "Clear" },
          config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
        ),
      );
    }
  });

export const runSpeculativeCommitBuilderOnce = (
  requestedBaseHeaderHash: string,
): Effect.Effect<
  void,
  WorkerError | DatabaseError,
  Globals | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    if (
      !config.SPECULATIVE_COMMIT_BUILD ||
      hasActiveSpeculativeCommitSession()
    ) {
      return;
    }
    const state = yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
    if (
      (state._tag !== "Building" && state._tag !== "Invalidated") ||
      state.baseHeaderHash !== requestedBaseHeaderHash
    ) {
      return;
    }
    const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (resetInProgress) {
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T5");
      return;
    }
    const watermarks = yield* Ref.get(globals.USER_EVENT_BARRIER_WATERMARKS);
    if (
      !barrierWatermarksAreFresh({
        watermarks,
        nowMs: Date.now(),
        maxStalenessMs: config.USER_EVENT_BARRIER_MAX_STALENESS_MS,
      })
    ) {
      yield* Effect.logWarning(
        `Speculative commit build deferred because user-event barriers are stale base_header_hash=${requestedBaseHeaderHash}`,
      );
      return;
    }
    const acquired = yield* acquirePipelinePhase(globals, "speculative_build");
    if (!acquired) {
      return;
    }
    const startedAtMs = Date.now();
    yield* Ref.set(globals.SPECULATIVE_COMMIT_SESSION_ACTIVE, true);
    if (state._tag === "Invalidated") {
      yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (current) =>
        reduceSpeculativeCommitState(
          current,
          { _tag: "RebuildStarted", atMs: startedAtMs },
          config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
        ),
      );
    }
    const program = Effect.gen(function* () {
      const pending = yield* PendingBlockFinalizationsDB.retrieveActive();
      if (Option.isNone(pending)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Cannot speculate without an active submitted journal",
            cause: requestedBaseHeaderHash,
          }),
        );
      }
      const record = pending.value;
      const journalHeaderHash =
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
      const submittedTxHash =
        record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
          "hex",
        );
      if (
        journalHeaderHash !== requestedBaseHeaderHash ||
        submittedTxHash === undefined
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Active journal does not match the speculative base",
            cause: `requested=${requestedBaseHeaderHash},journal=${journalHeaderHash},submitted=${submittedTxHash ?? "missing"}`,
          }),
        );
      }
      const nativeMpfOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
      if (
        config.MPF_ENGINE === "architecture_g" &&
        nativeMpfOwner === undefined
      ) {
        return yield* Effect.fail(
          new WorkerError({
            worker: "speculative-commit-builder",
            message: "Architecture G native owner is not initialized",
            cause: requestedBaseHeaderHash,
          }),
        );
      }
      const nativeMpfInput =
        nativeMpfOwner === undefined
          ? undefined
          : {
              port: nativeMpfOwner.createWorkerPort(),
              durableRoot: (yield* Effect.promise(() =>
                nativeMpfOwner.diagnostics(),
              )).durableRoot,
              ownerBinarySha256: config.MPF_NATIVE_OWNER_BINARY_SHA256,
            };
      const candidate = yield* spawnSpeculativeSession({
        nativeMpf: nativeMpfInput,
        data: {
          availableConfirmedBlock: "",
          availableLocalFinalizationBlock: "",
          currentBlockStartTimeMs:
            record[
              PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
            ].getTime(),
          ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
          localFinalizationPending: false,
          mempoolTxsCountSoFar: 0,
          sizeOfProcessedTxsSoFar: 0,
          baseSnapshotId: `speculative:${journalHeaderHash}`,
          stateQueueHasUnmergedTail: true,
          speculativeBuild: {
            base: {
              headerHash: journalHeaderHash,
              utxosRoot:
                record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
              blockEndTimeMs:
                record[
                  PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
                ].getTime(),
              submittedTxHash,
            },
            watermarks,
            excludedMempoolTxIds: record.mempoolTxIds.map((txId) =>
              txId.toString("hex"),
            ),
            excludedDepositEventIds: record.depositEventIds.map((eventId) =>
              eventId.toString("hex"),
            ),
            excludedForcedTransactionEventIds:
              record.forcedTransactionEventIds.map((eventId) =>
                eventId.toString("hex"),
              ),
            excludedWithdrawalEventIds: record.withdrawalEventIds.map(
              (eventId) => eventId.toString("hex"),
            ),
          },
        },
      });
      yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (current) =>
        reduceSpeculativeCommitState(
          current,
          { _tag: "CandidateReady", candidate },
          config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
        ),
      );
      yield* speculativeBuildDuration(
        Effect.succeed(Duration.millis(Date.now() - startedAtMs)),
      );
      yield* Effect.logInfo(
        `pipeline_trace phase=candidate_ready candidate_id=${candidate.candidateId} base_header_hash=${candidate.baseHeaderHash}`,
      );
    });
    yield* program.pipe(
      Effect.catchAll((error) =>
        invalidateSpeculativeCommitCandidate(globals, config, "T7").pipe(
          Effect.zipRight(Effect.fail(error)),
        ),
      ),
      Effect.ensuring(releasePipelinePhase(globals)),
    );
  });

const waitForCandidate = (
  globals: Globals,
  confirmedHeaderHash: string,
): Effect.Effect<SpeculativeCommitState> =>
  Effect.gen(function* () {
    const deadline = Date.now() + 12_000;
    while (Date.now() < deadline) {
      const state = yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
      if (state._tag !== "Building") return state;
      if (state.baseHeaderHash !== confirmedHeaderHash) return state;
      yield* Effect.sleep("50 millis");
    }
    return yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
  });

const applySpeculativeSubmissionOutput = (
  globals: Globals,
  config: NodeConfigDep,
  result: FinishedSpeculativeWorkerSession,
  confirmationObservedAtMs: number,
  confirmationWaitMs: number,
): Effect.Effect<void, WorkerError, Database> =>
  Effect.gen(function* () {
    const { output } = result;
    if (result.localFinalizationRecovery !== undefined) {
      yield* publishCommitMempoolLedgerMutation(
        globals,
        result.localFinalizationRecovery,
        config.VALIDATION_LEDGER_DELTA_LOG_MAX,
      );
      yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
      yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
      yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0);
      yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0);
    }
    if (output.type === "SpeculativeCandidateInvalidatedOutput") {
      yield* invalidateSpeculativeCommitCandidate(
        globals,
        config,
        output.reason,
      );
      return;
    }
    if (output.type !== "SubmittedAwaitingConfirmationOutput") {
      yield* invalidateSpeculativeCommitCandidate(
        globals,
        config,
        output.type === "FailureOutput" ? "T7" : "T4",
      );
      return;
    }
    if (output.nativeMpfPromotion !== undefined) {
      const nativeMpfOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
      if (nativeMpfOwner === undefined) {
        return yield* Effect.fail(
          new WorkerError({
            worker: "speculative-commit-builder",
            message: "Native MPF promotion returned without an owner",
            cause: output.nativeMpfPromotion.handle.baseRoot,
          }),
        );
      }
      yield* promoteOrRecoverNativeMpf({
        owner: nativeMpfOwner,
        handle: output.nativeMpfPromotion.handle,
      }).pipe(
        Effect.mapError(
          (cause) =>
            new WorkerError({
              worker: "speculative-commit-builder",
              message:
                "Architecture G speculative post-submit promotion failed",
              cause,
            }),
        ),
      );
    }
    yield* publishCommitMempoolLedgerMutation(
      globals,
      output,
      config.VALIDATION_LEDGER_DELTA_LOG_MAX,
    );
    yield* speculativeCommitBlockNumTxGauge(
      Effect.succeed(BigInt(output.mempoolTxsCount)),
    );
    yield* Metric.increment(speculativeCommitBlockCounter);
    yield* Metric.incrementBy(
      speculativeCommitBlockTxCounter,
      BigInt(output.mempoolTxsCount),
    );
    yield* Ref.update(globals.BLOCKS_IN_QUEUE, (count) => count + 1);
    yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
    yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
    yield* Ref.set(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      output.submittedTxHash,
    );
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, Date.now());
    yield* Ref.set(
      globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
      output.blockEndTimeMs,
    );
    yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
    yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0);
    yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0);
    yield* Ref.set(globals.SPECULATIVE_COMMIT_SESSION_ACTIVE, false);
    yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (state) =>
      reduceSpeculativeCommitState(
        state,
        {
          _tag: "SubmitSucceeded",
          submittedHeaderHash: output.submittedHeaderHash,
          atMs: Date.now(),
        },
        config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
      ),
    );
    const submittedAtMs = Date.now();
    if (lastSubmittedAtMs > 0) {
      yield* commitCadenceTimer(
        Effect.succeed(Duration.millis(submittedAtMs - lastSubmittedAtMs)),
      );
    }
    lastSubmittedAtMs = submittedAtMs;
    yield* submitAfterConfirmTimer(
      Effect.succeed(Duration.millis(submittedAtMs - confirmationObservedAtMs)),
    );
    yield* l1ConfirmationWaitTimer(
      Effect.succeed(Duration.millis(confirmationWaitMs)),
    );
    yield* speculationOverlapGauge(
      Effect.succeed(
        speculationOverlapEfficiency({
          buildDurationMs: result.candidate.buildDurationMs,
          confirmationWaitMs,
        }),
      ),
    );
    yield* Metric.increment(speculationHitCounter);
    yield* Queue.offer(
      globals.SPECULATIVE_BUILD_WAKE_QUEUE,
      output.submittedHeaderHash,
    );
    yield* Effect.logInfo(
      `pipeline_trace phase=candidate_submitted submitted_header_hash=${output.submittedHeaderHash}`,
    );
  });

export const submitSpeculativeCandidateOnConfirmation = (
  wake: CommitSubmitWake,
): Effect.Effect<
  void,
  | WorkerError
  | DatabaseError
  | SDK.StateQueueError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | Error,
  Globals | Database | NodeConfig | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const {
      confirmedHeaderHash,
      confirmedTip,
      confirmationObservedAtMs,
      confirmationWaitMs,
    } = wake;
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    if (!config.SPECULATIVE_COMMIT_BUILD) return;
    if (yield* Ref.get(globals.RESET_IN_PROGRESS)) {
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T5");
      return;
    }
    const state = yield* waitForCandidate(globals, confirmedHeaderHash);
    if (state._tag !== "ReadyToSubmit") {
      if (
        shouldRetrySpeculativeConfirmationWake({
          state,
          confirmedHeaderHash,
        })
      ) {
        yield* Effect.sleep("50 millis");
        yield* Queue.offer(globals.COMMIT_SUBMIT_WAKE_QUEUE, wake);
      }
      return;
    }
    yield* runAfterL1ControlPlaneRelease(
      withL1ControlPlane(
        globals,
        { scope: "speculative_submit", maxHoldMs: 60_000 },
        Effect.gen(function* () {
          const nextState = reduceSpeculativeCommitState(
            state,
            {
              _tag: "ConfirmationObserved",
              confirmedHeaderHash,
              atMs: confirmationObservedAtMs,
            },
            config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
          );
          if (nextState._tag === "Invalidated") {
            yield* recordForeignTipMismatchBeforeInvalidation({
              expectedHeaderHash: state.baseHeaderHash,
              confirmedHeaderHash,
              confirmedTip,
              invalidateCandidate: invalidateSpeculativeCommitCandidate(
                globals,
                config,
                "T2",
              ),
            });
            return;
          }
          yield* Ref.set(globals.SPECULATIVE_COMMIT_STATE, nextState);
          if (nextState._tag !== "Submitting") return;
          yield* reachPipelinedCommitCrashCheckpoint(
            "confirmation_wake_before_journal",
          );
          const acquired = yield* acquirePipelinePhase(
            globals,
            "mutation_worker",
          );
          if (!acquired) {
            yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (current) =>
              reduceSpeculativeCommitState(
                current,
                { _tag: "SubmissionDeferred" },
                config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
              ),
            );
            yield* Effect.sleep("50 millis");
            yield* Queue.offer(globals.COMMIT_SUBMIT_WAKE_QUEUE, wake);
            return;
          }
          yield* Ref.set(globals.COMMIT_WORKER_ACTIVE, true);
          const leaseResult = yield* StateQueueMutationLeasesDB.tryWithLease(
            "block_commitment",
            (leaseToken) =>
              Effect.gen(function* () {
                const lucid = yield* Lucid;
                const contracts = yield* MidgardContracts;
                const snapshot = yield* fetchStateQueueSnapshotProgram(
                  lucid.api,
                  contracts.stateQueue,
                  "commit_preflight",
                );
                yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
                const localFinalizationPending = yield* Ref.get(
                  globals.LOCAL_FINALIZATION_PENDING,
                );
                const localFinalizationBlock = yield* Ref.get(
                  globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
                );
                const instruction =
                  yield* decideSpeculativeInstructionForLiveTip({
                    expectedHeaderHash: confirmedHeaderHash,
                    liveTail: snapshot.tailCommitBase.utxo,
                    submitInstruction: {
                      type: "SubmitSpeculativeCandidate",
                      confirmedBlock: snapshot.tailCommitBase.utxo,
                      stateQueueLeaseToken: leaseToken,
                      baseSnapshotId: snapshot.snapshotId,
                      stateQueueHasUnmergedTail:
                        snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
                      localFinalizationBlock:
                        localFinalizationPending &&
                        localFinalizationBlock !== ""
                          ? localFinalizationBlock
                          : undefined,
                    },
                  });
                if (instruction.type === "InvalidateSpeculativeCandidate") {
                  yield* invalidateSpeculativeCommitCandidate(
                    globals,
                    config,
                    "T2",
                  );
                  return;
                }
                const result = yield* finishSpeculativeSession(instruction);
                yield* applySpeculativeSubmissionOutput(
                  globals,
                  config,
                  result,
                  confirmationObservedAtMs,
                  confirmationWaitMs,
                );
                return result.localFinalizationRecovery?.finalizedHeaderHash;
              }),
            {
              ttlMs: config.STATE_QUEUE_MUTATION_LEASE_TTL_MS,
              renewIntervalMs:
                config.STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS,
            },
          ).pipe(
            Effect.tapError(() =>
              invalidateSpeculativeCommitCandidate(globals, config, "T7"),
            ),
            Effect.ensuring(releasePipelinePhase(globals)),
          );
          if (leaseResult._tag === "Busy") {
            yield* Effect.logInfo(
              `pipeline_trace phase=speculative_submission_deferred reason=state_queue_lease_busy confirmed_header_hash=${confirmedHeaderHash}`,
            );
            yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (current) =>
              reduceSpeculativeCommitState(
                current,
                { _tag: "SubmissionDeferred" },
                config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
              ),
            );
            yield* Effect.sleep("50 millis");
            yield* Queue.offer(globals.COMMIT_SUBMIT_WAKE_QUEUE, wake);
            return undefined;
          }
          return leaseResult.value;
        }),
      ),
      (headerHash) => headerHash,
      publishFinalizedDaPayloadBestEffort,
    );
  });

export const speculativeCommitBuilderFiber: Effect.Effect<
  void,
  never,
  Globals | Database | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const config = yield* NodeConfig;
  yield* Effect.logInfo("🟦 Speculative commit builder fiber started.");
  const pending = yield* PendingBlockFinalizationsDB.retrieveActive().pipe(
    Effect.catchAll(() => Effect.succeed(Option.none())),
  );
  if (Option.isSome(pending)) {
    const submittedTxHash =
      pending.value[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH];
    if (submittedTxHash !== null) {
      yield* Metric.increment(
        Metric.tagged(speculationInvalidationCounter, "reason", "T7"),
      );
      yield* Effect.logWarning(
        "pipeline_trace phase=candidate_invalidated reason=T7 state=restart_rebuild",
      );
      const baseHeaderHash =
        pending.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString(
          "hex",
        );
      yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (state) =>
        state._tag === "Idle"
          ? reduceSpeculativeCommitState(
              state,
              {
                _tag: "SubmittedBase",
                baseHeaderHash,
                atMs: Date.now(),
              },
              config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
            )
          : state,
      );
      yield* Queue.offer(globals.SPECULATIVE_BUILD_WAKE_QUEUE, baseHeaderHash);
    }
  }
  while (true) {
    const baseHeaderHash = yield* Queue.take(
      globals.SPECULATIVE_BUILD_WAKE_QUEUE,
    );
    yield* runSpeculativeCommitBuilderOnce(baseHeaderHash).pipe(
      Effect.catchAllCause(Effect.logWarning),
    );
  }
}).pipe(Effect.ensuring(shutdownSpeculativeCommitSession()));

export const speculativeCommitSubmitterFiber: Effect.Effect<
  void,
  never,
  Globals | Database | NodeConfig | Lucid | MidgardContracts
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Effect.logInfo("🟦 Speculative commit submitter wake fiber started.");
  while (true) {
    const wake = yield* Queue.take(globals.COMMIT_SUBMIT_WAKE_QUEUE);
    yield* submitSpeculativeCandidateOnConfirmation(wake).pipe(
      Effect.catchAllCause(Effect.logWarning),
    );
  }
});
