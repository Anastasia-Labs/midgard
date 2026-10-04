import { Duration, Effect, Metric, Queue, Ref, Runtime } from "effect";
import { Worker } from "worker_threads";

import { MpfEngineStateDB } from "../database/index.js";
import { Database, Globals, type NodeConfigDep } from "../services/index.js";
import {
  type WorkerInput,
  type WorkerOutput,
} from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import {
  type CommitWorkerMessage,
  promoteOrRecoverNativeMpf,
  publishCommitMempoolLedgerMutation,
  takeCommitWorkerOutput,
} from "./block-commitment.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";
import {
  commitCadenceTimer,
  type FinishedSpeculativeWorkerSession,
  l1ConfirmationWaitTimer,
  speculationHitCounter,
  speculationOverlapGauge,
  speculativeCommitBlockCounter,
  speculativeCommitBlockNumTxGauge,
  speculativeCommitBlockTxCounter,
  type SpeculativeCommitWorkerPort,
  submitAfterConfirmTimer,
} from "./speculative-commit-builder.persist-authenticated-foreign-tip-mismatch.js";
import {
  activeSession,
  buildingSession,
  invalidateSession,
  invalidateSpeculativeCommitCandidate,
  spawnSpeculativeSessionWithWorker,
  terminateAndClearWorkerSession,
} from "./speculative-commit-builder.spawn-speculative-session-with-worker.js";
import {
  reduceSpeculativeCommitState,
  speculationOverlapEfficiency,
  type SpeculativeCandidateSummary,
  type SpeculativeInvalidationReason,
} from "./speculative-commit-state.js";

export let lastSubmittedAtMs = 0;

export const hasActiveSpeculativeCommitSession = (): boolean =>
  activeSession !== undefined || buildingSession !== undefined;

export const spawnSpeculativeSession = (
  globals: Globals,
  config: NodeConfigDep,
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
      (message) =>
        takeCommitWorkerOutput(
          globals,
          message,
          config.VALIDATION_LEDGER_DELTA_LOG_MAX,
        ),
      releaseTerminatedWorkerLedgerLease,
    );
  });

export const spawnSpeculativeSessionForTest = (
  worker: SpeculativeCommitWorkerPort,
  afterTermination?: () => Promise<void>,
  takeOutput: (message: CommitWorkerMessage) => WorkerOutput | undefined = (
    message,
  ) =>
    message.type === "MempoolLedgerRevertedNotice" ||
    message.type === "CommitDaFrameNotice"
      ? undefined
      : message,
): Effect.Effect<SpeculativeCandidateSummary, WorkerError> =>
  spawnSpeculativeSessionWithWorker(() => worker, takeOutput, afterTermination);

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

export const acquirePipelinePhase = (
  globals: Globals,
  phase: "speculative_build" | "mutation_worker",
) =>
  Ref.modify(globals.COMMIT_PIPELINE_PHASE, (active) =>
    active === "idle" ? ([true, phase] as const) : ([false, active] as const),
  );

export const releasePipelinePhase = (globals: Globals) =>
  Effect.all(
    [
      Ref.set(globals.COMMIT_PIPELINE_PHASE, "idle"),
      Ref.set(globals.COMMIT_WORKER_ACTIVE, false),
    ],
    { discard: true },
  );

export const applySpeculativeSubmissionOutput = (
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
      yield* publishCommitMempoolLedgerMutation(
        globals,
        output,
        config.VALIDATION_LEDGER_DELTA_LOG_MAX,
      );
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
