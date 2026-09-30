import { Effect, Metric, Queue, Ref } from "effect";

import { Globals, type NodeConfigDep } from "../services/index.js";
import {
  type SpeculativeCommitWorkerInstruction,
  type WorkerOutput,
} from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import { type CommitWorkerMessage } from "./block-commitment.js";
import {
  type ActiveSpeculativeWorkerSession,
  type BuildingSpeculativeWorkerSession,
  type FinishedSpeculativeWorkerSession,
  speculationInvalidationCounter,
  type SpeculativeCommitWorkerPort,
  terminateWorkerSession,
} from "./speculative-commit-builder.persist-authenticated-foreign-tip-mismatch.js";
import {
  reduceSpeculativeCommitState,
  type SpeculativeCandidateSummary,
  type SpeculativeInvalidationReason,
} from "./speculative-commit-state.js";
import { makeAwaitedWorkerTerminator } from "./worker-lifecycle.js";

export let activeSession: ActiveSpeculativeWorkerSession | undefined;

export let buildingSession: BuildingSpeculativeWorkerSession | undefined;

export let nextWorkerGeneration = 0;

const clearWorkerSessionGeneration = (generation: number): void => {
  if (buildingSession?.generation === generation) buildingSession = undefined;
  if (activeSession?.generation === generation) activeSession = undefined;
};

export const terminateAndClearWorkerSession = (
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

export const spawnSpeculativeSessionWithWorker = (
  createWorker: () => SpeculativeCommitWorkerPort,
  takeOutput: (message: CommitWorkerMessage) => WorkerOutput | undefined,
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
    worker.on("message", (message: CommitWorkerMessage) => {
      const output = takeOutput(message);
      if (output === undefined || finalSettled) return;
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

export const finishSpeculativeSession = (
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

export const invalidateSession = (
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
