import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric, Option, Queue, Ref } from "effect";

import {
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { reachPipelinedCommitCrashCheckpoint } from "../e2e/pipelined-commit-crash-checkpoint.js";
import {
  type CommitSubmitWake,
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/state-queue-topology.js";
import { WorkerError } from "../workers/utils/common.js";
import { publishFullMempoolLedgerReload } from "./block-commitment.js";
import {
  publishFinalizedDaPayloadBestEffort,
  runAfterL1ControlPlaneRelease,
} from "./da-publication-trigger.js";
import {
  acquirePipelinePhase,
  applySpeculativeSubmissionOutput,
  releasePipelinePhase,
  shutdownSpeculativeCommitSession,
} from "./speculative-commit-builder.apply-speculative-submission-output.js";
import {
  decideSpeculativeInstructionForLiveTip,
  recordForeignTipMismatchBeforeInvalidation,
  speculationInvalidationCounter,
} from "./speculative-commit-builder.persist-authenticated-foreign-tip-mismatch.js";
import {
  runSpeculativeCommitBuilderOnce,
  waitForCandidate,
} from "./speculative-commit-builder.run-speculative-commit-builder-once.js";
import {
  finishSpeculativeSession,
  invalidateSpeculativeCommitCandidate,
} from "./speculative-commit-builder.spawn-speculative-session-with-worker.js";
import {
  reduceSpeculativeCommitState,
  shouldRetrySpeculativeConfirmationWake,
} from "./speculative-commit-state.js";

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
  | Globals
  | Database
  | NodeConfig
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
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
            const contracts = yield* MidgardContracts;
            yield* recordForeignTipMismatchBeforeInvalidation({
              expectedHeaderHash: state.baseHeaderHash,
              confirmedHeaderHash,
              confirmedTip,
              consensusProfile: contracts.consensusProfile,
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
                    consensusProfile: contracts.consensusProfile,
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
                const result = yield* finishSpeculativeSession(
                  instruction,
                ).pipe(
                  Effect.tapError(() =>
                    publishFullMempoolLedgerReload(
                      globals,
                      config.VALIDATION_LEDGER_DELTA_LOG_MAX,
                    ),
                  ),
                );
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
  Globals | Database | Lucid | NodeConfig
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
  | Globals
  | Database
  | NodeConfig
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
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
