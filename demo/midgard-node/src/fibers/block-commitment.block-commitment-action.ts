import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref, Schedule } from "effect";

import { StateQueueMutationLeasesDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import { WorkerError } from "../workers/utils/common.js";
import { buildAndSubmitCommitmentBlockAction } from "./block-commitment.build-and-submit-commitment-block-action.js";
import {
  shouldSkipIdleCommitPipelineBeforeSchedulerAlignment,
  shouldSkipScheduledLegacyCommitForSpeculation,
} from "./block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import {
  alignCommitSchedulerBeforeMutationWorkerIfIdle,
  registerPreLeaseCommitSchedulerDueWorkIfProven,
  releaseCommitMutationWorkerPhase,
  shouldSkipForRegisteredCommitDueWork,
  tryAcquireCommitMutationWorkerPhase,
} from "./block-commitment.should-skip-for-registered-commit-due-work.js";
import {
  publishFinalizedDaPayloadBestEffort,
  runAfterL1ControlPlaneRelease,
} from "./da-publication-trigger.js";

/**
 * Single scheduled commitment tick with a guard that prevents overlapping
 * commitment workers.
 */
export const blockCommitmentAction: Effect.Effect<
  void,
  | WorkerError
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
  | DatabaseError
  | Error,
  Globals | Lucid | MidgardContracts | Database | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const nodeConfig = yield* NodeConfig;
  yield* Ref.set(globals.HEARTBEAT_BLOCK_COMMITMENT, Date.now());
  const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
  if (!RESET_IN_PROGRESS) {
    if (nodeConfig.SPECULATIVE_COMMIT_BUILD) {
      const speculativeState = yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
      const localFinalizationPending = yield* Ref.get(
        globals.LOCAL_FINALIZATION_PENDING,
      );
      const localFinalizationBlock = yield* Ref.get(
        globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      );
      const recoveryMustRun =
        localFinalizationPending && localFinalizationBlock !== "";
      if (
        shouldSkipScheduledLegacyCommitForSpeculation({
          enabled: true,
          state: speculativeState,
          recoveryMustRun,
        })
      ) {
        return;
      }
    }
    if (yield* shouldSkipIdleCommitPipelineBeforeSchedulerAlignment) {
      return;
    }
    yield* runAfterL1ControlPlaneRelease(
      withL1ControlPlane(
        globals,
        { scope: "block_commitment", maxHoldMs: 180_000 },
        Effect.gen(function* () {
          if (yield* shouldSkipForRegisteredCommitDueWork) {
            return;
          }
          if (yield* registerPreLeaseCommitSchedulerDueWorkIfProven) {
            return;
          }
          if (yield* alignCommitSchedulerBeforeMutationWorkerIfIdle) {
            return;
          }
          const acquired = yield* tryAcquireCommitMutationWorkerPhase(globals);
          if (!acquired.acquired) {
            yield* Effect.logInfo(
              `🔹 Skipping block commitment trigger because commit pipeline phase is already active (phase=${acquired.activePhase}).`,
            );
            return;
          }

          yield* Effect.logInfo("🔹 New block commitment process started.");
          const leaseResult = yield* StateQueueMutationLeasesDB.tryWithLease(
            "block_commitment",
            (leaseToken) =>
              buildAndSubmitCommitmentBlockAction(leaseToken).pipe(
                Effect.withSpan("buildAndSubmitCommitmentBlockAction"),
              ),
            {
              ttlMs: nodeConfig.STATE_QUEUE_MUTATION_LEASE_TTL_MS,
              renewIntervalMs:
                nodeConfig.STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS,
            },
          ).pipe(Effect.ensuring(releaseCommitMutationWorkerPhase(globals)));
          if (leaseResult._tag === "Busy") {
            yield* Effect.logInfo(
              `🔹 Skipping block commitment trigger because the state-queue mutation lease is busy (${StateQueueMutationLeasesDB.describeActiveLease(
                leaseResult.activeLease,
              )}).`,
            );
            return undefined;
          }
          return leaseResult.value;
        }),
      ),
      (workerOutput) =>
        workerOutput?.type === "SuccessfulLocalFinalizationRecoveryOutput"
          ? workerOutput.finalizedHeaderHash
          : undefined,
      publishFinalizedDaPayloadBestEffort,
    );
  }
});

/**
 * Fiber wrapper that repeats block-commitment work on the provided schedule.
 */
export const blockCommitmentFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Globals | Lucid | MidgardContracts | Database | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fiber started.");
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
