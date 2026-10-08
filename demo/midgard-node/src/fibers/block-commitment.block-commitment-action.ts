import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref, Schedule } from "effect";

import { StateQueueMutationLeasesDB } from "../database/index.js";
import {
  PENDING_FINALIZATION_AGE_BOUND_MS,
  retrieveUnreconciledSignedSubmission,
} from "../database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { WorkerError } from "../workers/utils/common.js";
import { buildAndSubmitCommitmentBlockAction } from "./block-commitment.build-and-submit-commitment-block-action.js";
import { publishCommitHorizonLagReadiness } from "./block-commitment.commit-horizon-lag-readiness.js";
import { shouldSkipIdleCommitPipelineBeforeSchedulerAlignment } from "./block-commitment.should-skip-for-detailed-scheduler-due-work.js";
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

export const SKIPPED_ACTIVE_PENDING_FINALIZATION =
  "skipped_active_pending_finalization";

let reportedHeaderHex: string | undefined;
let reportedOverBound = false;

/**
 * True while a signed commit intent awaits the history owner's signed-intent
 * reconciliation: the commit worker would only refuse at its signed-submission
 * preflight, so the tick takes neither the L1 control plane nor the
 * state-queue lease and spawns no worker. Logged once per journal, again once
 * it outlives the bound, and once when it resolves. A pending local
 * finalization recovery always runs.
 */
export const shouldSkipForActivePendingFinalization = Effect.gen(function* () {
  const globals = yield* Globals;
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const localFinalizationBlock = yield* Ref.get(
    globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
  );
  if (localFinalizationPending && localFinalizationBlock !== "") return false;
  const signed = yield* retrieveUnreconciledSignedSubmission;
  if (signed === undefined) {
    if (reportedHeaderHex !== undefined) {
      yield* Effect.logInfo(
        `🔹 Resuming block commitment: signed commit intent header=${reportedHeaderHex} is reconciled.`,
      );
      reportedHeaderHex = undefined;
      reportedOverBound = false;
    }
    return false;
  }
  const headerHex = signed.headerHash.toString("hex");
  if (headerHex !== reportedHeaderHex) {
    reportedHeaderHex = headerHex;
    reportedOverBound = false;
    yield* Effect.logInfo(
      `🔹 Skipping block commitment ticks (${SKIPPED_ACTIVE_PENDING_FINALIZATION}): signed commit intent header=${headerHex} awaits the history owner's signed-intent reconciliation; pending_finalization_age:${signed.ageMs.toString()}.`,
    );
  } else if (
    !reportedOverBound &&
    signed.ageMs > PENDING_FINALIZATION_AGE_BOUND_MS
  ) {
    reportedOverBound = true;
    yield* Effect.logWarning(
      `🔹 Block commitment still skipped (${SKIPPED_ACTIVE_PENDING_FINALIZATION}): signed commit intent header=${headerHex} is unresolved past the bound; pending_finalization_age:${signed.ageMs.toString()}:${PENDING_FINALIZATION_AGE_BOUND_MS.toString()}.`,
    );
  }
  return true;
});

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
  | Globals
  | Lucid
  | MidgardContracts
  | Database
  | NodeConfig
  | ContractDeploymentIdentity
  | IntentJournal
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const nodeConfig = yield* NodeConfig;
  yield* Ref.set(globals.HEARTBEAT_BLOCK_COMMITMENT, Date.now());
  yield* publishCommitHorizonLagReadiness;
  const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
  if (!RESET_IN_PROGRESS) {
    if (yield* shouldSkipIdleCommitPipelineBeforeSchedulerAlignment) return;
    if (yield* shouldSkipForActivePendingFinalization) {
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
  | Globals
  | Lucid
  | MidgardContracts
  | Database
  | NodeConfig
  | ContractDeploymentIdentity
  | IntentJournal
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fiber started.");
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
