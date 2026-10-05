import { Duration, Effect, Schedule } from "effect";

import { foreignDaReconciliationFiber } from "../fibers/foreign-da-reconciliation.js";
import {
  admissionBacklogGaugeFiber,
  attestationTimeoutCorrectionFiber,
  blockCommitmentFiber,
  blockConfirmationFiber,
  daPublicationReconcilerFiber,
  fetchAndInsertTxOrderUTxOsFiber,
  L1_PROVIDER_EXACT_REFRESH_INTERVAL_MS,
  l1ProviderReadinessRefresherFiber,
  mergeFiber,
  monitorMempoolFiber,
  mpfPayloadAuditFiber,
  nativeMpfOwnerSupervisorFiber,
  operatorWatchdogFiber,
  retentionSweeperFiber,
  speculativeCommitBuilderFiber,
  speculativeCommitSubmitterFiber,
  txQueueProcessorFiber,
  userEventBarrierRefresherFiber,
} from "../fibers/index.js";
import { operatorMembershipFiber } from "../fibers/operator-membership.js";
import { settlementFiber } from "../fibers/settlement.js";
import { signedIntentRebroadcastFiber } from "../fibers/signed-intent-rebroadcast.js";
import { Globals } from "../services/globals.js";
import { type NodeConfigDep, writeBehindFiber } from "../services/index.js";
import {
  awaitHaltCleared,
  FIBER_HALT_SOURCES,
  type HeldFiber,
  pausedWhileHalted,
  restartedAcrossHalts,
} from "../services/liveness-halt.js";

/**
 * Builds a fixed Effect schedule from a millisecond interval.
 */
const mkSchedule = (millisBetweenRuns: number) =>
  Schedule.spaced(Duration.millis(millisBetweenRuns));

/** `makeFiber` on `mkSchedule(millisBetweenRuns)`, held before its first action
 * and between ticks while a source `FIBER_HALT_SOURCES[name]` names has a
 * raised liveness reason. */
const heldSchedule = <A, E, R>(
  name: HeldFiber,
  millisBetweenRuns: number,
  makeFiber: (schedule: Schedule.Schedule<number>) => Effect.Effect<A, E, R>,
) =>
  Effect.flatMap(Globals, (globals) =>
    Effect.flatMap(awaitHaltCleared(globals, FIBER_HALT_SOURCES[name]), () =>
      makeFiber(
        pausedWhileHalted(
          mkSchedule(millisBetweenRuns),
          globals,
          FIBER_HALT_SOURCES[name],
        ),
      ),
    ),
  );

/** `fiber`, stopped while a source `FIBER_HALT_SOURCES[name]` names has a
 * raised liveness reason and started again once it clears. */
const heldRestart = <A, E, R>(name: HeldFiber, fiber: Effect.Effect<A, E, R>) =>
  Effect.flatMap(Globals, (globals) =>
    restartedAcrossHalts(globals, FIBER_HALT_SOURCES[name], fiber),
  );

/**
 * The scheduled background fibers `runNode` runs for the node's lifetime,
 * keyed by name. `runNodeFiberSet` adds the fibers that need its startup
 * state (the retained-payload server and history owner). The HTTP listener is
 * scoped at the CLI boundary so local probes are available during startup.
 * None of them fails: a condition a fiber cannot resolve raises a liveness
 * reason instead, and the fibers whose effects that condition must stop are
 * held here until it clears (see `liveness-halt.ts`).
 */
export const nodeFibers = ({
  nodeConfig,
  withMonitoring,
}: {
  readonly nodeConfig: NodeConfigDep;
  readonly withMonitoring: boolean | undefined;
}) => ({
  admissionBacklogGauge: admissionBacklogGaugeFiber(
    mkSchedule(nodeConfig.ADMISSION_BACKLOG_REFRESH_MS),
  ),
  settlement: heldRestart("settlement", settlementFiber),
  writeBehind: writeBehindFiber,
  daPublicationReconciler: daPublicationReconcilerFiber(
    mkSchedule(nodeConfig.MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS),
  ),
  foreignDaReconciliation: foreignDaReconciliationFiber(
    mkSchedule(nodeConfig.MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS),
  ),
  blockCommitment: heldSchedule(
    "blockCommitment",
    nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT,
    blockCommitmentFiber,
  ),
  blockConfirmation: blockConfirmationFiber(
    mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_CONFIRMATION),
  ),
  signedIntentRebroadcast: signedIntentRebroadcastFiber(mkSchedule(1_000)),
  operatorWatchdog: heldSchedule(
    "operatorWatchdog",
    nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT,
    operatorWatchdogFiber,
  ),
  operatorMembership: operatorMembershipFiber,
  l1ProviderReadinessRefresher: l1ProviderReadinessRefresherFiber(
    mkSchedule(L1_PROVIDER_EXACT_REFRESH_INTERVAL_MS),
  ),
  userEventBarrierRefresher: nodeConfig.SPECULATIVE_COMMIT_BUILD
    ? userEventBarrierRefresherFiber(
        mkSchedule(nodeConfig.USER_EVENT_BARRIER_REFRESH_MS),
      )
    : Effect.void,
  speculativeCommitBuilder: nodeConfig.SPECULATIVE_COMMIT_BUILD
    ? heldRestart("speculativeCommitBuilder", speculativeCommitBuilderFiber)
    : Effect.void,
  speculativeCommitSubmitter: nodeConfig.SPECULATIVE_COMMIT_BUILD
    ? heldRestart("speculativeCommitSubmitter", speculativeCommitSubmitterFiber)
    : Effect.void,
  fetchAndInsertTxOrderUTxOs: fetchAndInsertTxOrderUTxOsFiber(
    mkSchedule(nodeConfig.WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES),
  ),
  retentionSweeper: retentionSweeperFiber(
    mkSchedule(nodeConfig.WAIT_BETWEEN_RETENTION_SWEEPS),
  ),
  merge: heldSchedule("merge", nodeConfig.WAIT_BETWEEN_MERGE_TXS, mergeFiber),
  attestationTimeoutCorrection: attestationTimeoutCorrectionFiber(
    mkSchedule(nodeConfig.WAIT_BETWEEN_MERGE_TXS),
  ),
  mpfPayloadAudit: mpfPayloadAuditFiber,
  nativeMpfOwnerSupervisor: nativeMpfOwnerSupervisorFiber(
    mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT),
  ),
  monitorMempool: withMonitoring
    ? monitorMempoolFiber(mkSchedule(1000))
    : Effect.void,
  txQueueProcessor: txQueueProcessorFiber(
    mkSchedule(nodeConfig.TX_QUEUE_POLL_INTERVAL_MS),
  ),
});

/**
 * The startup fiber whose failure still ends the process: the history owner
 * stopping. HTTP acquisition failure propagates at the CLI boundary. Every
 * other fiber `runNodeFiberSet` returns, startup or scheduled, has error channel
 * `never`.
 */
export type ProcessEndingStartupFiber = "historyOwnerStopped";

/**
 * Every fiber `runNode` runs for the node's lifetime: the ones built from its
 * startup state, then the scheduled set. `runNode` hands this record to
 * `Effect.all` as is, so a fiber missing here never starts. A startup fiber
 * may not reuse a scheduled fiber's name, which would silently drop one, and
 * may fail only if `ProcessEndingStartupFiber` names it.
 */
export const runNodeFiberSet = <
  Startup extends Record<string, Effect.Effect<unknown, unknown, unknown>>,
>({
  nodeConfig,
  withMonitoring,
  startupFibers,
}: {
  readonly nodeConfig: NodeConfigDep;
  readonly withMonitoring: boolean | undefined;
  readonly startupFibers: Startup & {
    readonly [K in keyof ReturnType<typeof nodeFibers>]?: never;
  } & NoInfer<{
      readonly [K in Exclude<
        keyof Startup,
        ProcessEndingStartupFiber
      >]: Effect.Effect<unknown, never, unknown>;
    }>;
}) => {
  const startup: Startup = startupFibers;
  return { ...startup, ...nodeFibers({ nodeConfig, withMonitoring }) };
};
