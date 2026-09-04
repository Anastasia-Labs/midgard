import { Duration, Effect, Metric, Queue, Ref, Schedule } from "effect";

import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import { fetchAndInsertDepositUTxOsForCommitBarrier } from "./fetch-and-insert-deposit-utxos.js";
import { fetchAndInsertTxOrderUTxOsForCommitBarrier } from "./fetch-and-insert-tx-order-utxos.js";
import { fetchAndInsertWithdrawalUTxOsForCommitBarrier } from "./fetch-and-insert-withdrawal-utxos.js";
import {
  minimumBarrierWatermarkMs,
  type UserEventBarrierWatermarks,
} from "./speculative-commit-state.js";

const userEventBarrierRefreshDuration = Metric.timer(
  "user_event_barrier_refresh_duration_ms",
  "Full three-source user-event barrier refresh duration",
);
const userEventBarrierStalenessGauge = Metric.gauge(
  "user_event_barrier_staleness_ms",
  { description: "Age of the oldest completed user-event barrier watermark" },
);

export const mergeBarrierWatermarks = (
  current: UserEventBarrierWatermarks,
  next: UserEventBarrierWatermarks,
): UserEventBarrierWatermarks => ({
  depositMs: Math.max(current.depositMs, next.depositMs),
  withdrawalMs: Math.max(current.withdrawalMs, next.withdrawalMs),
  txOrderMs: Math.max(current.txOrderMs, next.txOrderMs),
  refreshedAtMs: Math.max(current.refreshedAtMs, next.refreshedAtMs),
});

export const runUserEventBarrierRefresherPass: Effect.Effect<
  UserEventBarrierWatermarks,
  unknown,
  | Globals
  | MidgardContracts
  | ContractDeploymentIdentity
  | Lucid
  | Database
  | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const startedAtMs = Date.now();
  const requestedUpperBound = new Date(startedAtMs);
  const deposit =
    yield* fetchAndInsertDepositUTxOsForCommitBarrier(requestedUpperBound);
  const withdrawal =
    yield* fetchAndInsertWithdrawalUTxOsForCommitBarrier(deposit);
  const txOrder = yield* fetchAndInsertTxOrderUTxOsForCommitBarrier(withdrawal);
  const next: UserEventBarrierWatermarks = {
    depositMs: deposit.getTime(),
    withdrawalMs: withdrawal.getTime(),
    txOrderMs: txOrder.getTime(),
    refreshedAtMs: Date.now(),
  };
  const watermarks = yield* Ref.updateAndGet(
    globals.USER_EVENT_BARRIER_WATERMARKS,
    (current) => mergeBarrierWatermarks(current, next),
  );
  yield* userEventBarrierRefreshDuration(
    Effect.succeed(Duration.millis(Date.now() - startedAtMs)),
  );
  yield* userEventBarrierStalenessGauge(
    Effect.succeed(
      Math.max(0, Date.now() - minimumBarrierWatermarkMs(watermarks)),
    ),
  );
  const speculativeState = yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
  if (
    speculativeState._tag === "Building" ||
    speculativeState._tag === "Invalidated"
  ) {
    yield* Queue.offer(
      globals.SPECULATIVE_BUILD_WAKE_QUEUE,
      speculativeState.baseHeaderHash,
    );
  }
  return watermarks;
});

export const userEventBarrierRefresherFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  | Globals
  | MidgardContracts
  | ContractDeploymentIdentity
  | Lucid
  | Database
  | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo("🟦 User-event barrier refresher fiber started.");
    yield* Effect.repeat(
      withL1ControlPlane(
        globals,
        { scope: "user_event_barrier_refresh", maxHoldMs: 60_000 },
        runUserEventBarrierRefresherPass,
      ).pipe(
        Effect.withSpan("user-event-barrier-refresher"),
        Effect.catchAllCause(Effect.logWarning),
      ),
      schedule,
    );
  });
