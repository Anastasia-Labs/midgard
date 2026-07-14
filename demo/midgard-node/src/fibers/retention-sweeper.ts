import { Effect, Schedule } from "effect";

import {
  AddressHistoryDB,
  DaPayloadsDB,
  DepositsDB,
  MempoolTxDeltasDB,
  TxRejectionsDB,
  WithdrawalsDB,
} from "@/database/index.js";
import {
  computeRetentionCutoff,
  shouldPruneRetention,
} from "@/database/retention-policy.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database, NodeConfig } from "@/services/index.js";

/**
 * Periodic pruning for retention-controlled database tables.
 *
 * The sweeper only acts when retention pruning is enabled in config, keeping
 * the default path explicit and audit-friendly.
 */
export const retentionSweepAction: Effect.Effect<
  void,
  DatabaseError,
  Database | NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const prunedOrphanDeltas = yield* MempoolTxDeltasDB.deleteOrphans;
  if (!shouldPruneRetention(nodeConfig.RETENTION_DAYS)) {
    if (prunedOrphanDeltas > 0) {
      yield* Effect.logInfo(
        `🧹 Orphan mempool tx delta sweep done: mempool_tx_deltas=${prunedOrphanDeltas.toString()}`,
      );
    }
    return;
  }

  const cutoff = computeRetentionCutoff(new Date(), nodeConfig.RETENTION_DAYS);
  const [
    prunedDaPayloads,
    prunedTxRejections,
    prunedAddressHistory,
    prunedDeposits,
    prunedWithdrawals,
  ] = yield* Effect.all(
    [
      DaPayloadsDB.pruneOlderThan(cutoff),
      TxRejectionsDB.pruneOlderThan(cutoff),
      AddressHistoryDB.pruneOlderThan(cutoff),
      DepositsDB.pruneOlderThan(cutoff),
      WithdrawalsDB.pruneOlderThan(cutoff),
    ],
    { concurrency: "unbounded" },
  );

  yield* Effect.logInfo(
    `🧹 Retention sweep done (cutoff=${cutoff.toISOString()}): da_payloads=${prunedDaPayloads}, tx_rejections=${prunedTxRejections}, address_history=${prunedAddressHistory}, deposits_utxos=${prunedDeposits}, withdrawal_utxos=${prunedWithdrawals}, mempool_tx_deltas=${prunedOrphanDeltas}`,
  );
});

/**
 * Fiber wrapper that repeats the retention sweep on the provided schedule.
 */
export const retentionSweeperFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Database | NodeConfig> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🧹 Retention sweeper fiber started.");
    yield* Effect.repeat(
      retentionSweepAction.pipe(
        Effect.withSpan("retention-sweeper-fiber"),
        Effect.catchAllCause(Effect.logWarning),
      ),
      schedule,
    );
  });
