import { AddressHistoryDB, DepositsDB, TxRejectionsDB } from "@/database/index.js";
import {
  computeRetentionCutoff,
  shouldPruneRetention,
} from "@/database/retention-policy.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database, NodeConfig } from "@/services/index.js";
import { Effect, pipe, Schedule } from "effect";

export const retentionSweepAction: Effect.Effect<
  void,
  DatabaseError,
  Database | NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  if (!shouldPruneRetention(nodeConfig.RETENTION_DAYS)) {
    return;
  }

  const cutoff = computeRetentionCutoff(new Date(), nodeConfig.RETENTION_DAYS);
  const [prunedTxRejections, prunedAddressHistory, prunedDeposits] =
    yield* Effect.all(
      [
        TxRejectionsDB.pruneOlderThan(cutoff),
        AddressHistoryDB.pruneOlderThan(cutoff),
        DepositsDB.pruneOlderThan(cutoff),
      ],
      { concurrency: "unbounded" },
    );

  yield* Effect.logInfo(
    `🧹 Retention sweep done (cutoff=${cutoff.toISOString()}): tx_rejections=${prunedTxRejections}, address_history=${prunedAddressHistory}, deposits_utxos=${prunedDeposits}`,
  );
});

export const retentionSweeperFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Database | NodeConfig> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🧹 Retention sweeper fiber started.");
      yield* Effect.repeat(
        retentionSweepAction.pipe(
          Effect.withSpan("retention-sweeper-fiber"),
          Effect.catchAllCause(Effect.logWarning),
        ),
        schedule,
      );
    }),
  );
