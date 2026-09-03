import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { SqlClient } from "@effect/sql";
import { Effect, Metric, Schedule } from "effect";

import {
  AddressHistoryDB,
  DaPayloadsDB,
  DepositsDB,
  MempoolTxDeltasDB,
  TxRejectionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import {
  computeChallengeableCutoff,
  computeRetentionCutoff,
  shouldPruneRetention,
} from "../database/retention-policy.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  ContractDeploymentIdentity,
  Database,
  NodeConfig,
} from "../services/index.js";

/**
 * Executable deadline signal (GOAL_SPEC 9.4 / Q54): milliseconds remaining
 * before the oldest still-challengeable retained DA payload reaches its
 * challengeability deadline. Zero or below means retained evidence is at or
 * past the enforced horizon.
 */
const daPayloadRetentionDeadlineRemainingGauge = Metric.gauge(
  "da_payload_retention_deadline_remaining_ms",
  {
    description:
      "Milliseconds remaining before the oldest still-challengeable retained DA payload reaches its retention deadline",
  },
);

/**
 * Publishes the retention deadline gauge from the oldest retained DA payload
 * block end time. Missing rows publish the full window rather than zero, so an
 * empty table never looks like an emergency.
 */
const publishDaPayloadRetentionDeadline = (
  now: Date,
): Effect.Effect<void, never, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      readonly oldest_block_end_time: Date | null;
    }>`SELECT MIN(block_end_time) AS oldest_block_end_time FROM da_payloads`;
    const oldest = rows[0]?.oldest_block_end_time ?? null;
    const remainingMs =
      oldest === null
        ? MIDGARD_RETENTION_WINDOW.requiredRetentionMs
        : oldest.getTime() +
          MIDGARD_RETENTION_WINDOW.requiredRetentionMs -
          now.getTime();
    yield* daPayloadRetentionDeadlineRemainingGauge(
      Effect.succeed(remainingMs),
    );
  }).pipe(Effect.catchAllCause(() => Effect.void));

/**
 * Periodic pruning for retention-controlled database tables.
 *
 * The sweeper only acts when retention pruning is enabled in config, keeping
 * the default path explicit and audit-friendly.
 */
export const retentionSweepAction: Effect.Effect<
  void,
  DatabaseError,
  Database | NodeConfig | ContractDeploymentIdentity
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const deploymentIdentity = yield* ContractDeploymentIdentity;
  const sweptAt = new Date();
  const prunedOrphanDeltas = yield* MempoolTxDeltasDB.deleteOrphans;
  yield* publishDaPayloadRetentionDeadline(sweptAt);
  if (!shouldPruneRetention(nodeConfig.RETENTION_DAYS)) {
    if (prunedOrphanDeltas > 0) {
      yield* Effect.logInfo(
        `🧹 Orphan mempool tx delta sweep done: mempool_tx_deltas=${prunedOrphanDeltas.toString()}`,
      );
    }
    return;
  }

  const cutoff = computeRetentionCutoff(sweptAt, nodeConfig.RETENTION_DAYS);
  // Only DA payloads carry a challengeability horizon; the remaining tables
  // stay on the plain wall-clock retention cutoff.
  const challengeableCutoff = computeChallengeableCutoff(sweptAt);
  const [
    prunedDaPayloads,
    prunedTxRejections,
    prunedAddressHistory,
    prunedDeposits,
    prunedWithdrawals,
  ] = yield* Effect.all(
    [
      DaPayloadsDB.pruneOlderThan(
        cutoff,
        challengeableCutoff,
        deploymentIdentity.manifest,
      ),
      TxRejectionsDB.pruneOlderThan(cutoff),
      AddressHistoryDB.pruneOlderThan(cutoff),
      DepositsDB.pruneOlderThan(cutoff),
      WithdrawalsDB.pruneOlderThan(cutoff),
    ],
    { concurrency: "unbounded" },
  );

  yield* Effect.logInfo(
    `🧹 Retention sweep done (cutoff=${cutoff.toISOString()}, challengeableCutoff=${challengeableCutoff.toISOString()}): da_payloads=${prunedDaPayloads}, tx_rejections=${prunedTxRejections}, address_history=${prunedAddressHistory}, deposits_utxos=${prunedDeposits}, withdrawal_utxos=${prunedWithdrawals}, mempool_tx_deltas=${prunedOrphanDeltas}`,
  );
});

/**
 * Fiber wrapper that repeats the retention sweep on the provided schedule.
 */
export const retentionSweeperFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Database | NodeConfig | ContractDeploymentIdentity
> =>
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
