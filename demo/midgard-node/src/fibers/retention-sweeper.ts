import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data, Effect, Exit, Metric, Ref, Schedule } from "effect";

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
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { fetchCanonicalStateQueueNodesProgram } from "../services/state-queue-topology.js";

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
 * empty table never looks like an emergency. With an L1 view, the confirmed
 * head's payload and live queue headers' payloads are excluded: they are
 * retained past the horizon by design, so their age is not a deadline.
 */
const publishDaPayloadRetentionDeadline = (
  now: Date,
  view: DaPayloadsDB.RetentionL1View | undefined,
): Effect.Effect<void, never, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const exempt =
      view === undefined
        ? sql`TRUE`
        : sql`NOT ${sql.in("header_hash", [
            view.confirmedHeadHash,
            ...view.liveQueueHeaderHashes,
          ])}`;
    const rows = yield* sql<{
      readonly oldest_block_end_time: Date | null;
    }>`SELECT MIN(block_end_time) AS oldest_block_end_time FROM da_payloads WHERE ${exempt}`;
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
 * Raised once no authenticated L1 view has been obtained for longer than
 * `L1_VIEW_FATAL_MS`. A node that cannot read L1 can neither commit, merge,
 * nor decide retention, so the sweeper fails its fiber group and the process
 * exits instead of running on a stale view.
 */
export class RetentionL1ViewUnavailableError extends Data.TaggedError(
  "RetentionL1ViewUnavailableError",
)<{
  readonly l1ViewAgeMs: number;
  readonly l1ViewFatalMs: number;
  readonly cause: unknown;
}> {}

/**
 * Reads the retention exemption sets from L1 by walking the state-queue linked
 * list from its root NFT, the traversal the commit and merge fibers use.
 */
export const fetchRetentionL1View: Effect.Effect<
  DaPayloadsDB.RetentionL1View,
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HashingError,
  Lucid | MidgardContracts
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodes = yield* fetchCanonicalStateQueueNodesProgram(
    lucid.api,
    contracts.stateQueue,
  );
  let confirmedHeadHash: string | undefined;
  const liveQueueHeaderHashes: Buffer[] = [];
  for (const node of nodes) {
    if (node.datum.key === "Empty") {
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(
        node.datum,
      );
      confirmedHeadHash = data.headerHash;
    } else {
      const header = yield* SDK.getHeaderFromStateQueueDatum(node.datum);
      liveQueueHeaderHashes.push(
        Buffer.from(yield* SDK.hashBlockHeader(header), "hex"),
      );
    }
  }
  if (confirmedHeadHash === undefined) {
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: "State queue has no ConfirmedState root node",
        cause: `nodes=${nodes.length.toString()}`,
      }),
    );
  }
  return {
    confirmedHeadHash: Buffer.from(confirmedHeadHash, "hex"),
    liveQueueHeaderHashes,
  };
});

/**
 * One retention sweep.
 *
 * DA payloads are pruned on the consensus-derived challengeability horizon and
 * the L1 exemption sets whenever an L1 view is available, regardless of
 * RETENTION_DAYS. The wall-clock tables are pruned only when RETENTION_DAYS
 * enables it.
 */
export const retentionSweepAction = (
  view: DaPayloadsDB.RetentionL1View | undefined,
  sweptAt: Date = new Date(),
): Effect.Effect<
  void,
  DatabaseError,
  Database | NodeConfig | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    const prunedOrphanDeltas = yield* MempoolTxDeltasDB.deleteOrphans;
    const challengeableCutoff = computeChallengeableCutoff(sweptAt);
    const prunedDaPayloads =
      view === undefined
        ? 0
        : yield* DaPayloadsDB.pruneBeyondRetention({
            challengeableCutoff,
            view,
            deploymentIdentityDigest:
              deploymentIdentity.manifestId === undefined
                ? undefined
                : Buffer.from(deploymentIdentity.manifestId, "hex"),
          });
    yield* publishDaPayloadRetentionDeadline(sweptAt, view);
    if (!shouldPruneRetention(nodeConfig.RETENTION_DAYS)) {
      yield* Effect.logInfo(
        `🧹 Retention sweep done (challengeableCutoff=${challengeableCutoff.toISOString()}, wall-clock tables disabled by RETENTION_DAYS=0): da_payloads=${prunedDaPayloads}, mempool_tx_deltas=${prunedOrphanDeltas}`,
      );
      return;
    }

    const cutoff = computeRetentionCutoff(sweptAt, nodeConfig.RETENTION_DAYS);
    const [
      prunedTxRejections,
      prunedAddressHistory,
      prunedDeposits,
      prunedWithdrawals,
    ] = yield* Effect.all(
      [
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

export type RetentionSweeperOptions = {
  /** L1 view source; the live state queue by default. */
  readonly fetchL1View?: Effect.Effect<
    DaPayloadsDB.RetentionL1View,
    unknown,
    Lucid | MidgardContracts
  >;
  readonly nowMs?: () => number;
};

/**
 * Fiber wrapper that repeats the retention sweep on the provided schedule.
 *
 * A sweep that cannot obtain a fresh L1 view logs `retention_pass_skipped` and
 * prunes no DA payload. An L1 read that does not finish within one sweep
 * interval counts as a failed view and is abandoned, so a hung read cannot
 * stall the loop past the deadline. Once the last good view is older than
 * `L1_VIEW_FATAL_MS` (validated at config load; default: the on-chain DA
 * attestation timeout) the fiber fails with `RetentionL1ViewUnavailableError`;
 * every other failure is logged and retried on the next sweep.
 */
export const retentionSweeperFiber = (
  schedule: Schedule.Schedule<number>,
  options: RetentionSweeperOptions = {},
): Effect.Effect<
  void,
  RetentionL1ViewUnavailableError,
  Database | NodeConfig | ContractDeploymentIdentity | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const fetchL1View = options.fetchL1View ?? fetchRetentionL1View;
    const nowMs = options.nowMs ?? (() => Date.now());
    const l1ViewFatalMs = nodeConfig.L1_VIEW_FATAL_MS;
    // L1_VIEW_FATAL_MS is at least three sweep intervals, so abandoning a read
    // after one interval still leaves the deadline check two sweeps of room.
    const l1ViewTimeoutMs = nodeConfig.WAIT_BETWEEN_RETENTION_SWEEPS;
    const lastL1ViewAtMs = yield* Ref.make(nowMs());
    yield* Effect.logInfo("🧹 Retention sweeper fiber started.");
    const sweep = Effect.gen(function* () {
      const startedAtMs = nowMs();
      const viewExit = yield* Effect.exit(
        Effect.disconnect(fetchL1View).pipe(Effect.timeout(l1ViewTimeoutMs)),
      );
      if (Exit.isFailure(viewExit)) {
        const l1ViewAgeMs = nowMs() - (yield* Ref.get(lastL1ViewAtMs));
        yield* Effect.logWarning(
          `retention_pass_skipped: no authenticated L1 view (age=${l1ViewAgeMs.toString()}ms, deadline=${l1ViewFatalMs.toString()}ms)`,
          viewExit.cause,
        );
        if (l1ViewAgeMs > l1ViewFatalMs) {
          return yield* Effect.fail(
            new RetentionL1ViewUnavailableError({
              l1ViewAgeMs,
              l1ViewFatalMs,
              cause: viewExit.cause,
            }),
          );
        }
        yield* retentionSweepAction(undefined, new Date(startedAtMs)).pipe(
          Effect.catchAllCause(Effect.logWarning),
        );
        return;
      }
      yield* Ref.set(lastL1ViewAtMs, startedAtMs);
      yield* retentionSweepAction(viewExit.value, new Date(startedAtMs)).pipe(
        Effect.catchAllCause(Effect.logWarning),
      );
    }).pipe(Effect.withSpan("retention-sweeper-fiber"));
    yield* Effect.repeat(sweep, schedule);
  });
