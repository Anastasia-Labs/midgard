import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Cause, Duration, Effect, Exit, Metric, Ref, Schedule } from "effect";

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
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
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
 * head's payload, live queue headers' payloads and finality-held payloads are
 * excluded: they are retained past the horizon by design, so their age is not
 * a deadline.
 */
const publishDaPayloadRetentionDeadline = (
  now: Date,
  view: DaPayloadsDB.RetentionL1View | undefined,
  deploymentIdentityDigest: Buffer | undefined,
): Effect.Effect<void, never, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const exempt =
      view === undefined
        ? sql`TRUE`
        : sql`NOT ${sql.in("header_hash", [
            view.confirmedHeadHash,
            ...view.liveQueueHeaderHashes,
          ])} AND NOT ${DaPayloadsDB.finalityHeldPayload(
            sql,
            deploymentIdentityDigest,
          )}`;
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
 * The readiness reason raised once no authenticated L1 view has been obtained
 * for longer than `L1_VIEW_FATAL_MS`. A node that cannot read L1 can neither
 * commit, merge, nor decide retention, so past that age the sweeper stops
 * sweeping altogether instead of running on a stale view, and keeps reading
 * L1 until a view returns.
 */
export const RETENTION_L1_VIEW_STALE = "retention_l1_view_stale";

const RETENTION_L1_VIEW_SOURCE = "retention_sweeper";

/** A walk may take this many times its last successful duration before it is
 * abandoned. */
export const RETENTION_L1_VIEW_TIMEOUT_WALK_FACTOR = 4;

/**
 * The time one L1 read may take before it is abandoned: at least one sweep
 * interval and `RETENTION_L1_VIEW_TIMEOUT_WALK_FACTOR` times the last
 * successful walk, doubled for every consecutive abandoned read, so a queue
 * whose walk outgrew the interval is still read; never more than
 * `L1_VIEW_FATAL_MS`.
 */
export const retentionL1ViewTimeoutMs = (input: {
  readonly sweepMs: number;
  readonly fatalMs: number;
  readonly lastWalkMs: number | undefined;
  readonly consecutiveTimeouts: number;
}): number => {
  const base = Math.max(
    input.sweepMs,
    RETENTION_L1_VIEW_TIMEOUT_WALK_FACTOR * (input.lastWalkMs ?? 0),
  );
  const scaled = base * 2 ** Math.min(input.consecutiveTimeouts, 30);
  return Math.max(1, Math.min(scaled, Math.max(input.sweepMs, input.fatalMs)));
};

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
    const deploymentIdentityDigest =
      deploymentIdentity.manifestId === undefined
        ? undefined
        : Buffer.from(deploymentIdentity.manifestId, "hex");
    const prunedDaPayloads =
      view === undefined
        ? 0
        : yield* DaPayloadsDB.pruneBeyondRetention({
            challengeableCutoff,
            view,
            deploymentIdentityDigest,
          });
    yield* publishDaPayloadRetentionDeadline(
      sweptAt,
      view,
      deploymentIdentityDigest,
    );
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
 * prunes no DA payload. An L1 read that does not finish within its timeout
 * (see `retentionL1ViewTimeoutMs`) counts as a failed view and is abandoned,
 * so a hung read cannot stall the loop. Once the last good view is older than
 * `L1_VIEW_FATAL_MS` (validated at config load) the sweeper stops sweeping and
 * raises `retention_l1_view_stale` in readiness; it keeps reading L1, and the
 * first good view clears the reason and sweeps again. Never fails.
 */
export const retentionSweeperFiber = (
  schedule: Schedule.Schedule<number>,
  options: RetentionSweeperOptions = {},
): Effect.Effect<
  void,
  never,
  | Database
  | NodeConfig
  | ContractDeploymentIdentity
  | Lucid
  | MidgardContracts
  | Globals
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const globals = yield* Globals;
    const fetchL1View = options.fetchL1View ?? fetchRetentionL1View;
    const nowMs = options.nowMs ?? (() => Date.now());
    const l1ViewFatalMs = nodeConfig.L1_VIEW_FATAL_MS;
    const sweepMs = nodeConfig.WAIT_BETWEEN_RETENTION_SWEEPS;
    const lastL1ViewAtMs = yield* Ref.make(nowMs());
    const walk = yield* Ref.make<{
      readonly lastWalkMs: number | undefined;
      readonly consecutiveTimeouts: number;
    }>({ lastWalkMs: undefined, consecutiveTimeouts: 0 });
    yield* Effect.logInfo("🧹 Retention sweeper fiber started.");
    const sweep = Effect.gen(function* () {
      const startedAtMs = nowMs();
      const timeoutMs = retentionL1ViewTimeoutMs({
        sweepMs,
        fatalMs: l1ViewFatalMs,
        ...(yield* Ref.get(walk)),
      });
      const viewExit = yield* Effect.exit(
        Effect.disconnect(fetchL1View).pipe(
          Effect.timeout(timeoutMs),
          Effect.timed,
        ),
      );
      if (Exit.isFailure(viewExit)) {
        const timedOut = [...Cause.failures(viewExit.cause)].some(
          Cause.isTimeoutException,
        );
        yield* Ref.update(walk, (current) => ({
          ...current,
          consecutiveTimeouts: timedOut ? current.consecutiveTimeouts + 1 : 0,
        }));
        const l1ViewAgeMs = nowMs() - (yield* Ref.get(lastL1ViewAtMs));
        yield* Effect.logWarning(
          `retention_pass_skipped: no authenticated L1 view (age=${l1ViewAgeMs.toString()}ms, deadline=${l1ViewFatalMs.toString()}ms, read_timeout=${timeoutMs.toString()}ms)`,
          viewExit.cause,
        );
        if (l1ViewAgeMs > l1ViewFatalMs) {
          yield* raiseLivenessIncident(
            globals,
            RETENTION_L1_VIEW_SOURCE,
            RETENTION_L1_VIEW_STALE,
            `the last authenticated L1 view is ${l1ViewAgeMs.toString()} ms old (deadline ${l1ViewFatalMs.toString()} ms); retention sweeps stop until a view returns`,
          );
          return;
        }
        yield* retentionSweepAction(undefined, new Date(startedAtMs)).pipe(
          Effect.catchAllCause(Effect.logWarning),
        );
        return;
      }
      const [walkDuration, view] = viewExit.value;
      yield* Ref.set(walk, {
        lastWalkMs: Duration.toMillis(walkDuration),
        consecutiveTimeouts: 0,
      });
      yield* Ref.set(lastL1ViewAtMs, startedAtMs);
      yield* clearLivenessIncident(globals, RETENTION_L1_VIEW_SOURCE);
      yield* retentionSweepAction(view, new Date(startedAtMs)).pipe(
        Effect.catchAllCause(Effect.logWarning),
      );
    }).pipe(Effect.withSpan("retention-sweeper-fiber"));
    yield* Effect.repeat(sweep, schedule);
  });
