import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import { heightAtDepth } from "@al-ft/midgard-l1-follower/heads";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Cause,
  Clock,
  Duration,
  Effect,
  Either,
  Exit,
  Metric,
  Ref,
  Schedule,
} from "effect";

import {
  AddressHistoryDB,
  DaPayloadsDB,
  MempoolTxDeltasDB,
  StateQueueMutationLeasesDB,
  TxRejectionsDB,
} from "../database/index.js";
import { pruneFinalizedBeyondChallengeability } from "../database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import {
  computeChallengeableCutoff,
  computeHousekeepingCutoff,
  resolveHousekeepingRetentionDays,
  shouldPruneRetention,
} from "../database/retention-policy.js";
import { DatabaseError } from "../database/utils/common.js";
import { l1NowUnixTimeMs, L1SlotUnknownError } from "../l1-heads.js";
import { pruneFinalQueueTerminals } from "../l1-queue-terminals/index.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { requireLandedStateQueue } from "../services/landed-state-queue.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { settlementDepthParameters } from "../services/settlement.status.js";
import {
  RETENTION_HISTORY_PRUNE_BUDGET_MS,
  withRetentionHistoryProducer,
} from "./retention-sweeper.history-producer.js";

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
            "da_payloads.header_hash",
            view.finalThroughHeight,
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
 * The retention exemption sets from the landed state queue (P1): the
 * confirmed header and every live block header, and the greatest final
 * height at the queue's view (deeper than the deployment's k).
 */
export const fetchRetentionL1View: Effect.Effect<
  DaPayloadsDB.RetentionL1View,
  SDK.StateQueueError,
  MidgardContracts | ContractDeploymentIdentity | SqlClient.SqlClient
> = Effect.gen(function* () {
  const contracts = yield* MidgardContracts;
  const { securityParameter } = yield* settlementDepthParameters;
  const queue = yield* requireLandedStateQueue(
    contracts.stateQueue,
    "the retention sweep",
  );
  if (queue.root === null)
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: "State queue has no ConfirmedState root node",
        cause: `nodes=${queue.nodes.length.toString()}`,
      }),
    );
  return {
    confirmedHeadHash: Buffer.from(queue.root.headerHash, "hex"),
    liveQueueHeaderHashes: queue.nodes.map((node) =>
      Buffer.from(node.headerHash, "hex"),
    ),
    finalThroughHeight: heightAtDepth(queue.view.height, securityParameter + 1),
  };
});

/**
 * One retention sweep.
 *
 * DA payloads are pruned on the consensus-derived challengeability horizon and
 * the L1 exemption sets whenever an L1 view is available, regardless of
 * RETENTION_DAYS; then the final queue-terminal rows that name no retained
 * payload or journal (`pruneFinalQueueTerminals`). Housekeeping runs only while the window
 * `resolveHousekeepingRetentionDays` derives from the verified manifest (or
 * an explicit longer RETENTION_DAYS) is non-zero, never inside the DA
 * challenge horizon (`computeHousekeepingCutoff`):
 *  - tx rejections and address history past the window (address history of a
 *    transaction still in the mempool is kept);
 *  - ended state-queue mutation leases past the window;
 *  - with an L1 view and a verified deployment, finalized journals past the
 *    window, under the follower write gate
 *    (`withRetentionHistoryProducer`), each kept while challenge-relevant.
 * Deposit and withdrawal rows are retained: settlement proofs recompute the
 * whole header's root, and a completed job records confirmation, without the
 * block identity/depth needed to prove payout finality. Local consumed/finalized
 * status cannot authorize deleting either unpaid events or their proof siblings.
 */
export const retentionSweepAction = (
  view: DaPayloadsDB.RetentionL1View | undefined,
  sweptAt: Date,
): Effect.Effect<
  void,
  DatabaseError,
  Database | NodeConfig | ContractDeploymentIdentity | Globals
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
    const daPayloadsPruned =
      view === undefined
        ? Either.right(0)
        : yield* Effect.either(
            DaPayloadsDB.pruneBeyondRetention({
              challengeableCutoff,
              view,
              deploymentIdentityDigest,
            }),
          );
    yield* publishDaPayloadRetentionDeadline(
      sweptAt,
      view,
      deploymentIdentityDigest,
    );
    if (Either.isLeft(daPayloadsPruned))
      return yield* Effect.fail(daPayloadsPruned.left);
    const prunedDaPayloads = daPayloadsPruned.right;
    const prunedQueueTerminals =
      view?.finalThroughHeight === undefined
        ? 0
        : yield* pruneFinalQueueTerminals(view.finalThroughHeight);
    // Startup already refused a window shorter than the manifest's
    // (assertDeploymentManifestMatchesConfig); should it still not resolve,
    // nothing is pruned.
    const retentionDays = yield* Effect.try(() =>
      resolveHousekeepingRetentionDays({
        configured: nodeConfig.RETENTION_DAYS,
        manifestRetentionDays:
          deploymentIdentity.manifest?.da.transportProfile.retentionDays,
      }),
    ).pipe(
      Effect.tapError((error) =>
        Effect.logWarning(`retention_housekeeping_skipped: ${error.message}`),
      ),
      Effect.orElseSucceed(() => 0),
    );
    if (!shouldPruneRetention(retentionDays)) {
      yield* Effect.logInfo(
        `🧹 Retention sweep done (challengeableCutoff=${challengeableCutoff.toISOString()}, housekeeping disabled: no verified manifest window and RETENTION_DAYS unset, or RETENTION_DAYS=0): da_payloads=${prunedDaPayloads}, queue_terminals=${prunedQueueTerminals}, mempool_tx_deltas=${prunedOrphanDeltas}`,
      );
      return;
    }

    const cutoff = computeHousekeepingCutoff(sweptAt, retentionDays);
    const [prunedTxRejections, prunedAddressHistory] = yield* Effect.all(
      [
        TxRejectionsDB.pruneOlderThan(cutoff),
        AddressHistoryDB.pruneOlderThan(cutoff),
      ],
      { concurrency: "unbounded" },
    );
    const prunedLeases = yield* StateQueueMutationLeasesDB.pruneSettledLeases({
      olderThanMs: retentionDays * RETENTION_MS_PER_DAY,
    });
    const prunedJournals =
      view === undefined || deploymentIdentityDigest === undefined
        ? undefined
        : yield* withRetentionHistoryProducer(
            Effect.gen(function* () {
              const deadlineMs =
                (yield* Clock.currentTimeMillis) +
                RETENTION_HISTORY_PRUNE_BUDGET_MS;
              return yield* pruneFinalizedBeyondChallengeability({
                challengeableCutoff: cutoff,
                view,
                deploymentIdentityDigest,
                deadlineMs,
              });
            }),
          );

    yield* Effect.logInfo(
      `🧹 Retention sweep done (retentionDays=${retentionDays.toString()}, cutoff=${cutoff.toISOString()}, challengeableCutoff=${challengeableCutoff.toISOString()}): da_payloads=${prunedDaPayloads}, queue_terminals=${prunedQueueTerminals}, tx_rejections=${prunedTxRejections}, address_history=${prunedAddressHistory}, state_queue_mutation_leases=${prunedLeases}, pending_block_finalizations=${prunedJournals ?? "skipped"}, mempool_tx_deltas=${prunedOrphanDeltas}`,
    );
  });

export type RetentionSweeperOptions = {
  /** L1 view source; the live state queue by default. */
  readonly fetchL1View?: Effect.Effect<
    DaPayloadsDB.RetentionL1View,
    unknown,
    | Lucid
    | MidgardContracts
    | NodeConfig
    | ContractDeploymentIdentity
    | Database
  >;
  /** Local clock for the L1 view's age; `Date.now` by default. */
  readonly nowMs?: () => number;
  /**
   * The L1 now (POSIX ms) every cutoff is computed from; by default the
   * `slotNow` of the node's Lucid client (plan §3.6).
   */
  readonly l1NowMs?: Effect.Effect<
    number,
    L1SlotUnknownError,
    | Lucid
    | MidgardContracts
    | NodeConfig
    | ContractDeploymentIdentity
    | Database
  >;
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
 *
 * Every cutoff is computed from the L1 `slotNow` (plan §3.6), never the wall
 * clock: a clock that runs fast must not make a still-challengeable payload
 * look prunable. While the L1 slot is unknown the sweep is skipped.
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
    const l1NowMs =
      options.l1NowMs ??
      Effect.flatMap(Lucid, (lucid) => l1NowUnixTimeMs(lucid.api));
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
      // Read before the view: a sweep that cannot date itself on L1 prunes
      // nothing.
      const sweptAt = yield* Effect.either(l1NowMs);
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
        if (Either.isLeft(sweptAt)) {
          yield* Effect.logWarning(
            `retention_pass_skipped: ${sweptAt.left.message}`,
          );
          return;
        }
        yield* retentionSweepAction(undefined, new Date(sweptAt.right)).pipe(
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
      if (Either.isLeft(sweptAt)) {
        yield* Effect.logWarning(
          `retention_pass_skipped: ${sweptAt.left.message}`,
        );
        return;
      }
      yield* retentionSweepAction(view, new Date(sweptAt.right)).pipe(
        Effect.catchAllCause(Effect.logWarning),
      );
    }).pipe(Effect.withSpan("retention-sweeper-fiber"));
    yield* Effect.repeat(sweep, schedule);
  });
