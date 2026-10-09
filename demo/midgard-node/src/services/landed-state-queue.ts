/**
 * The node's reads of its landed state queue (plan §5.5 P1, N2). Every
 * state-queue read in the node goes through here: the follower's facts at
 * its view, in the node database, never an L1 provider.
 *
 * - `readLandedStateQueue` reads P1 (`landedStateQueueIn`) in one SQL
 *   transaction; the view it was read at is part of the result.
 * - Everything that proposes (commit, merge, correction) needs a healthy
 *   queue: `requireLandedStateQueue` and the snapshot fail with a
 *   `StateQueueError` naming the unhealthy reason, so the proposal stops.
 *   The follower driver's hook holds `/readyz` on
 *   `STATE_QUEUE_UNHEALTHY` (`landedStateQueueHook`); reads that only
 *   report keep serving.
 */
import { postgresDialect } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Duration, Effect, Ref, Schedule } from "effect";

import { followerSqlTx } from "../database/follower-schema.js";
import {
  formatLandedStateQueue,
  landedElements,
  type LandedStateQueue,
  type LandedStateQueueElement,
  landedStateQueueIn,
  type LandedStateQueueRead,
  landedTail,
  stateQueueProjectionConfig,
} from "../l1-state-queue/index.js";
import { isConnectionClassError } from "../provider-retry.js";
import {
  type SerializedStateQueueUTxO,
  serializeStateQueueUTxO,
} from "../workers/utils/commit-block-header.js";
import type { Globals } from "./globals.js";

/** The deployment's state queue, as P1 reads it. */
export type StateQueueContract = Pick<
  SDK.AuthenticatedValidator,
  "spendingScriptAddress" | "policyId"
>;

/** The state queue a fetch config names. */
export const stateQueueContractOf = (
  fetchConfig: SDK.StateQueueFetchConfig,
): StateQueueContract => ({
  spendingScriptAddress: fetchConfig.stateQueueAddress,
  policyId: fetchConfig.stateQueuePolicyId,
});

const queueError = (message: string, cause: unknown) =>
  new SDK.StateQueueError({ message, cause });

/**
 * P1 at the follower's current view, read from the node database in one
 * transaction. A database failure is a `StateQueueError`: no read of the
 * queue falls back to L1.
 */
export const readLandedStateQueue = (
  stateQueue: StateQueueContract,
): Effect.Effect<
  LandedStateQueueRead,
  SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const config = stateQueueProjectionConfig(stateQueue);
    return yield* sql.withTransaction(
      Effect.flatMap(followerSqlTx, (tx) =>
        Effect.tryPromise(() =>
          landedStateQueueIn(tx, postgresDialect, config),
        ),
      ),
    );
  }).pipe(
    Effect.mapError((cause) =>
      queueError("The landed state queue is unreadable", cause),
    ),
  );

/** Why a read gave no healthy queue, as the error a proposal stops on. */
export const unhealthyQueueError = (
  read: LandedStateQueueRead,
  purpose: string,
): SDK.StateQueueError | null => {
  if (read.kind !== "ok")
    return queueError(
      `The landed state queue is unavailable for ${purpose}`,
      `${read.kind}: ${read.detail}`,
    );
  if (!read.queue.healthy)
    return queueError(
      `The landed state queue is unhealthy (${read.queue.reason ?? "unknown"}); ${purpose} stops`,
      formatLandedStateQueue(read.queue),
    );
  return null;
};

/** P1, failing unless it is readable and healthy (everything that proposes). */
export const requireLandedStateQueue = (
  stateQueue: StateQueueContract,
  purpose: string,
): Effect.Effect<LandedStateQueue, SDK.StateQueueError, SqlClient.SqlClient> =>
  Effect.flatMap(readLandedStateQueue(stateQueue), (read) => {
    const error = unhealthyQueueError(read, purpose);
    return error === null && read.kind === "ok"
      ? Effect.succeed(read.queue)
      : Effect.fail(
          error ?? queueError("The landed state queue is unavailable", purpose),
        );
  });

/** The healthy landed queue's elements, root first, in list order. */
export const landedStateQueueUTxOs = (
  stateQueue: StateQueueContract,
  purpose: string,
): Effect.Effect<
  readonly SDK.StateQueueUTxO[],
  SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.map(requireLandedStateQueue(stateQueue, purpose), (queue) =>
    landedElements(queue).map(({ element }) => element),
  );

/** The healthy landed queue's tail (the root of an empty queue). */
export const landedStateQueueTail = (
  stateQueue: StateQueueContract,
  purpose: string,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.flatMap(requireLandedStateQueue(stateQueue, purpose), (queue) => {
    const tail = landedTail(queue);
    return tail === null
      ? Effect.fail(queueError("The landed state queue has no root", purpose))
      : Effect.succeed(tail.element);
  });

/** The live node whose header hash is `headerHash`, if it is in the healthy queue. */
export const findLandedBlock = (
  queue: LandedStateQueue,
  headerHash: string,
): LandedStateQueueElement | undefined =>
  queue.nodes.find((node) => node.headerHash === headerHash.toLowerCase());

export type StateQueueSnapshotReason =
  | "startup"
  | "post_merge"
  | "commit_preflight"
  | "commit_revalidation"
  | "readiness"
  | "manual_status"
  | "recovery";

/** The commit base and root of a healthy landed queue at one follower view. */
export type StateQueueSnapshot = {
  /** `reason:generation:slot:root:tail`: equal ids are the same queue. */
  readonly snapshotId: string;
  readonly reason: StateQueueSnapshotReason;
  readonly view: Readonly<{ generation: number; slot: number }>;
  /** Blocks in the queue (the root excluded). */
  readonly blockCount: number;
  readonly root: {
    readonly outRef: string;
    readonly headerHash: string;
    readonly utxo: SerializedStateQueueUTxO;
  };
  readonly tailCommitBase: {
    readonly outRef: string;
    readonly headerHash: string;
    readonly utxo: SerializedStateQueueUTxO;
    readonly blockEndTimeMs: number;
    readonly roots: {
      readonly utxosRoot: string;
      readonly transactionsRoot: string;
      readonly depositsRoot: string;
      readonly withdrawalsRoot: string;
    };
  };
};

const tailRoots = (
  tail: SDK.StateQueueUTxO,
): Effect.Effect<
  StateQueueSnapshot["tailCommitBase"]["roots"] & {
    readonly blockEndTimeMs: number;
  },
  SDK.DataCoercionError
> =>
  Effect.gen(function* () {
    if (tail.datum.key === "Empty") {
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(
        tail.datum,
      );
      return {
        blockEndTimeMs: Number(data.endTime),
        utxosRoot: data.utxoRoot,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      };
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(tail.datum);
    return {
      blockEndTimeMs: Number(header.endTime),
      utxosRoot: header.utxosRoot,
      transactionsRoot: header.transactionsRoot,
      depositsRoot: header.depositsRoot,
      withdrawalsRoot: header.withdrawalsRoot,
    };
  });

/** The snapshot of a healthy landed queue. */
export const snapshotOfLandedQueue = (
  queue: LandedStateQueue,
  reason: StateQueueSnapshotReason,
): Effect.Effect<
  StateQueueSnapshot,
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
> =>
  Effect.gen(function* () {
    const root = queue.root;
    const tail = landedTail(queue);
    if (!queue.healthy || root === null || tail === null)
      return yield* Effect.fail(
        queueError(
          "Cannot derive a state-queue snapshot from an unhealthy queue",
          formatLandedStateQueue(queue),
        ),
      );
    const [rootUtxo, tailUtxo] = yield* Effect.all([
      serializeStateQueueUTxO(root.element),
      serializeStateQueueUTxO(tail.element),
    ]);
    const { blockEndTimeMs, ...roots } = yield* tailRoots(tail.element);
    const view = {
      generation: queue.view.generation,
      slot: queue.view.point.slot,
    };
    return {
      snapshotId: [
        reason,
        view.generation.toString(),
        view.slot.toString(),
        root.outRef,
        tail.outRef,
      ].join(":"),
      reason,
      view,
      blockCount: queue.nodes.length,
      root: {
        outRef: root.outRef,
        headerHash: root.headerHash,
        utxo: rootUtxo,
      },
      tailCommitBase: {
        outRef: tail.outRef,
        headerHash: tail.headerHash,
        utxo: tailUtxo,
        blockEndTimeMs,
        roots,
      },
    };
  });

/** The snapshot of the healthy landed queue at the follower's current view. */
export const landedStateQueueSnapshot = (
  stateQueue: StateQueueContract,
  reason: StateQueueSnapshotReason,
): Effect.Effect<
  StateQueueSnapshot,
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError,
  SqlClient.SqlClient
> =>
  Effect.flatMap(requireLandedStateQueue(stateQueue, reason), (queue) =>
    snapshotOfLandedQueue(queue, reason),
  );

/** How often, and how many times, a wait for the follower to land a tx re-reads P1. */
export const LANDED_QUEUE_VISIBILITY_DELAY = Duration.seconds(1);
export const LANDED_QUEUE_VISIBILITY_RETRIES = 180;

/** The waits `awaitLandedStateQueueSnapshot` re-reads P1 on. */
const VISIBILITY_WAITS = new WeakSet<object>();

const visibilityWait = (error: SDK.StateQueueError): SDK.StateQueueError => {
  VISIBILITY_WAITS.add(error);
  return error;
};

/**
 * The snapshot of the healthy landed queue once `landed` holds of it: after a
 * tx this node saw confirmed, until the follower has the block that holds
 * it. It re-reads P1 every `LANDED_QUEUE_VISIBILITY_DELAY`, at most
 * `LANDED_QUEUE_VISIBILITY_RETRIES` times, only while it waits on the
 * follower (the queue does not show it yet, or is not readable at the
 * follower's view) or the database read failed transiently
 * (`isConnectionClassError`). An unhealthy queue, any other read failure,
 * or a queue that never shows it fails with a `StateQueueError`, and the
 * next tick reads P1 again.
 */
export const awaitLandedStateQueueSnapshot = (
  stateQueue: StateQueueContract,
  reason: StateQueueSnapshotReason,
  landed: (queue: LandedStateQueue) => boolean,
  what: string,
  visibility: Readonly<{
    delay: Duration.DurationInput;
    retries: number;
  }> = {
    delay: LANDED_QUEUE_VISIBILITY_DELAY,
    retries: LANDED_QUEUE_VISIBILITY_RETRIES,
  },
): Effect.Effect<
  StateQueueSnapshot,
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError,
  SqlClient.SqlClient
> =>
  readLandedStateQueue(stateQueue).pipe(
    Effect.flatMap((read) => {
      if (read.kind !== "ok")
        return Effect.fail(
          visibilityWait(
            unhealthyQueueError(read, reason) ??
              queueError("The landed state queue is unavailable", reason),
          ),
        );
      const unhealthy = unhealthyQueueError(read, reason);
      if (unhealthy !== null) return Effect.fail(unhealthy);
      return landed(read.queue)
        ? Effect.succeed(read.queue)
        : Effect.fail(
            visibilityWait(
              queueError(
                `The landed state queue does not show ${what} yet`,
                formatLandedStateQueue(read.queue),
              ),
            ),
          );
    }),
    Effect.retry({
      schedule: Schedule.intersect(
        Schedule.spaced(visibility.delay),
        Schedule.recurs(visibility.retries),
      ),
      while: (error) =>
        VISIBILITY_WAITS.has(error) || isConnectionClassError(error),
    }),
    Effect.flatMap((queue) => snapshotOfLandedQueue(queue, reason)),
  );

/** The snapshot once the merge of `headerHash` has landed (its node left the queue). */
export const awaitPostMergeSnapshot = (
  stateQueue: StateQueueContract,
  headerHash: string,
) =>
  awaitLandedStateQueueSnapshot(
    stateQueue,
    "post_merge",
    (queue) => findLandedBlock(queue, headerHash) === undefined,
    `the merge of ${headerHash}`,
  );

/** The node's cached commit base and queue length, from a snapshot. */
export const refreshStateQueueGlobalsFromSnapshot = (
  globals: Globals,
  snapshot: StateQueueSnapshot,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    yield* Ref.set(
      globals.AVAILABLE_CONFIRMED_BLOCK,
      snapshot.tailCommitBase.utxo,
    );
    yield* Ref.set(
      globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
      snapshot.tailCommitBase.blockEndTimeMs,
    );
    yield* Ref.set(globals.BLOCKS_IN_QUEUE, snapshot.blockCount);
  });
