/**
 * The state queue's history at a follower view (plan §7.3, N3): every header
 * the retained queue outputs carried, merged or not, read from the follower
 * facts (spent queue outputs stay until the prune passes them, and the
 * landed frontier's prune floor keeps them while the frontier needs them).
 *
 * Processing walks a root's lineage back through it to the last header the
 * node processed, so the blocks a merge passed before the node processed
 * them are processed in order like any other; bootstrap sets the frontier
 * on the same lineage.
 */
import type { FactStore, View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  type LandedStateQueueElement,
  stateQueueHistoryIn,
  type StateQueueProjectionConfig,
} from "../l1-state-queue/index.js";
import type { MergePoint } from "./confirmed-merges.js";
import { ViewMoved } from "./ports.js";

/**
 * A queue node: its header hash, header, block time (its commit time),
 * availability status (whether a committee commitment binds its payload)
 * and the slot its node output entered the facts at.
 */
export type LandedNode = Readonly<{
  headerHash: string;
  header: SDK.Header;
  blockTime: bigint;
  daAvailability: SDK.DaAvailabilityStateQueueStatus;
  /**
   * The slot of the block that created the node output read (for a header
   * read from the history, its earliest retained one); null for a seeded
   * output, which entered the facts at the origin.
   */
  enteredSlot: number | null;
}>;

/** The landed node a queue node datum carries, its output entered at `enteredSlot`. */
export const landedNodeOf = (
  headerHash: string,
  datum: Parameters<typeof SDK.getStateQueueNodeFromStateQueueDatum>[0],
  enteredSlot: number | null,
) =>
  Effect.map(
    SDK.getStateQueueNodeFromStateQueueDatum(datum),
    (node): LandedNode => ({
      headerHash,
      header: node.header,
      blockTime: node.block_time,
      daAvailability: node.da_attestation,
      enteredSlot,
    }),
  );

export type QueueHistory = Readonly<{
  /** Every header a retained queue node carried, by header hash. */
  headers: ReadonlyMap<string, SDK.Header>;
  /** The block time of each node in `headers`. */
  blockTimes: ReadonlyMap<string, bigint>;
  /**
   * Each node's availability status; a commitment any retained output
   * carried is kept over Unattested.
   */
  statuses: ReadonlyMap<string, SDK.DaAvailabilityStateQueueStatus>;
  /** Each header's end time, from a node or a root that carried it. */
  endTimes: ReadonlyMap<string, bigint>;
  /**
   * The slot each node's earliest retained output was created at; null when
   * one was seeded.
   */
  enteredSlots: ReadonlyMap<string, number | null>;
  /**
   * The root output that made each header the queue root (the earliest
   * retained one, if a header was ever re-rooted), where its creation is
   * known.
   */
  roots: ReadonlyMap<string, MergePoint>;
}>;

/** The retained queue elements at `view`; fails `ViewMoved` if the follower left it. */
export const readQueueHistory = (
  store: Pick<FactStore, "dialect" | "transaction">,
  config: StateQueueProjectionConfig,
  view: View,
) =>
  Effect.gen(function* () {
    const read = yield* Effect.tryPromise(() =>
      // The view check takes FOR SHARE on Postgres: a read-write transaction.
      store.transaction("write", (tx) =>
        stateQueueHistoryIn(tx, store.dialect, config, view),
      ),
    );
    if (read.kind === "view_moved")
      return yield* Effect.fail(new ViewMoved({}));
    if (read.kind !== "ok")
      return yield* Effect.fail(
        new Error(`the queue history is unreadable: ${read.detail}`),
      );
    return read.elements;
  });

/** Decodes the retained elements; a node whose header does not decode carries no lineage. */
export const decodeHistory = (
  elements: readonly LandedStateQueueElement[],
): QueueHistory => {
  const headers = new Map<string, SDK.Header>();
  const blockTimes = new Map<string, bigint>();
  const statuses = new Map<string, SDK.DaAvailabilityStateQueueStatus>();
  const endTimes = new Map<string, bigint>();
  const enteredSlots = new Map<string, number | null>();
  const roots = new Map<string, MergePoint>();
  for (const element of elements) {
    if (element.element.datum.key === "Empty") {
      endTimes.set(element.headerHash, element.endTimeMs);
      const known = roots.get(element.headerHash);
      if (
        element.created !== null &&
        (known === undefined || element.created.slot < known.slot)
      )
        roots.set(element.headerHash, {
          slot: element.created.slot,
          outRef: element.outRef,
        });
      continue;
    }
    const node = Effect.runSync(
      Effect.either(
        SDK.getStateQueueNodeFromStateQueueDatum(element.element.datum),
      ),
    );
    if (node._tag === "Left") continue;
    headers.set(element.headerHash, node.right.header);
    blockTimes.set(element.headerHash, node.right.block_time);
    if (
      !statuses.has(element.headerHash) ||
      SDK.daAvailabilityStatusBinds(node.right.da_attestation)
    )
      statuses.set(element.headerHash, node.right.da_attestation);
    endTimes.set(element.headerHash, node.right.header.endTime);
    // A seeded output entered at the origin, before any created one.
    const entered = enteredSlots.get(element.headerHash);
    const slot = element.created?.slot ?? null;
    if (
      entered === undefined ||
      (entered !== null && (slot === null || slot < entered))
    )
      enteredSlots.set(element.headerHash, slot);
  }
  return { headers, blockTimes, statuses, endTimes, enteredSlots, roots };
};

/** `to` and its ancestors after the first one `stop` accepts, oldest first. */
export type Lineage = Readonly<{
  /** The ancestor `stop` accepted. */
  anchor: string;
  /** The headers after it, `to` last (empty when `stop` accepts `to`). */
  headers: readonly LandedNode[];
}>;

/** Walks back from `to` by parent hash; undefined if the history ends first. */
export const lineageBack = (
  history: QueueHistory,
  to: string,
  stop: (headerHash: string) => boolean,
): Lineage | undefined => {
  const headers: LandedNode[] = [];
  let cursor = to;
  while (!stop(cursor)) {
    const header = history.headers.get(cursor);
    const blockTime = history.blockTimes.get(cursor);
    if (
      header === undefined ||
      blockTime === undefined ||
      headers.length >= history.headers.size
    )
      return undefined;
    headers.unshift({
      headerHash: cursor,
      header,
      blockTime,
      daAvailability: history.statuses.get(cursor) ?? "Unattested",
      enteredSlot: history.enteredSlots.get(cursor) ?? null,
    });
    cursor = header.prevHeaderHash;
  }
  return { anchor: cursor, headers };
};
