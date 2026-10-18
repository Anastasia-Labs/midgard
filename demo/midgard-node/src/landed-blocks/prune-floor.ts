/**
 * The landed frontier's floor on the follower prune (plan §7.3, §11, N3).
 *
 * - Lagging (the merged root passed the frontier, or a live block waits
 *   unprocessed), the follower keeps every fact from the slot the frontier's
 *   header became the queue root (or, failing that, was appended): the queue
 *   history the walk from the frontier reads, and the events the blocks
 *   after it replay, retired ones included.
 * - Caught up or lagging, it also keeps every fact after `T` of a block
 *   starting at the frontier's end, less one slot (`earliestAdmissionSlot`,
 *   the bound the replay's forced-order coverage checks). A block a node
 *   replays starts at or after that end: every node after the frontier may
 *   still leave the queue, and the block after a removed one starts at its
 *   predecessor's end. So the forced orders such a block names stay in the
 *   facts once admitted.
 * - With the frontier's header outside the facts, it sets no floor.
 *
 * The prune reports how far the floor holds its boundary back, by the
 * floor's name (`floorLags`); readiness shows it as the degraded detail
 * `landed_frontier_prune_floor:<lag slots>`.
 */
import {
  changedUtxosIn,
  type FollowerProjection,
  type PruneFloor,
  type StoredOutput,
} from "@al-ft/midgard-l1-follower";
import { Effect } from "effect";

import {
  enteredSlot,
  queueElementOf,
  stateQueueProjection,
  type StateQueueProjectionConfig,
} from "../l1-state-queue/index.js";
import type { Database } from "../services/database.js";
import { earliestAdmissionSlot } from "./replay-foreign.complete.js";
import { Frontier, retrieveRows } from "./store.js";

/** What the floor needs from the node: the frontier and the processed headers. */
export type FrontierNeeds =
  | Readonly<{ frontier: string; processed: ReadonlySet<string> }>
  | undefined;

/** Reads the frontier and the processed rows from the node's tables. */
export const landedFrontierNeeds = Effect.gen(function* () {
  const frontier = yield* Frontier.retrieve;
  if (frontier === undefined) return undefined;
  const rows = yield* retrieveRows;
  return {
    frontier: frontier.headerHash,
    processed: new Set(
      rows
        .filter((row) => row.state === "processed")
        .map((row) => row.headerHash),
    ),
  } satisfies FrontierNeeds;
});

const earliest = (outputs: readonly StoredOutput[]): number | null =>
  outputs.length === 0 ? null : Math.min(...outputs.map(enteredSlot));

export const landedFrontierPruneFloor = (options: {
  readonly config: StateQueueProjectionConfig;
  /** The node's frontier needs; a failed read holds the prune back. */
  readonly needs: () => Promise<FrontierNeeds>;
  /** The slot holding a POSIX time, as the replay maps it; a throw holds the prune back. */
  readonly slotOf: (unixTimeMs: number) => number;
}): PruneFloor => ({
  name: "landed_frontier",
  floor: async ({ tx, dialect }) => {
    const needs = await options.needs().then(
      (value) => ({ ok: true as const, value }),
      () => ({ ok: false as const }),
    );
    // No frontier yet (or the node is unreadable): keep everything.
    if (!needs.ok || needs.value === undefined) return 0;
    const { frontier, processed } = needs.value;
    const read = await changedUtxosIn(
      tx,
      dialect,
      { by: "unit", policyId: Buffer.from(options.config.policyId, "hex") },
      null,
    );
    if (read.kind !== "ok") return 0;
    const elements = read.utxos.flatMap((stored) => {
      const element = queueElementOf(stored, options.config);
      return element === null ? [] : [{ stored, element }];
    });
    const isRoot = (entry: (typeof elements)[number]) =>
      entry.element.element.datum.key === "Empty";
    const live = elements.filter((entry) => entry.stored.spent === null);
    const liveRoot = live.find(isRoot);
    const carrying = (root: boolean) =>
      elements.filter(
        (entry) =>
          isRoot(entry) === root && entry.element.headerHash === frontier,
      );
    const header = carrying(true)[0] ?? carrying(false)[0];
    if (header === undefined) return null;
    const admitted = earliestAdmissionSlot(
      header.element.endTimeMs,
      options.slotOf,
    );
    if (admitted === null) return 0;
    const orders = Math.max(0, admitted - 1);
    if (
      liveRoot?.element.headerHash === frontier &&
      live.every(
        (entry) => isRoot(entry) || processed.has(entry.element.headerHash),
      )
    )
      return orders;
    const slot =
      earliest(carrying(true).map((entry) => entry.stored)) ??
      earliest(carrying(false).map((entry) => entry.stored)) ??
      0;
    return Math.min(orders, Math.max(0, slot - 1));
  },
});

/** The state-queue projection with the landed frontier's floor on its prune. */
export const landedStateQueueProjection = (
  config: StateQueueProjectionConfig,
  run: <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>,
  slotOf: (unixTimeMs: number) => number,
): FollowerProjection => ({
  ...stateQueueProjection(config),
  pruneFloor: landedFrontierPruneFloor({
    config,
    needs: () => run(landedFrontierNeeds),
    slotOf,
  }),
});
