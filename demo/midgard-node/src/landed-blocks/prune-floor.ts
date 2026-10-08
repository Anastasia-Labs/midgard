/**
 * The landed frontier's floor on the follower prune (plan §7.3, §11, N3).
 * While the confirmed-ledger frontier lags the queue (the merged root passed
 * it, or a live block waits unprocessed), the follower keeps every fact from
 * the slot the frontier's header became the queue root (or, failing that,
 * was appended): the queue history the walk from the frontier reads, and the
 * events the blocks after it replay, retired ones included. Caught up (the
 * live root is the frontier and every live node is processed), or with the
 * frontier's header outside the facts, it sets no floor.
 *
 * The prune reports how far the floor holds its boundary back
 * (`floorLagSlots`); readiness shows it as the degraded detail
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
    if (
      liveRoot?.element.headerHash === frontier &&
      live.every(
        (entry) => isRoot(entry) || processed.has(entry.element.headerHash),
      )
    )
      return null;
    const carrying = (root: boolean) =>
      elements
        .filter(
          (entry) =>
            isRoot(entry) === root && entry.element.headerHash === frontier,
        )
        .map((entry) => entry.stored);
    const slot = earliest(carrying(true)) ?? earliest(carrying(false));
    return slot === null ? null : Math.max(0, slot - 1);
  },
});

/** The state-queue projection with the landed frontier's floor on its prune. */
export const landedStateQueueProjection = (
  config: StateQueueProjectionConfig,
  run: <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>,
): FollowerProjection => ({
  ...stateQueueProjection(config),
  pruneFloor: landedFrontierPruneFloor({
    config,
    needs: () => run(landedFrontierNeeds),
  }),
});
