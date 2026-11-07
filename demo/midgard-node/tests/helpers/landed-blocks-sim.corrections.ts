/**
 * Corrections in the fork simulator (N4, plan §7.4): counting the admitted
 * corrections, their rollbacks and their relandings the node recomputed,
 * and a rollback deeper than k the follower refuses.
 */
import type { BlockSummary, FactStore } from "@al-ft/midgard-l1-follower";
import type { Effect } from "effect";

import { Frontier, retrieveRows } from "../../src/landed-blocks/store.js";
import type { Database } from "../../src/services/database.js";
import type { NativeMpfOwnerService } from "../../src/services/mpf-native-owner/protocol.js";
import type { LandedSimEnv } from "./landed-blocks-sim.ports.js";

type Stats = LandedSimEnv["stats"];

/**
 * A removal that landed on the event right after the last compared one took
 * processed rows out (an admitted correction); a rollback gives them back
 * and the same rows can be removed again. The node equals the model through
 * each, so each was a recompute: no halt, no intervention.
 */
export const correctionCounter = (stats: Stats) => {
  /** The event the last compared rows were compared at. */
  let lastComparedAt = 0;
  /** Rows an admitted correction removed, and those a rollback gave back. */
  const corrected = new Set<string>();
  const uncorrected = new Set<string>();
  return {
    count: (event: {
      readonly seen: number;
      readonly rollback: boolean;
      readonly removed: readonly string[];
      readonly rows: readonly string[];
    }) => {
      if (
        !event.rollback &&
        event.removed.length > 0 &&
        event.seen === lastComparedAt + 1
      ) {
        stats.correctionsAdmitted += 1;
        for (const header of event.removed) {
          if (uncorrected.delete(header)) stats.correctionsReadmitted += 1;
          corrected.add(header);
        }
      }
      if (event.rollback)
        for (const header of event.rows)
          if (corrected.delete(header)) {
            stats.correctionRollbacks += 1;
            uncorrected.add(header);
          }
    },
    compared: (seen: number) => {
      lastComparedAt = seen;
    },
  };
};

/**
 * A rollback deeper than k: the follower refuses it with
 * `rollback_beyond_k` (the node's readiness names it, the process stays up)
 * and changes no fact, so the node's landed rows, its frontier, its native
 * root and the follower's cursor are exactly as they were: an admitted
 * correction stays admitted, nothing is recomputed. Null when there is no
 * block beyond k to roll back to, or when the refusal held.
 */
export const refuseBeyondK = async (args: {
  readonly store: FactStore;
  readonly canonical: readonly BlockSummary[];
  readonly owner: NativeMpfOwnerService;
  readonly run: <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>;
  readonly stats: Stats;
}): Promise<string | null> => {
  const { store, run } = args;
  const cursor = await store.cursor();
  if (cursor === null) return null;
  const target = args.canonical.find(
    (block) =>
      block.height === cursor.height - store.securityParameter - 1 &&
      block.point.slot > cursor.prunedThroughSlot,
  );
  if (target === undefined) return null;
  const state = async () =>
    JSON.stringify({
      rows: (await run(retrieveRows))
        .map(
          (row) => `${row.headerHash}:${row.kind}:${row.state}:${row.applied}`,
        )
        .sort(),
      frontier: (await run(Frontier.retrieve))?.headerHash ?? null,
      root: (await args.owner.diagnostics()).durableRoot,
      cursor: await store.cursor(),
    });
  const before = await state();
  const result = await store.rewind(target.point);
  if (result.kind !== "intervention" || result.reason !== "rollback_beyond_k")
    return `a rollback of ${(cursor.height - target.height).toString()} blocks (k = ${store.securityParameter.toString()}) was not refused as rollback_beyond_k: ${JSON.stringify(result)}`;
  const after = await state();
  if (after !== before)
    return `a refused rollback beyond k changed the node: ${before} -> ${after}`;
  args.stats.beyondKRefused += 1;
  return null;
};
