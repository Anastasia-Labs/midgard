/**
 * Where `confirmed_ledger` stands, for status (plan §5.5 P10, N5): the
 * frontier, the slot of the merge that made it the queue root, and that
 * merge's level at the follower's tip (`landed`, `safe` at confirmation
 * depth, `final` past the security parameter). A frontier behind the root
 * whose own merge point is not retained reports the root's merge, which is
 * no deeper than its own, so the level never overstates.
 */
import {
  blockAtOrBeforeSlotIn,
  type ChainLevel,
  type DepthParameters,
  type FactStore,
  tipIn,
} from "@al-ft/midgard-l1-follower";
import { depth, levelAtDepth } from "@al-ft/midgard-l1-follower/heads";
import { Effect } from "effect";

import type { LandedStateQueueElement } from "../l1-state-queue/index.js";
import { retrieveMergeLinks } from "./confirmed-merges.js";
import { Frontier } from "./store.js";

export type ConfirmedLedgerPosition = Readonly<{
  headerHash: string;
  utxosRoot: string;
  /** Whether the frontier is the merged queue root at the run's view. */
  atRoot: boolean;
  /** The merge's slot; null when it predates the follower's origin. */
  mergeSlot: number | null;
  /** The merge's level at the follower's tip; null while unknown. */
  level: ChainLevel | null;
}>;

/** The frontier and its merge slot, against the queue root `root`. */
export const frontierMerge = (root: LandedStateQueueElement) =>
  Effect.gen(function* () {
    const frontier = yield* Frontier.retrieve;
    if (frontier === undefined) return undefined;
    const rootSlot = root.created?.slot ?? null;
    const atRoot = frontier.headerHash === root.headerHash;
    const link = atRoot
      ? undefined
      : (yield* retrieveMergeLinks).get(frontier.headerHash);
    return {
      ...frontier,
      atRoot,
      mergeSlot: link?.merge?.slot ?? rootSlot,
    };
  });

/** The level of a merge at `slot` (null: before the origin) at the store's tip. */
export const mergeLevel = (
  store: Pick<FactStore, "dialect" | "transaction">,
  slot: number | null,
  parameters: DepthParameters,
): Promise<ChainLevel | null> =>
  slot === null
    ? Promise.resolve("final")
    : store.transaction("read", async (tx) => {
        const tip = await tipIn(tx, store.dialect);
        if (tip === null) return null;
        const block = await blockAtOrBeforeSlotIn(tx, slot);
        // Below the retained window: past every rollback.
        if (block === null) return "final";
        return levelAtDepth(depth(tip.height, block.height), parameters);
      });
