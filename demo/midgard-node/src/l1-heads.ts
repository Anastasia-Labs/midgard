/**
 * The node's heads (plan §9): its L1 "now" and the block below its tip.
 *
 * `l1SlotNow(api)` is the only "now" for an L1 validity decision in the node
 * (plan §3.6): max(tip slot, last tip slot + elapsed / slotLength), from
 * `@al-ft/midgard-l1-follower/heads`, with the elapsed time read from a
 * monotonic clock. A wall clock that runs fast or jumps cannot move it. The
 * wall clock may still pick `valid_to` for a new tx, never declare anything
 * due, expired or mature.
 *
 * The tip comes from the L1 access the client was built over
 * (`l1-access.ts`): the follower's covered tip in a role process, the
 * node's ledger tip or the external provider's tip in a tool. A client
 * built over no access has no "now"; an emulator client answers with its
 * own exact chain slot.
 *
 * `l1BlockBelowCoveredTip(store, d)` is the heads source for "the block d
 * below the covered tip": the follower block at depth d + 1 under the
 * store's cursor (the covered tip has depth 1).
 */
import type {
  Cursor,
  Point as L1Point,
  StoredBlock,
} from "@al-ft/midgard-l1-follower";
import { depth, heightAtDepth } from "@al-ft/midgard-l1-follower/heads";
import {
  isEmulatorProvider,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { l1AccessOf, L1SlotUnknownError } from "./l1-access.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "./lucid-time.js";

export { L1_TIP_REFRESH_MS, L1SlotUnknownError } from "./l1-access.js";

/** Records a tip slot read outside `l1SlotNow` for this client's access. */
export const observeL1Tip = (api: LucidEvolution, tipSlot: number): void => {
  l1AccessOf(api)?.observeTip(tipSlot);
};

/**
 * The client's L1 `slotNow`, from the access it was built over. Fails only
 * while that access has read no tip yet. An emulator client answers with its
 * own chain slot; any other client without an access has no "now".
 */
export const l1SlotNow = (
  api: LucidEvolution,
): Effect.Effect<number, L1SlotUnknownError> => {
  const access = l1AccessOf(api);
  if (access !== undefined) return access.slotNow();
  if (isEmulatorProvider(api.config().provider))
    return Effect.sync(() => api.currentSlot());
  return Effect.fail(
    new L1SlotUnknownError({
      message:
        "L1 slot unknown: this Lucid client is not built over an L1 access adapter",
    }),
  );
};

/** The POSIX time (ms) at the start of `l1SlotNow`, for ms-domain bounds. */
export const l1NowUnixTimeMs = (
  api: LucidEvolution,
): Effect.Effect<number, L1SlotUnknownError> =>
  Effect.map(l1SlotNow(api), (slot) =>
    slotToUnixTimeForLucidOrEmulatorFallback(api, slot),
  );

/** The block d below the follower's covered tip, or why there is none. */
export type L1BlockBelowCoveredTip =
  | Readonly<{
      kind: "block";
      point: L1Point;
      height: number;
      /** `depth(tip, point)`: always d + 1. */
      depth: number;
      /** The covered tip (the store's cursor) the block was read under. */
      tip: Readonly<{ point: L1Point; height: number }>;
    }>
  | Readonly<{
      kind: "unavailable";
      /**
       * `not_initialized`: the store has no cursor yet. `outside_history`:
       * that height lies below the follower's origin or its pruned history.
       * Both are transient for a caller: it holds its horizon and retries.
       */
      reason: "not_initialized" | "outside_history";
      detail: string;
    }>;

/** The follower reads it needs: its cursor and a block by height. A
 * `FactStore` is one; a caller with only SQL passes the same rows. */
export type CoveredTipHeads = Readonly<{
  cursor(): Promise<Pick<Cursor, "point" | "height"> | null>;
  blockAtHeight(
    height: number,
  ): Promise<Pick<StoredBlock, "slot" | "hash" | "height"> | null>;
}>;

/**
 * The block d below the covered tip (the follower cursor), read through the
 * heads module's `depth()`: the block at depth d + 1. d = 0 is the covered
 * tip itself.
 */
export const l1BlockBelowCoveredTip = async (
  store: CoveredTipHeads,
  lagBlocks: number,
): Promise<L1BlockBelowCoveredTip> => {
  if (!Number.isSafeInteger(lagBlocks) || lagBlocks < 0)
    throw new RangeError(
      `the lag d must be a non-negative integer, got ${String(lagBlocks)}`,
    );
  const cursor = await store.cursor();
  if (cursor === null)
    return {
      kind: "unavailable",
      reason: "not_initialized",
      detail: "the follower store has no covered tip yet",
    };
  const height = heightAtDepth(cursor.height, lagBlocks + 1);
  const block = height < 0 ? null : await store.blockAtHeight(height);
  if (block === null)
    return {
      kind: "unavailable",
      reason: "outside_history",
      detail: `no follower block at height ${height.toString()} (${lagBlocks.toString()} below the covered tip at ${cursor.height.toString()}): below the origin or pruned`,
    };
  return {
    kind: "block",
    point: { slot: block.slot, hash: block.hash },
    height: block.height,
    depth: depth(cursor.height, block.height),
    tip: { point: cursor.point, height: cursor.height },
  };
};
