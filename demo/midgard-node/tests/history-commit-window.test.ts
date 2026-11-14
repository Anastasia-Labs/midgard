import type { View } from "@al-ft/midgard-l1-follower";
import { depth } from "@al-ft/midgard-l1-follower/heads";
import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { CommitAnchor } from "../src/database/commit-anchor.js";
import { FORCED_ORDERS_TABLE } from "../src/forced-orders/index.js";
import {
  FollowerWrite,
  type FollowerWritePermit,
  type GateView,
  gateViewOf,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import {
  commitEventHorizon,
  historyCommitTimingBudget,
} from "../src/services/history-commit-window.js";
import {
  assertCommitUserEventSourceCompleteness,
  COMMIT_ANCHOR_NOT_OF_VIEW_MESSAGE,
  COMMIT_ANCHOR_UNAVAILABLE_MESSAGE,
  COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE,
  COMMIT_END_ABOVE_FORCED_HORIZON_MESSAGE,
} from "../src/workers/commit-block-header/submission.commit-event-sources.js";
import {
  FOLLOWER_GENERATION,
  followerBlockHash,
  ingestFollowerViewUnowned,
  modelAnchorClock,
  modelSlotTime,
  writeFollowerTip,
} from "./helpers/follower-view.js";
import { openFollowerWriteGateAt } from "./helpers/follower-write-gate.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

/** The commit end-time horizon at commit-event depth `d`, planned at
 * `view` (a permit's), or at the ingested view (a fixture). */
const horizonAt = (d: number, view?: GateView) =>
  run(commitEventHorizon({ view, ...modelAnchorClock(d) }));

/** The anchor cap of the block at `slot`. */
const capOf = (slot: number) => slot * 1000 + EVENT_WAIT_DURATION_MS - 1;

/** A follower view of the model chain (`writeFollowerTip`'s hashes). */
const viewAt = (
  slot: number,
  height: number = slot,
  generation: number = FOLLOWER_GENERATION,
): View => ({
  generation,
  point: { slot, hash: followerBlockHash(slot, generation) },
  height,
});

/** The anchor block of the model chain at `slot`. */
const anchorAt = (
  slot: number,
  height: number = slot,
  generation: number = FOLLOWER_GENERATION,
): CommitAnchor => ({
  hash: followerBlockHash(slot, generation),
  height,
  slot,
});

/** The journal transaction's end-time recheck (no included events) under `permit`. */
const journalRecheck = (
  permit: FollowerWritePermit,
  blockEndTimeMs: number,
  commitAnchor: CommitAnchor | undefined,
  d: number,
) =>
  run(
    withFollowerWrite(
      assertCommitUserEventSourceCompleteness({
        blockEndTimeMs,
        commitAnchor,
        depth: d,
        slotToUnixTime: modelSlotTime,
        includedDepositEntries: [],
        includedForcedTransactionEntries: [],
        includedWithdrawalEntries: [],
      }),
    ).pipe(Effect.provideService(FollowerWrite, permit)),
  );

describe("authenticated commit window", () => {
  // E-N1-2 item 3, plan §8.1: the end time is bounded by min(the commit
  // anchor's cap, unbuilt forced orders); at d = 0 the anchor is the view.
  it("bounds the end time by the commit anchor of the ingested view, and rechecks it in the journal transaction", async () => {
    const ingestedSlot = 500;
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* ingestFollowerViewUnowned(ingestedSlot);
      }),
    );
    const expected = capOf(ingestedSlot);
    const anchor = anchorAt(ingestedSlot);
    expect(await horizonAt(0)).toEqual({ horizonMs: expected, anchor });
    const permit = await run(openFollowerWriteGateAt(viewAt(ingestedSlot)));
    expect(await horizonAt(0, permit.view)).toEqual({
      horizonMs: expected,
      anchor,
    });
    await expect(journalRecheck(permit, expected, anchor, 0)).resolves.toEqual(
      anchor,
    );
    await expect(
      journalRecheck(permit, expected + 1, anchor, 0),
    ).rejects.toThrow(COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE);
    await expect(
      journalRecheck(permit, expected, undefined, 0),
    ).rejects.toThrow(COMMIT_ANCHOR_UNAVAILABLE_MESSAGE);
  });

  // N10: a live forced order the node has not rebuilt yet (carriage still
  // pending) may fall due at its inclusion time, so no block reaches it.
  it("bounds the horizon below the earliest live forced order the node has not ingested", async () => {
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* ingestFollowerViewUnowned(500);
      }),
    );
    const follower = capOf(500);
    const horizon = async () => (await horizonAt(0))?.horizonMs;
    const order = (
      index: number,
      inclusionTime: number,
      spent: number | null,
    ) =>
      run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO ${sql(FORCED_ORDERS_TABLE)} ${sql.insert({
            order_tx_hash: Buffer.alloc(32, 7),
            order_output_index: index,
            order_tx_index: 0,
            block_hash: Buffer.alloc(32, 8),
            height: 10,
            order_slot: 400,
            spent_slot: spent,
            parent_slot: 399,
            parent_hash: Buffer.alloc(32, 9),
            inclusion_time: inclusionTime,
            status: "carriage_pending",
            reference_inputs: Buffer.alloc(0),
            block_datums: "{}",
          })}`;
        }),
      );
    expect(await horizon()).toBe(follower);
    // Spent before the node ingested it: it can no longer fall due.
    await order(0, 300_000, 450);
    expect(await horizon()).toBe(follower);
    await order(1, 400_000, null);
    expect(await horizon()).toBe(399_999);
    await order(2, 350_000, null);
    expect(await horizon()).toBe(349_999);
    const permit = await run(openFollowerWriteGateAt(viewAt(500)));
    await expect(
      journalRecheck(permit, 349_999, anchorAt(500), 0),
    ).resolves.toBeDefined();
    await expect(
      journalRecheck(permit, 350_000, anchorAt(500), 0),
    ).rejects.toThrow(COMMIT_END_ABOVE_FORCED_HORIZON_MESSAGE);
    // A bound past the anchor's leaves the anchor's.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE ${sql(FORCED_ORDERS_TABLE)} SET inclusion_time = ${follower + 10}`;
      }),
    );
    expect(await horizon()).toBe(follower);
    await expect(
      journalRecheck(permit, follower + 1, anchorAt(500), 0),
    ).rejects.toThrow(COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE);
  });

  it("allows no end time before the first ingestion or after a rewind removes the planning view", async () => {
    await run(resetApplicationTables);
    expect(await horizonAt(0)).toBeNull();
    await run(ingestFollowerViewUnowned(500));
    expect((await horizonAt(0))?.horizonMs).toBe(capOf(500));
    // A follower rewind: a new generation, and the ingested block is gone.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE l1_follower_cursor SET generation = ${FOLLOWER_GENERATION + 1}, slot = 400`;
        yield* sql`DELETE FROM l1_blocks WHERE slot = 500`;
      }),
    );
    expect(await horizonAt(0)).toBeNull();
    expect(await horizonAt(0, gateViewOf(viewAt(500)))).toBeNull();
    // The driver's next run ingests the new generation's view.
    await run(ingestFollowerViewUnowned(450, [], FOLLOWER_GENERATION + 1));
    expect((await horizonAt(0))?.horizonMs).toBe(capOf(450));
  });

  it("refuses an exhausted or invalid short-window attempt instead of moving its header end", () => {
    const end = 1_000_000 + EVENT_WAIT_DURATION_MS;
    const adequate = historyCommitTimingBudget({
      checkpoint: "pre_submit",
      resolvedEndTimeMs: end,
      nowMs: end - 10_000,
    });
    const exhausted = historyCommitTimingBudget({
      checkpoint: "pre_submit",
      resolvedEndTimeMs: end,
      nowMs: end - 9_999,
    });
    expect(adequate.satisfied).toBe(true);
    expect(exhausted.satisfied).toBe(false);
    expect(exhausted.resolvedEndTimeMs).toBe(adequate.resolvedEndTimeMs);
    expect(
      historyCommitTimingBudget({
        checkpoint: "pre_submit",
        resolvedEndTimeMs: end,
        nowMs: Number.NaN,
      }).satisfied,
    ).toBe(false);
  });
});

/**
 * The commit anchor on a synthetic follower chain at 1 s model slots:
 * real-looking gaps, with one-slot gaps where they make the edge tight. The
 * view is the last block (height 60, heads depth 1); the anchor is the
 * block at heads depth d + 1.
 */
const chainSlots = [
  1_000, 1_020, 1_041, 1_042, 1_070, 1_071, 1_090, 1_091, 1_092, 1_130, 1_150,
];
const FIRST_HEIGHT = 50;
const chain = chainSlots.map((slot, index) => ({
  slot,
  height: FIRST_HEIGHT + index,
}));
const tip = chain.at(-1)!;
const blockAt = (height: number) => chain[height - FIRST_HEIGHT]!;
/** The follower holds `blocks`, its cursor at the last, ingested there. */
const followChain = (blocks: typeof chain = chain) =>
  run(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      for (const block of blocks.slice(0, -1))
        yield* writeFollowerTip(block.slot, FOLLOWER_GENERATION, block.height);
      const last = blocks.at(-1)!;
      yield* ingestFollowerViewUnowned(
        last.slot,
        [],
        FOLLOWER_GENERATION,
        last.height,
      );
    }),
  );
/** The earliest event a block can hold: its inclusive validity upper bound is
 * at least the last millisecond of its own slot, and the enforced inclusion
 * time is that bound plus the event wait. */
const earliestInclusion = (slot: number) =>
  (slot + 1) * 1000 - 1 + EVENT_WAIT_DURATION_MS;
/** Heads depths under the view at `viewHeight` (the view is depth 1) of the
 * blocks that can hold an event due by `horizon`. */
const dueDepths = (horizon: number, viewHeight: number = tip.height) =>
  chain
    .filter(
      (block) =>
        block.height <= viewHeight && earliestInclusion(block.slot) <= horizon,
    )
    .map((block) => depth(viewHeight, block.height));

describe("commit anchor d blocks below the planning view", () => {
  it("is the planning view itself at d = 0, and holds at a lone view for any d > 0", async () => {
    await followChain([tip]);
    expect(await horizonAt(0)).toEqual({
      horizonMs: capOf(tip.slot),
      anchor: anchorAt(tip.slot, tip.height),
    });
    expect(await horizonAt(1)).toBeNull();
  });

  it.each([0, 1, 3])(
    "admits no event from the anchor %i below the view or above it",
    async (d) => {
      await followChain();
      const horizon = (await horizonAt(d))!;
      const anchor = chain.at(-1 - d)!;
      expect(horizon.anchor).toEqual(anchorAt(anchor.slot, anchor.height));
      expect(horizon.horizonMs).toBe(capOf(anchor.slot));
      const depths = dueDepths(horizon.horizonMs);
      expect(depths.length).toBeGreaterThan(0);
      // The anchor sits at heads depth d + 1; it and every block above it
      // hold no due event, so each due event has more than d + 1 blocks on
      // top of it, the anchor included.
      expect(Math.min(...depths)).toBe(d + 2);
    },
  );

  it("admits an event from the block just below the anchor exactly at the cap", async () => {
    // Blocks 1_091 and 1_092 are one slot apart: at d = 2 the anchor is
    // slot 1_092, and the earliest event of slot 1_091 lands on the cap.
    await followChain();
    const horizon = (await horizonAt(2))!.horizonMs;
    expect(earliestInclusion(1_091)).toBe(horizon);
    expect(earliestInclusion(1_092)).toBe(horizon + 1000);
  });

  it("holds while the follower has no block d below the view, and refuses an end above the cap or an anchor of another depth", async () => {
    await followChain(chain.slice(-3));
    expect(await horizonAt(3)).toBeNull();
    const cap = capOf(1_092);
    expect((await horizonAt(2))?.horizonMs).toBe(cap);
    const permit = await run(
      openFollowerWriteGateAt(viewAt(tip.slot, tip.height)),
    );
    const anchor = anchorAt(1_092, 58);
    await expect(journalRecheck(permit, cap, anchor, 2)).resolves.toEqual(
      anchor,
    );
    await expect(journalRecheck(permit, cap + 1, anchor, 2)).rejects.toThrow(
      COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE,
    );
    await expect(journalRecheck(permit, cap, anchor, 3)).rejects.toThrow(
      COMMIT_ANCHOR_NOT_OF_VIEW_MESSAGE,
    );
    // A block of another chain at the anchor height is not the anchor.
    await expect(
      journalRecheck(permit, cap, anchorAt(1_092, 58, 2), 2),
    ).rejects.toThrow(COMMIT_ANCHOR_NOT_OF_VIEW_MESSAGE);
  });

  /**
   * The permit's view P, not the follower's cursor, fixes the anchor. The
   * driver applied P at height 57 while the follower already followed to
   * 60; a rewind then lands between P and the cursor (to 58) and leaves P,
   * and the permit, valid. An end time anchored at the cursor would admit
   * an event of block 56, which the rewound chain buries under two blocks
   * only.
   */
  it("anchors at the permit's view while the follower's cursor is ahead of it", async () => {
    const d = 2;
    await followChain();
    const behind = blockAt(57);
    const permit = await run(
      openFollowerWriteGateAt(viewAt(behind.slot, behind.height)),
    );
    const horizon = (await horizonAt(d, permit.view))!;
    const anchor = blockAt(55);
    expect(horizon.anchor).toEqual(anchorAt(anchor.slot, anchor.height));
    expect(horizon.horizonMs).toBe(capOf(anchor.slot));
    expect(Math.min(...dueDepths(horizon.horizonMs, behind.height))).toBe(
      d + 2,
    );
    // The cursor's anchor (height 58) is not the permit view's, and the
    // permit view's anchor does not reach the cursor's cap.
    const cursorAnchor = blockAt(58);
    await expect(
      journalRecheck(
        permit,
        capOf(cursorAnchor.slot),
        anchorAt(cursorAnchor.slot, cursorAnchor.height),
        d,
      ),
    ).rejects.toThrow(COMMIT_ANCHOR_NOT_OF_VIEW_MESSAGE);
    await expect(
      journalRecheck(permit, capOf(cursorAnchor.slot), horizon.anchor, d),
    ).rejects.toThrow(COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE);
    // The rewind to 58: the permit's view stays on the chain.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rewound = blockAt(58);
        yield* sql`DELETE FROM l1_blocks WHERE height > ${rewound.height}`;
        yield* sql`UPDATE l1_follower_cursor SET slot = ${rewound.slot},
          hash = ${followerBlockHash(rewound.slot)}, height = ${rewound.height}`;
      }),
    );
    expect(await horizonAt(d, permit.view)).toEqual(horizon);
    await expect(
      journalRecheck(permit, horizon.horizonMs, horizon.anchor, d),
    ).resolves.toEqual(horizon.anchor);
  });
});
