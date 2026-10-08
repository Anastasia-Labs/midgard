import { depth } from "@al-ft/midgard-l1-follower/heads";
import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { followerEligibilityHorizon } from "../src/database/follower-events.js";
import { FORCED_ORDERS_TABLE } from "../src/forced-orders/index.js";
import type { HistoryOwnerCoverage } from "../src/services/event-history-owner.js";
import {
  HistoryProducer,
  type HistoryProducerPermit,
} from "../src/services/event-history-producer.js";
import {
  commitEventHorizon,
  type CommitHorizonLag,
  historyCommitTimingBudget,
  historyEligibilityHorizon,
} from "../src/services/history-commit-window.js";
import { refreshCommitUserEventSourcesThroughBlockEnd } from "../src/workers/commit-block-header/submission.js";
import {
  FOLLOWER_GENERATION,
  ingestFollowerViewUnowned,
  modelHorizonLag,
  writeFollowerTip,
} from "./helpers/follower-view.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

const permit: HistoryProducerPermit = {
  token: {
    deploymentIdentity: "11".repeat(32),
    ownerToken: "isolated model",
    generation: "1",
  },
  coverage: {
    bindingDigest: "22".repeat(32),
    checkpointRevision: "3",
    point: { id: "33".repeat(32), slot: 100 },
    snapshotDigest: "44".repeat(32),
    includedThroughMs: 1_000_000,
  },
};

describe("authenticated commit window", () => {
  it("keeps the observed point distinct from the conservative eligibility horizon", () => {
    const before = structuredClone(permit.coverage);
    const horizon = historyEligibilityHorizon(permit.coverage);
    // A future admission cannot have an inclusive upper bound before its
    // actual future slot; its enforced event-wait delay lies beyond this
    // horizon.
    expect(horizon).toBeLessThan(
      permit.coverage.includedThroughMs + EVENT_WAIT_DURATION_MS,
    );
    expect(permit.coverage).toEqual(before);
    expect(() =>
      historyEligibilityHorizon({
        ...permit.coverage,
        includedThroughMs: Number.MAX_SAFE_INTEGER,
      }),
    ).toThrow();
  });

  // E-N1-2 item 3: the final recheck bounds the end time by min(owner
  // coverage, follower ingestion, unbuilt forced orders); it polls nothing.
  it.each([
    ["owner coverage", 5_000],
    ["follower ingestion", 500],
  ] as const)(
    "bounds the final recheck by %s, the lesser horizon",
    async (_bound, ingestedSlot) => {
      await run(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          yield* ingestFollowerViewUnowned(ingestedSlot);
        }),
      );
      const expected = Math.min(
        historyEligibilityHorizon(permit.coverage),
        ingestedSlot * 1000 + EVENT_WAIT_DURATION_MS - 1,
      );
      expect(
        await run(commitEventHorizon(permit.coverage, modelHorizonLag(0))),
      ).toBe(expected);
      // Never past the follower's covered tip: an event due by the horizon
      // was admitted no later than the tip the driver ingested.
      expect(expected - EVENT_WAIT_DURATION_MS + 1).toBeLessThanOrEqual(
        ingestedSlot * 1000,
      );
      const recheck = (end: number) =>
        run(
          refreshCommitUserEventSourcesThroughBlockEnd(
            end,
            modelHorizonLag(0),
          ).pipe(Effect.provideService(HistoryProducer, permit)),
        );
      await recheck(expected);
      await expect(recheck(expected + 1)).rejects.toThrow(
        /exceeds the ingested event horizon/,
      );
    },
  );

  // N10: a live forced order the node has not rebuilt yet (carriage still
  // pending) may fall due at its inclusion time, so no block reaches it.
  it("bounds the horizon below the earliest live forced order the node has not ingested", async () => {
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* ingestFollowerViewUnowned(500);
      }),
    );
    const follower = 500_000 + EVENT_WAIT_DURATION_MS - 1;
    const horizon = () =>
      run(commitEventHorizon(undefined, modelHorizonLag(0)));
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
    // A bound past the follower's leaves the follower's.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE ${sql(FORCED_ORDERS_TABLE)} SET inclusion_time = ${follower + 10}`;
      }),
    );
    expect(await horizon()).toBe(follower);
    await expect(
      run(
        refreshCommitUserEventSourcesThroughBlockEnd(
          follower + 1,
          modelHorizonLag(0),
        ).pipe(Effect.provideService(HistoryProducer, permit)),
      ),
    ).rejects.toThrow(/exceeds the ingested event horizon/);
  });

  it("allows no end time before the first ingestion or after a rewind removes the ingested view", async () => {
    const horizon = () =>
      run(commitEventHorizon(permit.coverage, modelHorizonLag(0)));
    await run(resetApplicationTables);
    expect(await horizon()).toBeNull();
    await run(ingestFollowerViewUnowned(500));
    expect(await horizon()).toBe(500_000 + EVENT_WAIT_DURATION_MS - 1);
    // A follower rewind: a new generation, and the ingested block is gone.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE l1_follower_cursor SET generation = ${FOLLOWER_GENERATION + 1}, slot = 400`;
        yield* sql`DELETE FROM l1_blocks WHERE slot = 500`;
      }),
    );
    expect(await horizon()).toBeNull();
    await expect(
      run(
        refreshCommitUserEventSourcesThroughBlockEnd(
          0,
          modelHorizonLag(0),
        ).pipe(Effect.provideService(HistoryProducer, permit)),
      ),
    ).rejects.toThrow(/exceeds the ingested event horizon/);
    // The driver's next run ingests the new generation's view.
    await run(ingestFollowerViewUnowned(450, [], FOLLOWER_GENERATION + 1));
    expect(await horizon()).toBe(450_000 + EVENT_WAIT_DURATION_MS - 1);
  });

  it("refuses an exhausted or invalid short-window attempt instead of moving its header end", () => {
    const end = historyEligibilityHorizon(permit.coverage) + 1;
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
 * The horizon lag d (U3) on a synthetic follower chain at 1 s model slots:
 * real-looking gaps, with one-slot gaps where they make the edge tight. The
 * covered tip is the last block (height 60, heads depth 1).
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
/** Coverage far past the follower, so the follower side bounds the min. */
const wideCoverage = {
  ...permit.coverage,
  includedThroughMs: 10_000_000,
};
/** The earliest event a block can hold: its inclusive validity upper bound is
 * at least the last millisecond of its own slot, and the enforced inclusion
 * time is that bound plus the event wait. */
const earliestInclusion = (slot: number) =>
  (slot + 1) * 1000 - 1 + EVENT_WAIT_DURATION_MS;
/** Heads depths (the covered tip is depth 1) of blocks that can hold an
 * event due by `horizon`. */
const dueDepths = (horizon: number) =>
  chain
    .filter((block) => earliestInclusion(block.slot) <= horizon)
    .map((block) => depth(tip.height, block.height));
/** Fails the test if the slot clock is ever read. */
const unreadClock: CommitHorizonLag<Error> = {
  lagBlocks: 0,
  slotToUnixTime: Effect.fail(new Error("the slot clock was read at d = 0")),
};
/** The commit horizon at the program tip before the lag: min(journal
 * coverage, follower ingestion), null without an ingestion. */
const unlaggedOracle = (
  follower: number | null,
  coverage: HistoryOwnerCoverage | undefined,
) =>
  follower === null
    ? null
    : coverage === undefined
      ? follower
      : Math.min(follower, historyEligibilityHorizon(coverage));

describe("horizon lag d on the commit end time", () => {
  it("keeps d = 0 byte-identical to the unlagged horizon, reading no lagged block and no clock", async () => {
    const states = [
      ["no follower ingestion", () => run(resetApplicationTables)],
      ["an ingestion at a lone tip", () => followChain([tip])],
      ["an ingestion on the chain", () => followChain()],
    ] as const;
    for (const [, arrange] of states) {
      await arrange();
      const follower = await run(followerEligibilityHorizon);
      for (const coverage of [undefined, permit.coverage, wideCoverage]) {
        const horizon = await run(commitEventHorizon(coverage, unreadClock));
        expect(horizon).toBe(unlaggedOracle(follower, coverage));
      }
    }
    // At the lone tip no block lies below it: any d > 0 would hold.
    await followChain([tip]);
    expect(
      await run(commitEventHorizon(wideCoverage, modelHorizonLag(1))),
    ).toBeNull();
    expect(await run(commitEventHorizon(wideCoverage, unreadClock))).toBe(
      tip.slot * 1000 + EVENT_WAIT_DURATION_MS - 1,
    );
  });

  it.each([0, 1, 3])(
    "admits no event from the block %i below the follower's covered tip or above it",
    async (lagBlocks) => {
      await followChain();
      const horizon = await run(
        commitEventHorizon(wideCoverage, modelHorizonLag(lagBlocks)),
      );
      const lagged = chain.at(-1 - lagBlocks)!;
      expect(horizon).toBe(lagged.slot * 1000 + EVENT_WAIT_DURATION_MS - 1);
      const depths = dueDepths(horizon!);
      expect(depths.length).toBeGreaterThan(0);
      // The lagged block sits at heads depth d + 1; it and every block above
      // it hold no due event, so each due event has more than d blocks on top.
      expect(Math.min(...depths)).toBe(lagBlocks + 2);
    },
  );

  it("admits an event from the block just below the lagged one exactly at the cap", async () => {
    // Blocks 1_091 and 1_092 are one slot apart: at d = 2 the lagged block is
    // slot 1_092, and the earliest event of slot 1_091 lands on the cap.
    await followChain();
    const horizon = await run(
      commitEventHorizon(wideCoverage, modelHorizonLag(2)),
    );
    expect(earliestInclusion(1_091)).toBe(horizon);
    expect(earliestInclusion(1_092)).toBe(horizon! + 1000);
  });

  it("caps the journal side of the min with the same follower block", async () => {
    await followChain();
    const cap = 1_130_000 + EVENT_WAIT_DURATION_MS - 1;
    const below = {
      ...permit.coverage,
      includedThroughMs: 1_100_000,
    };
    expect(
      await run(commitEventHorizon(wideCoverage, modelHorizonLag(1))),
    ).toBe(cap);
    expect(await run(commitEventHorizon(below, modelHorizonLag(1)))).toBe(
      historyEligibilityHorizon(below),
    );
    expect(await run(commitEventHorizon(undefined, modelHorizonLag(1)))).toBe(
      cap,
    );
  });

  it("holds while the follower has no block d below its tip, and refuses the final end above the lagged cap", async () => {
    await followChain(chain.slice(-3));
    expect(
      await run(commitEventHorizon(wideCoverage, modelHorizonLag(3))),
    ).toBeNull();
    const cap = 1_092_000 + EVENT_WAIT_DURATION_MS - 1;
    expect(
      await run(commitEventHorizon(wideCoverage, modelHorizonLag(2))),
    ).toBe(cap);
    const wide: HistoryProducerPermit = { ...permit, coverage: wideCoverage };
    const refresh = (end: number, lagBlocks: number) =>
      run(
        refreshCommitUserEventSourcesThroughBlockEnd(
          end,
          modelHorizonLag(lagBlocks),
        ).pipe(Effect.provideService(HistoryProducer, wide)),
      );
    await expect(refresh(cap, 2)).resolves.toBeUndefined();
    await expect(refresh(cap + 1, 2)).rejects.toThrow(
      /exceeds the ingested event horizon/,
    );
    await expect(refresh(cap, 3)).rejects.toThrow(
      /exceeds the ingested event horizon/,
    );
  });
});
