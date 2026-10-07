import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  HistoryProducer,
  type HistoryProducerPermit,
} from "../src/services/event-history-producer.js";
import {
  commitEventHorizon,
  historyCommitTimingBudget,
  historyEligibilityHorizon,
} from "../src/services/history-commit-window.js";
import { refreshCommitUserEventSourcesThroughBlockEnd } from "../src/workers/commit-block-header/submission.js";
import {
  FOLLOWER_GENERATION,
  ingestFollowerViewUnowned,
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

  // E-N1-2 item 3: the final refresh bounds the end time by min(owner
  // coverage, follower ingestion); it never polls deposits or withdrawals.
  it.each([
    ["owner coverage", 5_000],
    ["follower ingestion", 500],
  ] as const)(
    "bounds the final refresh by %s, the lesser horizon, polling only tx orders",
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
      expect(await run(commitEventHorizon(permit.coverage))).toBe(expected);
      // Never past the follower's covered tip: an event due by the horizon
      // was admitted no later than the tip the driver ingested.
      expect(expected - EVENT_WAIT_DURATION_MS + 1).toBeLessThanOrEqual(
        ingestedSlot * 1000,
      );
      const calls: string[] = [];
      const refresh = (end: number) =>
        run(
          refreshCommitUserEventSourcesThroughBlockEnd(end, {
            txOrder: (upperBound: Date) =>
              Effect.sync(() => {
                calls.push(`tx-order:${upperBound.getTime().toString()}`);
                return upperBound;
              }),
          }).pipe(Effect.provideService(HistoryProducer, permit)),
        );
      await refresh(expected);
      expect(calls).toEqual([`tx-order:${expected.toString()}`]);
      calls.length = 0;
      await expect(refresh(expected + 1)).rejects.toThrow(
        /exceeds the ingested event horizon/,
      );
      expect(calls).toEqual([]);
    },
  );

  it("allows no end time before the first ingestion or after a rewind removes the ingested view", async () => {
    const horizon = () => run(commitEventHorizon(permit.coverage));
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
        refreshCommitUserEventSourcesThroughBlockEnd(0, {
          txOrder: (upperBound: Date) => Effect.succeed(upperBound),
        }).pipe(Effect.provideService(HistoryProducer, permit)),
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
