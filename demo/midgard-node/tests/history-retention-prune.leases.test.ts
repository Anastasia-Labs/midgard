import "./utils.js";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { StateQueueMutationLeasesDB } from "../src/database/index.js";
import {
  DAY_MS,
  insertLease,
  journals,
  leaseTokens,
  run,
} from "./history-retention-prune.fixtures.js";

/** The manifest-derived housekeeping window (15 days today). */
const MANIFEST_WINDOW_MS = 15 * DAY_MS;

describe("pruning ended state-queue mutation leases", () => {
  const seed = Effect.gen(function* () {
    // Three long-ended leases, the oldest by acquisition.
    for (const index of [0, 1, 2])
      yield* insertLease({
        token: `old-${index.toString()}`,
        status: index === 1 ? "failed" : "released",
        acquiredAgoMs: 30 * DAY_MS + index * 1_000,
        releasedAgoMs: 30 * DAY_MS,
      });
    // Ended inside the window: kept though it is not among the newest.
    yield* insertLease({
      token: "recently-ended",
      status: "released",
      acquiredAgoMs: 29 * DAY_MS,
      releasedAgoMs: 60_000,
    });
    // One hundred ended leases, newer by acquisition, all past the window.
    for (let index = 0; index < 100; index++)
      yield* insertLease({
        token: `recent-${index.toString().padStart(3, "0")}`,
        status: "released",
        acquiredAgoMs: 20 * DAY_MS - index * 1_000,
        releasedAgoMs: 16 * DAY_MS,
      });
    yield* insertLease({
      token: "active",
      status: "active",
      acquiredAgoMs: 1_000,
      releasedAgoMs: null,
    });
  });

  it("removes ended leases past the window outside the newest inspectable rows, never the active one", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* seed;
        const removed = yield* StateQueueMutationLeasesDB.pruneSettledLeases({
          olderThanMs: MANIFEST_WINDOW_MS,
          batchLimit: 2,
        });
        return { removed, tokens: yield* leaseTokens };
      }),
    );
    // The newest 100 are the active lease and recent-001..recent-099, so
    // recent-000 falls out with the three old ones.
    expect(StateQueueMutationLeasesDB.INSPECTABLE_LEASE_ROWS).toBe(100);
    expect(result.removed).toBe(4);
    expect(result.tokens).toHaveLength(101);
    expect(result.tokens).toContain("active");
    expect(result.tokens).toContain("recently-ended");
    expect(result.tokens).not.toContain("recent-000");
    expect(result.tokens).toContain("recent-001");
    for (const old of ["old-0", "old-1", "old-2"])
      expect(result.tokens).not.toContain(old);
  });

  it("removes nothing when every ended lease is inside the window", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* seed;
        const removed = yield* StateQueueMutationLeasesDB.pruneSettledLeases({
          olderThanMs: 60 * DAY_MS,
        });
        return { removed, tokens: yield* leaseTokens };
      }),
    );
    expect(result.removed).toBe(0);
    expect(result.tokens).toHaveLength(105);
  });

  it("keeps an ended lease past the window while a retained journal names it", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* seed;
        // The journal fixture names "lease-token".
        yield* insertLease({
          token: "lease-token",
          status: "released",
          acquiredAgoMs: 40 * DAY_MS,
          releasedAgoMs: 40 * DAY_MS,
        });
        yield* journals([
          { label: "named", status: "finalized", endedAgoMs: DAY_MS },
        ]);
        const removed = yield* StateQueueMutationLeasesDB.pruneSettledLeases({
          olderThanMs: MANIFEST_WINDOW_MS,
        });
        return { removed, tokens: yield* leaseTokens };
      }),
    );
    expect(result.tokens).toContain("lease-token");
    expect(result.removed).toBe(4);
  });
});
