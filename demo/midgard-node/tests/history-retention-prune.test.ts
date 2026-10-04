import "./utils.js";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { StateQueueMutationLeasesDB } from "../src/database/index.js";
import { pruneFinalizedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import {
  DAY_MS,
  DEPLOYMENT,
  insertLease,
  journals,
  leaseTokens,
  recordMergedOutcome,
  recordMergeJob,
  recordObserverState,
  remainingLabels,
  run,
} from "./history-retention-prune.fixtures.js";
import { header } from "./local-mutation-job-abandonment.journal-fixture.js";

/** The journal prune under the test deployment; it never runs without one. */
const prune = (
  args: Omit<
    Parameters<typeof pruneFinalizedBeyondChallengeability>[0],
    "deploymentIdentityDigest"
  >,
) =>
  pruneFinalizedBeyondChallengeability({
    deploymentIdentityDigest: DEPLOYMENT,
    ...args,
  });

/** The manifest-derived housekeeping window (15 days today). */
const MANIFEST_WINDOW_MS = 15 * DAY_MS;

const ALL = [
  "old-plain",
  "old-confirmed-head",
  "old-live-queue",
  "old-abandoned",
  "old-pending",
  "recent-finalized",
];

describe("pruning finalized journals beyond challengeability", () => {
  it("removes only old finalized journals no exemption covers", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals([
          { label: "old-plain", status: "finalized", endedAgoMs: 10 * DAY_MS },
          {
            label: "old-confirmed-head",
            status: "finalized",
            endedAgoMs: 10 * DAY_MS,
          },
          {
            label: "old-live-queue",
            status: "finalized",
            endedAgoMs: 9 * DAY_MS,
          },
          {
            label: "old-abandoned",
            status: "abandoned",
            endedAgoMs: 10 * DAY_MS,
            // A completed merge job: only the status keeps it.
            mergeJob: "completed",
          },
          {
            label: "recent-finalized",
            status: "finalized",
            endedAgoMs: 60_000,
          },
          {
            label: "old-pending",
            status: "pending_submission",
            endedAgoMs: 10 * DAY_MS,
            // A completed merge job: only the status keeps it.
            mergeJob: "completed",
          },
        ]);
        const removed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view: {
            confirmedHeadHash: header("old-confirmed-head"),
            liveQueueHeaderHashes: [header("old-live-queue")],
          },
        });
        return { removed, remaining: yield* remainingLabels(ALL) };
      }),
    );
    expect(result.removed).toBe(1);
    expect(result.remaining).toEqual(ALL.filter((l) => l !== "old-plain"));
  });

  it("keeps the newest finalized journal even past the horizon, and works in batches", async () => {
    const labels = ["f1", "f2", "f3", "f4"];
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (20 - index) * DAY_MS,
          })),
        );
        const view = {
          confirmedHeadHash: header("not-journaled"),
          liveQueueHeaderHashes: [],
        };
        const cutoff = new Date(Date.now() - DAY_MS);
        const capped = yield* prune({
          challengeableCutoff: cutoff,
          view,
          batchLimit: 1,
          maxBatches: 2,
        });
        const afterCap = yield* remainingLabels(labels);
        const rest = yield* prune({
          challengeableCutoff: cutoff,
          view,
          batchLimit: 1,
        });
        const again = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        return {
          capped,
          afterCap,
          rest,
          again,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    // Oldest first, two batches of one.
    expect(result.capped).toBe(2);
    expect(result.afterCap).toEqual(["f3", "f4"]);
    expect(result.rest).toBe(1);
    expect(result.again).toBe(0);
    expect(result.remaining).toEqual(["f4"]);
  });

  it("keeps an old finalized journal until its confirmed merge has been folded locally", async () => {
    const labels = ["merge-running", "merge-unstarted", "newest"];
    const view = {
      confirmedHeadHash: header("not-journaled"),
      liveQueueHeaderHashes: [],
    };
    const cutoff = new Date(Date.now() - DAY_MS);
    const result = await run(
      Effect.gen(function* () {
        yield* journals([
          {
            label: "merge-running",
            status: "finalized",
            endedAgoMs: 10 * DAY_MS,
            mergeJob: "running",
          },
          {
            label: "merge-unstarted",
            status: "finalized",
            endedAgoMs: 9 * DAY_MS,
            mergeJob: "none",
          },
          { label: "newest", status: "finalized", endedAgoMs: 8 * DAY_MS },
        ]);
        const whileUnmerged = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        const keptWhileUnmerged = yield* remainingLabels(labels);
        yield* recordMergeJob(header("merge-running"), "completed");
        yield* recordMergeJob(header("merge-unstarted"), "completed");
        const onceMerged = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        return {
          whileUnmerged,
          keptWhileUnmerged,
          onceMerged,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.whileUnmerged).toBe(0);
    expect(result.keptWhileUnmerged).toEqual(labels);
    expect(result.onceMerged).toBe(2);
    expect(result.remaining).toEqual(["newest"]);
  });

  it("keeps the journal DA retention holds for finality under the deployment", async () => {
    const labels = ["older-merge", "latest-final-merge", "newest"];
    const view = {
      confirmedHeadHash: header("not-journaled"),
      liveQueueHeaderHashes: [],
    };
    const cutoff = new Date(Date.now() - DAY_MS);
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (10 - index) * DAY_MS,
          })),
        );
        yield* recordMergedOutcome(header("older-merge"), 10);
        yield* recordMergedOutcome(header("latest-final-merge"), 11);
        const held = yield* prune({ challengeableCutoff: cutoff, view });
        return { held, remaining: yield* remainingLabels(labels) };
      }),
    );
    expect(result.held).toBe(1);
    expect(result.remaining).toEqual(["latest-final-merge", "newest"]);
  });

  it("removes nothing inside the challengeability horizon", async () => {
    const labels = ["in-window-1", "in-window-2"];
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: DAY_MS / 2,
          })),
        );
        const removed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view: {
            confirmedHeadHash: header("not-journaled"),
            liveQueueHeaderHashes: [],
          },
        });
        return { removed, remaining: yield* remainingLabels(labels) };
      }),
    );
    expect(result.removed).toBe(0);
    expect(result.remaining).toEqual(labels);
  });
});

describe("journals a recorded correction transition names", () => {
  const view = {
    confirmedHeadHash: header("not-journaled"),
    liveQueueHeaderHashes: [],
  };
  const labels = ["admitted-merge", "pending-removal", "unnamed", "newest"];

  it("keeps a journal named by an admitted or a pending observer transition, past every horizon", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        // Admitted at confirmation depth is not finality (k = 2160): the
        // observer still records it, so the journal is kept.
        yield* recordObserverState({
          admitted: [header("admitted-merge")],
          pending: [header("pending-removal")],
        });
        const removed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
        });
        return { removed, remaining: yield* remainingLabels(labels) };
      }),
    );
    expect(result.removed).toBe(1);
    expect(result.remaining).toEqual([
      "admitted-merge",
      "pending-removal",
      "newest",
    ]);
  });

  it("reads an observer record stored unwrapped as well as string-wrapped", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        yield* recordObserverState({
          admitted: [header("admitted-merge"), header("pending-removal")],
          wrapped: false,
        });
        const removed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
        });
        return { removed, remaining: yield* remainingLabels(labels) };
      }),
    );
    expect(result.removed).toBe(1);
    expect(result.remaining).toEqual([
      "admitted-merge",
      "pending-removal",
      "newest",
    ]);
  });

  it("starts no batch once the permit budget's deadline has passed", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        const removed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
          deadlineMs: Date.now() - 1,
        });
        return { removed, remaining: yield* remainingLabels(labels) };
      }),
    );
    expect(result.removed).toBe(0);
    expect(result.remaining).toEqual(labels);
  });
});

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
