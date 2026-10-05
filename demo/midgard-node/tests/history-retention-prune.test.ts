import "./utils.js";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { pruneFinalizedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import {
  DAY_MS,
  DEPLOYMENT,
  journals,
  recordDepositMember,
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
        yield* recordObserverState();
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
        yield* recordObserverState();
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
        yield* recordObserverState();
        const whileUnmerged = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        const keptWhileUnmerged = yield* remainingLabels(labels);
        yield* recordMergeJob(header("merge-running"), "completed");
        yield* recordMergeJob(header("merge-unstarted"), "completed");
        // Folded locally after the observer last saved: kept until it has
        // seen the queue again.
        const beforeObserved = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        yield* recordObserverState();
        const onceMerged = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        return {
          whileUnmerged,
          keptWhileUnmerged,
          beforeObserved,
          onceMerged,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.whileUnmerged).toBe(0);
    expect(result.keptWhileUnmerged).toEqual(labels);
    expect(result.beforeObserved).toBe(0);
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
        yield* recordObserverState();
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
        yield* recordObserverState();
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

  it("keeps a journal the observer's cursor still queues: its merge is not replayed yet", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        yield* recordObserverState({ cursorQueue: [header("unnamed")] });
        const whileQueued = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
        });
        const keptWhileQueued = yield* remainingLabels(labels);
        // The next reconcile replays the merge and admits it.
        yield* recordObserverState({ admitted: [header("unnamed")] });
        const whileAdmitted = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
        });
        // Proven final beyond k and dropped from the admitted list.
        yield* recordObserverState();
        const oncePastK = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
        });
        return {
          whileQueued,
          keptWhileQueued,
          whileAdmitted,
          oncePastK,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.whileQueued).toBe(2);
    expect(result.keptWhileQueued).toEqual(["unnamed", "newest"]);
    expect(result.whileAdmitted).toBe(0);
    expect(result.oncePastK).toBe(1);
    expect(result.remaining).toEqual(["newest"]);
  });

  it("removes no journal while the deployment has no correction-observer record", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        const unobserved = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
        });
        const keptUnobserved = yield* remainingLabels(labels);
        yield* recordObserverState();
        const observed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view,
        });
        return {
          unobserved,
          keptUnobserved,
          observed,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.unobserved).toBe(0);
    expect(result.keptUnobserved).toEqual(labels);
    expect(result.observed).toBe(3);
    expect(result.remaining).toEqual(["newest"]);
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
        yield* recordObserverState();
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

describe("journals with an orphaned event member", () => {
  it("keeps a journal whose deposit member's L1 origin rolled back, and removes one whose member is canonical", async () => {
    const labels = ["orphan-member", "canonical-member", "newest"];
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        yield* recordDepositMember("orphan-member", false);
        yield* recordDepositMember("canonical-member", true);
        yield* recordObserverState();
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
    expect(result.removed).toBe(1);
    expect(result.remaining).toEqual(["orphan-member", "newest"]);
  });
});
