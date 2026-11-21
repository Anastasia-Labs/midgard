import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { pruneFoldedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-applied-missing-da-payloads.js";
import { header } from "./helpers/own-journal-fixture.js";
import {
  deleteLiveQueueNode,
  insertLiveQueueNode,
  insertQueueTerminal,
} from "./helpers/queue-terminal-rows.js";
import {
  DAY_MS,
  DEPLOYMENT,
  journals,
  recordDepositMember,
  remainingLabels,
  run,
  setJournalStatus,
} from "./history-retention-prune.fixtures.js";

/** The journal prune under the test deployment; it never runs without one. */
const prune = (
  args: Omit<
    Parameters<typeof pruneFoldedBeyondChallengeability>[0],
    "deploymentIdentityDigest"
  >,
) =>
  pruneFoldedBeyondChallengeability({
    deploymentIdentityDigest: DEPLOYMENT,
    ...args,
  });

const ALL = [
  "old-plain",
  "old-confirmed-head",
  "old-live-queue",
  "old-unlanded",
  "old-landed",
  "recent-folded",
];

describe("pruning folded journals beyond challengeability", () => {
  it("removes only old folded journals no exemption covers", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals([
          { label: "old-plain", status: "folded", endedAgoMs: 10 * DAY_MS },
          {
            label: "old-confirmed-head",
            status: "folded",
            endedAgoMs: 10 * DAY_MS,
          },
          {
            label: "old-live-queue",
            status: "folded",
            endedAgoMs: 9 * DAY_MS,
          },
          {
            label: "old-unlanded",
            status: "unlanded",
            endedAgoMs: 10 * DAY_MS,
          },
          { label: "recent-folded", status: "folded", endedAgoMs: 60_000 },
          { label: "old-landed", status: "landed", endedAgoMs: 10 * DAY_MS },
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

  it("keeps the newest folded journal even past the horizon, and works in batches", async () => {
    const labels = ["f1", "f2", "f3", "f4"];
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "folded" as const,
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

  it("keeps an old applied journal until its block is folded", async () => {
    const labels = ["applied-older", "applied", "newest"];
    const view = {
      confirmedHeadHash: header("not-journaled"),
      liveQueueHeaderHashes: [],
    };
    const cutoff = new Date(Date.now() - DAY_MS);
    const result = await run(
      Effect.gen(function* () {
        yield* journals([
          {
            label: "applied-older",
            status: "applied",
            endedAgoMs: 10 * DAY_MS,
          },
          { label: "applied", status: "applied", endedAgoMs: 9 * DAY_MS },
          { label: "newest", status: "folded", endedAgoMs: 8 * DAY_MS },
        ]);
        const whileApplied = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        const keptWhileApplied = yield* remainingLabels(labels);
        yield* setJournalStatus(header("applied-older"), "folded");
        yield* setJournalStatus(header("applied"), "folded");
        const onceFolded = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        return {
          whileApplied,
          keptWhileApplied,
          onceFolded,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.whileApplied).toBe(0);
    expect(result.keptWhileApplied).toEqual(labels);
    expect(result.onceFolded).toBe(2);
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
            status: "folded" as const,
            endedAgoMs: (10 - index) * DAY_MS,
          })),
        );
        yield* insertQueueTerminal({
          headerHash: header("older-merge"),
          outcome: "merged",
          height: 10,
        });
        yield* insertQueueTerminal({
          headerHash: header("latest-final-merge"),
          outcome: "merged",
          height: 11,
        });
        const held = yield* prune({
          challengeableCutoff: cutoff,
          view: { ...view, finalThroughHeight: 11 },
        });
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
            status: "folded" as const,
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

describe("journals a landed tx took out of the queue, or the facts still queue", () => {
  const view = {
    confirmedHeadHash: header("not-journaled"),
    liveQueueHeaderHashes: [],
  };
  const labels = ["landed-merge", "landed-removal", "unnamed", "newest"];
  const cutoff = () => new Date(Date.now() - DAY_MS);

  it("keeps a journal a landed merge or removal took out of the queue until that tx is final, past every horizon", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "folded" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        // Landed at height 50: a rollback of at most k can still undo both.
        yield* insertQueueTerminal({
          headerHash: header("landed-merge"),
          outcome: "merged",
          height: 50,
        });
        yield* insertQueueTerminal({
          headerHash: header("landed-removal"),
          outcome: "removed",
          height: 50,
          txIndex: 1,
        });
        const whileNotFinal = yield* prune({
          challengeableCutoff: cutoff(),
          view: { ...view, finalThroughHeight: 49 },
        });
        const keptWhileNotFinal = yield* remainingLabels(labels);
        // Final: the removal releases; the merge is the boundary.
        const onceFinal = yield* prune({
          challengeableCutoff: cutoff(),
          view: { ...view, finalThroughHeight: 50 },
        });
        return {
          whileNotFinal,
          keptWhileNotFinal,
          onceFinal,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.whileNotFinal).toBe(1);
    expect(result.keptWhileNotFinal).toEqual([
      "landed-merge",
      "landed-removal",
      "newest",
    ]);
    expect(result.onceFinal).toBe(1);
    expect(result.remaining).toEqual(["landed-merge", "newest"]);
  });

  it("keeps a journal the facts still queue, whatever the caller's view, until a later merge is final", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "folded" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        // The caller's view misses it; the facts hold its live node.
        yield* insertLiveQueueNode(header("unnamed"));
        const whileQueued = yield* prune({
          challengeableCutoff: cutoff(),
          view: { ...view, finalThroughHeight: 59 },
        });
        const keptWhileQueued = yield* remainingLabels(labels);
        // Its merge lands at 60.
        yield* deleteLiveQueueNode(header("unnamed"));
        yield* insertQueueTerminal({
          headerHash: header("unnamed"),
          outcome: "merged",
          height: 60,
        });
        const whileMerging = yield* prune({
          challengeableCutoff: cutoff(),
          view: { ...view, finalThroughHeight: 59 },
        });
        // A later merge lands and is final: no longer the boundary.
        yield* insertQueueTerminal({
          headerHash: header("not-journaled-later"),
          outcome: "merged",
          height: 61,
        });
        const oncePastK = yield* prune({
          challengeableCutoff: cutoff(),
          view: { ...view, finalThroughHeight: 61 },
        });
        return {
          whileQueued,
          keptWhileQueued,
          whileMerging,
          oncePastK,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.whileQueued).toBe(2);
    expect(result.keptWhileQueued).toEqual(["unnamed", "newest"]);
    expect(result.whileMerging).toBe(0);
    expect(result.oncePastK).toBe(1);
    expect(result.remaining).toEqual(["newest"]);
  });

  it("starts no batch once the permit budget's deadline has passed", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "folded" as const,
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

describe("journals with an orphaned event member", () => {
  it("keeps a journal whose deposit member's L1 origin rolled back, and removes one whose member is canonical", async () => {
    const labels = ["orphan-member", "canonical-member", "newest"];
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "folded" as const,
            endedAgoMs: (40 - index) * DAY_MS,
          })),
        );
        yield* recordDepositMember("orphan-member", false);
        yield* recordDepositMember("canonical-member", true);
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
