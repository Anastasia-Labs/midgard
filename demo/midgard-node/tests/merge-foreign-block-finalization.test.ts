/**
 * Merging is permissionless, so this node often merges a block another
 * operator committed. It holds no journal for such a block: once the merge
 * confirms, it finalizes nothing locally and counts no failure, and
 * landed-block processing folds the block into `confirmed_ledger` in queue
 * order. A block of this node's own whose journal is missing still fails
 * its finalization, counted.
 */

import { Effect, Either, Metric } from "effect";
import { describe, expect, it } from "vitest";

import { MutationJobsDB } from "../src/database/index.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier, markApplied } from "../src/landed-blocks/store.js";
import { Globals } from "../src/services/index.js";
import { finalizeSubmittedMerge } from "../src/transactions/state-queue/merge-to-confirmed-state.js";
import { mergeLocalFinalizationFailureCounter } from "../src/transactions/state-queue/merge-to-confirmed-state.landed-unfinalized-merges.js";
import {
  AFTER_A,
  AFTER_F,
  confirmedAt,
  confirmedKeys,
  fixture,
  harness,
  inNode,
  ports,
  queueOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

/** This node's operator key: not the key that signed the fixture's blocks. */
const OWN_OPERATOR = "33".repeat(28);

const failures = Effect.map(
  Metric.value(mergeLocalFinalizationFailureCounter),
  (state) => state.count,
);

const inNodeWithGlobals = <A>(work: Effect.Effect<A, unknown, any>) =>
  inNode(work.pipe(Effect.provide(Globals.Default)));

describe("merging another operator's block", () => {
  it("finalizes nothing locally and counts no failure, and landed-block processing folds the block into the confirmed ledger", async () => {
    // Own A on genesis, then another operator's F on A: F spends G1 and
    // produces F1, so its fold changes the confirmed ledger.
    const { a, f, genesisState, journalA } = await fixture();
    expect(f.header.operatorVkey).not.toBe(OWN_OPERATOR);
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
    const landed = ports(state);
    await inNodeWithGlobals(
      Effect.gen(function* () {
        yield* processLandedQueue(landed, queueOf(genesisState, [a, f]));
        // The rebase applied F (its own suites cover the rebase).
        yield* markApplied([f.hash]);
        yield* processLandedQueue(landed, queueOf(confirmedAt(a, ""), [f]));
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));

        // This node's merge of F confirmed on L1.
        const before = yield* failures;
        yield* finalizeSubmittedMerge({
          headerHash: Buffer.from(f.hash, "hex"),
          header: f.header,
          txIds: [],
          ownOperatorKeyHash: OWN_OPERATOR,
        });
        expect(yield* failures).toBe(before);
        expect(
          yield* MutationJobsDB.retrieveByJobId(
            MutationJobsDB.confirmedMergeFinalizationJobId(f.hash),
          ),
        ).toBeUndefined();

        // Landed-block processing sees the merged root at F and folds it.
        yield* processLandedQueue(landed, queueOf(confirmedAt(f, ""), []));
        expect((yield* Frontier.retrieve)?.headerHash).toBe(f.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_F));
      }),
    );
  }, 120_000);

  it("still fails, counted, for a block of this node's own whose journal is missing", async () => {
    const { f } = await fixture();
    await inNodeWithGlobals(
      Effect.gen(function* () {
        const before = yield* failures;
        const result = yield* Effect.either(
          finalizeSubmittedMerge({
            headerHash: Buffer.from(f.hash, "hex"),
            header: f.header,
            txIds: [],
            ownOperatorKeyHash: f.header.operatorVkey,
          }),
        );
        expect(Either.isLeft(result) && result.left.message).toBe(
          "Failed to finalize confirmed-state merge locally because the pending-finalization journal is missing",
        );
        expect(yield* failures).toBe(before + 1n);
      }),
    );
  }, 120_000);
});
