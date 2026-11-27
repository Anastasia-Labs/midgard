/**
 * Merging is permissionless, so this node often merges a block another
 * operator committed. It holds no journal for such a block, and the merge
 * writes no local ledger: landed-block processing folds the block into
 * `confirmed_ledger` in queue order once the merged root covers it.
 */

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier, markApplied } from "../src/landed-blocks/store.js";
import { Globals } from "../src/services/globals.globals.js";
import {
  AFTER_A,
  AFTER_F,
  confirmedAt,
  confirmedKeys,
  fixture,
  type Harness,
  harness,
  inInstalledNode,
  ports,
  queueOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

/** This node's operator key: not the key that signed the fixture's blocks. */
const OWN_OPERATOR = "33".repeat(28);

const inNodeWithGlobals = <A>(
  state: Harness,
  work: Effect.Effect<A, unknown, any>,
) => inInstalledNode(state)(work.pipe(Effect.provide(Globals.Default)));

describe("merging another operator's block", () => {
  it("leaves the confirmed ledger to landed-block processing, which folds the block once the merged root covers it", async () => {
    // Own A on genesis, then another operator's F on A: F spends G1 and
    // produces F1, so its fold changes the confirmed ledger.
    const { a, f, genesisState, journalA } = await fixture();
    expect(f.header.operatorVkey).not.toBe(OWN_OPERATOR);
    const state = harness();
    state.journals.set(a.hash, journalA);
    const landed = ports(state);
    await inNodeWithGlobals(
      state,
      Effect.gen(function* () {
        yield* processLandedQueue(landed, queueOf(genesisState, [a, f]));
        // The rebase applied A and F (its own suites cover the rebase).
        yield* markApplied([a.hash, f.hash]);
        yield* processLandedQueue(landed, queueOf(confirmedAt(a, ""), [f]));
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));

        // This node's merge of F confirmed on L1; landed-block processing
        // sees the merged root at F and folds it.
        yield* processLandedQueue(landed, queueOf(confirmedAt(f, ""), []));
        expect((yield* Frontier.retrieve)?.headerHash).toBe(f.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_F));
      }),
    );
  }, 120_000);
});
