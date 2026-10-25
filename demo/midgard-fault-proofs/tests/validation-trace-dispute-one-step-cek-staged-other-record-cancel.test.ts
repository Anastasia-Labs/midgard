/** The validation-trace dispute runner at a staged builtin step (arm 31) on
 * the emulator (support/validation-trace-dispute-one-step-staged-other-record.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip: with
 * both threads of our case closed and each staging output still live,
 * another prover removes the block against its own record while a thread
 * another run on our key Inited stands at the opener.
 *
 * Read from L1, our run cancels that thread first, then spends each staging
 * output back to our wallet, one transaction each, and ends resolved by
 * their record only once the cancel is past rollback recovery and each
 * spend back is final at release depth. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  type OurRun,
  RELEASE_DEPTH,
  stagedOtherRecord,
} from "./support/validation-trace-dispute-one-step-staged-other-record.js";
import { RELEASE_FINALITY_POLICY } from "./workflow.make-adapter.js";

const other = stagedOtherRecord({ sibling: true });

describe("validation-trace dispute runner: a staged step's staging and a thread of ours at the opener, once another prover removes the block against its own record", () => {
  beforeAll(other.setUp, 1_800_000);

  afterAll(other.tearDown);

  it("cancels the thread, then spends each staging output back to our wallet, and ends resolved by their record once all of it is final", async () => {
    const stagings = await other.playToClose();
    const theirRecord = await other.removeByTheirs();
    const runs: OurRun[] = [];
    const resolved = await other.runToResolution(stagings, runs);
    expect(resolved.resolution.referencedProofTokenOutRef).toBe(theirRecord);
    expect("terminal" in resolved).toBe(false);
    const { observedAt } = runs.at(-1)!;

    const cancel = await other.spentBy(other.siblingThread());
    expect(cancel.signed.map(([hash]) => hash)).toStrictEqual([cancel.hash]);
    expect(observedAt - cancel.block).toBeGreaterThanOrEqual(
      RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth,
    );
    const intents = other.spendBackIntents(resolved.entries);
    for (const outRef of stagings) {
      const { hash, block } = await other.expectSpentBack(outRef);
      expect([...(intents.get(outRef) ?? [])], outRef).toStrictEqual([hash]);
      expect(block, outRef).toBeGreaterThan(cancel.block);
      expect(observedAt - block, outRef).toBeGreaterThanOrEqual(RELEASE_DEPTH);
    }
  }, 1_800_000);
});
