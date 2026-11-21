/** The validation-trace dispute runner at a staged builtin step (arm 31) on
 * the emulator (support/validation-trace-dispute-one-step-staged-other-record.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip: with
 * both threads of our case closed and each staging output still live,
 * another prover removes the block against its own record while a thread
 * an earlier process of ours Inited stands at the opener.
 *
 * Read from L1, our run, its case open, opens no second case from that
 * thread: it cancels the thread first, then spends each staging output back
 * to our wallet, one transaction each, and ends resolved by their record
 * once the cancel is past rollback recovery. The ending waits on no spend
 * back's finality, and the process follows each spend back until it is
 * final: once each spend back rolls back, our next runs send it again, the
 * same signed bytes, sign nothing new for the outputs, and end resolved by
 * their record again. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  type OurRun,
  stagedOtherRecord,
} from "./support/validation-trace-dispute-one-step-staged-other-record.js";
import { RELEASE_FINALITY_POLICY } from "./workflow.make-adapter.js";

const other = stagedOtherRecord({ sibling: true });

describe("validation-trace dispute runner: a staged step's staging and a thread of ours at the opener, once another prover removes the block against its own record", () => {
  beforeAll(other.setUp, 1_800_000);

  afterAll(other.tearDown);

  it("cancels the thread, then spends each staging output back to our wallet, ends resolved by their record once the cancel is final, and spends the outputs back again, the same bytes, once those spend backs roll back", async () => {
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
    const sends = other.spendBackSends();
    for (const outRef of stagings) {
      const { hash, block } = await other.expectSpentBack(outRef);
      expect([...(sends.get(outRef) ?? [])], outRef).toStrictEqual([hash]);
      expect(block, outRef).toBeGreaterThan(cancel.block);
    }
    await other.spentBackAgainAfterRollback();
  }, 1_800_000);
});
