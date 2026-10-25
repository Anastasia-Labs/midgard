/** The validation-trace dispute runner at a staged builtin step (arm 31) on
 * the emulator (support/validation-trace-dispute-one-step-staged-other-record.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip: with
 * both threads of our case closed and each staging output still live,
 * another run on our key, with a journal of its own, removes the block
 * against its own record. Our run reads that removal as a terminal on our
 * key whose record its journal never created.
 *
 * With no thread of ours standing, read from L1, our run spends each staging
 * output back to our wallet, one transaction each, and ends resolved by that
 * record, never as its own conviction, only once each spend back is final
 * at release depth. With a thread a third run on our key Inited standing at
 * the opener, our run cancels that thread first, then spends each staging
 * output back, and ends resolved by that record once the cancel is past
 * rollback recovery and each spend back is final at release depth. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import {
  type OurRun,
  RELEASE_DEPTH,
  stagedOtherRecord,
} from "./support/validation-trace-dispute-one-step-staged-other-record.js";
import { RELEASE_FINALITY_POLICY } from "./workflow.make-adapter.js";

/** Plays `other` to the removal and our run to its resolution, and checks
 * from L1 that each staging output came back to our wallet by exactly one
 * transaction, final at release depth when the run resolved; the run's
 * outcomes, the tip it last observed at and each spend back's block. */
const resolveOnOurKey = async (other: ReturnType<typeof stagedOtherRecord>) => {
  const stagings = await other.playToClose();
  const theirRecord = await other.removeByTheirs();
  const runs: OurRun[] = [];
  const resolved = await other.runToResolution(stagings, runs);
  expect(resolved.resolution.referencedProofTokenOutRef).toBe(theirRecord);
  expect("terminal" in resolved).toBe(false);
  const { observedAt } = runs.at(-1)!;
  const intents = other.spendBackIntents(resolved.entries);
  const blocks: number[] = [];
  for (const outRef of stagings) {
    const { hash, block } = await other.expectSpentBack(outRef);
    expect([...(intents.get(outRef) ?? [])], outRef).toStrictEqual([hash]);
    expect(observedAt - block, outRef).toBeGreaterThanOrEqual(RELEASE_DEPTH);
    blocks.push(block);
  }
  return { runs, observedAt, blocks };
};

describe("validation-trace dispute runner: a staged step's staging, once another run on our key removes the block against its own record", () => {
  const other = stagedOtherRecord({ ourKey: true });

  beforeAll(other.setUp, 1_800_000);

  afterAll(other.tearDown);

  it("spends each staging output back to our wallet and ends resolved by the other run's record only once each spend back is final at release depth", async () => {
    const { runs } = await resolveOnOurKey(other);
    // Our run waited on the spend backs before it let the resolution go.
    expect(
      runs.some(
        ({ result }) =>
          result.kind === "pending" &&
          result.reason.startsWith(VALIDATION_TRACE_STAGING_OUTSTANDING),
      ),
    ).toBe(true);
  }, 1_800_000);
});

describe("validation-trace dispute runner: a staged step's staging and a thread of ours at the opener, once another run on our key removes the block against its own record", () => {
  const other = stagedOtherRecord({ ourKey: true, sibling: true });

  beforeAll(other.setUp, 1_800_000);

  afterAll(other.tearDown);

  it("cancels the thread, then spends each staging output back to our wallet, and ends resolved by the other run's record once all of it is final", async () => {
    const { observedAt, blocks } = await resolveOnOurKey(other);
    const cancel = await other.spentBy(other.siblingThread());
    expect(cancel.signed.map(([hash]) => hash)).toStrictEqual([cancel.hash]);
    expect(observedAt - cancel.block).toBeGreaterThanOrEqual(
      RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth,
    );
    for (const block of blocks) expect(block).toBeGreaterThan(cancel.block);
  }, 1_800_000);
});
