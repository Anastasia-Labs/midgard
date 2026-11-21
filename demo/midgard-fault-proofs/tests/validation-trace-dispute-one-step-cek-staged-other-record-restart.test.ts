/** The validation-trace dispute runner at a staged builtin step (arm 31) on
 * the emulator (support/validation-trace-dispute-one-step-staged-other-record.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip: with
 * both threads of our case closed and each staging output still live,
 * another prover removes the block against its own record while a thread
 * another run on our key Inited stands at the opener, and our runner is
 * started again over its journal once the cancel has landed and before the
 * spend backs are final.
 *
 * Read from L1, the thread is cancelled and each staging output spent back
 * to our wallet by exactly one transaction the ledger took, the cancel
 * first, and our run ends resolved by their record only once all of it is
 * final. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import {
  type OurRun,
  RELEASE_DEPTH,
  stagedOtherRecord,
} from "./support/validation-trace-dispute-one-step-staged-other-record.js";
import { onLedger } from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import { RELEASE_FINALITY_POLICY } from "./workflow.make-adapter.js";

const other = stagedOtherRecord({ sibling: true });
const { drive } = other;

describe("validation-trace dispute runner: a staged step's staging and a thread of ours at the opener, once another prover removes the block against its own record, over a restart", () => {
  beforeAll(other.setUp, 1_800_000);

  afterAll(other.tearDown);

  it("cancels the thread and spends each staging output back once each over a restart, the cancel first, and ends resolved by their record once all of it is final", async () => {
    const stagings = await other.playToClose();
    const theirRecord = await other.removeByTheirs();
    const runs: OurRun[] = [];
    const { scene } = drive.dispute();
    const spent = (outRef: string) => !onLedger(scene, parseOwnOutRef(outRef));
    let restartedAt: number | undefined;
    const resolved = await other.runToResolution(stagings, runs, async () => {
      if (
        restartedAt !== undefined ||
        !spent(other.siblingThread()) ||
        !stagings.some(spent)
      )
        return;
      // The cancel has landed and a spend back with it, none of them final.
      restartedAt = scene.h.emulator.blockHeight;
      for (const outRef of stagings.filter(spent)) {
        const { block } = await other.spentBy(outRef);
        expect(restartedAt - block, outRef).toBeLessThan(RELEASE_DEPTH);
      }
      drive.restart({ wrap: other.holding });
    });
    expect(restartedAt).toBeDefined();
    expect(resolved.resolution.referencedProofTokenOutRef).toBe(theirRecord);
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
