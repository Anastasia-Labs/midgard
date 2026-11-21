/** Our staging outputs (arm 31) under the watcher's prover funding once the
 * challenged header has rolled back, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: our workflow
 * staged each run of the step and played its thread past the
 * PrepareSelected move, whose moves read the staging, when the header rolls
 * back. Every later turn runs under the reconciliation-only permit the
 * watcher derives from the decision, and each transaction is final only
 * `RELEASE_DEPTH` blocks under the tip.
 *
 * Reconciliation-only authority refuses the thread's next move: while the
 * thread stands past the PrepareSelected move, each run holds for a fresh
 * submission and each staging output stays where it is, past the case's
 * expiry as before it. Once its defender closes the case, each comes back
 * to our wallet before the case's ending, a reclaim that authority admits
 * by its kind; the ending waits on no spend back's finality: a spend back
 * that rolls back leaves its outputs live, the process signs nothing new
 * for them, and a proof-materials sweep selects them
 * (support/validation-trace-dispute-funded-one-step-staging-held.ts). */
import { beforeAll, describe, expect, it } from "vitest";

import { topUpProver } from "./support/installed-prover-wallet.js";
import {
  fundedStagingScene,
  runs,
} from "./support/validation-trace-dispute-funded-one-step-staging.js";
import {
  describeRun,
  expectFreshHoldPastExpiry,
  expectSpentBackOnClose,
  isFreshSubmissionHold,
  RELEASE_DEPTH,
} from "./support/validation-trace-dispute-funded-one-step-staging-held.js";
import { outRefOf } from "./support/validation-trace-dispute-funded-one-step-turns.js";
import {
  ADA,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";

let releaseDepth = 0;

const held = fundedStagingScene({
  releaseDepth: () => releaseDepth,
});
const { at, turns, place, playTo, open, stagedOutputs } = held;
/** The staging outputs, as the ledger held them once the thread moved past
 * the PrepareSelected move. */
let staged: string[] = [];

const live = async () => (await stagedOutputs()).map(outRefOf).sort();

describe("validation-trace dispute staging outputs under reconciliation-only funding after a header rollback, while our thread stands past the PrepareSelected move", () => {
  beforeAll(open, 1_800_000);

  it(
    "keeps each staging output under the reconciliation-only permit while the thread stands past the PrepareSelected move, each run refusing the thread's next move as a fresh submission",
    async () => {
      await topUpProver(at.scene.h as never, 400n * ADA);
      await playTo("core");
      staged = await live();
      expect(staged).toHaveLength(runs.length);
      at.dispute.rollBackHeader();
      releaseDepth = RELEASE_DEPTH;
      const signed = at.scene.l1.signedCbors.size;
      const history: string[] = [];
      for (let turn = 0; turn < RELEASE_DEPTH + 4; turn += 1) {
        const result = await turns().runOnce();
        history.push(describeRun(result));
        expect(isFreshSubmissionHold(result), history.join("\n")).toBe(true);
      }
      expect(await live(), history.join("\n")).toStrictEqual(staged);
      expect(at.scene.l1.signedCbors.size, history.join("\n")).toBe(signed);
      expect((await place())?.holder, history.join("\n")).toBe("core");
    },
    TIMEOUT,
  );

  it(
    "holds for a fresh submission, run after run, once the case's expiry has passed, signing nothing and leaving each staging output and the thread where they are",
    async () => {
      await expectFreshHoldPastExpiry(held, staged, "core");
    },
    TIMEOUT,
  );

  it(
    "spends each back to our wallet under the reconciliation-only permit once its defender closed the case, before the ending, and leaves them for a sweep once those spend backs roll back",
    async () => {
      await expectSpentBackOnClose(held, staged);
    },
    TIMEOUT,
  );
});
