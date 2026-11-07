/** Our staging outputs (arm 31) under the watcher's prover funding once the
 * challenged header has rolled back, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: our workflow
 * staged each run of the step and played its thread past the
 * PrepareSelected move, whose moves read the staging, when the header rolls
 * back. Every later turn runs under the reconciliation-only permit the
 * watcher derives from the workflow it recorded, and each transaction is
 * final only `RELEASE_DEPTH` blocks under the tip.
 *
 * While the thread stands past the PrepareSelected move, each staging
 * output stays where it is, and once the case's expiry has passed each run
 * reports them under a named alarm and signs nothing. Once its defender
 * closes the case, each comes back to our wallet, one transaction each, and
 * the case's ending is taken only once each spend back is final at release
 * depth (support/validation-trace-dispute-funded-one-step-staging-held.ts). */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { topUpProver } from "./support/installed-prover-wallet.js";
import {
  fundedStagingScene,
  runs,
} from "./support/validation-trace-dispute-funded-one-step-staging.js";
import {
  describeRun,
  expectHeldPastExpiry,
  expectSpentBackOnClose,
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

  afterAll(async () => {
    await at.dispute?.close();
  });

  it(
    "keeps each staging output under the reconciliation-only permit while the thread stands past the PrepareSelected move",
    async () => {
      await topUpProver(at.scene.h as never, 400n * ADA);
      await playTo("core");
      staged = await live();
      expect(staged).toHaveLength(runs.length);
      await at.dispute.rollBackHeader();
      releaseDepth = RELEASE_DEPTH;
      const history: string[] = [];
      for (let turn = 0; turn < RELEASE_DEPTH + 4; turn += 1)
        history.push(describeRun(await turns().runOnce()));
      expect(await live(), history.join("\n")).toStrictEqual(staged);
      expect((await place())?.holder, history.join("\n")).toBe("core");
    },
    TIMEOUT,
  );

  it(
    "reports each staging output it holds under a named alarm, run after run, once the case's expiry has passed, signing nothing",
    async () => {
      await expectHeldPastExpiry(held, staged, "core");
    },
    TIMEOUT,
  );

  it(
    "spends each back to our wallet once its defender closed the case, and takes the ending only once each spend back is final at release depth",
    async () => {
      await expectSpentBackOnClose(held, staged);
    },
    TIMEOUT,
  );
});
