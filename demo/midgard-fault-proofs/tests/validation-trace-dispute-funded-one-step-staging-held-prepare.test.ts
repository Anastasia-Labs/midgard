/** Our staging outputs (arm 31) under the watcher's prover funding once the
 * challenged header has rolled back, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: our workflow
 * staged each run of the step while its thread stood at the PrepareSelected
 * move's holder, and the header rolls back before that move lands. Every
 * later turn runs under the reconciliation-only permit the watcher derives
 * from the workflow it recorded, and each transaction is final only
 * `RELEASE_DEPTH` blocks under the tip.
 *
 * While the thread stands at the PrepareSelected move's holder, each staging
 * output stays where it is, and once the case's expiry has passed each run
 * reports them under a named alarm and signs nothing. Once its defender
 * closes the case, each comes back to our wallet, one transaction each, and
 * the case's ending is taken only once each spend back is final at release
 * depth (support/validation-trace-dispute-funded-one-step-staging-held.ts). */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "../src/validation-trace-dispute/actions.js";
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

/** Why the test's runner waits instead of sending the PrepareSelected
 * move. */
const PREPARE_HELD = "the PrepareSelected move is held by the test";

const held = fundedStagingScene({
  releaseDepth: () => releaseDepth,
});
const { at, turns, place, open, stagedOutputs } = held;
/** The staging outputs, as the ledger held them with the thread at the
 * PrepareSelected move's holder. */
let staged: string[] = [];

const live = async () => (await stagedOutputs()).map(outRefOf).sort();

describe("validation-trace dispute staging outputs under reconciliation-only funding after a header rollback, while our thread stands at the PrepareSelected move's holder", () => {
  beforeAll(open, 1_800_000);

  afterAll(async () => {
    await at.dispute?.close();
  });

  it(
    "keeps each staging output under the reconciliation-only permit while the thread stands at the PrepareSelected move's holder",
    async () => {
      await topUpProver(at.scene.h as never, 400n * ADA);
      expect((await place())?.holder).toBe("prepare");
      // Our workflow stages each run; the test holds the move that would
      // read them.
      const holding = turns((adapter) => ({
        ...adapter,
        observe: async (context) => {
          const observed = await adapter.observe(context);
          return observed.kind === "action_required" &&
            observed.action.input.schemaVersion ===
              VALIDATION_TRACE_DISPUTE_ACTION &&
            parseValidationTraceAction(observed.action).stage === "resolve"
            ? { kind: "pending", reason: PREPARE_HELD }
            : observed;
        },
      }));
      const staging: string[] = [];
      for (let turn = 0; turn < 30; turn += 1) {
        const result = await holding.runOnce();
        staging.push(describeRun(result));
        if (result.kind === "pending" && result.reason === PREPARE_HELD) break;
      }
      expect(staging.at(-1), staging.join("\n")).toBe(
        `pending: ${PREPARE_HELD}`,
      );
      staged = await live();
      expect(staged, staging.join("\n")).toHaveLength(runs.length);
      await at.dispute.rollBackHeader();
      releaseDepth = RELEASE_DEPTH;
      const history: string[] = [];
      for (let turn = 0; turn < RELEASE_DEPTH + 4; turn += 1)
        history.push(describeRun(await turns().runOnce()));
      expect(await live(), history.join("\n")).toStrictEqual(staged);
      expect((await place())?.holder, history.join("\n")).toBe("prepare");
    },
    TIMEOUT,
  );

  it(
    "reports each staging output it holds under a named alarm, run after run, once the case's expiry has passed, signing nothing",
    async () => {
      await expectHeldPastExpiry(held, staged, "prepare");
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
