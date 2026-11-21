/** The validation-trace dispute's staging of a staged builtin step's
 * operands under the watcher's prover funding, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts, while the
 * step's first staging is in flight. Our wallet holds a large coin and one
 * plain coin; the ledger loses the first staging in flight. Each turn
 * meanwhile is a recorded pending result for this workflow: the first
 * staging is sent again over the same coin, the next run is staged at once
 * over the other coin, which the first does not spend, and no coin is spent
 * twice. Once the ledger takes the first, the thread plays through the
 * staged chain to the award. */
import { beforeAll, describe, expect, it } from "vitest";

import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import {
  type LedgerTransaction,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  digestOf,
  fundedStagingScene,
  runs,
} from "./support/validation-trace-dispute-funded-one-step-staging.js";
import {
  ADA,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  confirmedOneStepMoves,
  playThreads,
} from "./support/validation-trace-dispute-one-step-play.js";
import { stagingSends } from "./support/validation-trace-dispute-one-step-staged-drive.js";

const { at, turns, place, open, stagingAddress, depositOf } =
  fundedStagingScene();

/** Whether `transaction` stages the run `datumCbor`. */
const stages = (transaction: LedgerTransaction, datumCbor: string) =>
  transaction.outputs.some(
    ({ address, datum }) => address === stagingAddress() && datum === datumCbor,
  );

const STAGE = "validation-trace-stage:";

/** The runs the watcher's processes sent a staging for (an action id is
 * the run and its generation). */
const sentRuns = () =>
  stagingSends(at.dispute.sent()).map(
    (actionId) => actionId.slice(STAGE.length).split(":")[0]!,
  );

describe("validation-trace dispute staging of a staged builtin step's operands while its first staging is in flight", () => {
  beforeAll(open, 1_800_000);

  it(
    "stages the step's next run over another coin while its first staging is in flight, sending the first again over the same coin and spending no coin twice; once the first lands, the thread plays on",
    async () => {
      const { h } = at.scene;
      const [first, next] = runs as [(typeof runs)[0], (typeof runs)[0]];
      await topUpProver(h as never, 200n * ADA);
      await drainProver(
        h as never,
        depositOf(first.datumCbor) + depositOf(next.datumCbor) + 20n * ADA,
      );
      let inFlight = true;
      const watch = watchLedger(h.emulator, (transaction) =>
        inFlight && stages(transaction, first.datumCbor)
          ? "withhold"
          : "accept",
      );
      const history: string[] = [];
      try {
        for (let turn = 0; turn < 8; turn += 1) {
          const last = await at.dispute.turn();
          history.push(
            "reason" in last ? `${last.kind}: ${last.reason}` : last.kind,
          );
          // A recorded pending result for this workflow, never a stall.
          expect(last.kind, history.join("\n")).toBe("pending");
        }
        // The first staging was sent, and sent again, over one coin.
        const sent = watch.withheld;
        expect(sent.length, history.join("\n")).toBeGreaterThan(1);
        const [coin] = sent[0]!.inputs;
        expect(sent[0]!.inputs).toStrictEqual([coin]);
        for (const transaction of sent) {
          expect(stages(transaction, first.datumCbor)).toBe(true);
          expect(transaction.inputs).toContain(coin);
        }
        // The next run was staged meanwhile over a coin the first does not
        // spend: a pending transaction of another action holds no build.
        const meanwhile = watch.ours();
        expect(meanwhile, history.join("\n")).toHaveLength(1);
        expect(stages(meanwhile[0]!, next.datumCbor)).toBe(true);
        expect(meanwhile[0]!.inputs).not.toContain(coin);
        expect(sentRuns()).toContain(digestOf(next.datumCbor));

        inFlight = false;
        let landed: readonly LedgerTransaction[] = [];
        for (let turn = 0; turn < 12 && landed.length === 0; turn += 1) {
          const result = await at.dispute.turn();
          history.push(
            "reason" in result
              ? `${result.kind}: ${result.reason}`
              : result.kind,
          );
          expect(result.kind, history.join("\n")).not.toBe("stalled");
          landed = watch
            .ours()
            .filter((transaction) => stages(transaction, first.datumCbor));
        }
        expect(landed, history.join("\n")).toHaveLength(1);
        expect(landed[0]!.inputs).toStrictEqual([coin]);
        const ours = watch.ours();
        // No two of our transactions the ledger took share an input.
        const consumed = ours.flatMap(({ inputs }) => inputs);
        expect(new Set(consumed).size).toBe(consumed.length);
      } finally {
        watch.restore();
      }

      const { history: played } = await playThreads({
        scene: at.scene,
        runner: turns(),
        sides: [at.side],
        until: async () =>
          confirmedOneStepMoves(await at.dispute.confirmed()).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 60,
      });
      expect(await place(), played.join("\n")).toBeUndefined();
      expect(new Set(sentRuns())).toStrictEqual(
        new Set(runs.map(({ datumCbor }) => digestOf(datumCbor))),
      );
    },
    TIMEOUT,
  );
});
