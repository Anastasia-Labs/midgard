/** The validation-trace dispute's staging of a staged builtin step's
 * operands under the watcher's prover funding, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts, while the
 * step's first staging is in flight. Our wallet's one plain coin funds both
 * of the step's runs; the ledger loses the first staging in flight. Each
 * turn meanwhile is a recorded pending result for this workflow: the next
 * run is not built while the first staging is unresolved, the first staging
 * is sent again over the same coin, and no coin is spent twice. Once the
 * ledger takes it, the next run stages from its change and the thread plays
 * through the staged chain to the award. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
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
import { entriesOf } from "./support/validation-trace-dispute-one-step-staged-drive.js";

const { at, turns, place, open, stagingAddress, depositOf } =
  fundedStagingScene();

/** Whether `transaction` stages the run `datumCbor`. */
const stages = (transaction: LedgerTransaction, datumCbor: string) =>
  transaction.outputs.some(
    ({ address, datum }) => address === stagingAddress() && datum === datumCbor,
  );

/** The runs the journal of `result` holds a staging intent for. */
const intendedRuns = (result: FraudProofWorkflowRunResult) =>
  entriesOf(result).flatMap(({ event }) =>
    event.kind === "submission_intent" &&
    event.actionInput.stage === "stage_operands"
      ? [String(event.actionInput.run)]
      : [],
  );

describe("validation-trace dispute staging of a staged builtin step's operands while its first staging is in flight", () => {
  beforeAll(open, 1_800_000);

  afterAll(async () => {
    await at.dispute?.close();
  });

  it(
    "holds the step's next run while its first staging is in flight over our one plain coin, sending the first again over the same coin and spending no coin twice; once the first lands, the next stages from its change and the thread plays on",
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
        let last: FraudProofWorkflowRunResult | undefined;
        for (let turn = 0; turn < 8; turn += 1) {
          last = await at.dispute.turn();
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
        // The next run was not built while the first is unresolved.
        expect(watch.ours()).toStrictEqual([]);
        expect(intendedRuns(last!)).not.toContain(digestOf(next.datumCbor));

        inFlight = false;
        let staged: LedgerTransaction | undefined;
        for (let turn = 0; turn < 12 && staged === undefined; turn += 1) {
          const result = await at.dispute.turn();
          history.push(
            "reason" in result
              ? `${result.kind}: ${result.reason}`
              : result.kind,
          );
          expect(result.kind, history.join("\n")).not.toBe("stalled");
          staged = watch
            .ours()
            .find((transaction) => stages(transaction, next.datumCbor));
        }
        expect(staged, history.join("\n")).toBeDefined();
        const ours = watch.ours();
        const landed = ours.filter((transaction) =>
          stages(transaction, first.datumCbor),
        );
        expect(landed).toHaveLength(1);
        expect(landed[0]!.inputs).toStrictEqual([coin]);
        // The next run stages from the first staging's change.
        expect(
          staged!.inputs.every((input) =>
            input.startsWith(`${landed[0]!.txHash}#`),
          ),
        ).toBe(true);
        // No two of our transactions the ledger took share an input.
        const consumed = ours.flatMap(({ inputs }) => inputs);
        expect(new Set(consumed).size).toBe(consumed.length);
      } finally {
        watch.restore();
      }

      const { result, history: played } = await playThreads({
        scene: at.scene,
        runner: turns(),
        sides: [at.side],
        until: async (run) =>
          confirmedOneStepMoves(entriesOf(run)).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 60,
      });
      expect(await place(), played.join("\n")).toBeUndefined();
      expect(new Set(intendedRuns(result))).toStrictEqual(
        new Set(runs.map(({ datumCbor }) => digestOf(datumCbor))),
      );
    },
    TIMEOUT,
  );
});
