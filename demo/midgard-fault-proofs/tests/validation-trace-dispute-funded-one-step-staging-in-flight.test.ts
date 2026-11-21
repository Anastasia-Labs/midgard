/** The validation-trace dispute's staging of a staged builtin step's
 * operands under the watcher's prover funding, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts, with no block
 * between our own submissions. Our wallet holds a large coin and one plain
 * coin. One turn, carrying on over its own moves as the watcher's does,
 * stages the step's first run, then its next run while the
 * first waits in our node's mempool, over a coin the first does not spend,
 * and pauses only at the staging depth gate; turns without a block send
 * nothing more. Once blocks land, the thread plays through the staged chain
 * to the award: each run is staged once, and no coin is spent twice. */
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_SHALLOW } from "../src/validation-trace-dispute/one-step-staging.js";
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

describe("validation-trace dispute staging of a staged builtin step's operands through the mempool", () => {
  beforeAll(open, 1_800_000);

  it(
    "stages the step's next run while its first staging waits in the mempool, over a coin the first does not spend, pausing only at the staging depth gate; once blocks land, the thread plays on",
    async () => {
      const { h } = at.scene;
      const [first, next] = runs as [(typeof runs)[0], (typeof runs)[0]];
      await topUpProver(h as never, 200n * ADA);
      await drainProver(
        h as never,
        depositOf(first.datumCbor) + depositOf(next.datumCbor) + 20n * ADA,
      );
      const watch = watchLedger(h.emulator);
      const pending = () =>
        new Set(Object.keys(h.emulator.mempool).map((key) => key.slice(0, 64)));
      const history: string[] = [];
      try {
        const height = h.emulator.blockHeight;
        for (let turn = 0; turn < 3; turn += 1) {
          const result = await at.dispute.turn(undefined, {
            chain: true,
            block: false,
          });
          history.push(
            "reason" in result
              ? `${result.kind}: ${result.reason}`
              : result.kind,
          );
          // Every run is staged; the step waits for their depth alone.
          expect(result, history.join("\n")).toMatchObject({
            kind: "pending",
            reason: VALIDATION_TRACE_STAGING_SHALLOW,
          });
        }
        expect(h.emulator.blockHeight).toBe(height);
        // Both stagings are in the mempool, sent once each, the next run's
        // over a coin the first's does not spend.
        const staged = watch.ours();
        expect(staged, history.join("\n")).toHaveLength(2);
        const [firstStaging, nextStaging] = staged as [
          LedgerTransaction,
          LedgerTransaction,
        ];
        expect(stages(firstStaging, first.datumCbor)).toBe(true);
        expect(stages(nextStaging, next.datumCbor)).toBe(true);
        for (const { txHash } of staged) expect(pending()).toContain(txHash);
        expect(
          nextStaging.inputs.filter((input) =>
            firstStaging.inputs.includes(input),
          ),
        ).toStrictEqual([]);
        expect(sentRuns()).toStrictEqual([
          digestOf(first.datumCbor),
          digestOf(next.datumCbor),
        ]);

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
        // Each run was staged once, and no two of our transactions the
        // ledger took share an input.
        expect(sentRuns()).toStrictEqual(
          runs.map(({ datumCbor }) => digestOf(datumCbor)),
        );
        const consumed = watch.ours().flatMap(({ inputs }) => inputs);
        expect(new Set(consumed).size).toBe(consumed.length);
      } finally {
        watch.restore();
      }
    },
    TIMEOUT,
  );
});
