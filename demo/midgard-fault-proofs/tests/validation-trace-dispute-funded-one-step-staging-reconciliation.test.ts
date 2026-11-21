/** The spend back of our staging outputs (arm 31) under the watcher's prover
 * funding once the challenged header has rolled back, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: our workflow
 * staged each run of the step and played its thread to the award, and each
 * staging output is still live when the header rolls back. Every later turn
 * runs reconciliation-only, under which the orchestrator admits a spend back
 * of our own staging outputs by its kind, and nothing else new: each staging
 * output goes back to our wallet alone, one input and one output, no mint. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_RECLAIM } from "../src/validation-trace-dispute/one-step-staging-journal.js";
import { topUpProver } from "./support/installed-prover-wallet.js";
import {
  fundedStagingScene,
  runs,
} from "./support/validation-trace-dispute-funded-one-step-staging.js";
import { outRefOf } from "./support/validation-trace-dispute-funded-one-step-turns.js";
import type { AdapterWrap } from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  confirmedOneStepMoves,
  playThreads,
} from "./support/validation-trace-dispute-one-step-play.js";
import {
  isStagingAction,
  landedSpenderOf,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";

const { at, turns, open, stagingAddress, stagedOutputs } = fundedStagingScene();
/** The staging outputs the funded play staged, one per run. */
const stagedOutRefs: string[] = [];

/** Holds each spend back our workflow would take, as a wait. */
const holdingSpendBacks: AdapterWrap = (adapter) => ({
  ...adapter,
  observe: async (context) => {
    const observed = await adapter.observe(context);
    return observed.kind === "action_required" &&
      observed.action.input.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM
      ? { kind: "pending", reason: "spend backs held by the test" }
      : observed;
  },
});

describe("validation-trace dispute staging outputs of ours under reconciliation-only funding after a header rollback, with the thread at its award", () => {
  beforeAll(open, 1_800_000);

  it(
    "plays the thread through the staged chain to the award with each staging output live, then the header rolls back",
    async () => {
      await topUpProver(at.scene.h as never, 400n * ADA);
      const { history } = await playThreads({
        scene: at.scene,
        runner: turns(holdingSpendBacks),
        sides: [at.side],
        until: async () =>
          confirmedOneStepMoves(await at.dispute.confirmed()).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 60,
      });
      const staged = at.dispute
        .sent()
        .filter(({ actionId }) => isStagingAction(actionId));
      for (const { txHash } of staged) {
        const outputs = CML.Transaction.from_cbor_hex(
          at.scene.l1.signedCbors.get(txHash)!,
        )
          .body()
          .outputs();
        for (let index = 0; index < outputs.len(); index += 1)
          if (coreToTxOutput(outputs.get(index)).address === stagingAddress())
            stagedOutRefs.push(outRefOf({ txHash, outputIndex: index }));
      }
      expect(stagedOutRefs, history.join("\n")).toHaveLength(runs.length);
      expect((await stagedOutputs()).map(outRefOf).sort()).toStrictEqual(
        [...stagedOutRefs].sort(),
      );
      at.dispute.rollBackHeader();
    },
    TIMEOUT,
  );

  it(
    "spends each staging output of ours back to our wallet under reconciliation-only authority once the header has rolled back",
    async () => {
      const live = async () => (await stagedOutputs()).map(outRefOf).sort();
      const history: string[] = [];
      for (let turn = 0; turn < 30; turn += 1) {
        const result = await turns().runOnce();
        history.push(
          "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
        );
        if ((await live()).every((outRef) => !stagedOutRefs.includes(outRef)))
          break;
      }
      expect(await live(), history.join("\n")).toStrictEqual([]);
      const reclaims = await Promise.all(
        stagedOutRefs.map(
          async (outRef) =>
            await landedSpenderOf(
              at.scene.l1.signedCbors,
              at.scene.h.emulator,
              outRef,
            ),
        ),
      );
      expect(reclaims.every((txHash) => txHash !== undefined)).toBe(true);
      for (const txHash of reclaims as string[]) {
        const body = CML.Transaction.from_cbor_hex(
          at.scene.l1.signedCbors.get(txHash)!,
        ).body();
        expect(body.inputs().len()).toBe(1);
        expect(body.outputs().len()).toBe(1);
        expect(body.mint()).toBeUndefined();
        expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
          at.scene.h.proverSigner.address,
        );
        console.info(`reconciliation spend back fee ${body.fee().toString()}`);
      }
    },
    TIMEOUT,
  );
});
