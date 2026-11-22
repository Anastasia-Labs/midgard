/** The validation-trace dispute's staging of a staged builtin step's
 * operands (arm 31) under the watcher's prover funding, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: the watcher
 * has bisected our case's one thread to the PrepareSelected move's holder.
 *
 * A wallet below the collateral line holds the step's first staging under
 * the operator's alarm, naming the case, the thread, the deadline and what
 * the step still needs from our wallet, before any byte reaches the ledger.
 * A wallet above the line stages the first run; run below it, the next is
 * held the same way, the first run's staging output live and no wallet coin
 * spent twice. Topped up, each run left is staged, the thread plays through
 * the staged chain to the award, and each staging output is spent back to
 * our wallet; a spend back the ledger drops after its confirmation is sent
 * again, its exact signed bytes. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/validation-trace-dispute/wallet-move-unfunded-alarm.js";
import {
  COLLATERAL_LINE,
  drainProverToOneCoin,
  topUpProver,
} from "./support/installed-prover-wallet.js";
import { watchLedger } from "./support/own-outputs-installed.js";
import {
  digestOf,
  fundedStagingScene,
  runs,
} from "./support/validation-trace-dispute-funded-one-step-staging.js";
import { outRefOf } from "./support/validation-trace-dispute-funded-one-step-turns.js";
import { turnUntil } from "./support/validation-trace-dispute-funded-runner.js";
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
import {
  onLedger,
  rollBack,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

const { at, turns, place, open, stagingAddress, stagedOutputs, depositOf } =
  fundedStagingScene();
/** The action-id prefix of a staging output's spend back. */
const RECLAIM = "validation-trace-reclaim-staging:";
/** The staging outputs the funded play staged, one per run, by out-ref. */
const stagedOutputsByOutRef = new Map<string, UTxO>();

/**
 * Turns the watcher twice from a wallet holding one coin of `held` and
 * expects each turn to hold the staging of the step's run `index` under the
 * operator's alarm, naming what the step still needs, before any byte of it
 * reaches the ledger; returns our transactions the ledger took meanwhile.
 */
const expectHeldUnderAlarm = async (held: bigint, index: number) => {
  const { h } = at.scene;
  await topUpProver(h as never, held + 200n * ADA);
  await drainProverToOneCoin(h as never, held);
  const watch = watchLedger(h.emulator);
  try {
    await holdTwice(index);
  } finally {
    watch.restore();
  }
  expect(outRefOf((await place())!.utxo)).toBe(outRefOf(at.threadAtPrepare));
  return watch.ours();
};

/** Two turns, each holding the staging of the step's run `index` under the
 * operator's alarm. */
const holdTwice = async (index: number) => {
  for (let pause = 0; pause < 2; pause += 1) {
    const { result, history } = await turnUntil(
      at.dispute,
      VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
    );
    expect(result.kind, history.join("\n")).toBe("pending");
    if (result.kind !== "pending") return;
    expect(result.reason).toContain(
      `cannot fund validation-trace-stage:${digestOf(runs[index]!.datumCbor)}:0 from its plain-ADA coins`,
    );
    // What the step still needs is text for the operator only.
    expect(result.reason).toMatch(
      new RegExp(
        `: ${(runs.length - index).toString()} runs / \\d+ hops remain; about \\d+ ADA needed: `,
      ),
    );
    expect(result.reason).toContain(
      `on thread ${outRefOf(at.threadAtPrepare)} of case ${at.caseUnit},`,
    );
    // The PrepareSelected move it waits on must land before the case
    // expires.
    expect(result.reason).toContain(
      `before its deadline ${at.caseExpiresAtMs.toString()};`,
    );
    expect(validationTraceRunState(result)).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      namedState: {
        name: VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
        alarm: true,
      },
    });
  }
};

describe("validation-trace dispute staging of a staged builtin step's operands under the watcher's prover funding", () => {
  beforeAll(open, 1_800_000);

  it(
    "holds the step's first staging under the operator's alarm, naming the case, the thread and the deadline, when our wallet cannot build it, before any byte reaches the ledger",
    async () => {
      expect(runs.length).toBeGreaterThan(1);
      expect(await expectHeldUnderAlarm(1n * ADA, 0)).toHaveLength(0);
      expect(await stagedOutputs()).toStrictEqual([]);
    },
    TIMEOUT,
  );

  it(
    "stages the step's first run from a wallet above the collateral line, then holds the next run under the operator's alarm once the wallet runs below it, the first run's staging output live and no wallet coin spent twice",
    async () => {
      const { h } = at.scene;
      // Back above the collateral line.
      await topUpProver(h as never, COLLATERAL_LINE + 200n * ADA);
      const watch = watchLedger(h.emulator);
      try {
        await playThreads({
          scene: at.scene,
          runner: turns(),
          sides: [at.side],
          until: async () => (await stagedOutputs()).length > 0,
          limit: 12,
        });
      } finally {
        watch.restore();
      }
      const ours = watch.ours();
      expect(ours).toHaveLength(1);
      const [staging] = ours;
      expect(
        staging!.outputs.filter(({ address }) => address === stagingAddress()),
      ).toMatchObject([
        {
          datum: runs[0]!.datumCbor,
          assets: { lovelace: depositOf(runs[0]!.datumCbor) },
        },
      ]);
      // The operator leaves one coin below the collateral line.
      expect(await expectHeldUnderAlarm(1n * ADA, 1)).toHaveLength(0);
      const live = await stagedOutputs();
      expect(live.map(outRefOf)).toStrictEqual([
        outRefOf({
          txHash: staging!.txHash,
          outputIndex: staging!.outputs.findIndex(
            ({ address }) => address === stagingAddress(),
          ),
        }),
      ]);
      expect(live[0]!.datum).toBe(runs[0]!.datumCbor);
      // The staging's wallet coins are spent once, and by nothing else.
      const consumed = staging!.inputs;
      expect(new Set(consumed).size).toBe(consumed.length);
      expect(
        await at.scene.h.emulator.getUtxosByOutRef(
          consumed.map(parseOutRefLabel),
        ),
      ).toStrictEqual([]);
    },
    TIMEOUT,
  );

  it(
    "stages each run left once our wallet is topped up, and plays the thread through the staged chain to the award",
    async () => {
      // Back above the collateral line.
      await topUpProver(at.scene.h as never, COLLATERAL_LINE + 400n * ADA);
      const { history } = await playThreads({
        scene: at.scene,
        runner: turns(),
        sides: [at.side],
        until: async () =>
          confirmedOneStepMoves(await at.dispute.confirmed()).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 60,
      });
      expect(await place(), history.join("\n")).toBeUndefined();
      const staged = at.dispute
        .sent()
        .filter(({ actionId }) => isStagingAction(actionId));
      expect(staged.length, history.join("\n")).toBe(runs.length);
      const signed = staged.map(({ txHash }) => ({
        txHash,
        cbor: at.scene.l1.signedCbors.get(txHash)!,
      }));
      reportFit(
        "funded staging",
        signed.map(({ cbor }) => cbor),
      );
      for (const { txHash, cbor } of signed) {
        const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
        for (let index = 0; index < outputs.len(); index += 1) {
          const output = coreToTxOutput(outputs.get(index));
          if (output.address === stagingAddress())
            stagedOutputsByOutRef.set(
              outRefOf({ txHash, outputIndex: index }),
              { ...output, txHash, outputIndex: index },
            );
        }
      }
      expect(stagedOutputsByOutRef.size).toBe(runs.length);
    },
    TIMEOUT,
  );

  it(
    "sends a spend back again, its exact signed bytes, when the ledger drops it after its confirmation",
    async () => {
      const { h } = at.scene;
      // A spend back goes out as soon as no thread needs its output, beside
      // the thread's own moves, so one may have landed during the play. The
      // ledger drops one that confirmed and whose own output is still
      // unspent, so nothing after it rests on it.
      let dropped: { txHash: string; consumes: string } | undefined;
      for (let turn = 0; turn < 30 && dropped === undefined; turn += 1) {
        for (const { actionId, txHash } of at.dispute.sent()) {
          const consumes = actionId.slice(RECLAIM.length);
          if (
            dropped === undefined &&
            actionId.startsWith(RECLAIM) &&
            stagedOutputsByOutRef.has(consumes) &&
            (await h.emulator.getTransactionStatus(txHash)).status ===
              "confirmed" &&
            (await h.emulator.getUtxosByOutRef([{ txHash, outputIndex: 0 }]))
              .length === 1
          )
            dropped = { txHash, consumes };
        }
        if (dropped === undefined) await turns().runOnce();
      }
      if (dropped === undefined) throw new Error("no spend back confirmed");
      const staging = stagedOutputsByOutRef.get(dropped.consumes)!;
      rollBack(h.emulator, dropped.txHash, staging);
      expect(onLedger(at.scene, staging)).toBe(true);
      const sent = at.dispute.rebroadcasts.length;

      for (let turn = 0; turn < 12; turn += 1) {
        await turns().runOnce();
        if (!onLedger(at.scene, staging)) break;
      }
      expect(onLedger(at.scene, staging)).toBe(false);
      expect(
        await landedSpenderOf(
          at.scene.l1.signedCbors,
          h.emulator,
          dropped.consumes,
        ),
      ).toBe(dropped.txHash);
      expect(at.dispute.rebroadcasts.slice(sent)).toContain(
        at.scene.l1.signedCbors.get(dropped.txHash),
      );
    },
    TIMEOUT,
  );

  it(
    "spends each staging output back to our wallet once the thread has settled",
    async () => {
      for (let turn = 0; turn < 30; turn += 1) {
        await turns().runOnce();
        if ((await stagedOutputs()).length === 0) break;
      }
      expect(await stagedOutputs()).toStrictEqual([]);
      const reclaims = await Promise.all(
        [...stagedOutputsByOutRef.keys()].map(
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
        expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
          at.scene.h.proverSigner.address,
        );
      }
    },
    TIMEOUT,
  );
});
