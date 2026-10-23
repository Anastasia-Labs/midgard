/** The validation-trace dispute's staging of a staged builtin step's
 * operands (arm 31) under the watcher's prover funding, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: the watcher
 * has bisected our case's one thread to the PrepareSelected move's holder.
 *
 * A wallet too short to build the step's first staging holds it under the
 * operator's alarm, naming the case, the thread, the deadline and what the
 * step still needs from our wallet, before any byte reaches the ledger,
 * releasing what it reserved. A wallet that can build the first staging but
 * not the next stages the first and holds the next the same way, the first
 * run's staging output live and no wallet coin spent twice. Topped up, each
 * run left is staged under the permit, the thread plays through the staged
 * chain to the award, and each staging output is spent back to our wallet
 * under the permit; a spend back the ledger drops after its confirmation is
 * sent again, its exact signed bytes. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_RECLAIM } from "../src/validation-trace-dispute/one-step-staging-journal.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
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
  entriesOf,
  landedSpenderOf,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  onLedger,
  rollBack,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

const { at, turns, place, open, stagingAddress, stagedOutputs, depositOf } =
  fundedStagingScene();
/** The staging outputs the funded play staged, one per run. */
const stagedOutRefs: string[] = [];

/**
 * Turns the watcher twice from a wallet holding `held` and expects each
 * turn to hold the staging of the step's run `index` under the operator's
 * alarm, naming what the step still needs, before any byte of it reaches
 * the ledger, releasing what it reserved; returns our transactions the
 * ledger took meanwhile.
 */
const expectHeldUnderAlarm = async (held: bigint, index: number) => {
  const { h } = at.scene;
  await topUpProver(h as never, 200n * ADA);
  await drainProver(h as never, held);
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
 * operator's alarm, releasing what it reserved. */
const holdTwice = async (index: number) => {
  for (let pause = 0; pause < 2; pause += 1) {
    const { result, history } = await turnUntil(
      at.dispute,
      VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
    );
    expect(result.kind, history.join("\n")).toBe("pending");
    if (result.kind !== "pending") return;
    expect(result.reason).toContain(
      `cannot fund validation-trace-stage:${digestOf(runs[index]!.datumCbor)}:`,
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
    for (const reservation of await at.dispute.readReservations())
      expect(reservation).toMatchObject({ activeInputs: [] });
  }
};

describe("validation-trace dispute staging of a staged builtin step's operands under the prover funding permit", () => {
  beforeAll(open, 1_800_000);

  afterAll(async () => {
    await at.dispute?.close();
  });

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
    "stages the step's first run from a wallet that can build it, then holds the next run under the operator's alarm, the first run's staging output live and no wallet coin spent twice",
    async () => {
      // Enough for the first staging and its fee, short of the next.
      const held = depositOf(runs[0]!.datumCbor) + 5n * ADA;
      expect(held).toBeLessThan(
        depositOf(runs[0]!.datumCbor) + depositOf(runs[1]!.datumCbor),
      );
      const ours = await expectHeldUnderAlarm(held, 1);
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
      const consumed = ours.flatMap(({ inputs }) => inputs);
      expect(new Set(consumed).size).toBe(consumed.length);
      expect(
        await at.scene.h.emulator.getUtxosByOutRef(
          consumed.map(parseOwnOutRef),
        ),
      ).toStrictEqual([]);
    },
    TIMEOUT,
  );

  it(
    "stages each run left under the funding permit once our wallet is topped up, and plays the thread through the staged chain to the award",
    async () => {
      await topUpProver(at.scene.h as never, 400n * ADA);
      const { result, history } = await playThreads({
        scene: at.scene,
        runner: turns(),
        sides: [at.side],
        until: async (run) =>
          confirmedOneStepMoves(entriesOf(run)).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 60,
      });
      expect(await place(), history.join("\n")).toBeUndefined();
      const staged = entriesOf(result).filter(
        ({ event }) =>
          event.kind === "submission_intent" &&
          event.actionInput.stage === "stage_operands",
      );
      expect(staged.length, history.join("\n")).toBe(runs.length);
      const signed = staged.flatMap(({ event }) =>
        event.kind === "submission_intent"
          ? [
              {
                txHash: event.txHash,
                cbor: at.scene.l1.signedCbors.get(event.txHash)!,
              },
            ]
          : [],
      );
      reportFit(
        "funded staging",
        signed.map(({ cbor }) => cbor),
      );
      for (const { txHash, cbor } of signed) {
        const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
        for (let index = 0; index < outputs.len(); index += 1)
          if (coreToTxOutput(outputs.get(index)).address === stagingAddress())
            stagedOutRefs.push(outRefOf({ txHash, outputIndex: index }));
      }
      expect(stagedOutRefs).toHaveLength(runs.length);
    },
    TIMEOUT,
  );

  it(
    "sends a spend back again, its exact signed bytes, when the ledger drops it after its confirmation",
    async () => {
      const { h } = at.scene;
      const live = new Map(
        (await stagedOutputs()).map((utxo) => [outRefOf(utxo), utxo]),
      );
      expect(live.size).toBeGreaterThan(0);
      let dropped: { txHash: string; consumes: string } | undefined;
      for (let turn = 0; turn < 30 && dropped === undefined; turn += 1) {
        const entries = entriesOf(await turns().runOnce());
        dropped = entries.flatMap(({ event }) =>
          event.kind === "submission_intent" &&
          event.actionInput.schemaVersion ===
            VALIDATION_TRACE_STAGING_RECLAIM &&
          live.has(event.actionInput.consumes as string) &&
          entries.some(
            ({ event: later }) =>
              later.kind === "confirmed" && later.txHash === event.txHash,
          )
            ? [
                {
                  txHash: event.txHash,
                  consumes: event.actionInput.consumes as string,
                },
              ]
            : [],
        )[0];
      }
      if (dropped === undefined) throw new Error("no spend back confirmed");
      const staging = live.get(dropped.consumes)!;
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
    "spends each staging output back to our wallet under the funding permit once the thread has settled",
    async () => {
      for (let turn = 0; turn < 30; turn += 1) {
        await turns().runOnce();
        if ((await stagedOutputs()).length === 0) break;
      }
      expect(await stagedOutputs()).toStrictEqual([]);
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
        expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
          at.scene.h.proverSigner.address,
        );
      }
    },
    TIMEOUT,
  );
});
