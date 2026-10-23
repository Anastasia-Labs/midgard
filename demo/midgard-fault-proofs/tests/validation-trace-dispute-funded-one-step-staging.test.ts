/** The validation-trace dispute's staging of a staged builtin step's
 * operands (arm 31) under the watcher's prover funding. A funded watcher
 * opens our case; the scene's operator defends it with a trace agreeing
 * with ours up to the state before a staged step, and reveals from it; the
 * watcher bisects the thread to the PrepareSelected move's holder.
 *
 * A wallet too short to build the step's first staging holds it under the
 * operator's alarm, naming the case, the thread, the deadline and what the
 * step still needs from our wallet, before any byte reaches the ledger,
 * releasing what it reserved. A wallet that can build the first staging but
 * not the next stages the first and holds the next the same way, the first
 * run's staging output live and no wallet coin spent twice. Topped up, each
 * run left is staged under the permit, the thread plays through the staged
 * chain to the award, and each staging output is spent back to our wallet
 * under the permit.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import {
  confirmedIntents,
  VALIDATION_TRACE_STAGING_RECLAIM,
} from "../src/validation-trace-dispute/one-step-staging-journal.js";
import { validationTraceCaseExpiryMs } from "../src/validation-trace-dispute/threads.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import { watchLedger } from "./support/own-outputs-installed.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import type { CapturedMove } from "./support/validation-trace-dispute-funded-moves.js";
import {
  fundedOneStepTurns,
  outRefOf,
} from "./support/validation-trace-dispute-funded-one-step-turns.js";
import {
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  caseAndBond,
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  publishBuiltinArmReferences,
  publishOneStepReferences,
} from "./support/validation-trace-dispute-one-step.js";
import {
  confirmedOneStepMoves,
  playThreads,
  type Side,
} from "./support/validation-trace-dispute-one-step-play.js";
import {
  entriesOf,
  lastStagedStep,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  defendScripted,
  reportFit,
  scriptedTrace,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";
import {
  STAGED_SHA2_SCRIPT,
  stagedScript,
  stagedStep,
} from "./support/validation-trace-dispute-staged-scripts.js";

let scene: Scene;
let dispute: FundedDispute;
let side: Side;
let caseUnit: string;
let caseExpiresAtMs: number;
let threadAtPrepare: UTxO;
const moves: CapturedMove[] = [];
/** The staging outputs the funded play staged, one per run. */
const stagedOutRefs: string[] = [];

const { turns, place, playTo } = fundedOneStepTurns({
  scene: () => scene,
  dispute: () => dispute,
  side: () => side,
  moves,
});

/** The runs our staging of the step stages, in order. */
const runs = SDK.planMidgardCekStaging(
  stagedStep(STAGED_SHA2_SCRIPT, 18n).stagedOperands,
);

/** Our staging address. */
const stagingAddress = () =>
  SDK.midgardCekStagingAddress("Custom", scene.h.proverSigner.paymentKeyHash);

/** Our staging outputs on the ledger now. */
const stagedOutputs = async () =>
  await scene.h.emulator.getUtxos(stagingAddress());

/** The deposit our staging of the run `datumCbor` holds. */
const depositOf = (datumCbor: string) =>
  SDK.midgardCekStagingDeposit({
    address: stagingAddress(),
    datumCbor,
    coinsPerUtxoByte: BigInt(
      scene.h.emulator.protocolParameters.coinsPerUtxoByte,
    ),
  });

/** The sha-256 of the run `datumCbor`, which its staging's action names. */
const digestOf = (datumCbor: string) =>
  createHash("sha256").update(Buffer.from(datumCbor, "hex")).digest("hex");

/**
 * Turns the watcher twice from a wallet holding `held` and expects each
 * turn to hold the staging of the step's run `index` under the operator's
 * alarm, naming what the step still needs, before any byte of it reaches
 * the ledger, releasing what it reserved; returns our transactions the
 * ledger took meanwhile.
 */
const expectHeldUnderAlarm = async (held: bigint, index: number) => {
  const { h } = scene;
  await topUpProver(h as never, 200n * ADA);
  await drainProver(h as never, held);
  const watch = watchLedger(h.emulator);
  try {
    await holdTwice(index);
  } finally {
    watch.restore();
  }
  expect(outRefOf((await place())!.utxo)).toBe(outRefOf(threadAtPrepare));
  return watch.ours();
};

/** Two turns, each holding the staging of the step's run `index` under the
 * operator's alarm, releasing what it reserved. */
const holdTwice = async (index: number) => {
  for (let pause = 0; pause < 2; pause += 1) {
    const { result, history } = await turnUntil(
      dispute,
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
      `on thread ${outRefOf(threadAtPrepare)} of case ${caseUnit},`,
    );
    // The PrepareSelected move it waits on must land before the case
    // expires.
    expect(result.reason).toContain(
      `before its deadline ${caseExpiresAtMs.toString()};`,
    );
    expect(validationTraceRunState(result)).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      namedState: {
        name: VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
        alarm: true,
      },
    });
    for (const reservation of await dispute.readReservations())
      expect(reservation).toMatchObject({ activeInputs: [] });
  }
};

describe("validation-trace dispute staging of a staged builtin step's operands under the prover funding permit", () => {
  beforeAll(async () => {
    let oneStep: ValidationTraceOneStepReferences | undefined;
    scene = await setUpScene(
      async (contracts) => {
        oneStep = {
          ...(await publishOneStepReferences(contracts)),
          cekBuiltins: await publishBuiltinArmReferences(contracts),
        };
      },
      { failingScript: stagedScript(STAGED_SHA2_SCRIPT) },
    );
    await drainProver(scene.h as never, FUNDED);
    dispute = await openFundedDispute(scene, {
      oneStep: oneStep!,
      liveParameters: true,
    });
    const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN);
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput } = await caseAndBond(scene, opened.result);
    caseUnit = Object.keys(caseOutput.assets).find((unit) =>
      unit.startsWith(scene.chain.case.policyId),
    )!;
    caseExpiresAtMs = validationTraceCaseExpiryMs(
      Data.from(caseOutput.datum!, SDK.Case),
    );
    const defender = sceneOperator(scene);
    const material = await failingMaterial(scene);
    const trace = scriptedTrace(material, await lastStagedStep(material));
    const threadOutRef = await defendScripted(
      scene,
      caseOutput,
      defender,
      trace,
    );
    const [thread] = await scene.h.emulator.getUtxosByOutRef([
      parseOwnOutRef(threadOutRef),
    ]);
    side = { defender, trace, unit: threadUnitOf(scene, thread!) };
    await playTo("prepare");
    threadAtPrepare = (await place())!.utxo;
  }, 1_800_000);

  afterAll(async () => {
    await dispute?.close();
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
        await scene.h.emulator.getUtxosByOutRef(consumed.map(parseOwnOutRef)),
      ).toStrictEqual([]);
    },
    TIMEOUT,
  );

  it(
    "stages each run left under the funding permit once our wallet is topped up, and plays the thread through the staged chain to the award",
    async () => {
      await topUpProver(scene.h as never, 400n * ADA);
      const { result, history } = await playThreads({
        scene,
        runner: turns(),
        sides: [side],
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
                cbor: scene.l1.signedCbors.get(event.txHash)!,
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
    "spends each staging output back to our wallet under the funding permit once the thread has settled",
    async () => {
      const reclaimsOf = (result: FraudProofWorkflowRunResult) =>
        confirmedIntents(entriesOf(result), VALIDATION_TRACE_STAGING_RECLAIM);
      let last = await turns().runOnce();
      for (let turn = 0; turn < 30; turn += 1) {
        if (
          (await stagedOutputs()).length === 0 &&
          reclaimsOf(last).length === runs.length
        )
          break;
        last = await turns().runOnce();
      }
      expect(await stagedOutputs()).toStrictEqual([]);
      const reclaims = reclaimsOf(last);
      expect(
        reclaims.map(({ actionInput }) => String(actionInput.consumes)).sort(),
      ).toEqual([...stagedOutRefs].sort());
      for (const { txHash } of reclaims) {
        const body = CML.Transaction.from_cbor_hex(
          scene.l1.signedCbors.get(txHash)!,
        ).body();
        expect(body.inputs().len()).toBe(1);
        expect(body.outputs().len()).toBe(1);
        expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
          scene.h.proverSigner.address,
        );
      }
    },
    TIMEOUT,
  );
});
