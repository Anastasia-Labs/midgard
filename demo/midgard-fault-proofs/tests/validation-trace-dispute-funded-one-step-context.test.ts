/** The validation-trace dispute's one-step resolution through a CEK context
 * step under the watcher's prover funding, its terms following the
 * emulator's live fees. A funded watcher opens our case; the scene's operator
 * defends it with a trace agreeing with ours up to the state before a step
 * the context reference stage verifies, and reveals from it; the watcher
 * bisects the thread and resolves it through the CEK resolver to the context
 * step resolver, its control, the stage and the context settle, every move
 * under the funding permit it mints each turn.
 *
 * A watcher stopped after signing the context step resolver's hop and before
 * its bytes reach the ledger offers only those bytes again until they
 * expire, then makes a single re-signed attempt: exactly one move spends the
 * thread. With fees risen until the stage's hop is priced over the per-move
 * cap, a wallet holding exactly the terms' wallet-move reserve pays that hop
 * whole and its successor keeps all of the thread's lovelace; with fees back,
 * the thread settles, is awarded and closes into our case.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import { CEK_CONTEXT_STAGE_HOLDERS } from "../src/validation-trace-dispute/one-step-holders.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import {
  type CapturedMove,
  recordedOutput,
} from "./support/validation-trace-dispute-funded-moves.js";
import {
  confirmsSpendOf,
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
  expectRealMoveFee,
  expectWithinWalletMoveShape,
  landedMoveFee,
} from "./support/validation-trace-dispute-move-fees.js";
import {
  publishContextReferences,
  publishOneStepReferences,
} from "./support/validation-trace-dispute-one-step.js";
import { CONTEXT_STAGE_STEP_FROM } from "./support/validation-trace-dispute-one-step-context-drive.js";
import {
  confirmedOneStepMoves,
  intentsFor,
  playThreads,
  type Side,
} from "./support/validation-trace-dispute-one-step-play.js";
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

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

const STAGE = CEK_CONTEXT_STAGE_HOLDERS.reference;

let scene: Scene;
let dispute: FundedDispute;
let side: Side;
/** Every move the watcher built, with what it spent as it was then. */
const moves: CapturedMove[] = [];

const { turns, place, playTo, holderAddress, stopBeforeBroadcastAt } =
  fundedOneStepTurns({
    scene: () => scene,
    dispute: () => dispute,
    side: () => side,
    moves,
  });

describe("validation-trace dispute one-step resolution through a CEK context step under the prover funding permit", () => {
  beforeAll(async () => {
    // Published before the scene's blocks, so the publications take none of
    // the source verification's window from the failing block's end, and
    // before the prover's wallet is drained to its funding.
    let oneStep: ValidationTraceOneStepReferences | undefined;
    scene = await setUpScene(async (contracts) => {
      oneStep = {
        ...(await publishOneStepReferences(contracts)),
        cekContext: await publishContextReferences(contracts),
      };
    });
    await drainProver(scene.h as never, FUNDED);
    dispute = await openFundedDispute(scene, {
      oneStep: oneStep!,
      liveParameters: true,
    });
    const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN);
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput } = await caseAndBond(scene, opened.result);
    const defender = sceneOperator(scene);
    const trace = scriptedTrace(
      await failingMaterial(scene),
      CONTEXT_STAGE_STEP_FROM.reference,
    );
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
  }, 1_800_000);

  afterAll(async () => {
    await dispute?.close();
  });

  it(
    "supersedes the context step resolver's hop, stopped after signing and before broadcast, with one re-signed attempt once its bytes expire: exactly one move spends the thread",
    async () => {
      await playTo("context");
      const stop = await stopBeforeBroadcastAt("context");
      await dispute.restart();
      const { result, history } = await playThreads({
        scene,
        runner: turns(),
        sides: [side],
        until: async (run) => confirmsSpendOf(run, stop.consumes),
        limit: 20,
      });
      if (!("entries" in result)) throw new Error("no workflow journal");
      const spends = confirmedOneStepMoves(result.entries).filter(
        (move) => move.consumes === stop.consumes,
      );
      expect(spends, history.join("\n")).toHaveLength(1);
      // Only the stopped process's exact bytes are ever offered again.
      expect(dispute.rebroadcasts.length).toBeGreaterThan(0);
      for (const offered of dispute.rebroadcasts)
        expect(offered).toBe(stop.hex);
      const intents = intentsFor(result.entries, "resolve", stop.consumes);
      expect(intents.map(({ attempt }) => attempt)).toStrictEqual([1, 2]);
      expect(intents[0]!.txHash).toBe(stop.hash);
      expect(intents[1]!.txHash).not.toBe(stop.hash);
      expect(spends[0]!.txHash).toBe(intents[1]!.txHash);
      // The re-signed attempt moved the thread to the context control.
      expect(recordedOutput(scene, `${spends[0]!.txHash}#0`).address).toBe(
        holderAddress("contextControl"),
      );
      const { emulator } = scene.h;
      await expect(emulator.submitTx(stop.hex)).rejects.toThrow();
      expect((await emulator.getTransactionStatus(stop.hash)).status).not.toBe(
        "confirmed",
      );
    },
    TIMEOUT,
  );

  it(
    "pays the stage's hop priced over the cap from a wallet holding exactly the wallet-move reserve, its successor keeping all of the thread's lovelace, then settles, is awarded and closes into our case",
    async () => {
      const { h } = scene;
      await playTo(STAGE);
      const before = (await place())!.utxo;
      // Fees rise until the stage's hop is priced over the per-move cap,
      // and stay under the funding permit's ceiling at the fees the watcher
      // read when it started.
      const parameters = h.emulator.protocolParameters;
      h.emulator.protocolParameters = { ...parameters, minFeeA: 300 };
      let hop: string;
      try {
        await topUpProver(h as never, 100n * ADA);
        await drainProver(h as never, await dispute.walletMoveReserve());
        // Until the hop lands, before any later move is built at these fees.
        await playThreads({
          scene,
          runner: turns(),
          sides: [side],
          until: async () =>
            outRefOf((await place())!.utxo) !== outRefOf(before),
          limit: 6,
        });
        const landedHops = moves.filter(
          ({ thread, signed }) =>
            outRefOf(thread) === outRefOf(before) &&
            scene.l1.signedCbors.has(signed.toHash()),
        );
        expect(landedHops).toHaveLength(1);
        hop = landedHops[0]!.signed.toHash();
      } finally {
        h.emulator.protocolParameters = parameters;
      }
      const fee = await landedMoveFee(scene, hop);
      expect(fee.paid).toBeGreaterThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
      expect(fee.paid).toBeGreaterThanOrEqual(fee.ledger);
      expect(recordedOutput(scene, `${hop}#0`).assets.lovelace).toBe(
        before.assets.lovelace,
      );
      expect(recordedOutput(scene, `${hop}#0`).address).toBe(
        holderAddress("contextSettle"),
      );
      // Fees back: the thread settles, is awarded and closes into our case.
      await topUpProver(h as never, 100n * ADA);
      const { result, history } = await playThreads({
        scene,
        runner: turns(),
        sides: [side],
        until: async (run) =>
          "entries" in run &&
          confirmedOneStepMoves(run.entries).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 20,
      });
      expect(await place(), history.join("\n")).toBeUndefined();
      if (!("entries" in result)) throw new Error("no workflow journal");
      const landed = confirmedOneStepMoves(result.entries);
      expect(landed.at(-1)?.stage).toBe("close_awarded");
      // The journal confirms the hop as the one move spending its thread.
      expect(
        landed
          .filter(({ consumes }) => consumes === outRefOf(before))
          .map(({ txHash }) => txHash),
      ).toStrictEqual([hop]);
      // Every other one-step move paid its real fee from its thread.
      for (const { txHash } of landed)
        if (txHash !== hop)
          expectRealMoveFee(await landedMoveFee(scene, txHash));
      await expectWithinWalletMoveShape(
        scene,
        landed.map(({ txHash }) => txHash),
      );
      const kinds = ["enter_resolution", "resolve", "close_awarded"];
      for (const kind of kinds)
        reportFit(
          `funded context ${kind}`,
          moves
            .filter(({ authority }) => authority.kind === kind)
            .map(({ signed }) => signed.toCBOR()),
        );
    },
    TIMEOUT,
  );
});
