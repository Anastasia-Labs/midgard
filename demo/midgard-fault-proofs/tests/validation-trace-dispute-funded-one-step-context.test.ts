/** The validation-trace dispute's one-step resolution through a CEK context
 * step under the watcher's prover funding. A funded watcher opens our case;
 * the scene's operator defends it with a trace agreeing with ours up to the
 * state before a step the context reference stage verifies, and reveals from
 * it; the watcher bisects the thread and resolves it through the CEK
 * resolver to the context step resolver, its control, the stage and the
 * context settle.
 *
 * A watcher stopped after signing the context step resolver's hop and before
 * its bytes reach the ledger is started again, holding nothing of the
 * stopped process: it builds the hop again from the chain, and exactly one
 * move spends the thread. With fees risen until the
 * stage's hop is priced over the per-move cap, a wallet holding exactly the
 * wallet-move reserve pays that hop
 * whole and its successor keeps all of the thread's lovelace; with fees back,
 * the thread settles, is awarded and closes into our case.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import { CEK_CONTEXT_STAGE_HOLDERS } from "../src/validation-trace-dispute/one-step-holders.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
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
  playThreads,
  sendsFor,
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
    dispute = await openFundedDispute(scene, { oneStep: oneStep! });
    const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN);
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput } = await caseAndBond(scene, dispute);
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
      parseOutRefLabel(threadOutRef),
    ]);
    side = { defender, trace, unit: threadUnitOf(scene, thread!) };
  }, 1_800_000);

  it(
    "builds the context step resolver's hop again after a stop between signing and broadcast: exactly one move spends the thread, the next process's own",
    async () => {
      await playTo("context");
      const stop = await stopBeforeBroadcastAt("context");
      dispute.restart();
      const { history } = await playThreads({
        scene,
        runner: turns(),
        sides: [side],
        until: async () => await confirmsSpendOf(dispute, stop.consumes),
        limit: 20,
      });
      const spends = confirmedOneStepMoves(await dispute.confirmed()).filter(
        (move) => move.consumes === stop.consumes,
      );
      expect(spends, history.join("\n")).toHaveLength(1);
      // The next process knows nothing of the stopped bytes: it rebroadcasts
      // nothing and sends the hop it builds from the chain, which may be the
      // very same transaction.
      expect(dispute.rebroadcasts).not.toContain(stop.hex);
      const sends = sendsFor(dispute.sent(), "resolve", stop.consumes);
      expect(sends).toHaveLength(2);
      expect(sends[0]!.txHash).toBe(stop.hash);
      expect(spends[0]!.txHash).toBe(sends[1]!.txHash);
      // The next process's hop moved the thread to the context control.
      expect(recordedOutput(scene, `${spends[0]!.txHash}#0`).address).toBe(
        holderAddress("contextControl"),
      );
      const { emulator } = scene.h;
      await expect(emulator.submitTx(stop.hex)).rejects.toThrow();
    },
    TIMEOUT,
  );

  it(
    "pays the stage's hop priced over the cap from a wallet holding exactly the wallet-move reserve, its successor keeping all of the thread's lovelace, then settles, is awarded and closes into our case",
    async () => {
      const { h } = scene;
      await playTo(STAGE);
      const before = (await place())!.utxo;
      // Fees rise until the stage's hop is priced over the per-move cap.
      const parameters = h.emulator.protocolParameters;
      h.emulator.protocolParameters = { ...parameters, minFeeA: 300 };
      let hop: string;
      try {
        await topUpProver(h as never, 100n * ADA);
        await drainProver(h as never, dispute.walletMoveReserve());
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
      const { history } = await playThreads({
        scene,
        runner: turns(),
        sides: [side],
        until: async () =>
          confirmedOneStepMoves(await dispute.confirmed()).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 20,
      });
      expect(await place(), history.join("\n")).toBeUndefined();
      const landed = confirmedOneStepMoves(await dispute.confirmed());
      expect(landed.at(-1)?.stage).toBe("close_awarded");
      // The ledger confirms the hop as the one move spending its thread.
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
            .filter((move) => move.kind === kind)
            .map(({ signed }) => signed.toCBOR()),
        );
    },
    TIMEOUT,
  );
});
