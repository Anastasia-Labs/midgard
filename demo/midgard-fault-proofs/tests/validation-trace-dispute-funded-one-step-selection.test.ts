/** The validation-trace dispute's one-step resolution through a CEK script
 * selection under the watcher's prover funding. A funded watcher, on a
 * deployment that installs the selection resolver and its yields and names
 * the script-decode deployment, records the decode of the script our trace
 * selects before Init, through the shared script-decode prerequisite, and
 * opens our case; the scene's operator defends it with a trace agreeing with
 * ours up to the state before the selection, and reveals from it; the
 * watcher bisects the thread and resolves it through the CEK resolver to the
 * selection resolver, every move and record under the funding permit it
 * mints each turn. The selection's hop needs more than the per-move cap: a
 * wallet holding exactly the terms' wallet-move reserve pays it whole, and
 * its successor keeps all of the thread's lovelace; the awarded thread then
 * closes into our case.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import {
  validationTraceOneStepContracts,
  type ValidationTraceOneStepHolder,
} from "../src/validation-trace-dispute/one-step-holders.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import {
  type CapturedMove,
  captureMoves,
  recordedOutput,
} from "./support/validation-trace-dispute-funded-moves.js";
import {
  type AdapterWrap,
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
  publishOneStepReferences,
  publishSelectionReferences,
} from "./support/validation-trace-dispute-one-step.js";
import { SELECTION_STEP_FROM } from "./support/validation-trace-dispute-one-step-case.js";
import {
  confirmedOneStepMoves,
  playThreads,
  type Side,
  threadPlace,
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

let scene: Scene;
let dispute: FundedDispute;
let side: Side;
/** Every move the watcher built, with what it spent as it was then. */
const moves: CapturedMove[] = [];
/** The stage of each action the watcher preflighted, in order. */
const order: string[] = [];

const outRefOf = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The watcher process's turns, each move it builds captured. */
const turns = () => ({
  runOnce: async () => await dispute.turn(captureMoves(scene, moves)),
});

/** Where the thread stands now. */
const place = async () =>
  await threadPlace(scene, dispute.deployment, side.unit);

/** The address of the one-step holder `holder`. */
const holderAddress = (holder: ValidationTraceOneStepHolder) =>
  validationTraceOneStepContracts(dispute.deployment).find(
    (contract) => contract.holder === holder,
  )!.address;

/** Records the stage of each action the watcher preflights. */
const recordOrder: AdapterWrap = (adapter) => ({
  ...adapter,
  preflight: async (context) => {
    const preflight = await adapter.preflight(context);
    order.push(String(context.action.input.stage));
    return preflight;
  },
});

describe("validation-trace dispute one-step resolution through a CEK script selection under the prover funding permit", () => {
  beforeAll(async () => {
    // Published before the scene's blocks, so the publications take none of
    // the source verification's window from the failing block's end, and
    // before the prover's wallet is drained to its funding.
    let oneStep: ValidationTraceOneStepReferences | undefined;
    scene = await setUpScene(async (contracts) => {
      oneStep = {
        ...(await publishOneStepReferences(contracts)),
        cekSelection: await publishSelectionReferences(contracts),
      };
    });
    await drainProver(scene.h as never, FUNDED);
    dispute = await openFundedDispute(scene, { oneStep: oneStep! });
    const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, {
      wrap: recordOrder,
      limit: 30,
    });
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput } = await caseAndBond(scene, opened.result);
    const defender = sceneOperator(scene);
    const trace = scriptedTrace(
      await failingMaterial(scene),
      SELECTION_STEP_FROM,
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

  it("records the decode of the script our trace selects under the funding permit before Init", () => {
    const finish = order.indexOf("script_decode_finish");
    expect(order[0]).toBe("script_decode_stage");
    expect(finish).toBeGreaterThan(0);
    expect(order.indexOf("init")).toBe(finish + 1);
  });

  it(
    "pays the selection's hop, priced over the cap, wholly from a wallet holding exactly the wallet-move reserve, its successor keeping all of the thread's lovelace, then closes the awarded thread into our case",
    async () => {
      const { h } = scene;
      await playThreads({
        scene,
        runner: turns(),
        sides: [side],
        until: async () => (await place())?.holder === "selection",
        limit: 60,
      });
      const before = (await place())!.utxo;
      await topUpProver(h as never, 100n * ADA);
      await drainProver(h as never, await dispute.walletMoveReserve());
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
      const hops = landed.filter(
        ({ consumes }) => consumes === outRefOf(before),
      );
      expect(hops).toHaveLength(1);
      const hop = hops[0]!.txHash;
      const fee = await landedMoveFee(scene, hop);
      expect(fee.paid).toBeGreaterThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
      expect(fee.paid).toBeGreaterThanOrEqual(fee.ledger);
      expect(recordedOutput(scene, `${hop}#0`).assets.lovelace).toBe(
        before.assets.lovelace,
      );
      expect(recordedOutput(scene, `${hop}#0`).address).toBe(
        scene.chain.award.spendingScriptAddress,
      );
      expect(holderAddress("selection")).toBe(before.address);
      // Every other one-step move paid its real fee from its thread.
      for (const { txHash } of landed)
        if (txHash !== hop)
          expectRealMoveFee(await landedMoveFee(scene, txHash));
      await expectWithinWalletMoveShape(
        scene,
        landed.map(({ txHash }) => txHash),
      );
      for (const kind of ["enter_resolution", "resolve", "close_awarded"])
        reportFit(
          `funded selection ${kind}`,
          moves
            .filter(({ authority }) => authority.kind === kind)
            .map(({ signed }) => signed.toCBOR()),
        );
    },
    TIMEOUT,
  );
});
