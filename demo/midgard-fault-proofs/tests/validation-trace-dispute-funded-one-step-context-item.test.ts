/** The validation-trace dispute's one-step resolution through the redeemer
 * item step of a CEK context step under the watcher's prover funding. A
 * funded watcher opens our case; the scene's operator defends it with a
 * trace agreeing with ours up to the state before a step the item bind stage
 * verifies, and reveals from it; the watcher bisects the thread and resolves
 * it through the item bind stage into the shared item chain, whose holders
 * read the pending item step from the thread's own L1 history.
 *
 * A watcher stopped after signing a hop at the shared chain and before its
 * bytes reach the ledger offers only those bytes again until they expire,
 * then makes a single re-signed attempt: exactly one move spends the
 * thread. A history that pays the thread to the chain's entry other than
 * once holds the run under a named alarm with nothing signed, and the run
 * goes on once the history is whole again: the thread is awarded and closes
 * into our case.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import type {
  ValidationTraceCaseHistory,
  ValidationTraceCaseTransaction,
} from "../src/validation-trace-dispute/case-history.js";
import { VALIDATION_TRACE_ITEM_PENDING_HOLD } from "../src/validation-trace-dispute/threads.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { drainProver } from "./support/installed-prover-wallet.js";
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
  caseAndBond,
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  publishContextReferences,
  publishOneStepReferences,
} from "./support/validation-trace-dispute-one-step.js";
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

/** The defender's last agreed state: the next step is a selection scan's
 * step the item bind stage verifies. */
const AGREED_UP_TO = 95;

let scene: Scene;
let dispute: FundedDispute;
let side: Side;
/** Every move the watcher built, with what it spent as it was then. */
const moves: CapturedMove[] = [];
/** How the history the watcher reads misstates the thread's payment to the
 * shared item chain's entry, if it does. */
let misstate: "dropped" | "repeated" | undefined;

const { turns, place, playTo, holderAddress, stopBeforeBroadcastAt } =
  fundedOneStepTurns({
    scene: () => scene,
    dispute: () => dispute,
    side: () => side,
    moves,
  });

/** Whether `transaction` pays the thread to the shared item chain's entry. */
const paysEntry = ({ bodyCbor }: ValidationTraceCaseTransaction) => {
  const outputs = CML.TransactionBody.from_cbor_hex(bodyCbor).outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === holderAddress("contextItemEntry") &&
      output.assets[side.unit] === 1n
    )
      return true;
  }
  return false;
};

/** The authentic history, with the thread's own history misstated as
 * `misstate` says. */
const misstated = (
  authentic: ValidationTraceCaseHistory,
): ValidationTraceCaseHistory => ({
  transactions: async (input) => {
    const transactions = await authentic.transactions(input);
    if (misstate === undefined || input.caseUnit !== side.unit)
      return transactions;
    return transactions.flatMap((transaction) =>
      !paysEntry(transaction)
        ? [transaction]
        : misstate === "dropped"
          ? []
          : [transaction, transaction],
    );
  },
});

describe("validation-trace dispute one-step resolution through the redeemer item step under the prover funding permit", () => {
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
      caseHistory: misstated,
    });
    const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN);
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput } = await caseAndBond(scene, opened.result);
    const defender = sceneOperator(scene);
    const trace = scriptedTrace(await failingMaterial(scene), AGREED_UP_TO);
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
    "supersedes a hop at the shared item chain, stopped after signing and before broadcast, with one re-signed attempt once its bytes expire: exactly one move spends the thread",
    async () => {
      await playTo("redeemerItemTraversalNormalizer");
      const stop = await stopBeforeBroadcastAt(
        "redeemerItemTraversalNormalizer",
      );
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
      // The re-signed attempt, which read the pending item step from the
      // thread's history again, moved the thread along the chain.
      expect(recordedOutput(scene, `${spends[0]!.txHash}#0`).address).toBe(
        holderAddress("redeemerItemOuterNormalizer"),
      );
      const { emulator } = scene.h;
      await expect(emulator.submitTx(stop.hex)).rejects.toThrow();
      expect((await emulator.getTransactionStatus(stop.hash)).status).not.toBe(
        "confirmed",
      );
    },
    TIMEOUT,
  );

  it.each([
    ["dropped", 0],
    ["repeated", 2],
  ] as const)(
    "holds the run under a named alarm, with nothing signed, while the thread's history pays it to the chain's entry %s (%i times)",
    async (how, times) => {
      await playTo("redeemerItemOuterNormalizer");
      const before = (await place())!.utxo;
      const built = moves.length;
      misstate = how;
      try {
        for (let run = 0; run < 2; run += 1) {
          const result = validationTraceRunState(await turns().runOnce());
          expect(result).toMatchObject({
            kind: "pending",
            resumeOnObservation: true,
            namedState: {
              name: VALIDATION_TRACE_ITEM_PENDING_HOLD,
              alarm: true,
            },
          });
          if (!("reason" in result)) throw new Error("no reason");
          expect(result.reason).toContain(`${side.unit} `);
          expect(result.reason).toContain(` ${times.toString()} times`);
        }
      } finally {
        misstate = undefined;
      }
      expect(moves).toHaveLength(built);
      expect(outRefOf((await place())!.utxo)).toBe(outRefOf(before));
    },
    TIMEOUT,
  );

  it(
    "goes on once the history is whole: the thread is awarded and closes into our case",
    async () => {
      const { history } = await playThreads({
        scene,
        runner: turns(),
        sides: [side],
        until: async (run) =>
          "entries" in run &&
          confirmedOneStepMoves(run.entries).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 40,
      });
      expect(await place(), history.join("\n")).toBeUndefined();
    },
    TIMEOUT,
  );
});
