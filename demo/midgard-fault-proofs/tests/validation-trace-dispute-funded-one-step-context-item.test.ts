/** The validation-trace dispute's one-step resolution through the redeemer
 * item step of a CEK context step under the watcher's prover funding. A
 * funded watcher opens our case; the scene's operator defends it with a
 * trace agreeing with ours up to the state before a step the item bind stage
 * verifies, and reveals from it; the watcher bisects the thread and resolves
 * it through the item bind stage into the shared item chain, whose holders
 * read the pending item step from the thread's own L1 history.
 *
 * A watcher stopped after signing a hop at the shared chain and before its
 * bytes reach the ledger is started again, holding nothing of the stopped
 * process: it builds the hop again from the chain, and exactly one move
 * spends the thread. A history that pays the thread to the chain's entry
 * other than once holds the run under a named alarm with nothing signed, and the run
 * goes on once the history is whole again: the thread is awarded and closes
 * into our case.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import type {
  ValidationTraceCaseHistory,
  ValidationTraceCaseTransaction,
} from "../src/validation-trace-dispute/case-history.js";
import { VALIDATION_TRACE_ITEM_PENDING_HOLD } from "../src/validation-trace-dispute/threads.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
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
  scriptedTrace,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

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
 * `misstate` says; the raw L1 capture the chain view reads stays
 * authentic. */
const misstated = (
  authentic: ValidationTraceCaseHistory,
): ValidationTraceCaseHistory => ({
  ...(authentic.snapshot === undefined
    ? {}
    : { snapshot: authentic.snapshot.bind(authentic) }),
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

describe("validation-trace dispute one-step resolution through the redeemer item step under the watcher's prover funding", () => {
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
      caseHistory: misstated,
    });
    const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN);
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput } = await caseAndBond(scene, dispute);
    const defender = sceneOperator(scene);
    const trace = scriptedTrace(await failingMaterial(scene), AGREED_UP_TO);
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
    "builds the hop at the shared item chain again after a stop between signing and broadcast: exactly one move spends the thread, the next process's own",
    async () => {
      await playTo("redeemerItemTraversalNormalizer");
      const stop = await stopBeforeBroadcastAt(
        "redeemerItemTraversalNormalizer",
      );
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
      // The next process's hop, which read the pending item step from the
      // thread's history again, moved the thread along the chain.
      expect(recordedOutput(scene, `${spends[0]!.txHash}#0`).address).toBe(
        holderAddress("redeemerItemOuterNormalizer"),
      );
      const { emulator } = scene.h;
      await expect(emulator.submitTx(stop.hex)).rejects.toThrow();
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
        until: async () =>
          confirmedOneStepMoves(await dispute.confirmed()).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 40,
      });
      expect(await place(), history.join("\n")).toBeUndefined();
    },
    TIMEOUT,
  );
});
