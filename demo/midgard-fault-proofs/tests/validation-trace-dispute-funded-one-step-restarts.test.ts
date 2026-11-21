/** The validation-trace dispute's one-step resolution across a watcher
 * process stop, under the prover funding permit, on the emulator. A funded
 * watcher opens our case; the scene's operator defends it with a trace that
 * agrees with ours up to the state before a CEK compute step and reveals its
 * midpoints from that trace; the watcher bisects the thread and enters its
 * one-step resolution, whose holders its funding terms name from the signed
 * deployment. The watcher signs a resolution move and journals it, and
 * stops before its bytes reach the ledger. The next process offers only the
 * stopped bytes again until they expire, then makes a single re-signed
 * attempt, and the stopped bytes can no longer land: exactly one move
 * spends the stopped move's thread.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { beforeAll, describe, expect, it } from "vitest";

import { parseValidationTraceAction } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import {
  type AdapterWrap,
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  caseAndBond,
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import { publishOneStepReferences } from "./support/validation-trace-dispute-one-step.js";
import {
  confirmedOneStepMoves,
  intentsFor,
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
  scriptedTrace,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

/** The defender's last agreed state: the next step is a CEK compute step. */
const AGREED_UP_TO = 116;

let scene: Scene;
let dispute: FundedDispute;
let side: Side;

const outRefOf = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The watcher process's turns, as the play runs them. */
const turns = (wrap?: AdapterWrap) => ({
  runOnce: async () => await dispute.turn(wrap),
});

/**
 * Stops the watcher process at the submit of the first resolution move it
 * signs, once its intent is durable and before its bytes reach the ledger.
 * The stopped turn never returns.
 */
const stopBeforeResolveBroadcast = async () => {
  let signed:
    | { readonly hash: string; readonly hex: string; readonly consumes: string }
    | undefined;
  let stopped!: () => void;
  const stop = new Promise<void>((resolve) => {
    stopped = resolve;
  });
  const wrap: AdapterWrap = (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      const { stage, consumes } = parseValidationTraceAction(context.action);
      const transaction = workflowPreflightTransaction(preflight);
      if (stage === "resolve" && transaction !== undefined)
        signed ??= {
          hash: transaction.toHash(),
          hex: transaction.toTransaction().to_cbor_hex(),
          consumes: consumes!,
        };
      return preflight;
    },
    submit: async (input) => {
      if (
        signed === undefined ||
        parseValidationTraceAction(input.action).consumes !== signed.consumes
      )
        return await adapter.submit(input);
      stopped();
      return await new Promise<never>(() => undefined);
    },
  });
  const play = playThreads({
    scene,
    runner: turns(wrap),
    sides: [side],
    until: async () => false,
    limit: 40,
  });
  await Promise.race([
    stop,
    play.then(() => {
      throw new Error("the process made no resolution move");
    }),
  ]);
  if (signed === undefined) throw new Error("no resolution move was signed");
  return signed;
};

/** Whether the journal `result` holds confirms a move spending `consumes`. */
const confirmsSpendOf = (
  result: FraudProofWorkflowRunResult,
  consumes: string,
) =>
  "entries" in result &&
  confirmedOneStepMoves(result.entries).some(
    (move) => move.consumes === consumes,
  );

describe("validation-trace dispute one-step resolution across a watcher process stop", () => {
  beforeAll(async () => {
    scene = await setUpScene();
    // Published before the prover's wallet is drained to its funding.
    const oneStep = await publishOneStepReferences(scene);
    await drainProver(scene.h as never, FUNDED);
    dispute = await openFundedDispute(scene, { oneStep });
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

  it(
    "supersedes a resolution move's unsent signed bytes once they expire after a stop before broadcast: one re-signed attempt lands, and the stopped bytes can no longer land",
    async () => {
      try {
        const stop = await stopBeforeResolveBroadcast();
        await dispute.restart();
        const { result, history } = await playThreads({
          scene,
          runner: turns(),
          sides: [side],
          until: async (run) => confirmsSpendOf(run, stop.consumes),
          limit: 20,
        });
        if (!("entries" in result)) throw new Error("no workflow journal");
        // Exactly one move spent the stopped move's thread.
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
        const place = await threadPlace(scene, dispute.deployment, side.unit);
        expect(place === undefined ? undefined : outRefOf(place.utxo)).not.toBe(
          stop.consumes,
        );
        const { emulator } = scene.h;
        await expect(emulator.submitTx(stop.hex)).rejects.toThrow();
        expect(
          (await emulator.getTransactionStatus(stop.hash)).status,
        ).not.toBe("confirmed");
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
