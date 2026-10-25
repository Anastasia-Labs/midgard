/** The funded watcher's scene over the staging of a staged builtin step's
 * operands (arm 31): a funded watcher opens our case; the scene's operator
 * defends it with a trace agreeing with ours up to the state before a staged
 * step (sha2_256 over 16 KB, two runs), and reveals from it; the watcher
 * bisects the thread to the PrepareSelected move's holder.
 *
 * The category is not installed until its defender exists; the scene stands
 * in for that install so the production classifier mints the objective's
 * actuation permit. */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import { validationTraceCaseExpiryMs } from "../../src/validation-trace-dispute/threads.js";
import type { ValidationTraceOneStepReferences } from "../../src/validation-trace-dispute/transactions.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import { drainProver } from "./installed-prover-wallet.js";
import { sceneOperator } from "./validation-trace-dispute-defend.js";
import type { CapturedMove } from "./validation-trace-dispute-funded-moves.js";
import { fundedOneStepTurns } from "./validation-trace-dispute-funded-one-step-turns.js";
import {
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./validation-trace-dispute-funded-runner.js";
import {
  caseAndBond,
  FUNDED,
} from "./validation-trace-dispute-funded-verify-source.js";
import {
  publishBuiltinArmReferences,
  publishOneStepReferences,
} from "./validation-trace-dispute-one-step.js";
import type { Side } from "./validation-trace-dispute-one-step-play.js";
import { lastStagedStep } from "./validation-trace-dispute-one-step-staged-drive.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./validation-trace-dispute-open-scene.js";
import {
  defendScripted,
  scriptedTrace,
  threadUnitOf,
} from "./validation-trace-dispute-scripted-defender.js";
import {
  STAGED_SHA2_SCRIPT,
  stagedScript,
  stagedStep,
} from "./validation-trace-dispute-staged-scripts.js";

/** The runs our staging of the step stages, in order. */
export const runs = SDK.planMidgardCekStaging(
  stagedStep(STAGED_SHA2_SCRIPT, 18n).stagedOperands,
);

/** The sha-256 of the run `datumCbor`, which its staging's action names. */
export const digestOf = (datumCbor: string) =>
  createHash("sha256").update(Buffer.from(datumCbor, "hex")).digest("hex");

/** The scene, opened by `open` (a test file's set-up), and what reads it;
 * a signed transaction of the watcher's is final `releaseDepth()` blocks
 * under the tip when given, read at each observation, else at the tip. */
export const fundedStagingScene = ({
  releaseDepth,
}: { readonly releaseDepth?: () => number } = {}) => {
  const at = {} as {
    scene: Scene;
    dispute: FundedDispute;
    side: Side;
    caseUnit: string;
    caseExpiresAtMs: number;
    bond: UTxO;
    threadAtPrepare: UTxO;
  };
  const moves: CapturedMove[] = [];
  const { turns, place, playTo } = fundedOneStepTurns({
    scene: () => at.scene,
    dispute: () => at.dispute,
    side: () => at.side,
    moves,
  });

  /** Our staging address. */
  const stagingAddress = () =>
    SDK.midgardCekStagingAddress(
      "Custom",
      at.scene.h.proverSigner.paymentKeyHash,
    );

  const open = async () => {
    let oneStep: ValidationTraceOneStepReferences | undefined;
    const scene = await setUpScene(
      async (contracts) => {
        oneStep = {
          ...(await publishOneStepReferences(contracts)),
          cekBuiltins: await publishBuiltinArmReferences(contracts),
        };
      },
      { failingScript: stagedScript(STAGED_SHA2_SCRIPT) },
    );
    at.scene = scene;
    await drainProver(scene.h as never, FUNDED);
    at.dispute = await openFundedDispute(scene, {
      oneStep: oneStep!,
      liveParameters: true,
      ...(releaseDepth === undefined ? {} : { releaseDepth }),
    });
    const opened = await turnUntil(at.dispute, VALIDATION_TRACE_CASE_OPEN);
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput, bond } = await caseAndBond(scene, opened.result);
    at.bond = bond;
    at.caseUnit = Object.keys(caseOutput.assets).find((unit) =>
      unit.startsWith(scene.chain.case.policyId),
    )!;
    at.caseExpiresAtMs = validationTraceCaseExpiryMs(
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
    at.side = { defender, trace, unit: threadUnitOf(scene, thread!) };
    await playTo("prepare");
    at.threadAtPrepare = (await place())!.utxo;
  };

  return {
    at,
    moves,
    turns,
    place,
    playTo,
    open,
    stagingAddress,
    /** Our staging outputs on the ledger now. */
    stagedOutputs: async () =>
      await at.scene.h.emulator.getUtxos(stagingAddress()),
    /** The deposit our staging of the run `datumCbor` holds. */
    depositOf: (datumCbor: string) =>
      SDK.midgardCekStagingDeposit({
        address: stagingAddress(),
        datumCbor,
        coinsPerUtxoByte: BigInt(
          at.scene.h.emulator.protocolParameters.coinsPerUtxoByte,
        ),
      }),
  };
};
