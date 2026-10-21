/** The validation-trace dispute runner timing out a silent defender, on the
 * emulator. Two operators defend our case with scripted traces. While a
 * defender's clock runs, the runner reports the thread as awaiting its
 * counterparty until that clock's end. One defender never reveals, and
 * retires and recovers its bond, leaving both operator lists; once its clock
 * is spent the runner still moves its thread to the timeout validator and
 * closes it into our case, which mints the defender's lost-defense receipt
 * and returns the rest of the thread to us. The other defender reveals and
 * leaves the thread on our turn; the runner never times out a thread on our
 * turn, and once our own clock on it is spent it reports that thread lost.
 * game-v1 refuses a timeout one slot before the defender's clock ends, a
 * timeout of a thread on our turn while our clock runs, and a reveal of ours
 * whose validity range is past our clock's end. A move under the per-move
 * cap is paid by its thread, one over it by our wallet alone, its successor
 * keeping all the thread's lovelace; game-v1 refuses one keeping less. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Constr,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  confirmedValidationTraceStages,
  parseValidationTraceAction,
} from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceClaimMaterial } from "../src/validation-trace-dispute/claim-material.js";
import {
  buildValidationTraceGameMove,
  challengerRevealedThread,
  type ValidationTraceGameAction,
  validationTraceMoveWindow,
} from "../src/validation-trace-dispute/move-builders.js";
import {
  completeValidationTraceMove,
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
} from "../src/validation-trace-dispute/move-fee.js";
import {
  VALIDATION_TRACE_THREAD_COUNT_HOLD,
  VALIDATION_TRACE_THREAD_LOST,
  validationTraceDefenderDeadlineMs,
} from "../src/validation-trace-dispute/threads.js";
import {
  awaitingValidationTraceCounterparty,
  validationTraceRunState,
} from "../src/validation-trace-dispute/waiting.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { enrollSuccessor } from "./support/rotated-successor-operator.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  type Defender,
  sceneOperator,
} from "./support/validation-trace-dispute-defend.js";
import {
  expectRealMoveFee,
  landedMoveFee,
} from "./support/validation-trace-dispute-move-fees.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  operatorListsHolding,
  removeOperatorFromBothLists,
} from "./support/validation-trace-dispute-removed-defender.js";
import {
  makeRunner,
  type Runner,
  runnerDeployment,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";
import {
  defendScripted,
  isTurn,
  liveThread,
  reportFit,
  revealAsDefender,
  type ScriptedTrace,
  scriptedTrace,
  threadOf,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

const RECEIPT_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);

let scene: Scene;
let runner: Runner;
let material: ValidationTraceClaimMaterial;
let caseOutRef: string;
let operator: Defender;
let second: Defender;
let traceA: ScriptedTrace;
let traceB: ScriptedTrace;
/** The silent defender's thread, defended before any case runs. */
let threadA: string;
/** The silent defender's thread, and the revealing defender's. */
let unitA: string;
let unitB: string;
/** Thread B on our turn, as its defender's reveal left it. */
let revealedB: UTxO;
/** The signed bytes of each move the runner prepared, by stage. */
const prepared: { stage: string; cbor: string }[] = [];

const at = async (outRef: string): Promise<UTxO | undefined> =>
  (await scene.h.proverLucid.utxosByOutRef([parseOwnOutRef(outRef)]))[0];

const live = async (unit: string): Promise<UTxO> => {
  const utxo = await liveThread(scene, unit);
  if (utxo === undefined) throw new Error("the thread is not live");
  return utxo;
};

/** A game move by the prover on `thread`, from the emulator's now. */
const gameMove = (
  thread: UTxO,
  action: ValidationTraceGameAction,
  successor: (lower: bigint, upper: bigint) => SDK.CaseThread,
  address: string,
) => {
  const lucid = scene.h.proverLucid;
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const datum = Data.from(thread.datum!, SDK.CaseThreadDatum);
  const deployment = runnerDeployment(scene);
  return async () =>
    await completeValidationTraceMove({
      lucid,
      stage: action.kind,
      referenced: [deployment.references.game],
      build: (fee) =>
        buildValidationTraceGameMove({
          lucid,
          deployment,
          thread,
          action,
          successor: {
            address,
            datum: Data.to(
              {
                ...datum,
                data: successor(BigInt(validFrom), BigInt(validTo - 1)),
              },
              SDK.CaseThreadDatum,
            ),
          },
          fee,
          validFrom,
          validTo,
        }),
    });
};

/** Moves the emulator to the last slot whose time is not past `time`. */
const awaitLastSlotAtOrBefore = (time: number) => {
  const lucid = scene.h.proverLucid;
  let slot = lucid.unixTimeToSlot(time);
  while (lucid.slotToUnixTime(slot) > time) slot -= 1;
  while (lucid.slotToUnixTime(slot + 1) <= time) slot += 1;
  scene.h.emulator.awaitSlot(slot - lucid.currentSlot());
  expect(lucid.currentSlot()).toBe(slot);
};

describe("validation-trace dispute runner: timing out a silent defender", () => {
  beforeAll(async () => {
    scene = await setUpScene();
    const successor = await enrollSuccessor(
      scene.h as never,
      scene.operator.vkey,
    );
    runner = makeRunner(scene, {
      wrap: (adapter) => ({
        ...adapter,
        preflight: async (context) => {
          const preflight = await adapter.preflight(context);
          const signed = workflowPreflightTransaction(preflight);
          if (signed !== undefined)
            prepared.push({
              stage: parseValidationTraceAction(context.action).stage,
              cbor: signed.toCBOR(),
            });
          return preflight;
        },
      }),
    });
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    caseOutRef = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!.outputs[0]!.outRef;
    prepared.length = 0;
    material = await failingMaterial(scene);
    const stepCount = Number(material.stepCount);
    operator = sceneOperator(scene);
    second = {
      vkey: successor.keyHash,
      nodeUnit: successor.activeNodeUnit,
      lucid: successor.lucid,
    };
    traceA = scriptedTrace(material, Math.floor(stepCount / 3));
    traceB = scriptedTrace(material, Math.floor((2 * stepCount) / 3));
    threadA = await defendScripted(
      scene,
      (await at(caseOutRef))!,
      operator,
      traceA,
    );
    unitA = threadUnitOf(scene, (await at(threadA))!);
  }, 1_800_000);

  it("holds while its provider has not indexed a defended thread, then reports the thread as awaiting its defender until the defender's clock ends", async () => {
    // A provider that has not yet indexed the thread.
    const lucid = scene.h.proverLucid;
    const indexed = lucid.utxosAtWithUnit.bind(lucid);
    const lagging = vi
      .spyOn(lucid, "utxosAtWithUnit")
      .mockImplementation(async (address, unit) =>
        unit === unitA ? [] : await indexed(address, unit),
      );
    let held: Awaited<ReturnType<Runner["runOnce"]>>;
    try {
      held = await runner.runOnce(scene.failing);
    } finally {
      lagging.mockRestore();
    }
    expect(held.kind).toBe("pending");
    expect("reason" in held && held.reason).toMatch(
      new RegExp(
        `^${VALIDATION_TRACE_THREAD_COUNT_HOLD}: .* counts 1 open threads; spend-chain discovery found 0, so it holds until they agree$`,
        "u",
      ),
    );
    const result = awaitingValidationTraceCounterparty(
      await runner.runOnce(scene.failing),
    );
    const thread = threadOf(await live(unitA));
    expect(result).toMatchObject({
      kind: "awaiting_counterparty",
      outRef: threadA,
      responseDeadline: validationTraceDefenderDeadlineMs(thread),
    });
    expect(validationTraceDefenderDeadlineMs(thread)).toBe(
      Number(thread.turn_started_at + thread.defender_clock_ms),
    );
    expect(prepared).toStrictEqual([]);
  }, 300_000);

  it("refuses a timeout one slot before the defender's clock ends, and a timeout of a thread on our turn, at the game's clock checks", async () => {
    const threadB = await defendScripted(
      scene,
      (await at(`${(await live(unitA)).txHash}#0`))!,
      second,
      traceB,
      [operator.vkey],
    );
    unitB = threadUnitOf(scene, (await at(threadB))!);
    await revealAsDefender(scene, second, await live(unitB), traceB);
    revealedB = await live(unitB);
    expect(isTurn(threadOf(revealedB), "AwaitingChallenger")).toBe(true);
    const silent = await live(unitA);
    awaitLastSlotAtOrBefore(
      validationTraceDefenderDeadlineMs(threadOf(silent)),
    );
    const timeout = { kind: "EnterChallengerTimeout" } as const;
    const timeoutAddress = scene.chain.timeout.spendingScriptAddress;
    await expectOnchainRefusal(
      gameMove(silent, timeout, () => threadOf(silent), timeoutAddress),
      {
        refusedBy: "fraud_proofs/validation_trace/game_v1",
        check:
          /current_time_lower > thread\.turn_started_at \+ thread\.defender_clock_ms/u,
      },
    );
    await expectOnchainRefusal(
      gameMove(revealedB, timeout, () => threadOf(revealedB), timeoutAddress),
      {
        refusedBy: "fraud_proofs/validation_trace/game_v1",
        check:
          /current_time_lower > thread\.turn_started_at \+ thread\.challenger_clock_ms/u,
      },
    );
    expect(await live(unitA)).toStrictEqual(silent);
  }, 300_000);

  it("refuses our reveal with a validity range past our deadline on the thread, at the game's deadline check (the charge)", async () => {
    const thread = threadOf(revealedB);
    const ourDeadline = Number(
      thread.turn_started_at + thread.challenger_clock_ms,
    );
    expect(ourDeadline).toBeGreaterThan(
      validationTraceDefenderDeadlineMs(threadOf(await live(unitA))),
    );
    awaitLastSlotAtOrBefore(ourDeadline + 1_000);
    // The reveal's whole validity range, from its lower bound on, is past it.
    expect(
      validationTraceMoveWindow(scene.h.proverLucid).validFrom,
    ).toBeGreaterThan(ourDeadline);
    const turn = thread.turn;
    if (typeof turn === "string" || !("AwaitingChallenger" in turn))
      throw new Error("not on our turn");
    const proof = SDK.validationTraceProofDataFromCore(
      material.leafProof(Number(turn.AwaitingChallenger.midpoint)),
    );
    await expectOnchainRefusal(
      gameMove(
        revealedB,
        { kind: "RevealChallenger", proof },
        (_lower, upper) =>
          challengerRevealedThread({
            thread,
            proof,
            lower: thread.turn_started_at,
            upper,
          }),
        revealedB.address,
      ),
      {
        refusedBy: "fraud_proofs/validation_trace/game_v1",
        check: /elapsed <= clock/u,
      },
    );
  }, 300_000);

  it("refuses a timeout our wallet pays whose successor keeps less than its thread's lovelace, at the thread's value rule, and never has our wallet pay a move under the cap", async () => {
    const silent = await live(unitA);
    const lucid = scene.h.proverLucid;
    const deployment = runnerDeployment(scene);
    const address = scene.chain.timeout.spendingScriptAddress;
    // The thread pays `fee`; our wallet's coin selection balances the rest.
    const build = (fee: bigint) =>
      buildValidationTraceGameMove({
        lucid,
        deployment,
        thread: silent,
        action: { kind: "EnterChallengerTimeout" },
        successor: { address, datum: silent.datum! },
        fee,
        ...validationTraceMoveWindow(lucid),
      });
    const timeout = (fee: bigint) => async () =>
      await build(fee).complete({ localUPLCEval: true });
    // A move under the cap that its thread cannot pay is not our wallet's.
    await expect(
      completeValidationTraceMove({
        lucid,
        stage: "timeout",
        referenced: [deployment.references.game],
        build: (fee) => {
          if (fee > 0n) throw new Error("the thread does not pay");
          return build(fee);
        },
      }),
    ).rejects.toThrow("the thread does not pay");
    // The honest twin: our wallet pays the whole fee and the successor keeps
    // all of the thread's lovelace; every script it runs passes.
    const honest = CML.Transaction.from_cbor_hex(
      (await timeout(0n)()).toCBOR(),
    ).body();
    expect(honest.inputs().len()).toBeGreaterThan(1);
    expect(honest.outputs().get(0).amount().coin()).toBe(
      silent.assets.lovelace,
    );
    // The thread paying one lovelace of it beside our wallet.
    await expectOnchainRefusal(timeout(1n), {
      refusedBy: "fraud_proofs/validation_trace/game_v1",
      check:
        /^expect value_rule\( lovelace_of\(input_value\), lovelace_of\(output\.value\), fraud_prover, \)$/u,
    });
    expect(await live(unitA)).toStrictEqual(silent);
  }, 300_000);

  it("times out the silent defender, though it left both operator lists mid-case, and closes its thread into our case with its lost-defense receipt, and never times out the thread on our turn", async () => {
    // The silent defender retires and recovers its bond before we move.
    await removeOperatorFromBothLists(
      scene,
      operator.vkey,
      scene.h.funderLucid,
    );
    expect(await operatorListsHolding(scene, operator.vkey)).toStrictEqual([]);
    const silent = await live(unitA);
    // At a dearer byte price the timeout's fee passes the cap.
    const parameters = scene.h.emulator.protocolParameters;
    scene.h.emulator.protocolParameters = { ...parameters, minFeeA: 5_000 };
    await scene.h.proverLucid.switchProvider(scene.h.emulator);
    try {
      const moved = () => prepared.some(({ stage }) => stage === "timeout");
      for (let turn = 0; turn < 4 && !moved(); turn += 1)
        await runner.runOnce(scene.failing);
    } finally {
      scene.h.emulator.protocolParameters = parameters;
      await scene.h.proverLucid.switchProvider(scene.h.emulator);
    }
    const { result, history } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_THREAD_LOST,
    );
    if (!("entries" in result) || !("reason" in result))
      throw new Error("no workflow journal");
    expect(result.reason, history.join("\n")).toContain(revealedB.txHash);
    const stages = confirmedValidationTraceStages(result.entries);
    const moves = stages.filter(({ action }) =>
      ["reveal", "timeout", "close_thread"].includes(action.stage),
    );
    expect(
      moves.map(({ action }) => [action.stage, action.consumes]),
    ).toStrictEqual([
      ["timeout", `${silent.txHash}#${silent.outputIndex.toString()}`],
      ["close_thread", `${moves[0]!.txHash}#0`],
    ]);
    expect(prepared.map(({ stage }) => stage)).toStrictEqual(
      moves.map(({ action }) => action.stage),
    );
    const { chain } = scene;
    // The silent defender's thread token burned; the receipt minted.
    expect(await liveThread(scene, unitA)).toBeUndefined();
    const closeHash = moves[1]!.txHash;
    const caseOutput = (await at(`${closeHash}#0`))!;
    const caseDatum = Data.from(caseOutput.datum!, SDK.Case);
    expect(caseOutput.address).toBe(chain.case.spendingScriptAddress);
    expect(caseDatum.open_threads).toBe(1n);
    const name = unitA.slice(56);
    const [receipt] = await scene.h.proverLucid.utxosAtWithUnit(
      chain.award.spendingScriptAddress,
      chain.award.policyId + name,
    );
    expect(receipt?.txHash).toBe(closeHash);
    expect(receipt!.assets).toStrictEqual({
      lovelace: RECEIPT_LOVELACE,
      [chain.award.policyId + name]: 1n,
    });
    expect(Data.from(receipt!.datum!, SDK.LostDefenseReceipt)).toStrictEqual({
      case_id: threadOf(silent).case_id,
      defender_vkh: operator.vkey,
      outcome: "ChallengerWins",
      bounty_claimant: caseDatum.bounty_claimant,
    });
    // Our wallet paid all of the timeout's fee, over the cap; the close's
    // thread (beside the case) its real fee. The rest, less receipt, is ours.
    const timedOut = moves[0]!;
    const fees = await Promise.all(
      moves.map(async ({ txHash }) => await landedMoveFee(scene, txHash)),
    );
    const [timeoutBody, closeBody] = [timedOut.txHash, closeHash].map((hash) =>
      CML.Transaction.from_cbor_hex(scene.l1.signedCbors.get(hash)!).body(),
    );
    expect(fees[0]!.paid).toBeGreaterThan(
      VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
    );
    expect(timeoutBody!.inputs().len()).toBeGreaterThan(1);
    expect(timeoutBody!.outputs().get(0).amount().coin()).toBe(
      silent.assets.lovelace,
    );
    expectRealMoveFee(fees[1]!);
    expect(closeBody!.inputs().len()).toBe(2);
    const leftover = await at(`${closeHash}#2`);
    const claimant = { type: "Key", hash: caseDatum.bounty_claimant } as const;
    expect(leftover?.address).toBe(credentialToAddress(network, claimant));
    expect(leftover?.assets).toStrictEqual({
      lovelace:
        (silent.assets.lovelace ?? 0n) - fees[1]!.paid - RECEIPT_LOVELACE,
    });
    expect(leftover!.datum).toBe(Data.to(new Constr(0, [timedOut.txHash, 0n])));
    // The thread on our turn was never moved.
    expect(await live(unitB)).toStrictEqual(revealedB);
  }, 1_800_000);

  it("reports the lost thread under its own readiness state, handing the session back at once", async () => {
    expect(
      validationTraceRunState(await runner.runOnce(scene.failing)),
    ).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      namedState: { name: VALIDATION_TRACE_THREAD_LOST, alarm: false },
    });
  }, 300_000);

  it("fits the timeout and the thread close within the transaction limits", () => {
    for (const stage of ["timeout", "close_thread"])
      reportFit(
        stage,
        prepared.filter((row) => row.stage === stage).map(({ cbor }) => cbor),
      );
  });
});
