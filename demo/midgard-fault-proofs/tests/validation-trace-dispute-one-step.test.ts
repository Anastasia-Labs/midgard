/** The validation-trace dispute runner driving the one-step resolution of a
 * defended thread, on the emulator. The scene's operator defends our case
 * with a trace that agrees with ours up to the state before a CEK compute
 * step and reveals its midpoints from that trace; the runner bisects the
 * thread to ReadyForOneStep, where a deployment without the one-step
 * resolution waits. With it, the runner enters the resolution and moves the
 * thread through the CEK resolver, the core step binder, the compute arm and
 * the settle to the award, then closes it into our case with the defender's
 * lost-defense receipt; the case waits for its defense deadline. A process
 * stopped at a resolution move after its broadcast lands those same bytes on
 * resume and sends nothing again. The boundary refuses a PrepareResolution whose resolver
 * index is not its pre-state phase's, and the CEK resolver refuses a
 * PrepareSelected whose claimed successor is not ours. */
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  confirmedValidationTraceStages,
  parseValidationTraceAction,
} from "../src/validation-trace-dispute/actions.js";
import {
  VALIDATION_TRACE_CASE_OPEN,
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
} from "../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceClaimMaterial } from "../src/validation-trace-dispute/claim-material.js";
import { validationTraceMoveWindow } from "../src/validation-trace-dispute/move-builders.js";
import { completeValidationTraceMove } from "../src/validation-trace-dispute/move-fee.js";
import {
  buildValidationTraceHop,
  validationTraceHop,
  type ValidationTraceHopHolder,
} from "../src/validation-trace-dispute/one-step-builders.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../src/validation-trace-dispute/one-step-holders.js";
import {
  hashOneStepEvidence,
  planValidationTraceThread,
  type ValidationTraceOneStepPlan,
} from "../src/validation-trace-dispute/one-step-plan.js";
import type { ValidationTraceDisputeDeployment } from "../src/validation-trace-dispute/transactions.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import {
  expectRealMoveFee,
  landedMoveFee,
} from "./support/validation-trace-dispute-move-fees.js";
import {
  oneStepDeployment,
  publishOneStepReferences,
} from "./support/validation-trace-dispute-one-step.js";
import {
  capturePrepared,
  confirmedOneStepMoves,
  intentsFor,
  isPendingFor,
  playThreads,
  type PreparedMove,
  type Side,
  threadPlace,
} from "./support/validation-trace-dispute-one-step-play.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";
import {
  defendScripted,
  reportFit,
  scriptedTrace,
  threadOf,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

/** The defender's last agreed state: the next step is a CEK compute step. */
const AGREED_UP_TO = 116;
/** A resolver index other than the CEK phase's. */
const WRONG_RESOLVER_INDEX = 10;

let scene: Scene;
let material: ValidationTraceClaimMaterial;
let side: Side;
let deployment: ValidationTraceDisputeDeployment;
let runner: Runner;
/** Why the runner without the one-step resolution waited. */
let notInstalled: string;
/** The thread at the game, ReadyForOneStep, before the resolution. */
let readyOutRef: string;
let readyThread: SDK.CaseThread;
/** Each transaction the runners with the one-step resolution signed. */
const prepared: PreparedMove[] = [];
/** The moves the full drive confirmed, in order. */
let driven: ReturnType<typeof confirmedOneStepMoves> = [];

const outRefOf = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const place = async () => {
  const found = await threadPlace(scene, deployment, side.unit);
  if (found === undefined) throw new Error("the thread is not live");
  return found;
};

/** A runner with the one-step resolution over the shared journal. */
const oneStepRunner = (
  wrap: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => FraudProofFamilyWorkflowAdapter = capturePrepared(prepared),
): Runner => makeRunner(scene, { journal: runner.journal, deployment, wrap });

/** Runs until the thread stands at `holder`. */
const advanceTo = async (holder: ValidationTraceOneStepHolder) =>
  await playThreads({
    scene,
    runner,
    sides: [side],
    until: async () => (await place()).holder === holder,
    limit: 20,
  });

/** Our plan of the thread's one step, from our trace. */
const ourPlan = async (): Promise<ValidationTraceOneStepPlan> => {
  const planning = await planValidationTraceThread(material, readyThread);
  if (planning.kind !== "plan") throw new Error(planning.reason);
  return planning.plan;
};

/** The hop of the thread from `holder` with `plan` to `address`, built and
 * completed as the runner completes it. */
const hopFrom =
  ({
    holder,
    plan,
    address,
  }: {
    readonly holder: ValidationTraceHopHolder;
    readonly plan: ValidationTraceOneStepPlan;
    readonly address: string;
  }) =>
  async () => {
    const { utxo } = await place();
    const lucid = scene.h.proverLucid;
    const prover = scene.h.proverSigner.paymentKeyHash;
    const reference = validationTraceOneStepHolder(
      deployment,
      holder,
    ).reference!;
    const hop = validationTraceHop({ plan, holder, utxo, prover });
    const window = validationTraceMoveWindow(lucid);
    return await completeValidationTraceMove({
      lucid,
      stage: "resolve",
      referenced: [reference],
      build: (fee) =>
        buildValidationTraceHop({
          lucid,
          thread: utxo,
          unit: side.unit,
          reference,
          hop,
          address,
          prover,
          fee,
          ...window,
        }),
    });
  };

/**
 * Stops a process at the submit of the next resolution move it signs, once
 * its intent is durable and its bytes have reached the ledger. The stopped
 * run never returns.
 */
const stopAfterResolveBroadcast = async (): Promise<
  Readonly<{ hash: string; consumes: string }>
> => {
  let signed: { hash: string; consumes: string } | undefined;
  let stopped!: () => void;
  const stop = new Promise<void>((resolve) => {
    stopped = resolve;
  });
  const stopping = oneStepRunner((adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      const { stage, consumes } = parseValidationTraceAction(context.action);
      const transaction = workflowPreflightTransaction(preflight);
      if (stage === "resolve" && transaction !== undefined)
        signed ??= { hash: transaction.toHash(), consumes: consumes! };
      return preflight;
    },
    submit: async (input) => {
      if (
        signed === undefined ||
        parseValidationTraceAction(input.action).consumes !== signed.consumes
      )
        return await adapter.submit(input);
      await adapter.submit(input);
      stopped();
      return await new Promise<never>(() => undefined);
    },
  }));
  const runs = (async () => {
    for (let run = 0; run < 5; run += 1) await stopping.runOnce(scene.failing);
    throw new Error("the process made no resolution move");
  })();
  await Promise.race([stop, runs]);
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

/** A fresh process over the stopped one's journal, run until a move spending
 * the stopped move's thread is confirmed; returns its journal's view. */
const resume = async (consumes: string) => {
  runner = oneStepRunner();
  const { result, history } = await playThreads({
    scene,
    runner,
    sides: [side],
    until: async (run) => confirmsSpendOf(run, consumes),
    limit: 10,
  });
  if (!("entries" in result)) throw new Error("no workflow journal");
  const moves = confirmedOneStepMoves(result.entries).filter(
    (move) => move.consumes === consumes,
  );
  // Exactly one move spent the stopped move's thread.
  expect(moves, history.join("\n")).toHaveLength(1);
  expect(outRefOf((await place()).utxo)).not.toBe(consumes);
  return { entries: result.entries, landed: moves[0]!.txHash };
};

describe("validation-trace dispute runner: one-step resolution of a CEK compute step", () => {
  beforeAll(async () => {
    scene = await setUpScene();
    runner = makeRunner(scene);
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    const caseOutRef = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!.outputs[0]!.outRef;
    material = await failingMaterial(scene);
    const defender = sceneOperator(scene);
    const trace = scriptedTrace(material, AGREED_UP_TO);
    const [caseUtxo] = await scene.h.proverLucid.utxosByOutRef([
      {
        txHash: caseOutRef.split("#")[0]!,
        outputIndex: Number(caseOutRef.split("#")[1]),
      },
    ]);
    const thread = await defendScripted(scene, caseUtxo!, defender, trace);
    const [defended] = await scene.h.proverLucid.utxosByOutRef([
      { txHash: thread.split("#")[0]!, outputIndex: 1 },
    ]);
    side = { defender, trace, unit: threadUnitOf(scene, defended!) };
    deployment = oneStepDeployment(
      scene,
      await publishOneStepReferences(scene),
    );
    const waited = await playThreads({
      scene,
      runner,
      sides: [side],
      until: async (run) =>
        isPendingFor(run, VALIDATION_TRACE_ONE_STEP_REQUIRED),
    });
    if (!("reason" in waited.result)) throw new Error("no reason");
    notInstalled = waited.result.reason;
    const ready = await place();
    expect(ready.holder).toBe("game");
    readyOutRef = outRefOf(ready.utxo);
    readyThread = threadOf(ready.utxo);
    runner = oneStepRunner();
  }, 1_800_000);

  it("waits at ReadyForOneStep while the deployment does not install the one-step resolution", async () => {
    expect(notInstalled).toContain("the one-step resolution is not installed");
    expect(readyThread.turn).toBe("ReadyForOneStep");
    expect(readyThread.low_index).toBe(BigInt(AGREED_UP_TO));
  }, 300_000);

  it("refuses a PrepareResolution whose resolver index is not its pre-state phase's, at the boundary", async () => {
    await advanceTo("boundary");
    const before = await place();
    const plan = await ourPlan();
    // The honest move differs only in its index and the resolver it pays,
    // the one that index names; the boundary's checks carry no trace of
    // their own, so the refusal is the validator's.
    await expectOnchainRefusal(
      hopFrom({
        holder: "boundary",
        plan: { ...plan, resolverIndex: WRONG_RESOLVER_INDEX },
        address:
          scene.chain.prepareResolvers[WRONG_RESOLVER_INDEX]!
            .spendingScriptAddress,
      }),
      {
        refusedBy: "fraud_proofs/validation_trace/boundary_v1",
        check: /^Validator returned false$/u,
      },
    );
    expect(outRefOf((await place()).utxo)).toBe(outRefOf(before.utxo));
  }, 600_000);

  it("refuses a PrepareSelected whose claimed successor is not ours, at the CEK resolver's semantic preparation", async () => {
    await advanceTo("prepare");
    const before = await place();
    const plan = await ourPlan();
    // The claimed successor is the step's pre-state, not our successor.
    const transitionCbor = replacePlutusConstrFieldCbor(
      plan.transitionCbor,
      [1],
      plutusConstrFieldCbor(before.utxo.datum!, [1, 0, 1]),
    );
    await expectOnchainRefusal(
      hopFrom({
        holder: "prepare",
        plan: {
          ...plan,
          transitionCbor,
          evidenceHash: hashOneStepEvidence(transitionCbor, plan.auxiliaryCbor),
        },
        address: validationTraceOneStepHolder(deployment, "core").address,
      }),
      {
        refusedBy: "fraud_proofs/validation_trace/cek_v1",
        check:
          /hash_machine_state\(transition\.claimed_successor\)\s*==\s*state\.challenger_successor_hash/u,
      },
    );
    expect(outRefOf((await place()).utxo)).toBe(outRefOf(before.utxo));
  }, 600_000);

  it("lands the same signed resolution move after a stop between its broadcast and its landing, sending nothing again", async () => {
    const stop = await stopAfterResolveBroadcast();
    const { entries, landed } = await resume(stop.consumes);
    expect(landed).toBe(stop.hash);
    expect(intentsFor(entries, "resolve", stop.consumes)).toStrictEqual([
      { attempt: 1, txHash: stop.hash },
    ]);
  }, 600_000);

  it("drives the thread from ReadyForOneStep through the CEK compute step to the award and closes it into our case with the defender's lost-defense receipt, then waits for the case's defense deadline", async () => {
    const { result, history } = await playThreads({
      scene,
      runner,
      sides: [side],
      until: async (run) => isPendingFor(run, VALIDATION_TRACE_CASE_OPEN),
      limit: 30,
    });
    if (!("entries" in result)) throw new Error("no workflow journal");
    driven = confirmedOneStepMoves(result.entries);
    expect(
      driven.map(({ stage }) => stage),
      history.join("\n"),
    ).toStrictEqual([
      "enter_resolution",
      "resolve",
      "resolve",
      "resolve",
      "resolve",
      "resolve",
      "close_awarded",
    ]);
    // Each move spends the thread the one before it left.
    expect(driven[0]!.consumes).toBe(readyOutRef);
    for (let index = 1; index < driven.length; index += 1)
      expect(driven[index]!.consumes).toBe(`${driven[index - 1]!.txHash}#0`);
    // Each move paid its real fee, below the cap.
    for (const { txHash } of driven)
      expectRealMoveFee(await landedMoveFee(scene, txHash));
    expect(await threadPlace(scene, deployment, side.unit)).toBeUndefined();
    const { chain, h } = scene;
    const close = driven.at(-1)!.txHash;
    const [caseOutput] = await h.proverLucid.utxosByOutRef([
      { txHash: close, outputIndex: 0 },
    ]);
    expect(caseOutput!.address).toBe(chain.case.spendingScriptAddress);
    const caseDatum = Data.from(caseOutput!.datum!, SDK.Case);
    expect(caseDatum.open_threads).toBe(0n);
    expect(caseDatum.defender_won).toBe(false);
    expect(caseDatum.bounty_claimant).toBe(h.proverSigner.paymentKeyHash);
    const [receipt] = await h.proverLucid.utxosAtWithUnit(
      chain.award.spendingScriptAddress,
      chain.award.policyId + side.unit.slice(56),
    );
    expect(receipt?.txHash).toBe(close);
    expect(Data.from(receipt!.datum!, SDK.LostDefenseReceipt)).toStrictEqual({
      case_id: readyThread.case_id,
      defender_vkh: side.defender.vkey,
      outcome: "ChallengerWins",
      bounty_claimant: h.proverSigner.paymentKeyHash,
    });
  }, 1_800_000);

  it("fits each one-step move within the transaction limits", () => {
    const signed = (txHash: string) => scene.l1.signedCbors.get(txHash)!;
    const resolves = driven.filter(({ stage }) => stage === "resolve");
    reportFit("EnterResolution", [signed(driven[0]!.txHash)]);
    for (const [index, label] of [
      "PrepareResolution",
      "PrepareSelected",
      "VerifyCoreStep",
      "VerifyArm compute",
      "Settle",
    ].entries())
      reportFit(label, [signed(resolves[index]!.txHash)]);
    reportFit("CloseAwardedThread", [signed(driven.at(-1)!.txHash)]);
  });
});
