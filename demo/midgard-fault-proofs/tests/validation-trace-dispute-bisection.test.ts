/** The validation-trace dispute runner bisecting defended threads, on the
 * emulator. Two operators defend our case, each with a scripted trace that
 * agrees with ours up to a different state and reveals its midpoints from
 * that trace through real game-v1 moves. The runner plays each thread on its
 * own turn with our midpoint, proven under our root, until both threads are
 * ReadyForOneStep on the step after their defender's last agreed state; it
 * then stops at a resumable "one-step required" state, and a fresh process
 * on the same journal makes no further move. game-v1 refuses a reveal whose
 * proof does not verify under our root, one on a thread not on our turn, and
 * one whose validity range is longer than L_d. */
import {
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
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
import {
  buildValidationTraceGameMove,
  challengerRevealedThread,
  validationTraceMoveWindow,
} from "../src/validation-trace-dispute/move-builders.js";
import { completeValidationTraceMove } from "../src/validation-trace-dispute/move-fee.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { continuePendingWorkflow } from "../src/workflow/pending-continuation.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { enrollSuccessor } from "./support/rotated-successor-operator.js";
import { machineState } from "./support/validation-game-traces.js";
import {
  type Defender,
  sceneOperator,
} from "./support/validation-trace-dispute-defend.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
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
import { DEPLOYMENT_FINGERPRINT } from "./workflow.make-adapter.js";

type Side = Readonly<{
  defender: Defender;
  trace: ScriptedTrace;
  unit: string;
}>;

let scene: Scene;
let runner: Runner;
let material: ValidationTraceClaimMaterial;
let sides: readonly [Side, Side];
/** The signed bytes of each move the runner prepared, by stage. */
const prepared: { stage: string; cbor: string }[] = [];
/** The signed bytes of each defender reveal. */
const defenderReveals: string[] = [];
let final: FraudProofWorkflowRunResult;

const hex = (bytes: Buffer) => bytes.toString("hex");

const live = async (side: Side): Promise<UTxO> => {
  const utxo = await liveThread(scene, side.unit);
  if (utxo === undefined) throw new Error("the thread is not live");
  return utxo;
};

/** A RevealChallenger by the prover on `thread` with `proof`, completed. */
const challengerReveal = ({
  thread,
  proof,
  rangeMs = MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  successor,
}: {
  readonly thread: UTxO;
  readonly proof: SDK.ValidationTraceProof;
  readonly rangeMs?: number;
  readonly successor?: SDK.CaseThread;
}) => {
  const lucid = scene.h.proverLucid;
  const { validFrom } = validationTraceMoveWindow(lucid);
  const validTo = validFrom + rangeMs;
  const datum = Data.from(thread.datum!, SDK.CaseThreadDatum);
  const next =
    successor ??
    challengerRevealedThread({
      thread: datum.data!,
      proof,
      lower: BigInt(validFrom),
      upper: BigInt(validTo - 1),
    });
  const deployment = runnerDeployment(scene);
  return async () =>
    await completeValidationTraceMove({
      lucid,
      stage: "reveal",
      referenced: [deployment.references.game],
      build: (fee) =>
        buildValidationTraceGameMove({
          lucid,
          deployment,
          thread,
          action: { kind: "RevealChallenger", proof },
          successor: {
            address: thread.address,
            datum: Data.to({ ...datum, data: next }, SDK.CaseThreadDatum),
          },
          fee,
          validFrom,
          validTo,
        }),
    });
};

/** Our proof of the midpoint the thread at `thread` waits on. */
const ourMidpointProof = (thread: UTxO): SDK.ValidationTraceProof => {
  const { turn } = threadOf(thread);
  if (typeof turn === "string") throw new Error("no midpoint");
  const midpoint =
    "AwaitingChallenger" in turn
      ? turn.AwaitingChallenger.midpoint
      : turn.AwaitingOperator.midpoint;
  return SDK.validationTraceProofDataFromCore(
    material.leafProof(Number(midpoint)),
  );
};

describe("validation-trace dispute runner: bisecting defended threads", () => {
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
    // Only the moves on the threads from here on.
    prepared.length = 0;
    const caseOutRef = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!.outputs[0]!.outRef;
    material = await failingMaterial(scene);
    const stepCount = Number(material.stepCount);
    expect(stepCount).toBeGreaterThanOrEqual(4);
    const operator = sceneOperator(scene);
    const second: Defender = {
      vkey: successor.keyHash,
      nodeUnit: successor.activeNodeUnit,
      lucid: successor.lucid,
    };
    const traceA = scriptedTrace(material, Math.floor(stepCount / 3));
    const traceB = scriptedTrace(material, Math.floor((2 * stepCount) / 3));
    const [caseUtxo] = await scene.h.proverLucid.utxosByOutRef([
      {
        txHash: caseOutRef.split("#")[0]!,
        outputIndex: Number(caseOutRef.split("#")[1]),
      },
    ]);
    const threadA = await defendScripted(scene, caseUtxo!, operator, traceA);
    const [defendedCase] = await scene.h.proverLucid.utxosByOutRef([
      { txHash: threadA.split("#")[0]!, outputIndex: 0 },
    ]);
    const threadB = await defendScripted(scene, defendedCase!, second, traceB, [
      operator.vkey,
    ]);
    const unitOf = async (outRef: string) => {
      const [utxo] = await scene.h.proverLucid.utxosByOutRef([
        { txHash: outRef.split("#")[0]!, outputIndex: 1 },
      ]);
      return threadUnitOf(scene, utxo!);
    };
    sides = [
      { defender: operator, trace: traceA, unit: await unitOf(threadA) },
      { defender: second, trace: traceB, unit: await unitOf(threadB) },
    ];
    // Thread A on our turn, thread B still on its defender's.
    const revealed = await revealAsDefender(
      scene,
      operator,
      await live(sides[0]),
      traceA,
    );
    defenderReveals.push(revealed.cbor);
  }, 1_800_000);

  it("refuses our reveal whose proof does not verify under our root", async () => {
    const thread = await live(sides[0]);
    const proof = ourMidpointProof(thread);
    const [first, ...rest] = proof.siblings;
    const tampered = {
      ...proof,
      siblings: [
        (first!.startsWith("0") ? "1" : "0") + first!.slice(1),
        ...rest,
      ],
    };
    await expectOnchainRefusal(challengerReveal({ thread, proof: tampered }), {
      refusedBy: "fraud_proofs/validation_trace/game_v1",
      check: /verify_trace_proof_against_root/u,
    });
  }, 300_000);

  it("refuses our reveal on a thread that is on its defender's turn", async () => {
    const thread = await live(sides[1]);
    expect(isTurn(threadOf(thread), "AwaitingOperator")).toBe(true);
    await expectOnchainRefusal(
      challengerReveal({
        thread,
        proof: ourMidpointProof(thread),
        successor: threadOf(thread),
      }),
      {
        refusedBy: "fraud_proofs/validation_trace/game_v1",
        check: /AwaitingChallenger/u,
      },
    );
  }, 300_000);

  it("refuses our reveal whose validity range is longer than L_d", async () => {
    const thread = await live(sides[0]);
    await expectOnchainRefusal(
      challengerReveal({
        thread,
        proof: ourMidpointProof(thread),
        rangeMs: MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS + 1_000,
      }),
      {
        refusedBy: "fraud_proofs/validation_trace/game_v1",
        check: /upper - lower <= max_length/u,
      },
    );
  }, 300_000);

  it("bisects each thread on its own to ReadyForOneStep on the step after its defender's last agreed state, then stops at a resumable one-step-required state", async () => {
    const history: string[] = [];
    for (let turn = 0; ; turn += 1) {
      if (turn > 400) throw new Error(`no end:\n${history.join("\n")}`);
      // Each defender answers as soon as our move has landed, before our
      // runner has seen it land: the runner confirms its move from the
      // move's own signed bytes, never from the successor the answer spends.
      let ready = 0;
      for (const side of sides) {
        const utxo = await live(side);
        const thread = threadOf(utxo);
        if (thread.turn === "ReadyForOneStep") ready += 1;
        else if (isTurn(thread, "AwaitingOperator"))
          defenderReveals.push(
            (await revealAsDefender(scene, side.defender, utxo, side.trace))
              .cbor,
          );
      }
      final = await runner.runOnce(scene.failing);
      history.push(
        "reason" in final ? `${final.kind}: ${final.reason}` : final.kind,
      );
      if (
        ready === sides.length &&
        !(
          final.kind === "pending" &&
          final.reason.startsWith("transaction for ")
        )
      )
        break;
    }
    expect(final.kind, history.join("\n")).toBe("pending");
    if (!("reason" in final)) throw new Error("no reason");
    expect(final.reason).toMatch(
      new RegExp(`^${VALIDATION_TRACE_ONE_STEP_REQUIRED}: `, "u"),
    );
    const pendingHash = hex(
      hashMidgardValidationMachineState(machineState(0, "cek")),
    );
    let rounds = 0n;
    for (const side of sides) {
      const thread = threadOf(await live(side));
      const k = side.trace.agreedUpTo;
      expect(thread).toMatchObject({
        turn: "ReadyForOneStep",
        low_index: BigInt(k),
        high_index: BigInt(k + 1),
        defender_high_hash: pendingHash,
        challenger_high_hash: hex(material.leafProof(k + 1).stateHash),
        defender_root: side.trace.root,
        challenger_root: material.challengerRoot,
      });
      if (k > 0)
        expect(thread.agreed_low_hash).toBe(
          hex(material.leafProof(k).stateHash),
        );
      expect(thread.round).toBeLessThanOrEqual(32n);
      rounds += thread.round;
    }
    // One confirmed reveal of ours per round, on each thread on its own.
    if (!("entries" in final)) throw new Error("no workflow journal");
    const reveals = confirmedValidationTraceStages(final.entries).filter(
      ({ action }) => action.stage === "reveal",
    );
    expect(BigInt(reveals.length)).toBe(rounds);
    expect(new Set(reveals.map(({ txHash }) => txHash)).size).toBe(
      reveals.length,
    );
    expect(prepared.filter(({ stage }) => stage === "reveal").length).toBe(
      reveals.length,
    );
    expect(prepared.every(({ stage }) => stage === "reveal")).toBe(true);
  }, 1_800_000);

  it("makes no further move from a fresh process on the same journal", async () => {
    if (!("entries" in final)) throw new Error("no workflow journal");
    const before = confirmedValidationTraceStages(final.entries).length;
    const count = prepared.length;
    const restarted = makeRunner(scene, { journal: runner.journal });
    const result = await restarted.runOnce(scene.failing);
    expect(result.kind).toBe("pending");
    if (!("reason" in result) || !("entries" in result))
      throw new Error("no reason");
    expect(result.reason).toMatch(
      new RegExp(`^${VALIDATION_TRACE_ONE_STEP_REQUIRED}: `, "u"),
    );
    expect(confirmedValidationTraceStages(result.entries)).toHaveLength(before);
    expect(prepared).toHaveLength(count);
  }, 300_000);

  // A session that kept polling would run to the 30-minute quiet bound.
  it("hands the session back at once at the one-step-required state, under its own readiness state, without a continuation stall", async () => {
    let runs = 0;
    const result = await continuePendingWorkflow({
      invocation: {
        mode: "run",
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        category: "validationTraceDispute",
        headerHash: scene.failing.evidence.headerHash,
      },
      journal: runner.journal,
      execute: async () => {
        runs += 1;
        return validationTraceRunState(await runner.runOnce(scene.failing));
      },
    });
    expect(runs).toBe(1);
    expect(result).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      namedState: { name: VALIDATION_TRACE_ONE_STEP_REQUIRED, alarm: false },
    });
  }, 120_000);

  it("fits every reveal within the transaction limits", () => {
    reportFit(
      "RevealChallenger",
      prepared.map(({ cbor }) => cbor),
    );
    reportFit("RevealOperator", defenderReveals);
  });
});
