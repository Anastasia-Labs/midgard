/** A scripted adversary defender on the Open scene: its trace agrees with the
 * challenger's up to a chosen state and is pending after it, it defends our
 * case with that trace through a real Defend, and it reveals its midpoints
 * from that trace through real game-v1 RevealOperator transactions. The
 * bisection between the two traces ends on the step after the last agreed
 * state. */
import {
  hashMidgardValidationMachineState,
  midgardValidationTraceBranchHash,
  midgardValidationTraceLeafHash,
  MidgardValidationTracePadding,
  type MidgardValidationTraceProof,
  validationTraceDepthForStepCount,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type TxSignBuilder, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import {
  buildValidationTraceOperatorReveal,
  validationTraceMoveWindow,
} from "../../src/validation-trace-dispute/move-builders.js";
import { completeValidationTraceMove } from "../../src/validation-trace-dispute/move-fee.js";
import { machineState } from "./validation-game-traces.js";
import {
  type Defender,
  defendWith,
  type DefenseTrace,
} from "./validation-trace-dispute-defend.js";
import {
  measureWithinTransactionLimits,
  type Scene,
} from "./validation-trace-dispute-open-scene.js";

type Hash = ReturnType<typeof midgardValidationTraceLeafHash>;

/** The state the scripted defender claims after the agreed prefix. */
const PENDING = machineState(0, "cek");

export type ScriptedTrace = DefenseTrace &
  Readonly<{
    /** The last state both traces hold. */
    agreedUpTo: number;
    proofAt: (stateIndex: number) => MidgardValidationTraceProof;
  }>;

/**
 * The defender's trace over the challenger's step count: the challenger's
 * state hashes at `0..agreedUpTo`, then a pending state through the
 * padding. Only the subtrees over the agreed prefix are hashed leaf by leaf.
 */
export const scriptedTrace = (
  material: ValidationTraceClaimMaterial,
  agreedUpTo: number,
): ScriptedTrace => {
  const stepCount = Number(material.stepCount);
  if (agreedUpTo < 0 || agreedUpTo >= stepCount)
    throw new Error("the defender must disagree at some step");
  const depth = validationTraceDepthForStepCount(stepCount);
  const pendingHash = hashMidgardValidationMachineState(PENDING);
  const padding = new MidgardValidationTracePadding(pendingHash);
  const agreed = Array.from(
    { length: agreedUpTo + 1 },
    (_, index) => material.leafProof(index).stateHash,
  );
  const memo = new Map<string, Hash>();
  /** The node over leaves [index * 2^height, (index + 1) * 2^height). */
  const node = (height: number, index: number): Hash => {
    if (index * 2 ** height > agreedUpTo) return padding.root(height);
    if (height === 0) return midgardValidationTraceLeafHash(agreed[index]!);
    const key = `${height.toString()}:${index.toString()}`;
    const cached = memo.get(key);
    if (cached !== undefined) return cached;
    const hash = midgardValidationTraceBranchHash(
      node(height - 1, 2 * index),
      node(height - 1, 2 * index + 1),
    );
    memo.set(key, hash);
    return hash;
  };
  const proofAt = (stateIndex: number): MidgardValidationTraceProof => {
    const siblings: Hash[] = [];
    let index = stateIndex;
    for (let height = 0; height < depth; height += 1) {
      siblings.push(node(height, index % 2 === 0 ? index + 1 : index - 1));
      index = Math.floor(index / 2);
    }
    return {
      stateIndex,
      stateHash: stateIndex <= agreedUpTo ? agreed[stateIndex]! : pendingHash,
      siblings,
    };
  };
  return Object.freeze({
    agreedUpTo,
    root: node(depth, 0).toString("hex"),
    lastState: PENDING,
    lastStateProof: proofAt(stepCount),
    proofAt,
  });
};

/** Defends `caseUtxo` by `defender` with `trace` after `priorDefenders`;
 * returns the thread's out-ref. */
export const defendScripted = async (
  scene: Scene,
  caseUtxo: UTxO,
  defender: Defender,
  trace: ScriptedTrace,
  priorDefenders: readonly string[] = [],
): Promise<string> =>
  `${await defendWith(scene, caseUtxo, defender, trace, priorDefenders)}#1`;

/**
 * The defender's RevealOperator on `thread`, now, from `trace`, paid from
 * the thread at its minimum fee like every move, signed by its wallet;
 * built, not submitted.
 */
export const buildDefenderReveal = async (
  scene: Scene,
  defender: Defender,
  thread: UTxO,
  trace: ScriptedTrace,
): Promise<TxSignBuilder> => {
  const { lucid } = defender;
  const turn = threadOf(thread).turn;
  if (typeof turn === "string" || !("AwaitingOperator" in turn))
    throw new Error("the thread is not on the defender's turn");
  const proof = SDK.validationTraceProofDataFromCore(
    trace.proofAt(Number(turn.AwaitingOperator.midpoint)),
  );
  const window = validationTraceMoveWindow(lucid);
  return await completeValidationTraceMove({
    lucid,
    stage: "RevealOperator",
    referenced: [scene.references.game],
    build: (fee) =>
      buildValidationTraceOperatorReveal({
        lucid,
        deployment: scene,
        thread,
        proof,
        fee,
        ...window,
      }),
  });
};

/** The defender reveals on `thread` and the reveal lands; returns the
 * signed reveal. */
export const revealAsDefender = async (
  scene: Scene,
  defender: Defender,
  thread: UTxO,
  trace: ScriptedTrace,
): Promise<Readonly<{ hash: string; cbor: string }>> => {
  const built = await buildDefenderReveal(scene, defender, thread, trace);
  const signed = await built.sign.withWallet().complete();
  const hash = await signed.submit();
  await defender.lucid.awaitTx(hash);
  return { hash, cbor: signed.toCBOR() };
};

/** The case-policy thread token `utxo` holds. */
export const threadUnitOf = (scene: Scene, utxo: UTxO): string => {
  const units = Object.keys(utxo.assets).filter((unit) =>
    unit.startsWith(scene.chain.case.policyId),
  );
  if (units.length !== 1) throw new Error("not a thread output");
  return units[0]!;
};

/** The live thread holding `unit`, at the game or the timeout validator. */
export const liveThread = async (
  scene: Scene,
  unit: string,
): Promise<UTxO | undefined> => {
  const { h, chain } = scene;
  const found = [
    ...(await h.proverLucid.utxosAtWithUnit(
      chain.game.spendingScriptAddress,
      unit,
    )),
    ...(await h.proverLucid.utxosAtWithUnit(
      chain.timeout.spendingScriptAddress,
      unit,
    )),
  ];
  if (found.length > 1) throw new Error("a thread token is in two outputs");
  return found[0];
};

export const threadOf = (utxo: UTxO): SDK.CaseThread =>
  Data.from(utxo.datum!, SDK.CaseThreadDatum).data!;

export const isTurn = (
  thread: SDK.CaseThread,
  turn: "AwaitingOperator" | "AwaitingChallenger",
): boolean => typeof thread.turn !== "string" && turn in thread.turn;

/** Checks each signed transaction against the limits and prints the largest
 * size and execution units among them. */
export const reportFit = (label: string, cbors: readonly string[]): void => {
  if (cbors.length === 0) throw new Error(`no ${label} to measure`);
  const measured = cbors.map(measureWithinTransactionLimits);
  const max = (
    key: "completeSignedBytes" | "executionMemory" | "executionSteps",
  ) => Math.max(...measured.map((row) => Number(row[key])));
  console.info(
    `fit ${label} (${measured.length.toString()} txs, max): bytes=${max("completeSignedBytes").toString()} mem=${max("executionMemory").toString()} steps=${max("executionSteps").toString()}`,
  );
};
