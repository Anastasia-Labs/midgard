/** The CEK step that computes a constant, for the one-step dispute rows at
 * the field bound: the failing script that carries the constant, the step
 * of our trace that computes it, our window over that step with its post
 * state returning another root of the constant, and a second case on the
 * failing block that commits that other post state and stands, on each of
 * its two threads, at the start of the one-step resolution. */
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  hashMidgardCekMachineState,
  hashMidgardCekValueNode,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  midgardValidationTraceBranchHash,
  midgardValidationTraceLeafHash,
  MidgardValidationTracePadding,
  type MidgardValidationTraceProof,
  validationTraceDepthForStepCount,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgram,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";
import { CML, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import { validationTraceMoveWindow } from "../../src/validation-trace-dispute/move-builders.js";
import { completeValidationTraceMove } from "../../src/validation-trace-dispute/move-fee.js";
import {
  buildValidationTraceHop,
  validationTraceHop,
  type ValidationTraceHopHolder,
} from "../../src/validation-trace-dispute/one-step-builders.js";
import { validationTraceOneStepHolder } from "../../src/validation-trace-dispute/one-step-holders.js";
import type { ValidationTraceOneStepPlan } from "../../src/validation-trace-dispute/one-step-plan.js";
import {
  MAXIMUM_ITEM_BYTES,
  plutusItem,
} from "./script-decode-plutus-max-shape.js";
import { FlatBits, program } from "./script-decode-walk-shapes.js";
import type { Defender } from "./validation-trace-dispute-defend.js";
import {
  challengerEntersResolution,
  challengerReveal,
  landWith,
  movedThread,
  operatorReveal,
} from "./validation-trace-dispute-defender-moves.js";
import type { TwoThreadCase } from "./validation-trace-dispute-one-step-case.js";
import { threadPlace } from "./validation-trace-dispute-one-step-play.js";
import {
  initAndOpen,
  verifySource,
} from "./validation-trace-dispute-open-scene.js";
import {
  defendScripted,
  isTurn,
  scriptedTrace,
  threadOf,
  threadUnitOf,
} from "./validation-trace-dispute-scripted-defender.js";

type Hash = ReturnType<typeof midgardValidationTraceLeafHash>;

/** `(program 1.1.0 (lam ctx [(lam pad (error)) (con bytestring P)]))` with P
 * `bytes` long, wrapped in one CBOR byte string. */
export const constantScript = (bytes: number): Buffer =>
  program((bits: FlatBits) => {
    bits.put(2, 4);
    bits.put(3, 4);
    bits.put(2, 4);
    bits.put(6, 4);
    bits.constant([1]);
    bits.chunks(Buffer.alloc(bytes, 0x5a));
  });

/** The constant script whose field-6 item fills the field bound, and the
 * length of its P. */
export const maximumConstantScript = () => {
  for (let bytes = MAXIMUM_ITEM_BYTES; bytes > 0; bytes -= 1) {
    const script = constantScript(bytes);
    if (plutusItem(script).length === MAXIMUM_ITEM_BYTES)
      return { script, bytes };
  }
  throw new Error("no constant script fills the field bound");
};

/** The step of our trace that computes a constant, the last one. */
export const computeConstantStep = async (
  material: ValidationTraceClaimMaterial,
): Promise<number> => {
  const stepCount = Number(material.stepCount);
  const indices = Array.from(
    { length: Math.min(stepCount, 256) },
    (_, offset) => stepCount - 1 - offset,
  );
  const windows = await material.stepsAt(indices);
  for (const [position, index] of indices.entries()) {
    const auxiliary = windows[position]!.witnesses[index]?.auxiliary;
    if (
      auxiliary?.kind === "cekCoreStep" &&
      auxiliary.step.witness.kind === "computeConstant"
    )
      return index;
  }
  throw new Error("our trace computes no constant");
};

/** A trace over our step count: our states up to `agreedUpTo`, then
 * `successorHash` up to the step count, then our Terminal Rejected state,
 * which also pads the tree. */
export const committedTrace = (
  material: ValidationTraceClaimMaterial,
  agreedUpTo: number,
  successorHash: Buffer,
) => {
  const stepCount = Number(material.stepCount);
  const depth = validationTraceDepthForStepCount(stepCount);
  const terminalHash = material.terminalLeafProof.stateHash;
  const after = new MidgardValidationTracePadding(successorHash);
  const past = new MidgardValidationTracePadding(terminalHash);
  const agreed = Array.from(
    { length: agreedUpTo + 1 },
    (_, index) => material.leafProof(index).stateHash,
  );
  const stateHashAt = (index: number) =>
    index <= agreedUpTo
      ? agreed[index]!
      : index < stepCount
        ? successorHash
        : terminalHash;
  const memo = new Map<string, Hash>();
  /** The node over leaves [index * 2^height, (index + 1) * 2^height). */
  const node = (height: number, index: number): Hash => {
    const first = index * 2 ** height;
    const last = first + 2 ** height - 1;
    if (first > agreedUpTo && last < stepCount) return after.root(height);
    if (first >= stepCount) return past.root(height);
    if (height === 0) return midgardValidationTraceLeafHash(stateHashAt(first));
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
    return { stateIndex, stateHash: stateHashAt(stateIndex), siblings };
  };
  return { root: node(depth, 0), proofAt };
};

export const outRef = (text: string) => ({
  txHash: text.split("#")[0]!,
  outputIndex: Number(text.split("#")[1]),
});

export const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The address of the first output of the signed transaction `cbor`. */
export const firstOutputAddress = (cbor: string) =>
  CML.Address.from_hex(
    CML.Transaction.from_cbor_hex(cbor)
      .body()
      .outputs()
      .get(0)
      .address()
      .to_hex(),
  ).to_bech32();

/** The step of our trace that computes P: the last state before it, our
 * window over it, P's value root and the root of P with one memory unit
 * more. */
export type ComputeConstantStep = Readonly<{
  agreedUpTo: number;
  ours: ValidationTraceSteps;
  valueRoot: Buffer;
  otherRoot: Buffer;
}>;

export const computeConstantStepOf = async (
  material: ValidationTraceClaimMaterial,
  script: Buffer,
): Promise<ComputeConstantStep> => {
  const agreedUpTo = await computeConstantStep(material);
  const ours = await material.steps(agreedUpTo);
  const auxiliary = ours.witnesses[agreedUpTo]!.auxiliary;
  if (
    auxiliary?.kind !== "cekCoreStep" ||
    auxiliary.step.witness.kind !== "computeConstant"
  )
    throw new Error("the step does not compute a constant");
  const valueRoot = Buffer.from(auxiliary.step.witness.valueRoot);
  const value = buildMidgardCanonicalCekProgram(script).material.get(
    valueRoot.toString("hex"),
  );
  if (value?.kind !== "value") throw new Error("P has no value node");
  const [, typeRoot, payloadRoot, payloadLength, semanticRoot, memory] =
    asArray(decodeSingleCbor(value.preimage), "value");
  const otherRoot = Buffer.from(
    hashMidgardCekValueNode({
      kind: "constant",
      typeRoot: asBytes(typeRoot, "type_root"),
      payloadRoot: asBytes(payloadRoot, "payload_root"),
      payloadLength: asBigInt(payloadLength, "payload_length"),
      semanticRoot: asBytes(semanticRoot, "semantic_root"),
      memory: asBigInt(memory, "memory") + 1n,
    }),
  );
  return { agreedUpTo, ours, valueRoot, otherRoot };
};

/** Our window over the step with its post state returning the other root
 * and its witness naming `named`: the successor's work witness binds that
 * post state. */
export const otherWindow = (
  { agreedUpTo, ours, otherRoot }: ComputeConstantStep,
  named: Buffer,
): ValidationTraceSteps => {
  const pre = ours.states[agreedUpTo]!;
  const successor = ours.states[agreedUpTo + 1]!;
  const witness = ours.witnesses[agreedUpTo]!;
  const next = ours.witnesses[agreedUpTo + 1]!;
  const auxiliary = witness.auxiliary;
  if (auxiliary?.kind !== "cekCoreStep")
    throw new Error("the step is not a CEK core step");
  const post = { ...auxiliary.step.post, focusRoot: otherRoot };
  const postHash = hashMidgardCekMachineState(auxiliary.step.post);
  const at = next.cbor.indexOf(postHash);
  if (at === -1 || next.cbor.indexOf(postHash, at + 1) !== -1)
    throw new Error("the successor's work witness names the post state once");
  const cbor = Buffer.concat([
    next.cbor.subarray(0, at),
    hashMidgardCekMachineState(post),
    next.cbor.subarray(at + postHash.length),
  ]);
  return {
    states: {
      [agreedUpTo]: pre,
      [agreedUpTo + 1]: {
        ...successor,
        workRoot: hashMidgardValidationWorkWitness({
          phase: next.phase,
          programCounter: next.programCounter,
          witnessCbor: cbor,
        }),
      },
    },
    witnesses: {
      [agreedUpTo]: {
        ...witness,
        auxiliary: {
          kind: "cekCoreStep",
          step: {
            pre: auxiliary.step.pre,
            post,
            witness: { kind: "computeConstant", valueRoot: named },
          },
        },
      },
      [agreedUpTo + 1]: { ...next, cbor },
    },
  };
};

/** The thread holding `unit`, wherever it stands. */
export const place = async (dispute: TwoThreadCase, unit: string) => {
  const found = await threadPlace(dispute.scene, dispute.deployment, unit);
  if (found === undefined) throw new Error("the thread is not live");
  return found;
};

/** The hop of the thread holding `unit` from `holder` with `plan`, built
 * and completed as the runner completes it. */
export const hopFrom =
  (
    dispute: TwoThreadCase,
    unit: string,
    plan: ValidationTraceOneStepPlan,
    holder: ValidationTraceHopHolder,
  ) =>
  async () => {
    const { scene, deployment } = dispute;
    const { utxo } = await place(dispute, unit);
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
          unit,
          reference,
          hop,
          address: validationTraceOneStepHolder(deployment, hop.next).address,
          prover,
          fee,
          ...window,
        }),
    });
  };

/** Opens a second case on the failing block whose trace commits the
 * successor returning the other root, has both operators defend it with our
 * trace up to that successor, bisects both threads to ReadyForOneStep on
 * the step and enters the resolution on each. Returns the committed
 * successor's hash and each thread as it stood ReadyForOneStep. */
export const secondCaseAtTheStep = async (
  dispute: TwoThreadCase,
  step: ComputeConstantStep,
) =>
  await secondCaseWithSuccessor(
    dispute,
    step.agreedUpTo,
    hashMidgardValidationMachineState(
      otherWindow(step, step.valueRoot).states[step.agreedUpTo + 1]!,
    ),
  );

/** Opens a second case on the failing block whose trace agrees with ours up
 * to `agreedUpTo` and commits `successorHash` after it, has both operators
 * defend it with our trace up to our successor, bisects both threads to
 * ReadyForOneStep on that step and enters the resolution on each. Returns
 * the committed successor's hash and each thread as it stood
 * ReadyForOneStep. */
export const secondCaseWithSuccessor = async (
  dispute: TwoThreadCase,
  agreedUpTo: number,
  successorHash: Buffer,
) => {
  const { scene, material, a, b } = dispute;
  const committed = committedTrace(material, agreedUpTo, successorHash);
  const lucid = scene.h.proverLucid;
  const { opener } = await initAndOpen(scene, scene.failing, {
    claimWitness: material.claimWitness,
    challengerRoot: committed.root.toString("hex"),
    stepCount: material.stepCount,
  });
  const verified = await landWith(
    lucid,
    await verifySource(scene, opener, {
      terminal: material.terminal,
      terminalProof: committed.proofAt(Number(material.stepCount)),
      initialState: material.initialState,
    }).build(),
  );
  const [caseUtxo] = await lucid.utxosByOutRef([
    { txHash: verified.hash, outputIndex: 0 },
  ]);
  const defense = scriptedTrace(material, agreedUpTo + 1);
  const defended = async (
    defender: Defender,
    at: UTxO,
    priorDefenders: readonly string[],
  ) => {
    const thread = await defendScripted(
      scene,
      at,
      defender,
      defense,
      priorDefenders,
    );
    const [utxo] = await lucid.utxosByOutRef([outRef(thread)]);
    const [caseAfter] = await lucid.utxosByOutRef([
      { ...outRef(thread), outputIndex: 0 },
    ]);
    return { thread: utxo!, caseAfter: caseAfter! };
  };
  const first = await defended(a.defender, caseUtxo!, []);
  const second = await defended(b.defender, first.caseAfter, [a.defender.vkey]);
  const ready: { unit: string; thread: SDK.CaseThread }[] = [];
  for (const [defender, start] of [
    [a.defender, first.thread],
    [b.defender, second.thread],
  ] as const) {
    let thread = start;
    while (threadOf(thread).turn !== "ReadyForOneStep") {
      const landed = isTurn(threadOf(thread), "AwaitingOperator")
        ? await landWith(
            defender.lucid,
            await operatorReveal(scene, defender, thread, defense.proofAt),
          )
        : await landWith(
            lucid,
            await challengerReveal(scene, thread, committed),
          );
      thread = await movedThread(lucid, landed);
    }
    ready.push({ unit: threadUnitOf(scene, thread), thread: threadOf(thread) });
    await landWith(lucid, await challengerEntersResolution(scene, thread));
  }
  return { committedSuccessorHash: successorHash.toString("hex"), ready };
};
