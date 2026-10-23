/** The CEK Data scalar step (iData, bData, unIData, unBData) over a value
 * past the direct payload bound, for the one-step dispute rows: the failing
 * scripts that take two such steps over constants past the bound, the step
 * of our trace that takes each, and our window over a step with its post
 * state returning another result. */
import {
  hashMidgardCekMachineState,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import {
  hashMidgardCekDirectValueWitness,
  type MidgardCekCoreStepWitness,
  type MidgardCekDirectValueWitness,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";
import { plutusV3ScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";

import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import { FlatBits, program } from "./script-decode-walk-shapes.js";

/** Flat builtin tags of the Data scalar steps. */
export const DATA_SCALAR_TAG = Object.freeze({
  iData: 40n,
  bData: 41n,
  unIData: 45n,
  unBData: 46n,
});

/** The length of each byte string, and of each byte string leaf. */
export const LEAF_BYTES = 12_000;
/** The magnitude of each integer, 10,001 bytes: its canonical CBOR is past
 * the direct payload bound. */
export const LONG_INTEGER = 2n ** (8n * 10_000n) + 5n;

const leafBytes = (salt: number) =>
  Uint8Array.from(
    { length: LEAF_BYTES },
    (_, index) => (index * 7 + salt) & 0xff,
  );

const magnitude = (value: bigint): Uint8Array => {
  const hex = value.toString(16);
  return Buffer.from(hex.length % 2 === 0 ? hex : `0${hex}`, "hex");
};

/** The CBOR head of major type `major` with argument `value`. */
const head = (major: number, value: number): Uint8Array =>
  value < 24
    ? Uint8Array.from([major * 32 + value])
    : value < 256
      ? Uint8Array.from([major * 32 + 24, value])
      : Uint8Array.from([major * 32 + 25, value >> 8, value & 255]);

/** A byte string's canonical Plutus Data CBOR: definite up to 64 bytes,
 * else an indefinite string of 64-byte segments. */
const cborBytes = (bytes: Uint8Array): Uint8Array => {
  if (bytes.length <= 64) return Buffer.concat([head(2, bytes.length), bytes]);
  const segments: Uint8Array[] = [Uint8Array.from([0x5f])];
  for (let start = 0; start < bytes.length; start += 64) {
    const segment = bytes.subarray(start, start + 64);
    segments.push(head(2, segment.length), segment);
  }
  segments.push(Uint8Array.from([0xff]));
  return Buffer.concat(segments);
};

/** A non-negative integer past 64 bits as Plutus Data CBOR: tag 2 over its
 * magnitude. */
const cborBigInteger = (value: bigint): Uint8Array =>
  Buffer.concat([Uint8Array.from([0xc2]), cborBytes(magnitude(value))]);

/** A Flat natural of any size: 7-bit groups, least significant first. */
const naturalBits = (bits: FlatBits, value: bigint) => {
  let rest = value;
  for (;;) {
    const group = Number(rest % 128n);
    rest /= 128n;
    bits.put(rest > 0n ? 128 + group : group, 8);
    if (rest === 0n) return;
  }
};

type Term = (bits: FlatBits) => void;

const apply =
  (fn: Term, argument: Term): Term =>
  (bits) => {
    bits.put(3, 4);
    fn(bits);
    argument(bits);
  };
const lam =
  (body: Term): Term =>
  (bits) => {
    bits.put(2, 4);
    body(bits);
  };
const error: Term = (bits) => bits.put(6, 4);
const builtin =
  (tag: bigint): Term =>
  (bits) => {
    bits.put(7, 4);
    bits.put(Number(tag), 7);
  };
const integerConstant =
  (value: bigint): Term =>
  (bits) => {
    bits.constant([0]);
    naturalBits(bits, value >= 0n ? 2n * value : -2n * value - 1n);
  };
const bytesConstant =
  (bytes: Uint8Array): Term =>
  (bits) => {
    bits.constant([1]);
    bits.chunks(bytes);
  };
const dataConstant =
  (cbor: Uint8Array): Term =>
  (bits) => {
    bits.constant([8]);
    bits.chunks(cbor);
  };

/** `(program 1.1.0 (lam ctx [(lam _ (error)) [(lam _ SECOND) FIRST]]))`:
 * FIRST, then SECOND, then a failure. */
const failingAfter = (first: Term, second: Term) =>
  plutusV3ScriptWitness(
    program(lam(apply(lam(error), apply(lam(second), first)))),
  );

/** unBData of a 12,000-byte leaf, then bData of a 12,000-byte string. */
export const BYTES_SCRIPT = failingAfter(
  apply(
    builtin(DATA_SCALAR_TAG.unBData),
    dataConstant(cborBytes(leafBytes(0))),
  ),
  apply(builtin(DATA_SCALAR_TAG.bData), bytesConstant(leafBytes(1))),
);

/** unIData of an integer leaf past the bound, then iData of an integer past
 * the bound. */
export const INTEGER_SCRIPT = failingAfter(
  apply(
    builtin(DATA_SCALAR_TAG.unIData),
    dataConstant(cborBigInteger(LONG_INTEGER)),
  ),
  apply(builtin(DATA_SCALAR_TAG.iData), integerConstant(LONG_INTEGER + 1n)),
);

/** The last state of our trace before its Data scalar step of `tag`. */
export const dataScalarStepBefore =
  (tag: bigint) =>
  async (material: ValidationTraceClaimMaterial): Promise<number> => {
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
        auxiliary.step.witness.kind === "executeBuiltinSemantic" &&
        auxiliary.step.witness.tag === tag
      )
        return index;
    }
    throw new Error(
      `our trace takes no Data scalar step of tag ${tag.toString()}`,
    );
  };

/** Our Data scalar step of `tag`: the last state before it, our window over
 * it, its witness and its result. */
export type DataScalarStep = Readonly<{
  agreedUpTo: number;
  ours: ValidationTraceSteps;
  witness: Extract<
    MidgardCekCoreStepWitness,
    { kind: "executeBuiltinSemantic" }
  >;
  result: Extract<MidgardCekDirectValueWitness, { kind: "semanticConstant" }>;
}>;

export const dataScalarStepOf = async (
  material: ValidationTraceClaimMaterial,
  tag: bigint,
): Promise<DataScalarStep> => {
  const agreedUpTo = await dataScalarStepBefore(tag)(material);
  const ours = await material.steps(agreedUpTo);
  const auxiliary = ours.witnesses[agreedUpTo]!.auxiliary;
  if (
    auxiliary?.kind !== "cekCoreStep" ||
    auxiliary.step.witness.kind !== "executeBuiltinSemantic"
  )
    throw new Error("the step is not a semantic builtin step");
  const witness = auxiliary.step.witness;
  const result = witness.result;
  if (result.kind !== "semanticConstant")
    throw new Error("the step's result is not presented by its summary");
  return { agreedUpTo, ours, witness, result };
};

/** The step's result naming the payload summary of another leaf. */
export const resultOfAnotherLeaf = ({ result }: DataScalarStep) => {
  const root = Buffer.from(result.witness.payload.root);
  root[root.length - 1]! ^= 1;
  return {
    kind: "semanticConstant",
    witness: {
      ...result.witness,
      payload: { ...result.witness.payload, root },
    },
  } as const;
};

/** The step's result one word heavier. */
export const resultOneWordHeavier = ({ result }: DataScalarStep) =>
  ({
    kind: "semanticConstant",
    witness: { ...result.witness, memory: result.witness.memory + 1n },
  }) as const;

/** Our window over the step with its post state returning `result` and its
 * witness naming it: the successor's work witness binds that post state. */
export const windowReturning = (
  { agreedUpTo, ours }: DataScalarStep,
  result: MidgardCekDirectValueWitness,
): ValidationTraceSteps => {
  const pre = ours.states[agreedUpTo]!;
  const successor = ours.states[agreedUpTo + 1]!;
  const witness = ours.witnesses[agreedUpTo]!;
  const next = ours.witnesses[agreedUpTo + 1]!;
  const auxiliary = witness.auxiliary;
  if (
    auxiliary?.kind !== "cekCoreStep" ||
    auxiliary.step.witness.kind !== "executeBuiltinSemantic"
  )
    throw new Error("the step is not a semantic builtin step");
  const post = {
    ...auxiliary.step.post,
    focusRoot: hashMidgardCekDirectValueWitness(result),
  };
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
            witness: { ...auxiliary.step.witness, result },
          },
        },
      },
      [agreedUpTo + 1]: { ...next, cbor },
    },
  };
};

/** The hash of the successor state of `window` after the step. */
export const successorHashOf = (
  { agreedUpTo }: DataScalarStep,
  window: ValidationTraceSteps,
) => hashMidgardValidationMachineState(window.states[agreedUpTo + 1]!);
