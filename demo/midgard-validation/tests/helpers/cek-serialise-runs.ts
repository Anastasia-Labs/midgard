import {
  encodeCbor,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
} from "@al-ft/midgard-core";
import {
  Builtin,
  ErrorUPLC,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import * as UPLC from "@lucid-evolution/uplc";
import { blake2b } from "@noble/hashes/blake2.js";

import type { MidgardCekExecutionStep } from "../../src/cek-executor.build-midgard-cek-execution-graph.js";
import type { MidgardCekSerialiseWitness } from "../../src/cek-serialise.js";
import { evaluateMidgardCekScript } from "../../src/phase-b.evaluate-cek-script.js";
import type { MidgardUplcEvalProgram } from "../../src/plutus-script-evaluator.js";
import { values } from "../fixtures/cek-serialise-loop-v1.vectors.mjs";
import {
  app,
  discard,
  ifThenElse,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  v,
} from "./cek-uplc-terms.js";

/**
 * Runs of serialiseData over the values of
 * `fixtures/cek-serialise-loop-v1.vectors.mjs`. A value past the source
 * constant envelope reaches a script only as data it is given, so each
 * script takes the value under test as its argument.
 */

const call = (builtin: Builtin, ...args: readonly UPLCTerm[]): UPLCTerm =>
  app(builtin as never, ...args);

const flat = (body: UPLCTerm): Buffer =>
  Buffer.from(UPLCEncoder.compile(new UPLCProgram([1, 1, 0], lam(body))));

export const runOver = (
  body: UPLCTerm,
  hex: string,
  mode: "proof" | "verdict",
) =>
  evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(flat(body)),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from(hex, "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: MIDGARD_CEK_TX_BUDGET,
    },
    mode,
  );

/** serialiseData over the script's argument. */
export const SERIALISE: UPLCTerm = discard(call(Builtin.serialiseData, v(0)));

/** The script's argument alone: SERIALISE less its builtin call. */
const ARGUMENT: UPLCTerm = discard(v(0));

/** Whether the CBOR serialiseData gives the argument hashes to `hash`. */
const hashesTo = (hash: Buffer): UPLCTerm =>
  ifThenElse(
    call(
      Builtin.equalsByteString,
      call(Builtin.blake2b_256, call(Builtin.serialiseData, v(0))),
      UPLCConst.byteString(hash as never),
    ),
    UPLCConst.unit,
    new ErrorUPLC(),
  );

export const blake2b256 = (hex: string): Buffer =>
  Buffer.from(blake2b(Buffer.from(hex, "hex"), { dkLen: 32 }));

export type MidgardCekSerialiseStep = MidgardCekExecutionStep & {
  readonly witness: MidgardCekSerialiseWitness;
};

export const serialiseSteps = (
  outcome: ReturnType<typeof runOver>,
): MidgardCekSerialiseStep[] =>
  (outcome.execution?.steps ?? []).filter(
    (step): step is MidgardCekSerialiseStep =>
      step.witness.kind === "executeBuiltinSerialise",
  );

export const valueNamed = (name: string) => {
  const value = values.find((known) => known.name === name);
  if (value === undefined) throw new Error(`no value ${name}`);
  return value;
};

/** A budget past any transaction's, for checks of a result alone. */
const UNBOUNDED = { cpu: 1_000_000_000_000n, memory: 1_000_000_000n };

/**
 * Whether the pinned PV11 evaluator accepts `body` over `hex` within
 * `budget`, and what it spends.
 */
const evaluated = (
  body: UPLCTerm,
  hex: string,
  budget: { readonly cpu: bigint; readonly memory: bigint },
) => {
  const evaluate = UPLC.eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluate(
    flat(body),
    [Buffer.from(hex, "hex")],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    budget.cpu,
    budget.memory,
    false,
  );
  try {
    return { ok: result.ok, cpu: result.cpu, memory: result.mem };
  } finally {
    result.free();
  }
};

/** The budget the pinned PV11 evaluator spends on `body` over `hex`. */
export const spent = (body: UPLCTerm, hex: string) => {
  const result = evaluated(body, hex, MIDGARD_CEK_TX_BUDGET);
  if (!result.ok) throw new Error("the PV11 evaluator refused the run");
  return { cpu: result.cpu, memory: result.memory };
};

/**
 * Whether the bytes serialiseData gives `hex`'s value under the pinned PV11
 * evaluator hash to `hash` (with a budget past a transaction's, since the
 * hash itself is not under test).
 */
export const evaluatorSerialisesTo = (hex: string, hash: Buffer): boolean =>
  evaluated(hashesTo(hash), hex, UNBOUNDED).ok;

/**
 * Whether the bytes serialiseData gives `hex`'s value under the pinned PV11
 * evaluator are `length` long. The evaluator refuses every byte-reading
 * builtin but lengthOfByteString over a byte string past 65,536 bytes, so a
 * longer result is checked by its length here and by its bytes against the
 * reference encoder.
 */
export const evaluatorSerialisesToLength = (
  hex: string,
  length: number,
): boolean =>
  evaluated(
    ifThenElse(
      call(
        Builtin.equalsInteger,
        call(Builtin.lengthOfByteString, call(Builtin.serialiseData, v(0))),
        UPLCConst.int(length),
      ),
      UPLCConst.unit,
      new ErrorUPLC(),
    ),
    hex,
    UNBOUNDED,
  ).ok;

/** The PV11 charge of one machine step (an application or a builtin). */
const MACHINE_STEP = { cpu: 16_000n, memory: 100n };

/**
 * The charge of serialiseData over `hex` under the pinned PV11 evaluator:
 * SERIALISE's spend less ARGUMENT's, less the application and builtin
 * machine steps SERIALISE adds.
 */
export const evaluatorSerialiseCharge = (hex: string) => {
  const withCall = spent(SERIALISE, hex);
  const without = spent(ARGUMENT, hex);
  return {
    cpu: withCall.cpu - without.cpu - 2n * MACHINE_STEP.cpu,
    memory: withCall.memory - without.memory - 2n * MACHINE_STEP.memory,
  };
};

/** The charge the proof-mode run records for its serialise step. */
export const stepCharge = (step: MidgardCekExecutionStep) => ({
  cpu: BigInt(step.post.cpu) - BigInt(step.pre.cpu),
  memory: BigInt(step.post.memory) - BigInt(step.pre.memory),
});
