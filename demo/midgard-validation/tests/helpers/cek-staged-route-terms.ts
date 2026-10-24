import { encodeCbor } from "@al-ft/midgard-core";
import { DataB } from "@harmoniclabs/plutus-data";
import {
  Builtin,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";

import type { MidgardCekExecutionStep } from "../../src/cek-executor.build-midgard-cek-execution-graph.js";
import { stageMidgardCekStagedBuiltinMaterial } from "../../src/cek-staged-builtin.material.js";
import { evaluateMidgardCekScript } from "../../src/phase-b.evaluate-cek-script.js";
import { app, discard, lam, MIDGARD_CEK_TX_BUDGET } from "./cek-uplc-terms.js";

/** Terms and runs for the staged-route tests: scripts whose builtin
 * operands pass the direct carriage bound. */

// Program constants stay within the direct carriage bound, so larger values
// are built at run time.
const PIECE = 8_000;

export const bytes = (length: number, salt = 0): UPLCTerm =>
  UPLCConst.byteString(
    new DataB(
      Uint8Array.from({ length }, (_, index) => (index * 7 + salt) & 0xff),
    ).bytes,
  );
export const text = (length: number, letter = "x"): UPLCTerm =>
  UPLCConst.str(letter.repeat(length));
export const int = (value: bigint): UPLCTerm => UPLCConst.int(value);
export const raw = (...values: readonly number[]): UPLCTerm =>
  UPLCConst.byteString(new DataB(Uint8Array.from(values)).bytes);

export const call = (
  builtin: Builtin,
  ...args: readonly UPLCTerm[]
): UPLCTerm => app(builtin as never, ...args);

/** `length` bytes, appended from pieces of at most 8,000 bytes. */
export const built = (length: number, salt = 0): UPLCTerm => {
  const pieces = Array.from({ length: Math.ceil(length / PIECE) }, (_, index) =>
    bytes(Math.min(PIECE, length - index * PIECE), salt + index),
  );
  return pieces.reduce((left, right) =>
    call(Builtin.appendByteString, left, right),
  );
};

/** `length` copies of `letter`, appended from pieces of 8,000 at most. */
export const builtText = (length: number, letter = "x"): UPLCTerm => {
  const pieces = Array.from({ length: Math.ceil(length / PIECE) }, (_, index) =>
    text(Math.min(PIECE, length - index * PIECE), letter),
  );
  return pieces.reduce((left, right) =>
    call(Builtin.appendString, left, right),
  );
};

/** An integer of `size` magnitude bytes. */
export const wide = (size: number, low = 0n): bigint =>
  (1n << BigInt(8 * size - 1)) + low;

const script = (body: UPLCTerm): Buffer =>
  encodeCbor(
    Buffer.from(
      UPLCEncoder.compile(new UPLCProgram([1, 1, 0], lam(discard(body)))),
    ),
  );

/** Runs a script that discards its context and evaluates `body`. */
export const run = (body: UPLCTerm, mode: "proof" | "verdict") =>
  evaluateMidgardCekScript(
    {
      scriptBytes: script(body),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from("d87980", "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: MIDGARD_CEK_TX_BUDGET,
    },
    mode,
  );

/** The step's witness with its operands staged on outputs of `seed`. */
export const staged = (step: MidgardCekExecutionStep, seed: number) => {
  const witness = step.witness;
  if (
    witness.kind !== "executeBuiltinStaged" &&
    witness.kind !== "executeBuiltinStagedFailure"
  ) {
    throw new Error("not staged");
  }
  let index = 0;
  const staging = witness.stagedOperands.map((operand) =>
    Array.from({ length: Math.ceil(operand.length / 12_285) }, () => ({
      transactionId: Buffer.alloc(32, seed),
      outputIndex: index++,
    })),
  );
  return {
    ...witness,
    material: stageMidgardCekStagedBuiltinMaterial(
      witness.material,
      witness.stagedOperands,
      staging,
    ),
  };
};
