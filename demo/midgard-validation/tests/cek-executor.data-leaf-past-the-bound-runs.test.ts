import { encodeCbor } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import {
  decodeCardanoFlatProgram,
  encodeMidgardCekCardanoFlatProgram,
} from "../src/cek-program.cardano-flat.js";
import { stageMidgardCekStagedBuiltinMaterial } from "../src/cek-staged-builtin.material.js";
import { evaluateMidgardCekScript } from "../src/phase-b.evaluate-cek-script.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { DataB, DataI } from "../src/uplc-data.js";
import {
  type Builtin,
  ErrorUPLC,
  UPLCProgram,
  type UPLCTerm,
} from "../src/uplc-term.js";
import { apply, data } from "./cek-executor.semantic-presentation.shapes.js";
import {
  discard,
  ifThenElse,
  lam,
  MIDGARD_CEK_TX_BUDGET,
} from "./helpers/cek-uplc-terms.js";
import { spelledDataFlat } from "./helpers/flat-data-spelling.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * iData, bData, unIData and unBData of a value past the direct payload bound
 * inside a script run: the proof-mode run (the structural executor, every
 * step checked by the consensus verifier) through the Data scalar step and
 * on into the staged steps over its result reaches the PV11 evaluator's
 * verdict and budget, on a script it accepts and on one it fails. An integer
 * leaf past 64 magnitude bytes is read only in its canonical spelling; a
 * script spelling its magnitude as one definite string is refused at
 * decoding and takes no step.
 */

const LEAF_BYTES = 12_000;

/** `length` bytes, the pattern moved by `salt`. */
const pattern = (length: number, salt = 0): Buffer =>
  Buffer.from(
    Uint8Array.from({ length }, (_, index) => (index * 7 + salt) & 0xff),
  );

/** An integer of 10,000 magnitude bytes, its canonical CBOR past the bound. */
const longInteger = (low: bigint): bigint => 2n ** (8n * 10_000n) + low;

const bytesConstant = (bytes: Buffer): UPLCTerm => UPLCConst.byteString(bytes);
const intConstant = (value: bigint): UPLCTerm => UPLCConst.int(value);

/** The script's program; `encodeMidgardCekCardanoFlatProgram` writes its
 * Data constants in canonical Plutus Data CBOR. */
const program = (body: UPLCTerm) =>
  new UPLCProgram([1, 1, 0], lam(discard(body)));

const run = (
  body: UPLCTerm,
  mode: "proof" | "verdict",
  flat = encodeMidgardCekCardanoFlatProgram(program(body)),
) =>
  evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(Buffer.from(flat)),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from("d87980", "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: MIDGARD_CEK_TX_BUDGET,
    },
    mode,
  );

const unit = UPLCConst.unit;
/** Unit when `condition` holds, else a failure. */
const check = (condition: UPLCTerm): UPLCTerm =>
  ifThenElse(condition, unit, new ErrorUPLC());

const call = (builtin: Builtin, ...args: readonly UPLCTerm[]): UPLCTerm =>
  apply(builtin as never, ...args);

const LONG = pattern(LEAF_BYTES);
const LONG_INTEGER = longInteger(3n);

/** The step's witness with its operands staged on outputs of `seed`. */
const staged = (step: MidgardCekExecutionStep, seed: number) => {
  const witness = step.witness;
  if (witness.kind !== "executeBuiltinStaged") throw new Error("not staged");
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

/** Every step of the proof-mode run, a staged step over its staged operands. */
const everyStepVerifies = (proof: ReturnType<typeof run>): boolean =>
  (proof.execution?.steps ?? []).every((step) =>
    verifyMidgardCekCoreStep(
      step.pre,
      step.post,
      step.witness.kind === "executeBuiltinStaged"
        ? staged(step, 9)
        : step.witness,
    ),
  );

const tagsOf = (proof: ReturnType<typeof run>, kind: string): bigint[] =>
  (proof.execution?.steps ?? []).flatMap((step) =>
    step.witness.kind === kind && "tag" in step.witness
      ? [step.witness.tag]
      : [],
  );

// Each run: its label, the script body, its PV11 verdict, the Data scalar
// tags its semantic steps hold, and the tag of the staged step over the
// result, if any.
const RUNS: readonly (readonly [
  string,
  UPLCTerm,
  "accepted" | "script_invalid",
  readonly bigint[],
  bigint | null,
])[] = [
  [
    "unBData of bData of a 12,000-byte string returns it",
    check(
      call(
        builtin("equalsByteString"),
        call(builtin("unBData"), call(builtin("bData"), bytesConstant(LONG))),
        bytesConstant(LONG),
      ),
    ),
    "accepted",
    [41n, 46n],
    15n,
  ],
  [
    "unIData of iData of an integer past the bound returns it",
    check(
      call(
        builtin("equalsInteger"),
        call(
          builtin("unIData"),
          call(builtin("iData"), intConstant(LONG_INTEGER)),
        ),
        intConstant(LONG_INTEGER),
      ),
    ),
    "accepted",
    [40n, 45n],
    7n,
  ],
  [
    "bData of unBData of a 12,000-byte leaf returns it",
    check(
      call(
        builtin("equalsByteString"),
        call(
          builtin("unBData"),
          call(
            builtin("bData"),
            call(builtin("unBData"), data(new DataB(LONG))),
          ),
        ),
        bytesConstant(LONG),
      ),
    ),
    "accepted",
    [46n, 41n, 46n],
    15n,
  ],
  [
    "iData of unIData of a leaf past the bound returns it",
    check(
      call(
        builtin("equalsInteger"),
        call(
          builtin("unIData"),
          call(
            builtin("iData"),
            call(builtin("unIData"), data(new DataI(LONG_INTEGER))),
          ),
        ),
        intConstant(LONG_INTEGER),
      ),
    ),
    "accepted",
    [45n, 40n, 45n],
    7n,
  ],
  [
    "appendByteString onto the unBData result",
    check(
      call(
        builtin("equalsInteger"),
        call(
          builtin("lengthOfByteString"),
          call(
            builtin("appendByteString"),
            call(builtin("unBData"), data(new DataB(LONG))),
            bytesConstant(pattern(5)),
          ),
        ),
        intConstant(BigInt(LEAF_BYTES + 5)),
      ),
    ),
    "accepted",
    [46n],
    13n,
  ],
  [
    "addInteger onto the unIData result",
    check(
      call(
        builtin("equalsInteger"),
        call(
          builtin("addInteger"),
          call(builtin("unIData"), data(new DataI(LONG_INTEGER))),
          intConstant(1n),
        ),
        intConstant(LONG_INTEGER + 1n),
      ),
    ),
    "accepted",
    [45n],
    7n,
  ],
  [
    "unBData of a 12,000-byte leaf compared with other bytes",
    check(
      call(
        builtin("equalsByteString"),
        call(builtin("unBData"), data(new DataB(LONG))),
        bytesConstant(pattern(LEAF_BYTES, 1)),
      ),
    ),
    "script_invalid",
    [46n],
    15n,
  ],
  [
    "unIData of a leaf past the bound compared with another integer",
    check(
      call(
        builtin("equalsInteger"),
        call(builtin("unIData"), data(new DataI(LONG_INTEGER))),
        intConstant(LONG_INTEGER + 1n),
      ),
    ),
    "script_invalid",
    [45n],
    7n,
  ],
  [
    "unBData of bData of a 64-byte string",
    check(
      call(
        builtin("equalsByteString"),
        call(
          builtin("unBData"),
          call(builtin("bData"), bytesConstant(pattern(64))),
        ),
        bytesConstant(pattern(64)),
      ),
    ),
    "accepted",
    [41n, 46n],
    null,
  ],
];

describe("CEK Data scalar steps past the direct payload bound in a run", () => {
  it.each(RUNS)(
    "proves %s with the PV11 evaluator's verdict and budget",
    (_, body, kind, semanticTags, stagedTag) => {
      const verdict = run(body, "verdict");
      const proof = run(body, "proof");
      expect(verdict.result.kind).toBe(kind);
      // A failure's detail names the evaluator; its kind is the verdict.
      if (kind === "accepted") expect(proof.result).toEqual(verdict.result);
      else expect(proof.result.kind).toBe(kind);
      expect(
        tagsOf(proof, "executeBuiltinSemantic").filter((tag) =>
          [40n, 41n, 45n, 46n].includes(tag),
        ),
      ).toEqual(semanticTags);
      const stagedTags = tagsOf(proof, "executeBuiltinStaged");
      if (stagedTag === null) expect(stagedTags).toEqual([]);
      else expect(stagedTags).toContain(stagedTag);
      expect(everyStepVerifies(proof)).toBe(true);
    },
  );
});

describe("an integer leaf past 64 magnitude bytes in a script's Flat", () => {
  const body = call(builtin("unIData"), data(new DataI(LONG_INTEGER)));

  it("is read in its canonical spelling, 64-byte segments", () => {
    const flat = encodeMidgardCekCardanoFlatProgram(program(body));
    expect(decodeCardanoFlatProgram(flat).ok).toBe(true);
  });

  it("is refused at decoding when its magnitude is one definite string", () => {
    // `program(body)` with the magnitude spelled as one definite byte
    // string: tag 2, then a byte string with a two-byte length.
    const digits = LONG_INTEGER.toString(16);
    const magnitude = Buffer.from(
      digits.padStart(digits.length + 1, "0"),
      "hex",
    );
    const length = Buffer.alloc(2);
    length.writeUInt16BE(magnitude.length);
    const definite = Buffer.concat([
      Buffer.from([0xc2, 0x59]),
      length,
      magnitude,
    ]);
    const flat = spelledDataFlat(program(body), [definite]);
    expect(flat.includes(Buffer.from([0xc2, 0x59]))).toBe(true);
    expect(decodeCardanoFlatProgram(flat).ok).toBe(false);
    // Phase one refuses the transaction; no evaluation reaches a verdict.
    for (const mode of ["verdict", "proof"] as const) {
      const evaluated = run(body, mode, flat);
      expect(evaluated.result.kind).toBe("indeterminate");
      expect(evaluated.execution?.steps ?? []).toEqual([]);
    }
  });
});
