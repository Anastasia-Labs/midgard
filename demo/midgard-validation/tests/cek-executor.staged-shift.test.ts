import { midgardCekFlatRoot } from "@al-ft/midgard-core";
import { CEKConst } from "@harmoniclabs/plutus-machine";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import type { MidgardCekDirectValueWitness } from "../src/cek-builtin.argument-kinds.js";
import {
  evaluateMidgardCekBuiltinOnValues,
  referenceConstantToDirectWitness,
} from "../src/cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { errorSuccessor } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import {
  midgardCekPlaceholderStaging,
  type MidgardCekStagedOperandSource,
  midgardCekStagedReferenceInputs,
  stageMidgardCekStagedBuiltinMaterial,
} from "../src/cek-staged-builtin.material.js";
import { verifyMidgardCekStagedBuiltin } from "../src/cek-staged-builtin.verify.js";
import { constT, UPLCConst } from "../src/uplc-constant.js";
import { DataB } from "../src/uplc-data.js";
import { type Builtin, ErrorUPLC, type UPLCTerm } from "../src/uplc-term.js";
import {
  built,
  call,
  int,
  run,
  staged,
} from "./helpers/cek-staged-route-terms.js";
import { app, ifThenElse, lam, mkCons, v } from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * shiftByteString and rotateByteString over an input past the direct
 * carriage bound. Protocol version 11 bounds neither input, so the staged
 * conversion step (arm 31, semantics group 9) proves them over an input of
 * any length, chunk by chunk. The longest input an honest script can make
 * within the transaction budget comes from serialiseData over copies of a
 * 65,536-byte string. The proof-mode run reaches the PV11 evaluator's
 * verdict and budget, and each step has exactly one provable result.
 */

const SHIFT = 82n;
const ROTATE = 83n;

const SIZES = [9_215, 9_216, 65_536, 65_537, 131_072] as const;

/** An input of `size` bytes built at run time. Protocol version 11 bounds
 * appendByteString's arguments at 65,536 bytes, so a longer input appends
 * two halves. */
const input = (size: number): UPLCTerm =>
  size <= 65_537
    ? built(size, 1)
    : call(
        builtin("appendByteString"),
        built(65_536, 1),
        built(size - 65_536, 2),
      );

type Row = readonly [name: string, tag: bigint, amount: bigint];

const ROWS: readonly Row[] = [
  ["shiftByteString towards the start by bits", SHIFT, 3n],
  [
    "shiftByteString towards the end across a chunk",
    SHIFT,
    -(8n * 4_095n + 5n),
  ],
  [
    "rotateByteString towards the start across a chunk",
    ROTATE,
    8n * 4_095n + 5n,
  ],
  ["rotateByteString towards the end by bits", ROTATE, -3n],
];

/** The amount the PV11 evaluator's run of a row takes. Its rotation by a
 * negative amount is quadratic in the input (seconds past 32,768 bytes), so
 * past 9,216 bytes the verdict is taken over the same rotation towards the
 * start, which costs the same: both builtins cost their input alone. */
const verdictAmount = (tag: bigint, amount: bigint, size: number): bigint =>
  tag === ROTATE && amount < 0n && size > 9_216 ? -amount : amount;

const PIECE = 8_000;

/** The bytes `built(length, salt)` builds at run time. */
const builtBytes = (length: number, salt = 0): Buffer =>
  Buffer.concat(
    Array.from({ length: Math.ceil(length / PIECE) }, (_, piece) =>
      Uint8Array.from(
        { length: Math.min(PIECE, length - piece * PIECE) },
        (_, index) => (index * 7 + salt + piece) & 0xff,
      ),
    ),
  );

const builtinOf = (tag: bigint): Builtin =>
  tag === SHIFT ? builtin("shiftByteString") : builtin("rotateByteString");

const stagedSteps = (
  outcome: ReturnType<typeof run>,
  tag: bigint,
): MidgardCekExecutionStep[] =>
  (outcome.execution?.steps ?? []).filter(
    ({ witness }) =>
      witness.kind === "executeBuiltinStaged" && witness.tag === tag,
  );

const UNCHARGED = { cpu: 0n, memory: 0n };

/** The step proves its result, and no failure arm proves a failure. */
const expectOneProvableResult = (step: MidgardCekExecutionStep): void => {
  const witness = step.witness;
  if (witness.kind !== "executeBuiltinStaged") throw new Error("not staged");
  expect(verifyMidgardCekCoreStep(step.pre, step.post, staged(step, 9))).toBe(
    true,
  );
  const failures: MidgardCekCoreStepWitness[] = [
    {
      kind: "executeBuiltinStagedFailure",
      tag: witness.tag,
      arguments: witness.arguments,
      material: { runs: { refs: [], operands: [] }, sources: [] },
      stagedOperands: [],
    },
    {
      kind: "executeBuiltinFailure",
      tag: witness.tag,
      arguments: witness.arguments,
    },
  ];
  const charge = {
    cpu: step.post.cpu - step.pre.cpu,
    memory: step.post.memory - step.pre.memory,
  };
  for (const failure of failures) {
    for (const budget of [charge, UNCHARGED]) {
      expect(
        verifyMidgardCekCoreStep(
          step.pre,
          errorSuccessor(step.pre, MidgardCekErrorCodes.BuiltinFailure, budget),
          failure,
        ),
      ).toBe(false);
    }
  }
};

/** The run reaches the evaluator's verdict (over `verdictBody`, by default
 * the same body), and each staged step of `tag` has one provable result;
 * returns those steps. */
const agrees = (
  body: UPLCTerm,
  tag: bigint,
  verdictBody: UPLCTerm = body,
): MidgardCekExecutionStep[] => {
  const verdict = run(verdictBody, "verdict");
  const proof = run(body, "proof");
  expect(proof.result).toEqual(verdict.result);
  const steps = stagedSteps(proof, tag);
  for (const step of steps) expectOneProvableResult(step);
  return steps;
};

// ## The longest input an honest script makes

const COPY = 65_536;

/** serialiseData of a list of five (or `copies`) copies of a 65,536-byte
 * string and its first `tail` bytes: the cheapest way to make a long byte
 * string. */
const serialised = (tail: number, copies = 5): UPLCTerm => {
  let list = mkCons(
    call(
      builtin("bData"),
      call(builtin("sliceByteString"), int(0n), int(BigInt(tail)), v(0)),
    ),
    UPLCConst.listOf(constT.data)([]),
  );
  for (let copy = 0; copy < copies; copy++) {
    list = mkCons(call(builtin("bData"), v(0)), list);
  }
  return app(
    lam(call(builtin("serialiseData"), call(builtin("listData"), list))),
    built(COPY, 5),
  );
};

/** The CBOR of a byte string as Cardano serialises it: chunks of 64 bytes
 * past 64. */
const bytesCbor = (bytes: Buffer): Buffer => {
  const head = (length: number): Buffer =>
    length < 24 ? Buffer.from([0x40 + length]) : Buffer.from([0x58, length]);
  if (bytes.length <= 64) return Buffer.concat([head(bytes.length), bytes]);
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < bytes.length; offset += 64) {
    const chunk = bytes.subarray(offset, offset + 64);
    chunks.push(head(chunk.length), chunk);
  }
  chunks.push(Buffer.from([0xff]));
  return Buffer.concat(chunks);
};

/** The bytes `serialised(tail, copies)` makes. */
const serialisedBytes = (tail: number, copies = 5): Buffer => {
  const copy = builtBytes(COPY, 5);
  return Buffer.concat([
    Buffer.from([0x9f]),
    ...Array.from({ length: copies }, () => bytesCbor(copy)),
    bytesCbor(copy.subarray(0, tail)),
    Buffer.from([0xff]),
  ]);
};

/** The largest tail whose shift or rotation (by 3 bits) fits the
 * transaction budget, and the input it makes. Both builtins cost their input
 * alone, so the amount's sign does not move the bound. */
const LONGEST = [
  ["shiftByteString", SHIFT, 3n, 31_208, 370_118],
  ["rotateByteString", ROTATE, 3n, 31_424, 370_340],
] as const;

describe("shiftByteString and rotateByteString over staged inputs", () => {
  it.each(
    ROWS.flatMap(([name, tag, amount]) =>
      SIZES.map((size) => [name, size, tag, amount] as const),
    ),
  )(
    "%s over %i bytes reaches the PV11 evaluator's verdict and budget",
    (_, size, tag, amount) => {
      const body = call(builtinOf(tag), input(size), int(amount));
      const verdictBody = call(
        builtinOf(tag),
        input(size),
        int(verdictAmount(tag, amount, size)),
      );
      expect(run(verdictBody, "verdict").result.kind).toBe("accepted");
      const steps = agrees(body, tag, verdictBody);
      if (size > 9_215) expect(steps).toHaveLength(1);
    },
  );

  it.each(LONGEST)(
    "%s: the transaction budget allows the longest input serialiseData makes",
    (_, tag, amount, tail, length) => {
      const longest = (extra: number) =>
        call(builtinOf(tag), serialised(tail + extra), int(amount));
      expect(run(longest(0), "verdict").result.kind).toBe("accepted");
      expect(run(longest(1), "verdict").result.kind).toBe("script_invalid");
      expect(serialisedBytes(tail)).toHaveLength(length);
    },
  );

  it("serialiseData makes the bytes the longest-input rows reproduce", () => {
    const holds = (condition: UPLCTerm): void => {
      const body = ifThenElse(condition, UPLCConst.unit, new ErrorUPLC());
      expect(run(body, "verdict").result.kind).toBe("accepted");
    };
    // Protocol version 11 bounds a hashed or sliced byte string at 65,536
    // bytes, so the longest input's length is checked, and its encoding
    // over a tail alone.
    for (const [, , , tail, length] of LONGEST) {
      holds(
        call(
          builtin("equalsInteger"),
          call(builtin("lengthOfByteString"), serialised(tail)),
          int(BigInt(length)),
        ),
      );
    }
    const [tail, copies] = [40_000, 0];
    holds(
      call(
        builtin("equalsByteString"),
        call(builtin("blake2b_256"), serialised(tail, copies)),
        UPLCConst.byteString(
          new DataB(blake2b(serialisedBytes(tail, copies), { dkLen: 32 }))
            .bytes,
        ),
      ),
    );
  });
});

// ## The staged step's relation

const presented = (value: CEKConst): MidgardCekDirectValueWitness =>
  referenceConstantToDirectWitness(value, true);

const byteString = (value: Buffer): CEKConst =>
  CEKConst.byteString(new DataB(value).bytes as never);

/** Whether the staged conversion step accepts `result` for builtin `tag`
 * over the staged `input` by `amount`. */
const shiftVerifies = (
  tag: bigint,
  input: Buffer,
  amount: bigint,
  result: MidgardCekDirectValueWitness,
  sources: readonly MidgardCekStagedOperandSource[] = [
    { kind: "staged", operand: 0 },
    { kind: "smallInteger", value: amount },
  ],
): boolean => {
  const material = stageMidgardCekStagedBuiltinMaterial(
    {
      runs: { refs: [], operands: [] },
      sources,
    },
    [input],
    midgardCekPlaceholderStaging([input]),
  );
  return verifyMidgardCekStagedBuiltin(
    tag,
    [byteString(input), CEKConst.int(amount)].map(presented),
    result,
    material,
    midgardCekStagedReferenceInputs(material, [input]),
  );
};

/** Cardano's result for builtin `tag` over `input` by `amount`. */
const shifted = (tag: bigint, input: Buffer, amount: bigint): Buffer => {
  const evaluated = evaluateMidgardCekBuiltinOnValues(Number(tag), [
    byteString(input),
    CEKConst.int(amount),
  ]);
  if (evaluated === "failure") throw new Error("builtin failed");
  return Buffer.from(evaluated.value as Uint8Array);
};

/** The 372,645-byte input of the on-chain maximum-shape rows: byte i is
 * (i + 1) mod 256. */
const MAX_SHAPE = Buffer.from(
  Uint8Array.from({ length: 372_645 }, (_, index) => (index + 1) & 0xff),
);

/** The flat roots of the on-chain maximum-shape rows' results. */
const MAX_SHAPE_ROWS = [
  [
    "a shift towards the start",
    SHIFT,
    8_003n,
    "5034610bed6f5f61831b2b18a36af6dacebda3d37d912465fcde996bd101fd22",
  ],
  [
    "a shift towards the end",
    SHIFT,
    -8_003n,
    "95db54de631587287faf289955ea51272dc14d56481ab7ad6738a010e1b5884e",
  ],
  [
    "a rotation",
    ROTATE,
    1_600_005n,
    "ea39496b399eb92fd8fb0e179ce948e191566ff75413d957c53fbab497c8c0d7",
  ],
] as const;

describe("the staged conversion step proves shifts and rotations", () => {
  it.each(MAX_SHAPE_ROWS)(
    "proves %s over 91 full chunks, rooted as the on-chain row pins it",
    (_, tag, amount, root) => {
      const result = shifted(tag, MAX_SHAPE, amount);
      expect(Buffer.from(midgardCekFlatRoot(result)).toString("hex")).toBe(
        root,
      );
      expect(
        shiftVerifies(tag, MAX_SHAPE, amount, presented(byteString(result))),
      ).toBe(true);
    },
  );

  it.each(LONGEST)(
    "%s: proves its result over the longest input an honest script makes",
    (_, tag, amount, tail) => {
      const longest = serialisedBytes(tail);
      for (const by of [amount, -amount, 8n * 300_000n + 1n]) {
        const result = shifted(tag, longest, by);
        expect(
          shiftVerifies(tag, longest, by, presented(byteString(result))),
        ).toBe(true);
        result[result.length - 1]! ^= 1;
        expect(
          shiftVerifies(tag, longest, by, presented(byteString(result))),
        ).toBe(false);
      }
    },
  );

  const bytes = builtBytes(70_000, 3);

  it("refuses the result of another amount or of the other builtin", () => {
    for (const [tag, other] of [
      [SHIFT, ROTATE],
      [ROTATE, SHIFT],
    ] as const) {
      const amount = 8n * 5_000n + 3n;
      expect(
        shiftVerifies(
          tag,
          bytes,
          amount,
          presented(byteString(shifted(tag, bytes, amount))),
        ),
      ).toBe(true);
      expect(
        shiftVerifies(
          tag,
          bytes,
          amount,
          presented(byteString(shifted(tag, bytes, amount + 1n))),
        ),
      ).toBe(false);
      expect(
        shiftVerifies(
          tag,
          bytes,
          amount,
          presented(byteString(shifted(other, bytes, amount))),
        ),
      ).toBe(false);
    }
  });

  it("refuses an amount other than the committed one, an extra source, or an amount past a machine integer", () => {
    const input: MidgardCekStagedOperandSource = { kind: "staged", operand: 0 };
    const by = (value: bigint): MidgardCekStagedOperandSource => ({
      kind: "smallInteger",
      value,
    });
    const result = presented(byteString(shifted(SHIFT, bytes, 12n)));
    expect(shiftVerifies(SHIFT, bytes, 12n, result)).toBe(true);
    expect(shiftVerifies(SHIFT, bytes, 11n, result, [input, by(12n)])).toBe(
      false,
    );
    expect(
      shiftVerifies(SHIFT, bytes, 12n, result, [input, by(12n), by(12n)]),
    ).toBe(false);
    const past = 2n ** 63n;
    expect(
      shiftVerifies(
        SHIFT,
        bytes,
        past,
        presented(byteString(Buffer.alloc(70_000))),
      ),
    ).toBe(false);
  });
});
