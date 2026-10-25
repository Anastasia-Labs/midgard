import { describe, expect, it } from "vitest";

import {
  hashMidgardCekDirectValueWitness,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES } from "../src/index.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { DataB, DataI } from "../src/uplc-data.js";
import type { UPLCTerm } from "../src/uplc-term.js";
import {
  apply,
  data,
  semanticStep,
} from "./cek-executor.semantic-presentation.shapes.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * iData, bData, unIData and unBData of a leaf past the direct payload bound.
 * The leaf and the integer or byte string it holds share one payload summary,
 * so the step is proved over the leaf node alone, at any size: the executor's
 * step is accepted, and a result of another value of the same size, a result
 * one word off, a revealed scalar or another leaf node is refused. The runs
 * through the step are in cek-executor.data-leaf-past-the-bound-runs.test.ts.
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

type Case = {
  readonly name: string;
  readonly tag: bigint;
  /** The step's term over the value `salt` picks. */
  readonly term: (salt: number) => UPLCTerm;
  readonly pastTheBound: boolean;
};

const CASES: readonly Case[] = [
  {
    name: "unBData of a 12,000-byte leaf",
    tag: 46n,
    term: (salt) =>
      apply(builtin("unBData"), data(new DataB(pattern(LEAF_BYTES, salt)))),
    pastTheBound: true,
  },
  {
    name: "unIData of a positive leaf past the bound",
    tag: 45n,
    term: (salt) =>
      apply(builtin("unIData"), data(new DataI(longInteger(BigInt(salt))))),
    pastTheBound: true,
  },
  {
    name: "unIData of a negative leaf past the bound",
    tag: 45n,
    term: (salt) =>
      apply(builtin("unIData"), data(new DataI(-longInteger(BigInt(salt))))),
    pastTheBound: true,
  },
  {
    name: "iData of an integer past the bound",
    tag: 40n,
    term: (salt) =>
      apply(builtin("iData"), intConstant(longInteger(BigInt(salt)))),
    pastTheBound: true,
  },
  {
    name: "bData of a 12,000-byte string",
    tag: 41n,
    term: (salt) =>
      apply(builtin("bData"), bytesConstant(pattern(LEAF_BYTES, salt))),
    pastTheBound: true,
  },
  {
    name: "unBData of a 64-byte leaf",
    tag: 46n,
    term: (salt) =>
      apply(builtin("unBData"), data(new DataB(pattern(64, salt)))),
    pastTheBound: false,
  },
  {
    name: "unIData of a 9-byte integer leaf",
    tag: 45n,
    term: (salt) =>
      apply(builtin("unIData"), data(new DataI(2n ** 64n + BigInt(salt)))),
    pastTheBound: false,
  },
  {
    name: "iData of a small integer",
    tag: 40n,
    term: (salt) => apply(builtin("iData"), intConstant(BigInt(5 + salt))),
    pastTheBound: false,
  },
  {
    name: "bData of a 64-byte string",
    tag: 41n,
    term: (salt) => apply(builtin("bData"), bytesConstant(pattern(64, salt))),
    pastTheBound: false,
  },
];

type Step = ReturnType<typeof semanticStep>;

/** The step's leaf summary: the Data side's payload. */
const leafPayload = (step: Step) => {
  const witness = step.witness;
  if (witness.kind !== "executeBuiltinSemantic") throw new Error("failed");
  const dataSide =
    witness.tag === 40n || witness.tag === 41n
      ? witness.result
      : witness.arguments[0]!;
  if (dataSide.kind !== "semanticConstant")
    throw new Error("expected the Data value by its summary");
  return dataSide.witness.payload;
};

const semanticResult = (step: Step): MidgardCekDirectValueWitness => {
  if (step.witness.kind !== "executeBuiltinSemantic") throw new Error("failed");
  return step.witness.result;
};

/** The result with its memory moved by `by` words. */
const resultMovedBy = (
  result: MidgardCekDirectValueWitness,
  by: bigint,
): MidgardCekDirectValueWitness => {
  if (result.kind !== "semanticConstant")
    throw new Error("expected a semantic result");
  return {
    kind: "semanticConstant",
    witness: { ...result.witness, memory: result.witness.memory + by },
  };
};

/** Whether the step is refused with `result` and the post state it names. */
const refusesResult = (
  step: Step,
  result: MidgardCekDirectValueWitness,
): boolean => {
  try {
    return !verifyMidgardCekCoreStep(
      step.pre,
      { ...step.post, focusRoot: hashMidgardCekDirectValueWitness(result) },
      { ...step.witness, result } as never,
    );
  } catch {
    return true;
  }
};

describe.each(CASES)("CEK $name", ({ tag, term, pastTheBound }) => {
  let built: { readonly step: Step; readonly other: Step } | undefined;
  const steps = () =>
    (built ??= { step: semanticStep(term(0)), other: semanticStep(term(1)) });

  it("is presented over the leaf node alone", () => {
    const { step } = steps();
    expect(step.witness.kind).toBe("executeBuiltinSemantic");
    if (step.witness.kind !== "executeBuiltinSemantic") return;
    expect(step.witness.tag).toBe(tag);
    expect(step.witness.material.dataNodes).toHaveLength(1);
    expect(step.witness.material.listNodes).toEqual([]);
    expect(step.witness.material.pairNodes).toEqual([]);
    expect(step.witness.material.scalarPreimages).toEqual([]);
    expect(
      leafPayload(step).cborLength >
        BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES),
    ).toBe(pastTheBound);
  });

  it("is accepted as the executor records it, with every step", () => {
    const { step } = steps();
    expect(step.allVerify).toBe(true);
    expect(verifyMidgardCekCoreStep(step.pre, step.post, step.witness)).toBe(
      true,
    );
  });

  it("is refused with the result of another value of its size", () => {
    const { step, other } = steps();
    const otherResult = semanticResult(other);
    expect(
      Buffer.from(hashMidgardCekDirectValueWitness(otherResult)).equals(
        Buffer.from(step.post.focusRoot),
      ),
    ).toBe(false);
    expect(refusesResult(step, otherResult)).toBe(true);
  });

  it("is refused when the post state names another result", () => {
    const { step, other } = steps();
    expect(
      verifyMidgardCekCoreStep(
        step.pre,
        {
          ...step.post,
          focusRoot: hashMidgardCekDirectValueWitness(semanticResult(other)),
        },
        step.witness,
      ),
    ).toBe(false);
  });

  it.each([1n, -1n])("is refused with a result %s words off", (by) => {
    const { step } = steps();
    const result = semanticResult(step);
    // A scalar result of a small step is revealed, not summarised.
    if (result.kind !== "semanticConstant") return;
    expect(refusesResult(step, resultMovedBy(result, by))).toBe(true);
  });

  it("is refused with a revealed scalar", () => {
    const { step } = steps();
    if (step.witness.kind !== "executeBuiltinSemantic") throw new Error();
    expect(
      verifyMidgardCekCoreStep(step.pre, step.post, {
        ...step.witness,
        material: {
          ...step.witness.material,
          scalarPreimages: [new Uint8Array([0x41, 0x00])],
        },
      }),
    ).toBe(false);
  });

  it("is refused with another leaf node", () => {
    const { step, other } = steps();
    if (
      step.witness.kind !== "executeBuiltinSemantic" ||
      other.witness.kind !== "executeBuiltinSemantic"
    )
      throw new Error();
    expect(
      verifyMidgardCekCoreStep(step.pre, step.post, {
        ...step.witness,
        material: other.witness.material,
      }),
    ).toBe(false);
  });
});
