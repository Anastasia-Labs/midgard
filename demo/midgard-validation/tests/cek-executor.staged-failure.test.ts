import {
  midgardCekFlatRoot,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  builtinValueToDirectWitness,
  evaluateMidgardCekBuiltinOnValues,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-direct-builtin.js";
import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import {
  midgardCekStagedFailure,
  type MidgardCekStagedOperand,
} from "../src/cek-executor.staged-failure.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { errorSuccessor } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { verifyMidgardCekStagedBuiltinFailure } from "../src/cek-staged-builtin.failure.js";
import type { MidgardCekStagedOperandSource } from "../src/cek-staged-builtin.material.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { DataB } from "../src/uplc-data.js";
import type { UPLCTerm } from "../src/uplc-term.js";
import {
  builtinState,
  integerOperand,
  presented,
} from "./helpers/cek-staged-failure-steps.js";
import { bytes, call, int, run } from "./helpers/cek-staged-route-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * A staged step's failure on an argument's bounds or content is proved by
 * the staged failure step (arm 36), checked at the step level, including the
 * integer cases a script cannot reach without a staged product step: an
 * argument of 4,097 words or more, and the 4,096-word band around the
 * [-2^262143, 2^262143 - 1] range. The PV11 evaluator decides the same range
 * on whole scripts in cek-executor.staged-route-failures.test.ts. Every
 * tampered failure step is refused.
 */

const ADD = 0n;
const SUBTRACT = 1n;
const MULTIPLY = 2n;
const DIVIDE = 3n;
const EQUALS_INTEGER = 7n;
const LESS_THAN = 8n;
const LESS_THAN_EQUALS = 9n;
const CONS = 11n;
const INDEX = 14n;
const DECODE_UTF8 = 25n;
const SHA2_256 = 18n;

const TOP = 2n ** 262_143n;
const MAX = TOP - 1n;
const MIN = -TOP;
const BOTTOM = MIN - 1n;
/** The least magnitude of 4,097 words. */
const PAST_BAND = 2n ** 262_144n;

const bytesOperand = (length: number): MidgardCekStagedOperand => {
  const value = Buffer.alloc(length, 0x61);
  return {
    type: "bytes",
    bytes: value,
    value: UPLCConst.byteString(value),
    leaf: {
      kind: "bytesLeaf",
      root: midgardCekFlatRoot(value),
      length: BigInt(length),
    },
  };
};

type Step = {
  readonly pre: MidgardCekMachineState;
  readonly post: MidgardCekMachineState;
  readonly witness: MidgardCekCoreStepWitness;
};

/** The failure step of builtin `tag` over these operands, or null. */
const failureStep = (
  tag: bigint,
  operands: readonly MidgardCekStagedOperand[],
): Step | null => {
  const arguments_ = operands.map(presented);
  const failure = midgardCekStagedFailure(
    tag,
    arguments_,
    operands,
    () =>
      evaluateMidgardCekBuiltinOnValues(
        Number(tag),
        operands.map((operand) => operand.value),
      ) === "failure",
  );
  if (failure === null) return null;
  const pre = builtinState(tag, arguments_);
  return {
    pre,
    post: errorSuccessor(
      pre,
      MidgardCekErrorCodes.BuiltinFailure,
      failure.budget,
    ),
    witness: failure.witness,
  };
};

const verifies = ({ pre, post, witness }: Step): boolean =>
  verifyMidgardCekCoreStep(pre, post, witness);

const sources = (step: Step): readonly MidgardCekStagedOperandSource[] =>
  step.witness.kind === "executeBuiltinStagedFailure"
    ? step.witness.material.sources
    : [];

const withSources = (
  step: Step,
  replaced: readonly MidgardCekStagedOperandSource[],
  stagedOperands?: readonly Buffer[],
): Step => {
  const witness = step.witness;
  if (witness.kind !== "executeBuiltinStagedFailure") throw new Error("kind");
  return {
    ...step,
    witness: {
      ...witness,
      material: { ...witness.material, sources: replaced },
      stagedOperands: stagedOperands ?? witness.stagedOperands,
    },
  };
};

const BOUNDED_INTEGER_TAGS = [ADD, SUBTRACT, LESS_THAN, LESS_THAN_EQUALS];

describe("staged failure steps of the integer builtins", () => {
  it.each(BOUNDED_INTEGER_TAGS)(
    "proves builtin %s fails on an argument of 4,097 words",
    (tag) => {
      const step = failureStep(tag, [
        integerOperand(1n),
        integerOperand(PAST_BAND),
      ])!;
      expect(step.witness.kind).toBe("executeBuiltinStagedFailure");
      expect(sources(step)).toEqual([]);
      expect(verifies(step)).toBe(true);
    },
  );

  it.each(
    BOUNDED_INTEGER_TAGS.flatMap((tag) => [
      [tag, TOP],
      [tag, BOTTOM],
    ]),
  )("proves builtin %s fails on %s, revealed by its CBOR", (tag, value) => {
    const step = failureStep(tag, [integerOperand(value), integerOperand(7n)])!;
    expect(sources(step).map((source) => source.kind)).toEqual([
      "staged",
      "integerLeaf",
    ]);
    expect(verifies(step)).toBe(true);
    // Unrevealed, the argument's band holds values in range too.
    expect(
      verifies(
        withSources(
          step,
          [integerOperand(value).leaf, integerOperand(7n).leaf],
          [],
        ),
      ),
    ).toBe(false);
  });

  it.each(
    BOUNDED_INTEGER_TAGS.flatMap((tag) => [
      [tag, MAX],
      [tag, MIN],
    ]),
  )("finds no failure of builtin %s on %s and refuses one", (tag, value) => {
    const operands = [integerOperand(7n), integerOperand(value)];
    expect(failureStep(tag, operands)).toBeNull();
    const pre = builtinState(tag, operands.map(presented));
    expect(
      verifies({
        pre,
        post: errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure),
        witness: {
          kind: "executeBuiltinStagedFailure",
          tag,
          arguments: operands
            .map(presented)
            .map(semanticizeMidgardCekDirectConstant),
          material: {
            runs: { refs: [], operands: [] },
            sources: [operands[0]!.leaf, { kind: "staged", operand: 0 }],
          },
          stagedOperands: [operands[1]!.bytes],
        },
      }),
    ).toBe(false);
  });

  it("finds no failure of equalsInteger on 4,097 words", () => {
    expect(
      failureStep(EQUALS_INTEGER, [
        integerOperand(PAST_BAND),
        integerOperand(PAST_BAND),
      ]),
    ).toBeNull();
  });

  it.each([MULTIPLY, DIVIDE])(
    "accepts builtin %s's failure on 4,097 words at the failure hop",
    (tag) => {
      const arguments_ = [integerOperand(PAST_BAND), integerOperand(3n)].map(
        presented,
      );
      const material = { runs: { refs: [], operands: [] }, sources: [] };
      expect(
        verifyMidgardCekStagedBuiltinFailure(tag, arguments_, material, []),
      ).toBe(true);
      expect(
        verifyMidgardCekStagedBuiltinFailure(
          EQUALS_INTEGER,
          arguments_,
          material,
          [],
        ),
      ).toBe(false);
    },
  );

  it("proves consByteString fails on a 10,000-byte integer", () => {
    const step = failureStep(CONS, [
      integerOperand(2n ** 80_000n),
      bytesOperand(100),
    ])!;
    expect(sources(step)).toEqual([]);
    expect(verifies(step)).toBe(true);
  });

  it("refuses a failure step that charges the builtin", () => {
    const step = failureStep(ADD, [
      integerOperand(PAST_BAND),
      integerOperand(1n),
    ])!;
    expect(
      verifies({ ...step, post: { ...step.post, cpu: step.post.cpu + 1n } }),
    ).toBe(false);
  });

  it("refuses an argument revealed rather than compacted", () => {
    const step = failureStep(INDEX, [
      bytesOperand(20_000),
      integerOperand(-1n),
    ])!;
    expect(verifies(step)).toBe(true);
    const witness = step.witness;
    if (witness.kind !== "executeBuiltinStagedFailure") throw new Error("kind");
    const revealed = [
      witness.arguments[0]!,
      builtinValueToDirectWitness(UPLCConst.int(-1n)),
    ];
    expect(
      verifies({ ...step, witness: { ...witness, arguments: revealed } }),
    ).toBe(false);
  });
});

/** The failure step that ends the proof-mode run of `body`. */
const lastStep = (body: UPLCTerm): MidgardCekExecutionStep => {
  const outcome = run(body, "proof");
  expect(outcome.result.kind).toBe("script_invalid");
  return outcome.execution!.steps.at(-1)!;
};

const text = (length: number): UPLCTerm =>
  UPLCConst.byteString(new DataB(Buffer.alloc(length, 0x61)).bytes);

/** decodeUtf8 of 10,000 bytes of text with `tail` appended. */
const decodeWithTail = (...tail: readonly number[]): UPLCTerm =>
  call(
    builtin("decodeUtf8"),
    call(
      builtin("appendByteString"),
      call(builtin("appendByteString"), text(5_000), text(5_000)),
      UPLCConst.byteString(new DataB(Uint8Array.from(tail)).bytes),
    ),
  );

describe("tampered staged failure steps", () => {
  it("refuses decodeUtf8's offset moved off the invalid character", () => {
    const step = lastStep(decodeWithTail(0x41, 0xff));
    expect(step.witness.kind).toBe("executeBuiltinStagedFailure");
    expect(verifies(step)).toBe(true);
    const [source, offset] = sources(step);
    expect(offset).toEqual({ kind: "smallInteger", value: 10_001n });
    for (const moved of [10_000n, 10_002n, -1n]) {
      expect(
        verifies(
          withSources(step, [source!, { kind: "smallInteger", value: moved }]),
        ),
      ).toBe(false);
    }
  });

  it("refuses decodeUtf8's offset at or inside a valid character", () => {
    const step = lastStep(decodeWithTail(0xc3, 0xa9, 0xe2, 0x82, 0xac, 0xff));
    expect(verifies(step)).toBe(true);
    const [source, offset] = sources(step);
    expect(offset).toEqual({ kind: "smallInteger", value: 10_005n });
    for (const moved of [10_000n, 10_001n, 10_002n, 10_003n]) {
      expect(
        verifies(
          withSources(step, [source!, { kind: "smallInteger", value: moved }]),
        ),
      ).toBe(false);
    }
  });

  it("refuses decodeUtf8's proof for another builtin over the same bytes", () => {
    const step = lastStep(decodeWithTail(0xff));
    expect(verifies(step)).toBe(true);
    const witness = step.witness;
    if (witness.kind !== "executeBuiltinStagedFailure") throw new Error("kind");
    const pre = builtinState(SHA2_256, witness.arguments);
    expect(
      verifies({
        pre,
        post: errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure),
        witness: { ...witness, tag: SHA2_256 },
      }),
    ).toBe(false);
  });

  it("refuses decodeUtf8's operand with another byte", () => {
    const step = lastStep(decodeWithTail(0xff));
    const [source, offset] = sources(step);
    expect(source?.kind).toBe("staged");
    const operand = Buffer.from(
      (step.witness as { stagedOperands: readonly Uint8Array[] })
        .stagedOperands[0]!,
    );
    operand[operand.length - 1] = 0x41;
    expect(verifies(withSources(step, [source!, offset!], [operand]))).toBe(
      false,
    );
  });

  it("refuses indexByteString's failure at another index or length", () => {
    const step = lastStep(
      call(
        builtin("indexByteString"),
        call(builtin("appendByteString"), bytes(8_000), bytes(8_000, 1)),
        int(16_000n),
      ),
    );
    expect(verifies(step)).toBe(true);
    const [source, index] = sources(step);
    expect(source?.kind).toBe("bytesLeaf");
    expect(index).toEqual({ kind: "smallInteger", value: 16_000n });
    for (const moved of [15_999n, 16_001n]) {
      expect(
        verifies(
          withSources(step, [source!, { kind: "smallInteger", value: moved }]),
        ),
      ).toBe(false);
    }
    if (source?.kind !== "bytesLeaf") throw new Error("kind");
    expect(
      verifies(withSources(step, [{ ...source, length: 16_001n }, index!])),
    ).toBe(false);
  });

  it("refuses a focus with the wrong argument count", () => {
    const step = failureStep(SHA2_256, [bytesOperand(65_537)])!;
    expect(verifies(step)).toBe(true);
    const witness = step.witness;
    if (witness.kind !== "executeBuiltinStagedFailure") throw new Error("kind");
    const arguments_ = [...witness.arguments, presented(bytesOperand(1))];
    const pre = builtinState(SHA2_256, arguments_);
    expect(
      verifies({
        pre,
        post: errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure),
        witness: { ...witness, arguments: arguments_ },
      }),
    ).toBe(false);
  });

  it("refuses a 65,536-byte argument as past its bound", () => {
    const step = failureStep(DECODE_UTF8, [bytesOperand(65_537)])!;
    expect(verifies(step)).toBe(true);
    const atBound = bytesOperand(65_536);
    const witness = step.witness;
    if (witness.kind !== "executeBuiltinStagedFailure") throw new Error("kind");
    expect(
      verifies({
        pre: builtinState(DECODE_UTF8, [presented(atBound)]),
        post: errorSuccessor(
          builtinState(DECODE_UTF8, [presented(atBound)]),
          MidgardCekErrorCodes.BuiltinFailure,
        ),
        witness: { ...witness, arguments: [presented(atBound)] },
      }),
    ).toBe(false);
  });
});
