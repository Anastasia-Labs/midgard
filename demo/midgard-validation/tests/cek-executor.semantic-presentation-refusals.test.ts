import { hashMidgardCekValueNode } from "@al-ft/midgard-core";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  hashMidgardCekDirectArguments,
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import { encodeMidgardCekCanonicalConstant } from "../src/cek-constant.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { pairDataSummary } from "../src/cek-machine.verify-semantic-builtin-control.js";
import {
  semanticStep,
  type SemanticStepWitness,
  SHAPES,
} from "./cek-executor.semantic-presentation.shapes.js";

// Each value a semantic step presents by summary is bound: moving its memory
// or its summary memory by one word, in the witness alone or in the
// pre-state's builtin value too, is refused, and so is the witness of
// another source.

/**
 * Whether the step is refused when presented as `present` builds it. A value
 * the encoding cannot hold (a negative memory) is refused as it is built.
 */
const refuses = (
  step: ReturnType<typeof semanticStep>,
  present: () => {
    readonly witness: SemanticStepWitness;
    readonly post?: typeof step.post;
  },
): boolean => {
  try {
    const { witness, post = step.post } = present();
    return !verifyMidgardCekCoreStep(step.pre, post, witness);
  } catch {
    return true;
  }
};

/** A semantic value with its memory and its summary memory moved. */
const shifted = (
  value: MidgardCekDirectValueWitness,
  memory: bigint,
  summaryMemory: bigint,
): MidgardCekDirectValueWitness => {
  if (value.kind !== "semanticConstant") {
    throw new Error("expected a semantic constant");
  }
  return {
    kind: "semanticConstant",
    witness: {
      ...value.witness,
      payload: {
        ...value.witness.payload,
        memory: value.witness.payload.memory + summaryMemory,
      },
      memory: value.witness.memory + memory,
    },
  };
};

/**
 * The step with argument `index`'s memory moved, the pre-state's builtin value
 * re-derived from the moved arguments, and a success's post-state charged
 * their budget. Only the arm's own memory binding can refuse it.
 */
const withBoundArgument = (
  step: ReturnType<typeof semanticStep>,
  index: number,
  memory: bigint,
) => {
  const arguments_ = step.witness.arguments.map((argument, at) =>
    at === index ? shifted(argument, memory, 0n) : argument,
  );
  const { root, count } = hashMidgardCekDirectArguments(arguments_);
  const pre = {
    ...step.pre,
    focusRoot: hashMidgardCekValueNode({
      kind: "builtin",
      tag: step.witness.tag,
      forcesRemaining: 0n,
      argumentsCount: count,
      argumentsRoot: root,
    }),
  };
  const budget = midgardCekDirectBuiltinBudget(step.witness.tag, arguments_);
  return {
    pre,
    post:
      step.witness.kind === "executeBuiltinSemantic"
        ? {
            ...step.post,
            cpu: pre.cpu + budget.cpu,
            memory: pre.memory + budget.memory,
          }
        : step.post,
    witness: { ...step.witness, arguments: arguments_ },
  };
};

const SHIFTS = [
  { field: "memory", memory: 1n, summaryMemory: 0n },
  { field: "memory", memory: -1n, summaryMemory: 0n },
  { field: "summary memory", memory: 0n, summaryMemory: 1n },
  { field: "summary memory", memory: 0n, summaryMemory: -1n },
] as const;

const presentedArguments = SHAPES.flatMap((shape) =>
  shape.arguments.flatMap((kind, index) =>
    kind === "semanticConstant"
      ? SHIFTS.map((shift) => ({
          ...shift,
          shape,
          index,
          by: Number(shift.memory + shift.summaryMemory),
        }))
      : [],
  ),
);

const presentedResults = SHAPES.flatMap((shape) =>
  shape.result === "semanticConstant"
    ? SHIFTS.map((shift) => ({
        ...shift,
        shape,
        by: Number(shift.memory + shift.summaryMemory),
      }))
    : [],
);

describe("semantic builtin steps refuse a moved presented value", () => {
  it.each(presentedArguments)(
    "$shape.name refuses argument $index with its $field off by $by",
    ({ shape, index, memory, summaryMemory }) => {
      const step = semanticStep(shape.term());
      expect(step.witness.arguments[index]?.kind).toBe("semanticConstant");
      expect(
        refuses(step, () => ({
          witness: {
            ...step.witness,
            arguments: step.witness.arguments.map((argument, at) =>
              at === index
                ? shifted(argument, memory, summaryMemory)
                : argument,
            ),
          },
        })),
      ).toBe(true);
    },
  );

  it.each(
    presentedArguments.filter(({ summaryMemory }) => summaryMemory === 0n),
  )(
    "$shape.name refuses argument $index with its memory off by $by in the pre-state too",
    ({ shape, index, memory }) => {
      const step = semanticStep(shape.term());
      expect(step.witness.arguments[index]?.kind).toBe("semanticConstant");
      let accepted: boolean;
      try {
        const moved = withBoundArgument(step, index, memory);
        accepted = verifyMidgardCekCoreStep(
          moved.pre,
          moved.post,
          moved.witness,
        );
      } catch {
        accepted = false;
      }
      expect(accepted).toBe(false);
    },
  );

  it("headList refuses the witness of another source with the same head", () => {
    const shape = (label: string) =>
      semanticStep(SHAPES.find(({ pinned }) => pinned === label)!.term());
    const step = shape("head_list_data");
    const other = shape("head_list_data_same_head");
    // The same head and budget give the same post-state, so only the binding
    // of the arguments to the pre-state's builtin value refuses it.
    expect(other.post).toEqual(step.post);
    expect(other.allVerify).toBe(true);
    expect(refuses(step, () => ({ witness: other.witness }))).toBe(true);
  });

  it("equalsData of equal values refuses False from a moved summary memory", () => {
    const step = semanticStep(
      SHAPES.find(({ pinned }) => pinned === "equals_data_true")!.term(),
    );
    if (step.witness.kind !== "executeBuiltinSemantic") {
      throw new Error("expected a semantic success");
    }
    const witness = step.witness;
    expect(
      refuses(step, () => {
        const { typeCbor, payloadCbor } = encodeMidgardCekCanonicalConstant(
          UPLCConst.bool(false),
        );
        const result: MidgardCekDirectValueWitness = {
          kind: "constant",
          witness: { typeCbor, payloadCbor },
        };
        return {
          witness: {
            ...witness,
            arguments: [
              shifted(witness.arguments[0]!, 0n, 1n),
              witness.arguments[1]!,
            ],
            result,
          },
          post: {
            ...step.post,
            focusRoot: hashMidgardCekDirectValueWitness(result),
          },
        };
      }),
    ).toBe(true);
  });

  it("mkPairData refuses a pair built from a moved summary memory", () => {
    const step = semanticStep(
      SHAPES.find(({ pinned }) => pinned === "mk_pair_data")!.term(),
    );
    if (step.witness.kind !== "executeBuiltinSemantic") {
      throw new Error("expected a semantic success");
    }
    const witness = step.witness;
    const first = shifted(witness.arguments[0]!, 0n, 1n);
    const second = witness.arguments[1]!;
    if (
      first.kind !== "semanticConstant" ||
      second.kind !== "semanticConstant" ||
      witness.result.kind !== "semanticConstant"
    ) {
      throw new Error("expected presented values");
    }
    const result: MidgardCekDirectValueWitness = {
      kind: "semanticConstant",
      witness: {
        ...witness.result.witness,
        payload: pairDataSummary(first.witness.payload, second.witness.payload),
      },
    };
    expect(
      refuses(step, () => ({
        witness: { ...witness, arguments: [first, second], result },
        post: {
          ...step.post,
          focusRoot: hashMidgardCekDirectValueWitness(result),
        },
      })),
    ).toBe(true);
  });

  it.each(presentedResults)(
    "$shape.name refuses a result with its $field off by $by",
    ({ shape, memory, summaryMemory }) => {
      const step = semanticStep(shape.term());
      if (step.witness.kind !== "executeBuiltinSemantic") {
        throw new Error("expected a semantic success");
      }
      const witness = step.witness;
      expect(witness.result.kind).toBe("semanticConstant");
      expect(
        refuses(step, () => {
          const result = shifted(witness.result, memory, summaryMemory);
          return {
            witness: { ...witness, result },
            post: {
              ...step.post,
              focusRoot: hashMidgardCekDirectValueWitness(result),
            },
          };
        }),
      ).toBe(true);
    },
  );
});
