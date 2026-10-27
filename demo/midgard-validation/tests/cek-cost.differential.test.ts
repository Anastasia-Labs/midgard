import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import { toCostModelV3 } from "@harmoniclabs/cardano-costmodels-ts";
import {
  type BuiltinCostsOf,
  costModelV3ToBuiltinCosts,
  Linear3InY,
  PartialBuiltin,
} from "@harmoniclabs/plutus-machine";
import { UPLCBuiltinTag } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { harmonicBuiltinTag } from "../src/cek-builtin.cardano-tags.js";
import { computeMidgardCekBuiltinBudget } from "../src/cek-cost.js";

/**
 * The cost twin against the costing it replaced: HarmonicLabs' Plutus V3
 * cost functions over the pinned cost model, with the three corrections the
 * package applied to them (variant E division, multiplyInteger over the
 * product of the sizes, verifyEd25519Signature linear in the message).
 */
const pinnedCostModel = toCostModelV3([
  ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
] as unknown as Parameters<typeof toCostModelV3>[0]);
const referenceBuiltinCosts = costModelV3ToBuiltinCosts(pinnedCostModel);
const coefficient = (name: string): bigint =>
  BigInt((pinnedCostModel as unknown as Record<string, number>)[name]!);

const divisionCpu = (name: string, symmetric: boolean) => {
  const c = (suffix: string): bigint =>
    coefficient(`${name}-cpu-arguments-${suffix}`);
  return {
    at: (x: bigint, y: bigint): bigint => {
      if (x < y && !symmetric) return c("constant");
      const [a, b] = x < y ? [y, x] : [x, y];
      const real =
        c("model-arguments-c00") +
        c("model-arguments-c10") * a +
        c("model-arguments-c01") * b +
        c("model-arguments-c20") * a * a +
        c("model-arguments-c11") * a * b +
        c("model-arguments-c02") * b * b;
      const minimum = c("model-arguments-minimum");
      return real < minimum ? minimum : real;
    },
  };
};

const CORRECTED_CPU = new Map<UPLCBuiltinTag, unknown>([
  [UPLCBuiltinTag.divideInteger, divisionCpu("divideInteger", true)],
  [UPLCBuiltinTag.quotientInteger, divisionCpu("quotientInteger", false)],
  [UPLCBuiltinTag.remainderInteger, divisionCpu("remainderInteger", false)],
  [UPLCBuiltinTag.modInteger, divisionCpu("modInteger", true)],
  [
    UPLCBuiltinTag.multiplyInteger,
    {
      at: (x: bigint, y: bigint): bigint =>
        coefficient("multiplyInteger-cpu-arguments-intercept") +
        coefficient("multiplyInteger-cpu-arguments-slope") * x * y,
    },
  ],
  [
    UPLCBuiltinTag.verifyEd25519Signature,
    new Linear3InY(
      coefficient("verifyEd25519Signature-cpu-arguments-intercept"),
      coefficient("verifyEd25519Signature-cpu-arguments-slope"),
    ),
  ],
]);

type Outcome =
  | { readonly cpu: bigint; readonly memory: bigint }
  | { readonly error: string };

const replacedBudget = (tag: number, sizes: readonly bigint[]): Outcome => {
  if (!Number.isInteger(tag) || tag < 0 || tag > 86)
    return { error: "V1 builtin tag is outside Plutus V3" };
  const builtinTag = harmonicBuiltinTag(tag);
  const arity = PartialBuiltin.getNRequiredArgsFor(builtinTag);
  if (sizes.length !== arity)
    return {
      error: `V1 builtin ${tag.toString(10)} requires ${arity.toString(10)} cost sizes`,
    };
  if (sizes.some((size) => size < 0n))
    return { error: "V1 builtin cost size must be non-negative" };
  const costs = referenceBuiltinCosts(builtinTag) as BuiltinCostsOf<
    typeof builtinTag
  >;
  const cpu = (CORRECTED_CPU.get(builtinTag) ?? costs.cpu) as {
    at: (...sizes: bigint[]) => bigint;
  };
  const memory = costs.mem as { at: (...sizes: bigint[]) => bigint };
  return { cpu: cpu.at(...sizes), memory: memory.at(...sizes) };
};

const twinBudget = (tag: number, sizes: readonly bigint[]): Outcome => {
  try {
    const { cpu, memory } = computeMidgardCekBuiltinBudget(tag, sizes);
    return { cpu, memory };
  } catch (error) {
    return { error: (error as Error).message };
  }
};

const GRID = [0n, 1n, 2n, 3n, 4n, 7n, 8n, 32n, 64n, 65n, 1_000n, 1n << 20n];

/**
 * Every `arity`-tuple of grid sizes; for six arguments, each rotation of the
 * grid and all zeros.
 */
const sizeTuples = (arity: number): bigint[][] => {
  if (arity === 6)
    return [
      ...GRID.map((_, index) =>
        Array.from(
          { length: 6 },
          (_, position) => GRID[(index + position) % GRID.length]!,
        ),
      ),
      Array.from({ length: 6 }, () => 0n),
    ];
  let tuples: bigint[][] = [[]];
  for (let position = 0; position < arity; position += 1)
    tuples = tuples.flatMap((tuple) => GRID.map((size) => [...tuple, size]));
  return tuples;
};

const ARITIES = [1, 2, 3, 6];

describe("the builtin cost twin charges what the replaced costing charged", () => {
  it("for tags 0 to 86 over a size grid of every arity", () => {
    let compared = 0;
    const differing: string[] = [];
    for (let tag = 0; tag <= 86; tag += 1)
      for (const arity of ARITIES)
        for (const sizes of sizeTuples(arity)) {
          compared += 1;
          const replaced = replacedBudget(tag, sizes);
          const twin = twinBudget(tag, sizes);
          if (
            JSON.stringify(replaced, bigints) !== JSON.stringify(twin, bigints)
          )
            differing.push(`${tag}(${sizes.join(",")})`);
        }
    expect(differing).toEqual([]);
    expect(compared).toBeGreaterThan(100_000);
  });

  it("for refused tags, sizes and argument counts", () => {
    const cases: [number, bigint[]][] = [
      [-1, [1n]],
      [101, [1n]],
      [1.5, [1n, 1n]],
      [0, [1n]],
      [0, [-1n, 1n]],
      [18, [-1n]],
      [36, [0n, 0n, 0n, 0n, 0n]],
      [86, []],
    ];
    for (const [tag, sizes] of cases)
      expect(twinBudget(tag, sizes)).toEqual(replacedBudget(tag, sizes));
  });
});

function bigints(_key: string, value: unknown): unknown {
  return typeof value === "bigint" ? value.toString(10) : value;
}
