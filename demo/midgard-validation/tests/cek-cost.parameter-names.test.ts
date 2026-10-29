import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { computeMidgardCekBuiltinBudget } from "../src/cek-cost.js";
import PARAMETER_NAMES from "./fixtures/plutus-v3-cost-model-parameter-names.json" with { type: "json" };

/**
 * Every coefficient of the cost twin (`cek-cost.ts`), tags 0 to 86, against
 * the pinned cost model by parameter name. The fixture is the Plutus V3
 * parameter order of the pinned aiken (`ParamName::V3` in
 * `crates/uplc/src/machine/cost_model.rs` at `demo/flake.lock`'s rev), whose
 * names index `PLUTUS_V3_CANONICAL_COST_MODEL_VIEW`. Each builtin's cost
 * functions are written here over named coefficients and compared with the
 * twin across a grid of argument sizes; every builtin coefficient of tags
 * 0 to 86 must be read.
 */

type Sizes = readonly bigint[];
type Cost = (sizes: Sizes) => bigint;
type Shape = (prefix: string) => Cost;

const read = new Set<string>();
const coefficient = (name: string): bigint => {
  const index = PARAMETER_NAMES.indexOf(name);
  if (index < 0) throw new Error(`no cost-model parameter ${name}`);
  read.add(name);
  return BigInt(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW[index]!);
};

const max = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;
const min = (left: bigint, right: bigint): bigint =>
  left < right ? left : right;

const linear =
  (size: (sizes: Sizes) => bigint): Shape =>
  (prefix) => {
    const intercept = coefficient(`${prefix}_intercept`);
    const slope = coefficient(`${prefix}_slope`);
    return (sizes) => intercept + slope * size(sizes);
  };

const constant: Shape = (prefix) => {
  const value = coefficient(prefix);
  return () => value;
};
const linearInX = linear(([x]) => x!);
const linearInY = linear(([, y]) => y!);
const linearInZ = linear(([, , z]) => z!);
const addedSizes = linear(([x, y]) => x! + y!);
const multipliedSizes = linear(([x, y]) => x! * y!);
const minSize = linear(([x, y]) => min(x!, y!));
const maxSize = linear(([x, y]) => max(x!, y!));
const linearInMaxYZ = linear(([, y, z]) => max(y!, z!));
const subtractedSizes: Shape = (prefix) => {
  const minimum = coefficient(`${prefix}_minimum`);
  return linear(([x, y]) => max(minimum, x! - y!))(prefix);
};
const linearOnDiagonal: Shape = (prefix) => {
  const offDiagonal = coefficient(`${prefix}_constant`);
  const onDiagonal = linearInX(prefix);
  return (sizes) => (sizes[0] === sizes[1] ? onDiagonal(sizes) : offDiagonal);
};
const linearInYAndZ: Shape = (prefix) => {
  const intercept = coefficient(`${prefix}_intercept`);
  const slope1 = coefficient(`${prefix}_slope1`);
  const slope2 = coefficient(`${prefix}_slope2`);
  return ([, y, z]) => intercept + slope1 * y! + slope2 * z!;
};
const quadraticIn =
  (index: number): Shape =>
  (prefix) => {
    const [c0, c1, c2] = ["c0", "c1", "c2"].map((name) =>
      coefficient(`${prefix}_${name}`),
    );
    return (sizes) =>
      c0! + c1! * sizes[index]! + c2! * sizes[index]! * sizes[index]!;
  };
const literalInYOrLinearInZ: Shape = (prefix) => {
  const linearZ = linearInZ(prefix);
  return (sizes) => (sizes[1] === 0n ? linearZ(sizes) : sizes[1]!);
};
/**
 * The division cost: below the diagonal (x < y) the constant when
 * `constantBelowDiagonal`, otherwise the quadratic over (larger, smaller)
 * sizes, floored at the minimum.
 */
const division =
  (constantBelowDiagonal: boolean): Shape =>
  (prefix) => {
    const below = coefficient(`${prefix}_constant`);
    const model = (name: string): bigint =>
      coefficient(`${prefix}_model_arguments_${name}`);
    const [c00, c10, c01, c20, c11, c02, minimum] = [
      "c00",
      "c10",
      "c01",
      "c20",
      "c11",
      "c02",
      "minimum",
    ].map(model);
    return ([x, y]) => {
      if (x! < y! && constantBelowDiagonal) return below;
      const [a, b] = x! < y! ? [y!, x!] : [x!, y!];
      return max(
        minimum!,
        c00! + c10! * a + c01! * b + c20! * a * a + c11! * a * b + c02! * b * b,
      );
    };
  };

/** `[parameter-name prefix, arity, cpu shape, memory shape]` by tag. */
const BUILTINS: readonly (readonly [string, number, Shape, Shape])[] = [
  ["AddInteger", 2, maxSize, maxSize],
  ["SubtractInteger", 2, maxSize, maxSize],
  ["MultiplyInteger", 2, multipliedSizes, addedSizes],
  ["DivideInteger", 2, division(false), subtractedSizes],
  ["QuotientInteger", 2, division(true), subtractedSizes],
  ["RemainderInteger", 2, division(true), linearInY],
  ["ModInteger", 2, division(false), linearInY],
  ["EqualsInteger", 2, minSize, constant],
  ["LessThanInteger", 2, minSize, constant],
  ["LessThanEqualsInteger", 2, minSize, constant],
  ["AppendByteString", 2, addedSizes, addedSizes],
  ["ConsByteString", 2, linearInY, addedSizes],
  ["SliceByteString", 3, linearInZ, linearInZ],
  ["LengthOfByteString", 1, constant, constant],
  ["IndexByteString", 2, constant, constant],
  ["EqualsByteString", 2, linearOnDiagonal, constant],
  ["LessThanByteString", 2, minSize, constant],
  ["LessThanEqualsByteString", 2, minSize, constant],
  ["Sha2_256", 1, linearInX, constant],
  ["Sha3_256", 1, linearInX, constant],
  ["Blake2b_256", 1, linearInX, constant],
  ["VerifyEd25519Signature", 3, linearInY, constant],
  ["AppendString", 2, addedSizes, addedSizes],
  ["EqualsString", 2, linearOnDiagonal, constant],
  ["EncodeUtf8", 1, linearInX, linearInX],
  ["DecodeUtf8", 1, linearInX, linearInX],
  ["IfThenElse", 3, constant, constant],
  ["ChooseUnit", 2, constant, constant],
  ["Trace", 2, constant, constant],
  ["FstPair", 1, constant, constant],
  ["SndPair", 1, constant, constant],
  ["ChooseList", 3, constant, constant],
  ["MkCons", 2, constant, constant],
  ["HeadList", 1, constant, constant],
  ["TailList", 1, constant, constant],
  ["NullList", 1, constant, constant],
  ["ChooseData", 6, constant, constant],
  ["ConstrData", 2, constant, constant],
  ["MapData", 1, constant, constant],
  ["ListData", 1, constant, constant],
  ["IData", 1, constant, constant],
  ["BData", 1, constant, constant],
  ["UnConstrData", 1, constant, constant],
  ["UnMapData", 1, constant, constant],
  ["UnListData", 1, constant, constant],
  ["UnIData", 1, constant, constant],
  ["UnBData", 1, constant, constant],
  ["EqualsData", 2, minSize, constant],
  ["MkPairData", 2, constant, constant],
  ["MkNilData", 1, constant, constant],
  ["MkNilPairData", 1, constant, constant],
  ["SerialiseData", 1, linearInX, linearInX],
  ["VerifyEcdsaSecp256k1Signature", 3, constant, constant],
  ["VerifySchnorrSecp256k1Signature", 3, linearInY, constant],
  ["Bls12_381_G1_add", 2, constant, constant],
  ["Bls12_381_G1_neg", 1, constant, constant],
  ["Bls12_381_G1_scalarMul", 2, linearInX, constant],
  ["Bls12_381_G1_equal", 2, constant, constant],
  ["Bls12_381_G1_compress", 1, constant, constant],
  ["Bls12_381_G1_uncompress", 1, constant, constant],
  ["Bls12_381_G1_hashToGroup", 2, linearInX, constant],
  ["Bls12_381_G2_add", 2, constant, constant],
  ["Bls12_381_G2_neg", 1, constant, constant],
  ["Bls12_381_G2_scalarMul", 2, linearInX, constant],
  ["Bls12_381_G2_equal", 2, constant, constant],
  ["Bls12_381_G2_compress", 1, constant, constant],
  ["Bls12_381_G2_uncompress", 1, constant, constant],
  ["Bls12_381_G2_hashToGroup", 2, linearInX, constant],
  ["Bls12_381_millerLoop", 2, constant, constant],
  ["Bls12_381_mulMlResult", 2, constant, constant],
  ["Bls12_381_finalVerify", 2, constant, constant],
  ["Keccak_256", 1, linearInX, constant],
  ["Blake2b_224", 1, linearInX, constant],
  ["IntegerToByteString", 3, quadraticIn(2), literalInYOrLinearInZ],
  ["ByteStringToInteger", 2, quadraticIn(1), linearInY],
  ["AndByteString", 3, linearInYAndZ, linearInMaxYZ],
  ["OrByteString", 3, linearInYAndZ, linearInMaxYZ],
  ["XorByteString", 3, linearInYAndZ, linearInMaxYZ],
  ["ComplementByteString", 1, linearInX, linearInX],
  ["ReadBit", 2, constant, constant],
  ["WriteBits", 3, linearInY, linearInX],
  ["ReplicateByte", 2, linearInX, linearInX],
  ["ShiftByteString", 2, linearInX, linearInX],
  ["RotateByteString", 2, linearInX, linearInX],
  ["CountSetBits", 1, linearInX, constant],
  ["FindFirstSetBit", 1, linearInX, constant],
  ["Ripemd_160", 1, linearInX, constant],
];

const PROBES = [
  0n,
  1n,
  2n,
  3n,
  5n,
  8n,
  64n,
  65n,
  70n,
  1000n,
  1n << 20n,
] as const;

/**
 * Every size vector of the arity over the probes, up to three arguments;
 * above that, each probe as every argument's size, plus 1, 2, 3, ... in turn.
 */
const sizeGrid = (arity: number): readonly Sizes[] => {
  if (arity > 3) {
    return [
      ...PROBES.map((size) => Array.from({ length: arity }, () => size)),
      Array.from({ length: arity }, (_, index) => BigInt(index + 1)),
    ];
  }
  let grid: Sizes[] = [[]];
  for (let argument = 0; argument < arity; argument += 1) {
    grid = grid.flatMap((prefix) => PROBES.map((size) => [...prefix, size]));
  }
  return grid;
};

describe("cost twin coefficients by parameter name", () => {
  it("lists the 350 Plutus V3 parameters once each", () => {
    expect(PARAMETER_NAMES).toHaveLength(
      PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.length,
    );
    expect(new Set(PARAMETER_NAMES).size).toBe(PARAMETER_NAMES.length);
  });

  it("costs tags 0 to 86 with the pinned model's coefficients", () => {
    expect(BUILTINS).toHaveLength(87);
    const mismatches: string[] = [];
    BUILTINS.forEach(([prefix, arity, cpuShape, memoryShape], tag) => {
      const cpu = cpuShape(`${prefix}_cpu_arguments`);
      const memory = memoryShape(`${prefix}_memory_arguments`);
      for (const sizes of sizeGrid(arity)) {
        const twin = computeMidgardCekBuiltinBudget(tag, sizes);
        const expected = { cpu: cpu(sizes), memory: memory(sizes) };
        if (twin.cpu !== expected.cpu || twin.memory !== expected.memory) {
          mismatches.push(
            `${tag.toString()} ${prefix} at [${sizes.join(", ")}]: twin ${twin.cpu.toString()}/${twin.memory.toString()}, model ${expected.cpu.toString()}/${expected.memory.toString()}`,
          );
        }
      }
    });
    expect(mismatches.slice(0, 20)).toStrictEqual([]);
    const builtinParameters = PARAMETER_NAMES.filter((name) =>
      BUILTINS.some(([prefix]) => name.startsWith(`${prefix}_`)),
    );
    expect(builtinParameters.filter((name) => !read.has(name))).toStrictEqual(
      [],
    );
  });
});
