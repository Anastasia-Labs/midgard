import {
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH,
} from "@al-ft/midgard-core";
import { toCostModelV3 } from "@harmoniclabs/cardano-costmodels-ts";
import {
  type BuiltinCostsOf,
  costModelV3ToBuiltinCosts,
  Linear3InY,
  PartialBuiltin,
} from "@harmoniclabs/plutus-machine";
import { UPLCBuiltinTag } from "@harmoniclabs/uplc";

export type MidgardCekBuiltinBudget = {
  readonly cpu: bigint;
  readonly memory: bigint;
};

const MIN_BUILTIN_TAG = 0;
const MAX_BUILTIN_TAG = 86;

if (
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.length !==
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH
) {
  throw new Error("V1 pinned Plutus V3 cost model must have 350 entries");
}
const pinnedCostModel = toCostModelV3([
  ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
] as unknown as Parameters<typeof toCostModelV3>[0]);
const referenceBuiltinCosts = costModelV3ToBuiltinCosts(pinnedCostModel);

type DivisionName =
  | "divideInteger"
  | "quotientInteger"
  | "remainderInteger"
  | "modInteger";

/**
 * Plutus V3 (variant E) division-family cpu: a quadratic in both sizes with a
 * minimum. divideInteger and modInteger apply it to (larger, smaller) on both
 * sides of the diagonal; quotientInteger and remainderInteger charge the
 * constant below it. The reference library drops the square on the c02 term
 * and charges the constant below the diagonal for every member.
 */
const divisionCpu = (name: DivisionName, symmetric: boolean) => {
  const coefficient = (suffix: string): bigint =>
    BigInt(
      (pinnedCostModel as unknown as Record<string, number>)[
        `${name}-cpu-arguments-${suffix}`
      ]!,
    );
  const constant = coefficient("constant");
  const minimum = coefficient("model-arguments-minimum");
  const c00 = coefficient("model-arguments-c00");
  const c01 = coefficient("model-arguments-c01");
  const c02 = coefficient("model-arguments-c02");
  const c10 = coefficient("model-arguments-c10");
  const c11 = coefficient("model-arguments-c11");
  const c20 = coefficient("model-arguments-c20");
  return {
    at: (x: bigint, y: bigint): bigint => {
      if (x < y && !symmetric) return constant;
      const [a, b] = x < y ? [y, x] : [x, y];
      const real =
        c00 + c10 * a + c01 * b + c20 * a * a + c11 * a * b + c02 * b * b;
      return real < minimum ? minimum : real;
    },
  };
};

const DIVISION_CPU = new Map<UPLCBuiltinTag, ReturnType<typeof divisionCpu>>([
  [UPLCBuiltinTag.divideInteger, divisionCpu("divideInteger", true)],
  [UPLCBuiltinTag.quotientInteger, divisionCpu("quotientInteger", false)],
  [UPLCBuiltinTag.remainderInteger, divisionCpu("remainderInteger", false)],
  [UPLCBuiltinTag.modInteger, divisionCpu("modInteger", true)],
]);

export const MIDGARD_CEK_PINNED_PLUTUS_V3_BUILTIN_COSTS = <
  Tag extends UPLCBuiltinTag,
>(
  tag: Tag,
): BuiltinCostsOf<Tag> => {
  const costs = referenceBuiltinCosts(tag);
  const division = DIVISION_CPU.get(tag);
  if (division !== undefined) {
    return { ...costs, cpu: division } as unknown as BuiltinCostsOf<Tag>;
  }
  if (tag === UPLCBuiltinTag.multiplyInteger) {
    // Plutus V3 (variant E) costs multiplyInteger cpu over the product of
    // the sizes. The reference library uses their sum.
    const intercept = BigInt(
      pinnedCostModel["multiplyInteger-cpu-arguments-intercept"],
    );
    const slope = BigInt(
      pinnedCostModel["multiplyInteger-cpu-arguments-slope"],
    );
    return {
      ...costs,
      cpu: { at: (x: bigint, y: bigint): bigint => intercept + slope * x * y },
    } as unknown as BuiltinCostsOf<Tag>;
  }
  if (tag === UPLCBuiltinTag.verifyEd25519Signature) {
    // Plutus costs Ed25519 linearly in the message (second argument).
    // The reference library incorrectly selects the signature instead.
    return {
      ...costs,
      cpu: new Linear3InY(
        BigInt(
          pinnedCostModel["verifyEd25519Signature-cpu-arguments-intercept"],
        ),
        BigInt(pinnedCostModel["verifyEd25519Signature-cpu-arguments-slope"]),
      ),
    } as BuiltinCostsOf<Tag>;
  }
  return costs;
};

const assertCostSize = (size: bigint): void => {
  if (size < 0n) {
    throw new Error("V1 builtin cost size must be non-negative");
  }
};

/**
 * Evaluates the exact pinned Plutus V3 builtin cost function over the sizes
 * supplied by the CEK semantics. Callers size polymorphic, width, count and
 * literal arguments exactly as cardano-node does before calling this helper.
 */
export const computeMidgardCekBuiltinBudget = (
  tag: number,
  costArgumentSizes: readonly bigint[],
): MidgardCekBuiltinBudget => {
  if (
    !Number.isInteger(tag) ||
    tag < MIN_BUILTIN_TAG ||
    tag > MAX_BUILTIN_TAG
  ) {
    throw new Error("V1 builtin tag is outside Plutus V3");
  }
  const builtinTag = tag as UPLCBuiltinTag;
  const arity = PartialBuiltin.getNRequiredArgsFor(builtinTag);
  if (costArgumentSizes.length !== arity) {
    throw new Error(
      `V1 builtin ${tag.toString(10)} requires ${arity.toString(10)} cost sizes`,
    );
  }
  costArgumentSizes.forEach(assertCostSize);

  const costs = MIDGARD_CEK_PINNED_PLUTUS_V3_BUILTIN_COSTS(builtinTag);
  const cpuAt = costs.cpu.at.bind(costs.cpu) as (...sizes: bigint[]) => bigint;
  const memoryAt = costs.mem.at.bind(costs.mem) as (
    ...sizes: bigint[]
  ) => bigint;
  return Object.freeze({
    cpu: cpuAt(...costArgumentSizes),
    memory: memoryAt(...costArgumentSizes),
  });
};
