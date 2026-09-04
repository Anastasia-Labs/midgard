import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import { toCostModelV3 } from "@harmoniclabs/cardano-costmodels-ts";
import {
  costModelV3ToBuiltinCosts,
  PartialBuiltin,
} from "@harmoniclabs/plutus-machine";
import { UPLCBuiltinTag } from "@harmoniclabs/uplc";

export type MidgardCekBuiltinBudget = {
  readonly cpu: bigint;
  readonly memory: bigint;
};

const MIN_BUILTIN_TAG = 0;
const MAX_BUILTIN_TAG = 86;

export const MIDGARD_CEK_PINNED_PLUTUS_V3_BUILTIN_COSTS =
  costModelV3ToBuiltinCosts(
    toCostModelV3([
      ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
    ] as unknown as Parameters<typeof toCostModelV3>[0]),
  );

const assertCostSize = (size: bigint): void => {
  if (size < 0n) {
    throw new Error("V1 builtin cost size must be non-negative");
  }
};

/**
 * Evaluates the exact pinned Plutus V3 builtin cost function over the sizes
 * supplied by the CEK semantics. Callers must normalize polymorphic and
 * bitwise arguments exactly as cardano-node does before calling this helper.
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

/**
 * Tags 75-77 charge both byte-string arguments after the selected
 * extend/truncate policy has made their lengths equal.
 */
export const normalizeMidgardCekBitwiseCostSizes = (
  shouldExtend: boolean,
  leftSize: bigint,
  rightSize: bigint,
): readonly [bigint, bigint, bigint] => {
  assertCostSize(leftSize);
  assertCostSize(rightSize);
  const normalized = shouldExtend
    ? leftSize > rightSize
      ? leftSize
      : rightSize
    : leftSize < rightSize
      ? leftSize
      : rightSize;
  return Object.freeze([1n, normalized, normalized]);
};
