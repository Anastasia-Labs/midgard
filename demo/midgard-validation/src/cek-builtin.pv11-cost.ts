import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";

import { type MidgardCekBuiltinBudget } from "./cek-cost.js";

/** Arity of the PV 11 additions, in Cardano's Flat order (87 through 100). */
const ARITIES = [3, 2, 1, 1, 2, 2, 2, 4, 3, 2, 2, 1, 1, 2] as const;

export const midgardCekPv11BuiltinArgumentCount = (tag: bigint): bigint => {
  const arity = ARITIES[Number(tag - 87n)];
  if (arity === undefined) throw new Error("not a PV 11 builtin addition");
  return BigInt(arity);
};

/** The four polymorphic list/array additions each require one type force. */
export const midgardCekPv11BuiltinForceCount = (tag: bigint): bigint => {
  midgardCekPv11BuiltinArgumentCount(tag);
  return tag >= 88n && tag <= 91n ? 1n : 0n;
};

// Ledger cost-vector slots, not the evaluator library's language-version
// defaults: PV 11 extends the Plutus V3 vector to 350 entries.
const coefficient = (index: number): bigint => {
  const value = PLUTUS_V3_CANONICAL_COST_MODEL_VIEW[index];
  if (value === undefined) throw new Error("incomplete PV 11 cost vector");
  return BigInt(value);
};
const linear = (index: number, size: bigint): bigint =>
  coefficient(index) + coefficient(index + 1) * size;

/** Plutus 1.71 cost functions over each builtin's denotation-specific sizes. */
export const computeMidgardCekPv11BuiltinBudget = (
  tag: bigint,
  sizes: readonly bigint[],
): MidgardCekBuiltinBudget => {
  if (
    BigInt(sizes.length) !== midgardCekPv11BuiltinArgumentCount(tag) ||
    sizes.some((size) => size < 0n)
  ) {
    throw new Error("invalid PV 11 builtin cost sizes");
  }
  const [x = 0n, y = 0n, z = 0n, u = 0n] = sizes;
  let cpu: bigint;
  let memory: bigint;
  switch (tag) {
    case 87n: {
      const base =
        coefficient(297) +
        coefficient(298) * y * z +
        coefficient(299) * y * z * z;
      cpu = x > z ? base + base / 2n : base;
      memory = linear(300, z);
      break;
    }
    case 88n:
      cpu = linear(302, x);
      memory = coefficient(304);
      break;
    case 89n:
      cpu = coefficient(305);
      memory = coefficient(306);
      break;
    case 90n:
      cpu = linear(307, x);
      memory = linear(309, x);
      break;
    case 91n:
      cpu = coefficient(311);
      memory = coefficient(312);
      break;
    case 92n:
      cpu = linear(313, x);
      memory = coefficient(315);
      break;
    case 93n:
      cpu = linear(316, x);
      memory = coefficient(318);
      break;
    case 94n:
      cpu = linear(319, u);
      memory = linear(321, u);
      break;
    case 95n:
      cpu = linear(323, z);
      memory = coefficient(325);
      break;
    case 96n:
      cpu =
        coefficient(326) +
        coefficient(327) * x +
        coefficient(328) * y +
        coefficient(329) * x * y;
      memory = linear(330, x + y);
      break;
    case 97n:
      cpu =
        x < y
          ? coefficient(332)
          : coefficient(333) + coefficient(334) * x + coefficient(335) * y;
      memory = coefficient(336);
      break;
    case 98n:
      cpu = linear(337, x);
      memory = linear(339, x);
      break;
    case 99n:
      cpu = coefficient(341) + coefficient(342) * x + coefficient(343) * x * x;
      memory = linear(344, x);
      break;
    case 100n:
      cpu = linear(346, y);
      memory = linear(348, y);
      break;
    default:
      throw new Error("not a PV 11 builtin addition");
  }
  // Cardano CostingInteger saturates at signed Int64, including literal counts.
  const maximum = (1n << 63n) - 1n;
  return Object.freeze({
    cpu: cpu > maximum ? maximum : cpu,
    memory: memory > maximum ? maximum : memory,
  });
};
