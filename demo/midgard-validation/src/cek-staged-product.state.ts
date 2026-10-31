/**
 * The state the staged product loop (`cek_staged_product_v1`) hands from one
 * hop to the next, `ProductStateV1 { carry, borrow, order, increment }`, as
 * an honest prover computes it for a staged multiplyInteger or division step
 * over `a` and `b`: the TypeScript twin of `columns_v1`.
 *
 * Magnitudes are taken in 512-byte limbs, least significant first. Column k
 * of x * y (x is |a| for multiplyInteger and |q| for a division, y is |b|)
 * plus the carry from column k - 1 gives the product's limb and the carry
 * into column k + 1. A division also steps the remainder's magnitude limb by
 * limb with a borrow, and compares it with |b|. When the claimed result is
 * negative its CBOR spells |n| - 1, so the loop carries one into each of its
 * limbs (`increment`) until a limb does not overflow.
 */
import { Constr, type Data } from "@lucid-evolution/lucid";

import { evaluateMidgardCekBuiltinOnValues } from "./cek-builtin.evaluate-direct-builtin.js";
import { midgardCekStagedProductLoop } from "./cek-staged-product.hops.js";
import { UPLCConst } from "./uplc-constant.js";

/** `ProductStateV1`: the loop's state between hops. */
export type MidgardCekStagedProductState = {
  readonly carry: bigint;
  readonly borrow: bigint;
  readonly order: bigint;
  readonly increment: bigint;
};

const LIMB_BITS = 4_096n;
const BASE = 1n << LIMB_BITS;

const magnitude = (value: bigint): bigint => (value < 0n ? -value : value);

/** The limbs of a non-negative `value`, least significant first. */
const limbsOf = (value: bigint): bigint[] => {
  const limbs: bigint[] = [];
  for (let rest = value; rest > 0n; rest >>= LIMB_BITS)
    limbs.push(rest & (BASE - 1n));
  return limbs;
};

const evaluate = (tag: number, a: bigint, b: bigint): bigint => {
  const value = evaluateMidgardCekBuiltinOnValues(tag, [
    UPLCConst.int(a),
    UPLCConst.int(b),
  ]);
  if (value === "failure") throw new Error("the step has no product loop");
  return value.value as bigint;
};

/**
 * The state after each column of the loop of builtin `tag` (2 to 6) over `a`
 * and `b`: entry k is the state the hop ending at column k + 1 hands on. A
 * hop ending at the last column ends the loop and hands on none. Throws for
 * a step with no loop (another builtin, or a division by zero).
 */
export const midgardCekStagedProductStates = (
  tag: bigint,
  a: bigint,
  b: bigint,
): MidgardCekStagedProductState[] => {
  const loop = midgardCekStagedProductLoop(tag, a, b);
  if (loop === null) throw new Error("the step has no product loop");
  const signsDiffer = a < 0n !== b < 0n;
  // The column factor x: |a| for multiplyInteger, else |q| for the division
  // q rounds as (remainderInteger's is quotientInteger's, modInteger's is
  // divideInteger's).
  const q =
    tag === 2n ? null : evaluate(tag === 3n || tag === 6n ? 3 : 4, a, b);
  const x = limbsOf(magnitude(q ?? a));
  const y = limbsOf(magnitude(b));
  const dividend = limbsOf(magnitude(a));
  // The claimed result the loop reads limb by limb: none for
  // quotientInteger and divideInteger, whose q is the result.
  const claimed = tag === 3n || tag === 4n ? null : evaluate(Number(tag), a, b);
  const claimedLimbs =
    claimed === null ? [] : limbsOf(claimed < 0n ? -claimed - 1n : claimed);
  const reversed = (tag === 3n || tag === 6n) && signsDiffer;
  let state: MidgardCekStagedProductState = {
    carry: 0n,
    borrow: 0n,
    order: 0n,
    increment: claimed !== null && claimed < 0n ? 1n : 0n,
  };
  const states: MidgardCekStagedProductState[] = [];
  for (let k = 0; k < loop.columns; k += 1) {
    let column = 0n;
    for (
      let i = Math.max(0, k - y.length + 1);
      i <= Math.min(k, x.length - 1);
      i += 1
    )
      column += x[i]! * y[k - i]!;
    const value = column + state.carry;
    const carry = value >> LIMB_BITS;
    const low = value & (BASE - 1n);
    let limb = low;
    let borrow = 0n;
    let order = 0n;
    if (tag !== 2n) {
      const dividendLimb = dividend[k] ?? 0n;
      const divisorLimb = y[k] ?? 0n;
      const difference = reversed
        ? low - dividendLimb - state.borrow
        : dividendLimb - low - state.borrow;
      borrow = difference < 0n ? 1n : 0n;
      limb = difference + borrow * BASE;
      order = limb > divisorLimb ? 1n : limb < divisorLimb ? -1n : state.order;
    }
    let increment = 0n;
    if (claimed !== null) {
      const spelled = (claimedLimbs[k] ?? 0n) + state.increment;
      increment = spelled === BASE ? 1n : 0n;
      if (limb !== spelled - increment * BASE)
        throw new Error("the claimed result differs from the product");
    }
    state = { carry, borrow, order, increment };
    states.push(state);
  }
  return states;
};

/** `ProductStateV1` as a hop's output carries it. */
export const midgardCekStagedProductStateData = (
  state: MidgardCekStagedProductState,
): Data =>
  new Constr(0, [state.carry, state.borrow, state.order, state.increment]);
