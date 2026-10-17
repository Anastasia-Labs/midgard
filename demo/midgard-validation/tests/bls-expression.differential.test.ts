/**
 * `evaluateMidgardCekBlsFinal` with its explicit-stack expression walk
 * against the recursive walk it replaces (vendored in `bls-expression.legacy.ts`):
 * seeded expression trees and DAGs, with correct and wrong expected roots and
 * ill-typed leaves, give the same evaluation or the same error. The deep-chain
 * case lives in the no-recursion depth tests.
 */
import {
  type FuzzRng,
  makeFuzzRng,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekBlsFinal,
  type MidgardCekBlsExpressionWitness,
} from "../src/cek-builtin.js";
import {
  expressionRoot,
  G1_POINTS,
  G2_POINTS,
} from "./bls-expression.fixtures.js";
import { legacyEvaluateMidgardCekBlsFinal } from "./bls-expression.legacy.js";
/** A random expression of at most `leaves` leaves; earlier nodes may be reused. */
const randomExpression = (
  rng: FuzzRng,
  leaves: number,
): MidgardCekBlsExpressionWitness => {
  const pool: MidgardCekBlsExpressionWitness[] = [];
  const build = (budget: number): MidgardCekBlsExpressionWitness => {
    if (pool.length > 0 && rng.chance(0.2)) return rng.pick(pool);
    let node: MidgardCekBlsExpressionWitness;
    if (budget <= 1 || rng.chance(0.3)) {
      const illTyped = rng.chance(0.03);
      node = {
        kind: "millerLoop",
        g1: illTyped ? rng.pick(G2_POINTS) : rng.pick(G1_POINTS),
        g2: rng.pick(G2_POINTS),
      };
    } else {
      const split = 1 + rng.int(budget - 1);
      node = {
        kind: "multiply",
        left: build(split),
        right: build(budget - split),
      };
    }
    pool.push(node);
    return node;
  };
  return build(leaves);
};

const attempt = <T>(
  run: () => T,
): { ok: true; value: T } | { ok: false; message: string } => {
  try {
    return { ok: true, value: run() };
  } catch (error) {
    return { ok: false, message: (error as Error).message };
  }
};

describe("evaluateMidgardCekBlsFinal vs the recursive evaluation", () => {
  it("agrees on 400 seeded expression pairs", () => {
    const outcomes = new Set<string>();
    for (let seed = 1; seed <= 400; seed += 1) {
      const rng = makeFuzzRng(seed);
      const left = randomExpression(rng, 1 + rng.int(7));
      const right = randomExpression(rng, 1 + rng.int(5));
      const wrongRoot = rng.chance(0.15);
      const leftRoot = wrongRoot ? Buffer.alloc(32, 7) : expressionRoot(left);
      const rightRoot = expressionRoot(right);
      const legacy = attempt(() =>
        legacyEvaluateMidgardCekBlsFinal(leftRoot, rightRoot, left, right),
      );
      const current = attempt(() =>
        evaluateMidgardCekBlsFinal(leftRoot, rightRoot, left, right),
      );
      expect(current, `seed ${seed.toString()}`).toEqual(legacy);
      outcomes.add(legacy.ok ? "ok" : legacy.message);
    }
    // Success, the root check, the leaf-count check and the leaf type check.
    expect([...outcomes].sort()).toEqual([
      "BLS expression leaf requires G1 and G2 constants",
      "BLS finalVerify expression exceeds the ten-leaf L1 proof reserve",
      "BLS finalVerify expression root mismatch",
      "ok",
    ]);
  }, 300_000);
});
