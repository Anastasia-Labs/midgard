import { encodeCbor } from "@al-ft/midgard-core";
import { MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { describe, expect, it } from "vitest";

import { midgardCekLoopHopFits } from "../src/cek-loop-hop-budget.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  midgardCekWriteBits,
  midgardCekWriteBitsLoopOf,
} from "../src/cek-write-bits.js";
import { midgardCekWriteBitsLoopOfWitness } from "../src/cek-write-bits-builtin.js";
import { planMidgardCekWriteBitsHops } from "../src/cek-write-bits-loop.hops.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { UPLCProgram, type UPLCTerm } from "../src/uplc-term.js";
import { bytesConstant } from "./helpers/cek-staged-failure-charge.js";
import { call, run } from "./helpers/cek-staged-route-terms.js";
import {
  countdown,
  discard,
  int,
  lam,
  mkCons,
  v,
} from "./helpers/cek-uplc-terms.js";
import {
  compactedArguments,
  compactedBytes,
  evaluatorAccepts,
  evaluatorWriteBits,
  evaluatorWritesTo,
  indicesConstant,
  stepCharge,
  writeBitsSteps,
} from "./helpers/cek-write-bits-runs.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * writeBits at the longest index list a transaction builds: 28,763 indices
 * in a list constant (the most an 8-byte input leaves room for in the
 * 32,768-byte script field) and 28,288 more prepended at run time (the most
 * the transaction budget pays for, 64 a round), 57,051 in all. The loop's
 * hops, planned to the hop budget, end with the PV11 evaluator's result and
 * charge. The executor proves the list constant alone here; a proof-mode run
 * over the run-time list takes about 345,000 machine steps.
 */

const INPUT = Buffer.alloc(8, 0x5a);
const CONSTANT_INDICES = 28_763;
const ROUNDS = 442;
const ROUND = 64;

const constantIndices = Array.from({ length: CONSTANT_INDICES }, (_, index) =>
  BigInt((index * 37) % 64),
);

/** The indices each round prepends, the last prepended first. */
const roundIndices = Array.from({ length: ROUND }, (_, index) => BigInt(index));

/** writeBits over the list constant with `rounds` rounds prepended. */
const writeBitsOverBuilt = (rounds: number): UPLCTerm =>
  call(
    builtin("writeBits"),
    bytesConstant(INPUT),
    countdown(
      rounds,
      indicesConstant(constantIndices),
      roundIndices.reduce<UPLCTerm>(
        (tail, index) => mkCons(int(index), tail),
        v(0),
      ),
    ),
    UPLCConst.bool(true),
  );

const scriptBytes = (body: UPLCTerm): number =>
  encodeCbor(
    Buffer.from(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram([1, 1, 0], lam(discard(body))),
      ),
    ),
  ).length;

/** The list `rounds` rounds build, in list order. */
const builtIndices = (rounds: number): bigint[] => [
  ...Array.from({ length: rounds }, () => [...roundIndices].reverse()).flat(),
  ...constantIndices,
];

describe("writeBits at the longest index list a transaction builds", () => {
  it("is 57,051 indices: one round more is past the transaction budget", () => {
    expect(scriptBytes(writeBitsOverBuilt(ROUNDS))).toBeLessThanOrEqual(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    expect(evaluatorAccepts(writeBitsOverBuilt(ROUNDS))).toBe(true);
    expect(evaluatorAccepts(writeBitsOverBuilt(ROUNDS + 1))).toBe(false);
    expect(builtIndices(ROUNDS)).toHaveLength(57_051);
  });

  it("is proved by a loop planned to the hop budget, ending with the PV11 evaluator's result and charge", () => {
    const indices = builtIndices(ROUNDS);
    const out = midgardCekWriteBits(INPUT, indices, true)!;
    const loop = midgardCekWriteBitsLoopOf({
      failure: false,
      arguments: compactedArguments(INPUT, indices, true),
      result: compactedBytes(out),
      input: INPUT,
      indices,
    });
    const hops = planMidgardCekWriteBitsHops(loop);
    expect(hops.every(({ cost }) => midgardCekLoopHopFits(cost))).toBe(true);
    expect(hops.at(-1)!.end).toBe(57_051);
    const evaluator = evaluatorWriteBits(INPUT, indices, true);
    expect(evaluator.ok && hops.at(-1)!.charge).toEqual(
      evaluator.ok && evaluator.charge,
    );
    expect(evaluatorWritesTo(INPUT, indices, true, out)).toBe(true);
  }, 60_000);

  it("over its list constant alone is proved by one loop step with the PV11 evaluator's charge", () => {
    const body = writeBitsOverBuilt(0);
    const proof = run(body, "proof");
    expect(proof.result.kind).toBe("accepted");
    const [step, ...others] = writeBitsSteps(proof.execution!.steps);
    expect(others).toHaveLength(0);
    const evaluator = evaluatorWriteBits(INPUT, constantIndices, true);
    expect(evaluator.ok && stepCharge(step!)).toEqual(
      evaluator.ok && evaluator.charge,
    );
    expect(verifyMidgardCekCoreStep(step!.pre, step!.post, step!.witness)).toBe(
      true,
    );
    const hops = planMidgardCekWriteBitsHops(
      midgardCekWriteBitsLoopOfWitness(step!.witness),
    );
    expect(hops.at(-1)!.next).toBeNull();
    expect(hops.at(-1)!.charge).toEqual(stepCharge(step!));
  }, 60_000);
});
