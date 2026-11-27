import { expect } from "vitest";

import type { MidgardCekBuiltinBudget } from "../../src/cek-cost.js";
import { verifyMidgardCekCoreStep } from "../../src/cek-machine.verify-builtin.js";
import { UPLCConst } from "../../src/uplc-constant.js";
import type { Builtin, UPLCTerm } from "../../src/uplc-term.js";
import {
  type Budget,
  builtBytes,
  bytesConstant,
  evaluatorCharge,
  runWithin,
} from "./cek-staged-failure-charge.js";
import { built, call, int, staged } from "./cek-staged-route-terms.js";
import { MIDGARD_CEK_TX_BUDGET } from "./cek-uplc-terms.js";
import { builtin } from "./uplc-builtins.js";

/** Rows of a builtin that fails beside a staged operand, checked against
 * the pinned PV11 evaluator. */

export const bool = (value: boolean): UPLCTerm => UPLCConst.bool(value);

const bigEndian = (value: Uint8Array): bigint =>
  BigInt(`0x${Buffer.from(value).toString("hex")}`);

/** An operand as a run-time term and as a program constant of its value. */
export type Operand = {
  readonly live: UPLCTerm;
  readonly constant: UPLCTerm;
};

export const constant = (term: UPLCTerm): Operand => ({
  live: term,
  constant: term,
});

export const bytes = (length: number, salt = 1): Operand => ({
  live: built(length, salt),
  constant: bytesConstant(builtBytes(length, salt)),
});

/** The integer of `length` big-endian bytes, built at run time from them;
 * its first byte is not zero, so it has `length / 8` words. */
export const integer = (length: number, negative = false): Operand => {
  const magnitude = bigEndian(builtBytes(length, 1));
  const positive = call(
    builtin("byteStringToInteger"),
    bool(true),
    built(length, 1),
  );
  return {
    live: negative
      ? call(builtin("subtractInteger"), int(0n), positive)
      : positive,
    constant: int(negative ? -magnitude : magnitude),
  };
};

export const small = (value: bigint): Operand => constant(int(value));

/** How the failure is charged: as the evaluator charges it, or not at all
 * where the direct failure arm charges nothing. */
export type Charge = "evaluator" | "uncharged";

const UNCHARGED: MidgardCekBuiltinBudget = { cpu: 0n, memory: 0n };

/**
 * Checks that `builtin` over `operands` fails in both modes, and that the
 * proof's last step is the staged failure of `tag`, charged as `charge`
 * says, and verifies.
 */
export const expectStagedFailureProved = (
  tag: bigint,
  builtin: Builtin,
  operands: readonly Operand[],
  charge: Charge,
  budget: Budget = MIDGARD_CEK_TX_BUDGET,
): void => {
  const body = call(builtin, ...operands.map(({ live }) => live));
  expect(runWithin(body, "verdict", budget).result.kind).toBe("script_invalid");
  const proof = runWithin(body, "proof", budget);
  expect(proof.result.kind).toBe("script_invalid");
  const last = proof.execution!.steps.at(-1)!;
  expect(last.witness.kind).toBe("executeBuiltinStagedFailure");
  expect("tag" in last.witness ? last.witness.tag : null).toBe(tag);
  expect(last.post.mode).toBe("haltError");
  const charged = {
    cpu: last.post.cpu - last.pre.cpu,
    memory: last.post.memory - last.pre.memory,
  };
  const evaluator = evaluatorCharge(
    call(builtin, ...operands.map((operand) => operand.constant)),
    budget,
  );
  if (charge === "evaluator") {
    expect(charged).toEqual(evaluator);
  } else {
    expect(evaluator.cpu > 0n).toBe(true);
    expect(charged).toEqual(UNCHARGED);
  }
  expect(verifyMidgardCekCoreStep(last.pre, last.post, staged(last, 9))).toBe(
    true,
  );
};
