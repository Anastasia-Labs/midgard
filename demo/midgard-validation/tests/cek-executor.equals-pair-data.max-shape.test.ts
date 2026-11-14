import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { type Builtin, ErrorUPLC, type UPLCTerm } from "../src/uplc-term.js";
import { runOver, spent, valueNamed } from "./helpers/cek-serialise-runs.js";
import {
  app,
  fstPair,
  headList,
  ifThenElse,
  sndPair,
  tailList,
  v,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * equalsData and mkPairData over Data at the honest maxima serialiseData is
 * proved at: 11,700 `Constr 0 []` in one list and one 370,000-byte leaf. Each
 * script takes the list `[a, b]` as its argument; the proof-mode run reaches
 * the pinned PV11 evaluator's verdict and budget, and every step it records
 * verifies.
 */

const call = (builtin: Builtin, ...args: readonly UPLCTerm[]): UPLCTerm =>
  app(builtin as never, ...args);

const items = call(builtin("unListData"), v(0));
const first = headList(items);
const second = headList(tailList(items));

/** Unit when `condition` holds, an error otherwise. */
const holds = (condition: UPLCTerm): UPLCTerm =>
  ifThenElse(condition, UPLCConst.unit, new ErrorUPLC());

/** Unit when `condition` fails, an error otherwise. */
const fails = (condition: UPLCTerm): UPLCTerm =>
  ifThenElse(condition, new ErrorUPLC(), UPLCConst.unit);

const EQUAL = holds(call(builtin("equalsData"), first, second));
const UNEQUAL = fails(call(builtin("equalsData"), first, second));
/** mkPairData's halves are the values it was given. */
const PAIR = holds(
  call(
    builtin("equalsData"),
    sndPair(call(builtin("mkPairData"), first, second)),
    fstPair(call(builtin("mkPairData"), second, first)),
  ),
);

/** The list `[a, b]` (non-empty lists are spelled indefinite). */
const pairOf = (a: string, b: string): string => `9f${a}${b}ff`;

/** `hex`'s value with its last leaf byte or last item changed. */
const altered = (name: string, hex: string): string => {
  if (name === "flat_11700_empty_constrs") {
    // The last `Constr 0 []` becomes `Constr 1 []`.
    expect(hex.endsWith("d87980ff")).toBe(true);
    return `${hex.slice(0, -8)}d87a80ff`;
  }
  // The last content byte, before the indefinite string's break.
  const at = hex.length - 4;
  const byte = parseInt(hex.slice(at, at + 2), 16) ^ 1;
  return `${hex.slice(0, at)}${byte.toString(16).padStart(2, "0")}ff`;
};

const MAXIMA = ["flat_11700_empty_constrs", "leaf_of_370000_bytes"] as const;

const provesAsTheEvaluator = (body: UPLCTerm, hex: string) => {
  const verdict = runOver(body, hex, "verdict");
  const proof = runOver(body, hex, "proof");
  expect(verdict.result.kind).toBe("accepted");
  expect(proof.result).toEqual(verdict.result);
  const { cpu, memory } = spent(body, hex);
  expect(proof.result).toEqual({ kind: "accepted", budget: { cpu, memory } });
  const steps = proof.execution?.steps ?? [];
  expect(steps.length).toBeGreaterThan(0);
  for (const { pre, post, witness } of steps) {
    expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
  }
};

describe("equalsData and mkPairData at the honest maxima", () => {
  it.each(MAXIMA)(
    "proves equalsData over two equal copies of %s",
    (name) => {
      const { hex } = valueNamed(name);
      provesAsTheEvaluator(EQUAL, pairOf(hex, hex));
    },
    600_000,
  );

  it.each(MAXIMA)(
    "proves equalsData over %s and a copy that differs at its end",
    (name) => {
      const { hex } = valueNamed(name);
      provesAsTheEvaluator(UNEQUAL, pairOf(hex, altered(name, hex)));
    },
    600_000,
  );

  it.each(MAXIMA)(
    "proves mkPairData over %s and a copy that differs at its end",
    (name) => {
      const { hex } = valueNamed(name);
      provesAsTheEvaluator(PAIR, pairOf(hex, altered(name, hex)));
    },
    600_000,
  );
});
