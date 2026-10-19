import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import { Builtin, constT, ErrorUPLC, UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { program } from "./helpers/cek-differential-builders.js";
import {
  app,
  headList,
  ifThenElse,
  int,
  mkCons,
  runMidgardCekProgram,
  tailList,
} from "./helpers/cek-uplc-terms.js";

// mkCons over `list<int>` with an integer of 2^64 or more, at the head or in
// the tail. Such an integer is a CBOR bignum in its committed form. The script
// reads the integer back with headList and compares it with equalsInteger,
// returning unit on a match and failing otherwise.

const TWO_POW_64 = 1n << 64n;

const intList = (values: readonly bigint[]) =>
  UPLCConst.listOf(constT.int)(values as never);

const readBack = (
  label: string,
  head: bigint,
  tail: readonly bigint[],
  readsTail: boolean,
  expected: bigint,
) => {
  const list = mkCons(int(head), intList(tail));
  return program(
    label,
    ifThenElse(
      app(
        Builtin.equalsInteger,
        headList(readsTail ? tailList(list) : list),
        int(expected),
      ),
      UPLCConst.unit,
      new ErrorUPLC(),
    ),
  );
};

const ROWS = [
  {
    case: readBack("head 2^64", TWO_POW_64, [1n, 2n], false, TWO_POW_64),
    terminal: "haltSuccess",
  },
  {
    case: readBack("head -2^200", -(1n << 200n), [1n], false, -(1n << 200n)),
    terminal: "haltSuccess",
  },
  {
    case: readBack("tail 2^64", 7n, [TWO_POW_64, 3n], true, TWO_POW_64),
    terminal: "haltSuccess",
  },
  {
    case: readBack(
      "head 2^64 against 2^64 + 1",
      TWO_POW_64,
      [1n, 2n],
      false,
      TWO_POW_64 + 1n,
    ),
    terminal: "haltError",
  },
  {
    case: readBack(
      "head 2^64 + 1 against 2^64",
      TWO_POW_64 + 1n,
      [1n, 2n],
      false,
      TWO_POW_64,
    ),
    terminal: "haltError",
  },
] as const;

describe("mkCons with an integer of 2^64 or more in a list of integers", () => {
  it.each(ROWS)(
    "$case.label reads back to $terminal in both modes",
    ({ case: row, terminal }) => {
      const proof = runMidgardCekProgram(row.prepare(), "proof");
      const verdict = runMidgardCekProgram(row.prepare(), "verdict");
      expect(proof.stopReason).toBe("halted");
      expect(verdict.stopReason).toBe("halted");
      expect(proof.terminalState.mode).toBe(terminal);
      expect(hashMidgardCekMachineState(verdict.terminalState)).toEqual(
        hashMidgardCekMachineState(proof.terminalState),
      );
    },
  );

  it.each(ROWS)(
    "$case.label takes a semantic mkCons step and every step verifies",
    ({ case: row }) => {
      const proof = runMidgardCekProgram(row.prepare(), "proof");
      expect(
        proof.steps.filter(
          (step) => step.witness.kind === "executeBuiltinSemantic",
        ).length,
      ).toBeGreaterThan(0);
      expect(
        proof.steps.every((step) =>
          verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
        ),
      ).toBe(true);
    },
  );
});
