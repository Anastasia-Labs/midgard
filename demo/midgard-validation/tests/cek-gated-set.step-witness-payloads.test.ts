import {
  type Data,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { Builtin, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  executeMidgardCekStructuralProgram,
  MIDGARD_CEK_GATED_SET_V1,
  MidgardCekOutsideGatedSet,
} from "../src/index.js";
import {
  app,
  data,
  int,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";

/**
 * The admission screen refuses runs whose step witnesses the on-chain
 * verifier cannot decode: a revealed constant payload holding a constructor
 * index above 127 or an integer outside [-2^64, 2^64 - 1]. Everything at the
 * boundary is admitted.
 */

const CONTEXT: Data = new DataConstr(0n, []);
const U64_MAX = 2n ** 64n - 1n;

const screenedRun = (script: UPLCTerm, mode: "proof" | "verdict") => {
  const prepared = prepareMidgardCekProgram(script, CONTEXT);
  return executeMidgardCekStructuralProgram({
    root: prepared.root,
    material: prepared.material.values(),
    constantWitnesses: prepared.constantWitnesses,
    maxSteps: 0xffff_ffff,
    executionBudget: MIDGARD_CEK_TX_BUDGET,
    mode,
    gatedSet: MIDGARD_CEK_GATED_SET_V1,
  });
};

/** The arm the screen stopped the run at, or null when it ran to the end. */
const refusedArm = (script: UPLCTerm, mode: "proof" | "verdict") => {
  try {
    const outcome = screenedRun(script, mode);
    expect(outcome.terminalState.mode).toBe("haltSuccess");
    return null;
  } catch (cause) {
    expect(cause).toBeInstanceOf(MidgardCekOutsideGatedSet);
    return (cause as MidgardCekOutsideGatedSet).arm;
  }
};

const constr = (index: bigint): Data => new DataConstr(index, []);
const returning = (constant: UPLCTerm): UPLCTerm => lam(constant);
const adding = (left: bigint, right: bigint): UPLCTerm =>
  lam(app(Builtin.addInteger, int(left), int(right)));

const admitted: readonly (readonly [string, UPLCTerm])[] = [
  ["a constructor index of 127", returning(data(constr(127n)))],
  ["an integer of 2^64 - 1", returning(int(U64_MAX))],
  ["an integer of -2^64", returning(int(-(2n ** 64n)))],
  [
    "Data integers at both bounds",
    returning(
      data(new DataList([new DataI(U64_MAX), new DataI(-(2n ** 64n))])),
    ),
  ],
  ["a builtin result of 2^64 - 1", adding(U64_MAX - 1n, 1n)],
  ["a builtin result of -2^64", adding(-(2n ** 64n) + 1n, -1n)],
];

const refused: readonly (readonly [string, UPLCTerm, string])[] = [
  [
    "a constructor index of 128",
    returning(data(constr(128n))),
    "computeConstant",
  ],
  ["an integer of 2^64", returning(int(2n ** 64n)), "computeConstant"],
  [
    "an integer of -2^64 - 1",
    returning(int(-(2n ** 64n) - 1n)),
    "computeConstant",
  ],
  [
    "a nested constructor index of 128",
    returning(
      data(new DataList([new DataI(1n), new DataConstr(0n, [constr(128n)])])),
    ),
    "computeConstant",
  ],
  [
    "a Data map key of 2^64",
    returning(
      data(new DataMap([new DataPair(new DataI(2n ** 64n), new DataI(0n))])),
    ),
    "computeConstant",
  ],
  ["a builtin result of 2^64", adding(U64_MAX, 1n), "executeBuiltinDirect"],
  [
    "a builtin result of -2^64 - 1",
    adding(-(2n ** 64n), -1n),
    "executeBuiltinDirect",
  ],
];

describe("CEK admission screen on step witness payloads", () => {
  it.each(admitted)("admits a run revealing %s", (_, script) => {
    expect(refusedArm(script, "verdict")).toBeNull();
    expect(refusedArm(script, "proof")).toBeNull();
  });

  it.each(refused)("refuses a run revealing %s", (_, script, arm) => {
    expect(refusedArm(script, "verdict")).toBe(arm);
    expect(refusedArm(script, "proof")).toBe(arm);
  });

  it("runs every refused script to completion without a set", () => {
    for (const [, script] of refused) {
      const prepared = prepareMidgardCekProgram(script, CONTEXT);
      const outcome = executeMidgardCekStructuralProgram({
        root: prepared.root,
        material: prepared.material.values(),
        constantWitnesses: prepared.constantWitnesses,
        maxSteps: 0xffff_ffff,
        executionBudget: MIDGARD_CEK_TX_BUDGET,
        mode: "verdict",
      });
      expect(outcome.terminalState.mode).toBe("haltSuccess");
    }
  });
});
