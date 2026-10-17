import { type Data, DataConstr, DataI } from "@harmoniclabs/plutus-data";
import { Builtin, constT, UPLCConst, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import {
  app,
  discard,
  force,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";

/**
 * A source constant whose direct form is costly for the on-chain verifier
 * reaches builtins through its semantic tree when its type has a semantic
 * form, and stays direct otherwise. Either way every step proves.
 */

const CONTEXT: Data = new DataConstr(0n, []);
const ITEMS = 16;

const integers = (): Data[] =>
  Array.from({ length: ITEMS }, (_, index) => new DataI(BigInt(index)));

const dataList = UPLCConst.listOf(constT.data)(integers());
const integerList = UPLCConst.listOf(constT.int)(
  Array.from({ length: ITEMS }, (_, index) => BigInt(index)) as never,
);
const pairList = UPLCConst.listOf(constT.pairOf(constT.data, constT.int))(
  integers().map(
    (item, index) =>
      UPLCConst.pairOf(constT.data, constT.int)(item, BigInt(index)).value,
  ) as never,
);
const listList = UPLCConst.listOf(constT.listOf(constT.data))(
  integers().map((item) => [item]) as never,
);
const dataPairs = UPLCConst.listOf(constT.pairOf(constT.data, constT.data))(
  integers().map(
    (item) => UPLCConst.pairOf(constT.data, constT.data)(item, item).value,
  ) as never,
);

const reading = (builtin: Builtin, constant: UPLCTerm): UPLCTerm =>
  lam(discard(app(force(builtin as never), constant)));

const run = (script: UPLCTerm) => {
  const prepared = prepareMidgardCekProgram(script, CONTEXT);
  return executeMidgardCekStructuralProgram({
    root: prepared.root,
    material: prepared.material.values(),
    constantWitnesses: prepared.constantWitnesses,
    maxSteps: 400_000,
    executionBudget: MIDGARD_CEK_TX_BUDGET,
  });
};

/** The kind of every argument of the run's builtin steps. */
const builtinArgumentKinds = (script: UPLCTerm): string[] => {
  const execution = run(script);
  expect(execution.terminalState.mode).toBe("haltSuccess");
  return execution.steps.flatMap((step) =>
    "arguments" in step.witness
      ? step.witness.arguments.map((argument) => argument.kind)
      : [],
  );
};

const semantic: readonly (readonly [string, UPLCTerm])[] = [
  ["headList of a Data list", reading(Builtin.headList, dataList)],
  ["tailList of a Data list", reading(Builtin.tailList, dataList)],
  ["headList of a Data pair list", reading(Builtin.headList, dataPairs)],
  ["tailList of an integer list", reading(Builtin.tailList, integerList)],
];

const direct: readonly (readonly [string, UPLCTerm])[] = [
  ["headList of a Data-integer pair list", reading(Builtin.headList, pairList)],
  ["tailList of a Data-integer pair list", reading(Builtin.tailList, pairList)],
  ["headList of a list of Data lists", reading(Builtin.headList, listList)],
  ["tailList of a list of Data lists", reading(Builtin.tailList, listList)],
];

describe("CEK source constants chosen by on-chain cost", () => {
  it.each(semantic)("reads %s through its semantic tree", (_, script) => {
    expect(builtinArgumentKinds(script)).toEqual(["semanticConstant"]);
  });

  it.each(direct)("reads %s directly", (_, script) => {
    expect(builtinArgumentKinds(script)).toEqual(["constant"]);
  });
});
