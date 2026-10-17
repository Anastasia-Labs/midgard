import { DataI } from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionStep,
} from "../../src/cek-executor.js";
import {
  lam,
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
} from "../helpers/cek-uplc-terms.js";
import { cataloguedClassOf } from "./cek-step-fuzz.catalogue.js";
import { executedBuiltin } from "./cek-step-fuzz.harness.js";
import {
  builtinCall,
  type CekStepFuzzProgram,
  cekStepFuzzProgram,
} from "./cek-step-fuzz.programs.js";

/**
 * Catalogue rows for steps of the fixed seed set, built by the TypeScript
 * trace builder as the step differential builds them. Each row gives a
 * step's catalogued class under each Aiken verdict; the differential
 * (`cek-step-fuzz.test.ts`) pins the verdict the Aiken machine returns.
 */

const HEAD_LIST = 33;
const NULL_LIST = 35;
const I_DATA = 40;

/** The one step of a program that executes `builtin`. */
const stepExecuting = (
  program: CekStepFuzzProgram,
  builtin: number,
): MidgardCekExecutionStep => {
  const prepared = prepareMidgardCekProgram(program.term, program.context);
  const execution = executeMidgardCekStructuralProgram({
    root: prepared.root,
    material: prepared.material.values(),
    constantWitnesses: prepared.constantWitnesses,
    maxSteps: 400_000,
    executionBudget: MIDGARD_CEK_TX_BUDGET,
  });
  const steps = execution.steps.filter(
    (step) => executedBuiltin(step) === builtin,
  );
  expect(steps).toHaveLength(1);
  return steps[0]!;
};

const ROWS = [
  {
    label: "seed 4028760658 builtin 37",
    seed: 4028760658,
    primary: 37,
    builtin: HEAD_LIST,
  },
  {
    label: "seed 4028760994 builtin 58",
    seed: 4028760994,
    primary: 58,
    builtin: NULL_LIST,
  },
] as const;

describe("catalogued classes of fixed-set steps over a list of lists", () => {
  for (const row of ROWS) {
    const program = cekStepFuzzProgram(row.seed, row.primary);
    const step = () => stepExecuting(program, row.builtin);

    it(`${row.label}: the step reveals a (list (list data)) constant with a constructor index above 127`, () => {
      expect(program.label).toBe(row.label);
      expect(program.tightBudget).toBe(false);
      const { witness } = step();
      expect(witness.kind).toBe("executeBuiltinSemantic");
      const [argument] = "arguments" in witness ? witness.arguments : [];
      expect(argument?.kind).toBe("constant");
      if (argument?.kind !== "constant") return;
      expect(Buffer.from(argument.witness.typeCbor).toString("hex")).toBe(
        "9f050508ff",
      );
      expect(Buffer.from(argument.witness.payloadCbor).toString("hex")).toMatch(
        /d866/,
      );
    });

    it(`${row.label}: a budget stop is nestedConstantCost`, () => {
      expect(cataloguedClassOf(step(), row.builtin, "overBudget")).toBe(
        "nestedConstantCost",
      );
    });

    it(`${row.label}: a failure or a refusal is interimDataDecoding`, () => {
      expect(cataloguedClassOf(step(), row.builtin, "failed")).toBe(
        "interimDataDecoding",
      );
      expect(cataloguedClassOf(step(), row.builtin, "refused")).toBe(
        "interimDataDecoding",
      );
    });

    it(`${row.label}: an accepted step is in no class`, () => {
      expect(cataloguedClassOf(step(), row.builtin, "accepted")).toBeNull();
    });
  }
});

describe("catalogued classes of a step in no budget class", () => {
  const aboveOneHead = 2n ** 64n;
  const program: CekStepFuzzProgram = {
    label: "iData of 2^64",
    term: lam(builtinCall(I_DATA, [UPLCConst.int(aboveOneHead)])),
    context: new DataI(0n),
    tightBudget: false,
  };
  const step = () => stepExecuting(program, I_DATA);

  it("iData of 2^64: the step's Data preimage holds an integer beyond one CBOR head", () => {
    const { witness } = step();
    expect(witness.kind).toBe("executeBuiltinSemantic");
    if (witness.kind !== "executeBuiltinSemantic") return;
    expect(
      witness.material.scalarPreimages.map((preimage) =>
        Buffer.from(preimage).toString("hex"),
      ),
    ).toContain("c249010000000000000000");
  });

  it("iData of 2^64: a budget stop is in no class", () => {
    expect(cataloguedClassOf(step(), I_DATA, "overBudget")).toBeNull();
  });

  it("iData of 2^64: a failure or a refusal is interimDataDecoding", () => {
    expect(cataloguedClassOf(step(), I_DATA, "failed")).toBe(
      "interimDataDecoding",
    );
    expect(cataloguedClassOf(step(), I_DATA, "refused")).toBe(
      "interimDataDecoding",
    );
  });

  it("iData of 2^64: an accepted step is in no class", () => {
    expect(cataloguedClassOf(step(), I_DATA, "accepted")).toBeNull();
  });
});
