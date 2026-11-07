/** The arm chain a staged builtin step (arm 31, semantics groups 7 and 8)
 * settles through, and the decode of an arm-31 step's material by the
 * semantic group its tag selects. */
import { StagedMaterialV1 } from "@al-ft/midgard-sdk";
import {
  type MidgardCekCoreStepWitness,
  midgardCekStagedBuiltinMaterialData,
  stageMidgardCekStagedBuiltinMaterial,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  cekCoreArmHops,
  decodeCekCoreSemanticMaterial,
} from "../src/validation-trace-dispute/one-step-core-chains.js";
import {
  STAGED_ADD_SCRIPT,
  STAGED_SHA2_SCRIPT,
  stagedStep,
  type StagedWitness,
} from "./support/validation-trace-dispute-staged-scripts.js";

const SHA2 = stagedStep(STAGED_SHA2_SCRIPT, 18n);

const ADD = stagedStep(STAGED_ADD_SCRIPT, 0n);

const BUILTIN_ROOT = new Uint8Array(32).fill(7);

const ROOTS = ["semanticRoots", "semanticResult", "semanticBudget"] as const;

/** A semantic builtin step's material with no nodes. */
const SEMANTIC_MATERIAL = new Constr(0, [[], [], [], []]);

/** The staged material with its operands staged on outputs of one
 * transaction. */
const stagedMaterial = (witness: StagedWitness) => {
  let next = 0;
  return stageMidgardCekStagedBuiltinMaterial(
    witness.material,
    witness.stagedOperands,
    witness.stagedOperands.map((operand) =>
      Array.from(
        { length: Math.max(1, Math.ceil(operand.length / 12_285)) },
        () => ({
          transactionId: Buffer.alloc(32, 0x5a),
          outputIndex: next++,
        }),
      ),
    ),
  );
};

describe("the arm chain of a staged builtin step", () => {
  it("settles a staged byte-string step through the semantic roots, result and budget, then the staged bytes stage", () => {
    const hops = cekCoreArmHops(5, SHA2, BUILTIN_ROOT);
    expect(hops.holders).toEqual([...ROOTS, "stagedBytes"]);
    expect(hops.facts).toHaveLength(4);
    // The roots write the arguments, the result adds the result; the budget
    // and the staged stage keep the result's facts.
    expect(hops.facts[0]).not.toBe(hops.facts[1]);
    expect(hops.facts.slice(1)).toEqual([
      hops.facts[1],
      hops.facts[1],
      hops.facts[1],
    ]);
  });

  it("settles a staged integer step through the semantic roots, result and budget, then the staged integer stage", () => {
    const hops = cekCoreArmHops(5, ADD, BUILTIN_ROOT);
    expect(hops.holders).toEqual([...ROOTS, "stagedInteger"]);
    expect(hops.facts).toHaveLength(4);
  });

  it("writes the same facts for a staged step as for the semantic step with the same arguments and result", () => {
    const { material: _material, stagedOperands: _operands, ...rest } = SHA2;
    const semantic = cekCoreArmHops(
      5,
      {
        ...rest,
        kind: "executeBuiltinSemantic",
        material: {
          dataNodes: [],
          listNodes: [],
          pairNodes: [],
          scalarPreimages: [],
        },
      } as MidgardCekCoreStepWitness,
      BUILTIN_ROOT,
    );
    expect(cekCoreArmHops(5, SHA2, BUILTIN_ROOT).facts).toEqual(semantic.facts);
  });

  it("refuses a staged step whose tag no semantic group takes", () => {
    expect(() =>
      cekCoreArmHops(5, { ...SHA2, tag: 60n }, BUILTIN_ROOT),
    ).toThrow(/no semantic stage/);
  });

  it("refuses a staged step outside group 5", () => {
    expect(() => cekCoreArmHops(3, SHA2, BUILTIN_ROOT)).toThrow();
  });
});

describe("the decode of an arm-31 step's material", () => {
  const staged = stagedMaterial(SHA2);
  const stagedData = midgardCekStagedBuiltinMaterialData(staged) as Data;

  it("reads a staged step's material as staged, with its output references in ledger order", () => {
    const decoded = decodeCekCoreSemanticMaterial(18n, stagedData);
    expect(decoded.kind).toBe("staged");
    if (decoded.kind !== "staged") return;
    expect(decoded.material.runs.refs).toEqual(
      staged.runs.refs.map((ref) => ({
        transactionId: Buffer.from(ref.transactionId).toString("hex"),
        outputIndex: BigInt(ref.outputIndex),
      })),
    );
    expect(Data.to(decoded.material, StagedMaterialV1)).toBe(
      Data.to(stagedData),
    );
    const integer = decodeCekCoreSemanticMaterial(
      0n,
      midgardCekStagedBuiltinMaterialData(stagedMaterial(ADD)) as Data,
    );
    expect(integer.kind).toBe("staged");
  });

  it("reads a semantic step's material as semantic", () => {
    expect(decodeCekCoreSemanticMaterial(39n, SEMANTIC_MATERIAL).kind).toBe(
      "semantic",
    );
  });

  it("refuses each material shape under the other group's tag", () => {
    expect(() =>
      decodeCekCoreSemanticMaterial(18n, SEMANTIC_MATERIAL),
    ).toThrow();
    expect(() =>
      decodeCekCoreSemanticMaterial(0n, SEMANTIC_MATERIAL),
    ).toThrow();
    expect(() => decodeCekCoreSemanticMaterial(39n, stagedData)).toThrow();
  });

  it("refuses the material of a tag no semantic group takes", () => {
    expect(() => decodeCekCoreSemanticMaterial(60n, stagedData)).toThrow();
    expect(() =>
      decodeCekCoreSemanticMaterial(60n, SEMANTIC_MATERIAL),
    ).toThrow();
  });
});
