/** The arm chain a staged builtin step (arm 31, semantics groups 7 and 8)
 * settles through, and the decode of an arm-31 step's material by the
 * semantic group its tag selects. */
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { StagedMaterialV1 } from "@al-ft/midgard-sdk";
import {
  evaluateMidgardCekScript,
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

type StagedWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinStaged" }
>;

/**
 * The staged step of builtin `tag` in the run of the failing script `hex`,
 * `(program 1.1.0 (lam ctx [(lam _ (error)) STEP]))`.
 */
const stagedStep = (hex: string, tag: bigint): StagedWitness => {
  const run = evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(Buffer.from(hex, "hex")),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from("d87980", "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: { cpu: 10_000_000_000n, memory: 16_500_000n },
    },
    "proof",
  );
  const witness = run.execution?.steps
    .map((step) => step.witness)
    .find(
      (candidate): candidate is StagedWitness =>
        candidate.kind === "executeBuiltinStaged" && candidate.tag === tag,
    );
  if (witness === undefined) {
    throw new Error(`the run has no staged step of ${tag.toString()}`);
  }
  return witness;
};

/** STEP: `[(builtin sha2_256) B]`, B the 16,384 bytes of fourteen
 * doublings of `#00` by `(lam x [(builtin appendByteString) x x])`. */
const SHA2 = stagedStep(
  "010100232637246466e28004004c8cdc50008009919b8a00100132337140020026466e28004004c8cdc50008009919b8a00100132337140020026466e28004004c8cdc50008009919b8a00100132337140020026466e28004004c8cdc5000800a44101000001",
  18n,
);

/** STEP: `[(lam x [(builtin addInteger) x x]) N]`, N the 4,163-byte integer
 * of nine squarings of 2^63 + 1 by `(lam x [(builtin multiplyInteger) x x])`. */
const ADD = stagedStep(
  "010100232632337000020026466e08004004c8cdc10008009919b8200100132337040020026466e08004004c8cdc10008009919b8200100132337040020026466e08004005208280808080808080800201",
  0n,
);

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
