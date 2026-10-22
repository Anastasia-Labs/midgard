import {
  commitMidgardCekBlob,
  hashMidgardCekDataNode,
  hashMidgardCekMachineState,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  evaluateMidgardCekDirectBuiltin,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekCanonicalConstant,
  encodeMidgardCekPlutusData,
  hashMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
  midgardCekConstantWitnessFromUplc,
  midgardCekIntegerMemorySize,
} from "../src/cek-constant.js";
import {
  midgardCekCoreStepWitnessData,
  midgardCekMachineStateData,
} from "../src/cek-data.js";
import {
  commitMidgardCekDataTree,
  encodeMidgardCekDataTreeInteger,
} from "../src/cek-data-tree.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import {
  type AikenStepOracle,
  makeAikenStepOracle,
} from "./fuzz/cek-step-fuzz.oracle.js";
import {
  discard,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";

const direct = (constant: UPLCConst): MidgardCekDirectValueWitness => ({
  kind: "constant",
  witness: midgardCekConstantWitnessFromUplc(constant),
});

const magnitude = (bytes: readonly number[]): bigint =>
  BigInt(`0x${Buffer.from(bytes).toString("hex")}`);

// 65-byte magnitude 0x0102…41 and 64-byte magnitude 0xfffe…c0.
const MAGNITUDE_65 = magnitude(Array.from({ length: 65 }, (_, i) => i + 1));
const MAGNITUDE_64 = magnitude(Array.from({ length: 64 }, (_, i) => 0xff - i));
const MAGNITUDE_129 = magnitude(
  Array.from({ length: 129 }, (_, i) => (i === 0 ? 0x80 : (i * 7 + 3) & 0xff)),
);

const hexRange = (from: number, count: number, step = 1): string =>
  Buffer.from(
    Array.from({ length: count }, (_, i) => (from + i * step) & 0xff),
  ).toString("hex");

// `aiken uplc eval` (v1.1.23+5adf783) of `[(builtin serialiseData) (con data
// (I n))]`; the same bytes are the Data constant inside `aiken uplc encode`
// Flat output for `(con data (I n))`.
const AIKEN_SERIALISE_DATA = {
  positive65: `c25f5840${hexRange(1, 64)}4141ff`,
  negative65: `c35f5840${hexRange(1, 64)}4141ff`,
  positive64: `c25840${hexRange(0xff, 64, -1)}`,
  negative64: `c35840${hexRange(0xff, 64, -1)}`,
  positive129:
    "c25f5840800a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bc5840c3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c4183ff",
} as const;

const CASES = [
  ["positive65", MAGNITUDE_65],
  ["negative65", -MAGNITUDE_65 - 1n],
  ["positive64", MAGNITUDE_64],
  ["negative64", -MAGNITUDE_64 - 1n],
  ["positive129", MAGNITUDE_129],
] as const;

const serialiseDataResult = (data: Data): Buffer => {
  const evaluated = evaluateMidgardCekDirectBuiltin(51n, [
    direct(UPLCConst.data(data)),
  ]);
  if (evaluated.kind !== "success" || evaluated.result.kind !== "constant") {
    throw new Error("serialiseData did not return a direct constant");
  }
  const decoded = decodeMidgardCekConstantWitness(evaluated.result.witness);
  if (!(decoded.payload instanceof DataB)) {
    throw new Error("serialiseData did not return bytes");
  }
  return Buffer.from(decoded.payload.bytes);
};

describe("Cardano integer layout in Data CBOR", () => {
  it.each(CASES)(
    "serialiseData of %s matches the pinned aiken bytes",
    (name, value) => {
      expect(serialiseDataResult(new DataI(value)).toString("hex")).toBe(
        AIKEN_SERIALISE_DATA[name],
      );
    },
  );

  it("serialiseData writes definite maps as Cardano does", () => {
    // `aiken uplc eval` of serialiseData over each literal.
    expect(
      serialiseDataResult(
        new DataMap([{ fst: new DataI(1n), snd: new DataI(1n) }]),
      ).toString("hex"),
    ).toBe("a10101");
    expect(
      serialiseDataResult(
        new DataMap<Data, Data>([
          {
            fst: new DataB(Buffer.from([0])),
            snd: new DataMap([
              { fst: new DataI(2n), snd: new DataList([new DataI(3n)]) },
            ]),
          },
          { fst: new DataI(4n), snd: new DataConstr(1n, []) },
        ]),
      ).toString("hex"),
    ).toBe("a24100a1029f03ff04d87a80");
  });

  it.each(CASES)("a %s Data constant payload matches aiken", (name, value) => {
    const canonical = encodeMidgardCekCanonicalConstant(
      UPLCConst.data(new DataI(value)),
    );
    expect(canonical.payloadCbor.toString("hex")).toBe(
      AIKEN_SERIALISE_DATA[name],
    );
  });

  it.each(CASES)(
    "commits %s with Cardano integer leaf bytes",
    (name, value) => {
      const tree = commitMidgardCekDataTree(new DataI(value));
      const raw = Buffer.from(AIKEN_SERIALISE_DATA[name], "hex");
      const expectedNode = {
        kind: "integer" as const,
        cborRoot: commitMidgardCekBlob(raw).root,
        cborLength: BigInt(raw.length),
        memory: 4n + midgardCekIntegerMemorySize(value),
      };
      expect(tree.root).toEqual(hashMidgardCekDataNode(expectedNode));
      expect(tree.cborLength).toBe(BigInt(raw.length));
      expect(encodeMidgardCekDataTreeInteger(value)).toEqual(raw);
    },
  );

  it("writes a 64-byte magnitude identically in both layouts", () => {
    for (const value of [MAGNITUDE_64, -MAGNITUDE_64 - 1n]) {
      expect(encodeMidgardCekDataTreeInteger(value)).toEqual(
        encodeMidgardCekPlutusData(new DataI(value)),
      );
    }
  });

  it("charges serialiseData by Cardano's word sizing of the Data", () => {
    // A 65-byte DataB is one Data node (4 words) plus nine 8-byte words.
    expect(
      midgardCekDirectBuiltinBudget(51n, [
        direct(UPLCConst.data(new DataB(Buffer.alloc(65, 7)))),
      ]),
    ).toEqual({ cpu: 3_728_562n, memory: 26n });
  });
});

// The ledger's serialisation either side of the 64-bit head boundary and of
// constructor 128, as `cbor.serialise` pins it in
// onchain/aiken/lib/midgard/cek-constant-v1.test.ak.
const BOUNDARY_INTEGERS = [
  [(1n << 64n) - 1n, "1bffffffffffffffff"],
  [1n << 64n, "c249010000000000000000"],
  [-(1n << 64n), "3bffffffffffffffff"],
  [-(1n << 64n) - 1n, "c349010000000000000000"],
] as const;

const BOUNDARY_CONSTRUCTORS = [
  [127n, "d9057880"],
  [128n, "d86682188080"],
] as const;

describe("Cardano layout at the 64-bit and constructor-128 boundaries", () => {
  it.each(BOUNDARY_INTEGERS)("every writer writes %s as %s", (value, hex) => {
    const raw = Buffer.from(hex, "hex");
    expect(encodeMidgardCekPlutusData(new DataI(value))).toEqual(raw);
    expect(
      encodeMidgardCekCanonicalConstant(UPLCConst.int(value)).payloadCbor,
    ).toEqual(raw);
    expect(
      encodeMidgardCekCanonicalConstant(UPLCConst.data(new DataI(value)))
        .payloadCbor,
    ).toEqual(raw);
    expect(encodeMidgardCekDataTreeInteger(value)).toEqual(raw);
    expect(commitMidgardCekDataTree(new DataI(value)).root).toEqual(
      hashMidgardCekDataNode({
        kind: "integer",
        cborRoot: commitMidgardCekBlob(raw).root,
        cborLength: BigInt(raw.length),
        memory: 4n + midgardCekIntegerMemorySize(value),
      }),
    );
  });

  it.each(BOUNDARY_CONSTRUCTORS)(
    "every writer writes constructor %s as %s",
    (index, hex) => {
      const raw = Buffer.from(hex, "hex");
      expect(encodeMidgardCekPlutusData(new DataConstr(index, []))).toEqual(
        raw,
      );
      expect(
        encodeMidgardCekCanonicalConstant(
          UPLCConst.data(new DataConstr(index, [])),
        ).payloadCbor,
      ).toEqual(raw);
    },
  );

  it("writes the constructor-128 leaf as the integer 128", () => {
    expect(encodeMidgardCekDataTreeInteger(128n).toString("hex")).toBe("1880");
  });

  it.each([
    "d80249010000000000000000",
    "d80349010000000000000000",
    "d866822080",
    "d86682c2410180",
  ])("refuses %s as a Data constant payload", (hex) => {
    expect(() =>
      decodeMidgardCekConstantWitness({
        typeCbor: Buffer.from("9f08ff", "hex"),
        payloadCbor: Buffer.from(hex, "hex"),
      }),
    ).toThrow();
  });
});

const TWO_64 = 1n << 64n;

// What a ComputeConstant step commits for each constant: its memory and its
// root. Cardano sizes an integer in 64-bit words, so the memory is 2 exactly
// from |x| = 2^64, and a Data value four words per node plus its integers'
// words. The root is checked against the Aiken machine below: its
// ComputeConstant rule derives the root from the same witness.
const BOUNDARY_CONSTANTS = [
  ["integer 2^64-1", UPLCConst.int(TWO_64 - 1n), 1n],
  ["integer 2^64", UPLCConst.int(TWO_64), 2n],
  ["integer -2^64", UPLCConst.int(-TWO_64), 2n],
  ["integer -2^64-1", UPLCConst.int(-TWO_64 - 1n), 2n],
  ["data 2^64-1", UPLCConst.data(new DataI(TWO_64 - 1n)), 5n],
  ["data 2^64", UPLCConst.data(new DataI(TWO_64)), 6n],
  ["data -2^64", UPLCConst.data(new DataI(-TWO_64)), 6n],
  ["data -2^64-1", UPLCConst.data(new DataI(-TWO_64 - 1n)), 6n],
  ["data constructor 127", UPLCConst.data(new DataConstr(127n, [])), 4n],
  ["data constructor 128", UPLCConst.data(new DataConstr(128n, [])), 4n],
  [
    "data constructor 127 over 2^64-1",
    UPLCConst.data(new DataConstr(127n, [new DataI(TWO_64 - 1n)])),
    9n,
  ],
  [
    "data constructor 128 over 2^64",
    UPLCConst.data(new DataConstr(128n, [new DataI(TWO_64)])),
    10n,
  ],
] as const;

// Each builtin's own charge: Cardano's PV 11 total for the program in
// cek-program.cardano-boundaries.vectors.ts, less the 100/100 start-up and
// the five 16000/100 machine steps of `[[builtin a] b]`.
const BOUNDARY_BUDGETS = [
  [
    "addInteger 2^64-1 and 1",
    0n,
    UPLCConst.int(TWO_64 - 1n),
    UPLCConst.int(1n),
    101_208n,
    2n,
  ],
  [
    "addInteger 2^64 and -1",
    0n,
    UPLCConst.int(TWO_64),
    UPLCConst.int(-1n),
    101_628n,
    3n,
  ],
  [
    "addInteger -2^64 and -1",
    0n,
    UPLCConst.int(-TWO_64),
    UPLCConst.int(-1n),
    101_628n,
    3n,
  ],
  [
    "addInteger -2^64-1 and 1",
    0n,
    UPLCConst.int(-TWO_64 - 1n),
    UPLCConst.int(1n),
    101_628n,
    3n,
  ],
  [
    "equalsData -2^64 and -2^64-1",
    47n,
    UPLCConst.data(new DataI(-TWO_64)),
    UPLCConst.data(new DataI(-TWO_64 - 1n)),
    1_061_822n,
    1n,
  ],
  [
    "equalsData 2^64-1 and 2^64",
    47n,
    UPLCConst.data(new DataI(TWO_64 - 1n)),
    UPLCConst.data(new DataI(TWO_64)),
    1_034_543n,
    1n,
  ],
  [
    "equalsData constructor 127 and 128",
    47n,
    UPLCConst.data(new DataConstr(127n, [])),
    UPLCConst.data(new DataConstr(128n, [])),
    1_007_264n,
    1n,
  ],
] as const;

describe("Constants and builtin charges at the 64-bit and constructor-128 boundaries", () => {
  it.each(BOUNDARY_CONSTANTS)(
    "commits the %s constant with Cardano's memory",
    (_label, constant, memory) => {
      const decoded = decodeMidgardCekConstantWitness(
        midgardCekConstantWitnessFromUplc(constant),
      );
      expect(midgardCekConstantMemorySize(decoded.type, decoded.payload)).toBe(
        memory,
      );
    },
  );

  it.each(BOUNDARY_BUDGETS)(
    "charges %s as Cardano does",
    (_name, tag, left, right, cpu, memory) => {
      expect(
        midgardCekDirectBuiltinBudget(tag, [direct(left), direct(right)]),
      ).toEqual({
        cpu,
        memory,
      });
    },
  );
});

const stateCbor = (state: MidgardCekMachineState): Uint8Array =>
  lucidDataToCborIterative(midgardCekMachineStateData(state) as never);

describe("Boundary constant roots against the Aiken machine", () => {
  let oracle: AikenStepOracle | undefined;

  // The oracle compiles the on-chain project twice per run, shared by every
  // file that uses it (about 45 s on a developer machine, a few minutes on a
  // loaded CI runner); a file that starts meanwhile waits for it.
  beforeAll(async () => {
    oracle = await makeAikenStepOracle();
  }, 300_000);

  afterAll(() => oracle?.close());

  it(
    "commits every boundary constant to the root the Aiken machine derives",
    { timeout: 600_000 },
    () => {
      const runs = BOUNDARY_CONSTANTS.map(([label, constant]) => {
        const prepared = prepareMidgardCekProgram(
          lam(discard(constant)),
          new DataConstr(0n, []),
        );
        const execution = executeMidgardCekStructuralProgram({
          root: prepared.root,
          material: prepared.material.values(),
          constantWitnesses: prepared.constantWitnesses,
          maxSteps: 1_000,
          executionBudget: MIDGARD_CEK_TX_BUDGET,
        });
        expect(execution.terminalState.mode).toBe("haltSuccess");
        const witness = midgardCekConstantWitnessFromUplc(constant);
        const root = Buffer.from(hashMidgardCekConstantWitness(witness));
        const computed = execution.steps.filter(
          (step) =>
            step.witness.kind === "computeConstant" &&
            root.equals(step.post.focusRoot),
        );
        expect(computed.map((step) => step.witness)).toEqual([
          { kind: "computeConstant", value: witness },
        ]);
        return { label, steps: execution.steps };
      });
      const steps = runs.flatMap(({ label, steps }) =>
        steps.map((step, index) => ({ label, index, step })),
      );
      const verdicts = oracle!.verify(
        steps.map(({ step }) => ({
          pre: stateCbor(step.pre),
          post: stateCbor(step.post),
          witness: lucidDataToCborIterative(
            midgardCekCoreStepWitnessData(step.witness) as never,
          ),
        })),
      );
      expect(
        verdicts.flatMap((verdict, at) => {
          const { label, index, step } = steps[at]!;
          return verdict === "accepted"
            ? []
            : [`${label} step ${index.toString()} (${step.witness.kind})`];
        }),
      ).toEqual([]);
      expect(
        oracle!.hash(steps.map(({ step }) => stateCbor(step.post))),
      ).toEqual(
        steps.map(({ step }) =>
          hashMidgardCekMachineState(step.post).toString("hex"),
        ),
      );
    },
  );
});
