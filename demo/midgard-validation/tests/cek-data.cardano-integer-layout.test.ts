import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekDirectBuiltin,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekCanonicalConstant,
  encodeMidgardCekPlutusData,
  midgardCekConstantWitnessFromUplc,
} from "../src/cek-constant.js";
import { buildMidgardCekDataScanTrace } from "../src/cek-data-scan.js";
import {
  commitMidgardCekDataTree,
  encodeMidgardCekDataTreeInteger,
} from "../src/cek-data-tree.js";

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

  it("the Data scan accepts the chunked form and refuses one block", () => {
    const chunked = Buffer.from(AIKEN_SERIALISE_DATA.positive65, "hex");
    const trace = buildMidgardCekDataScanTrace(chunked);
    expect(trace.terminal.offset).toBe(chunked.length);

    const singleBlock = encodeMidgardCekDataTreeInteger(MAGNITUDE_65);
    expect(singleBlock.length).toBe(68);
    expect(() => buildMidgardCekDataScanTrace(singleBlock)).toThrow(
      "V1 Data scan source is not canonical Data CBOR",
    );
  });

  it("keeps Data tree roots for integers over 64 bytes unchanged", () => {
    // Roots recorded from 8548b1960, before serialiseData and constant
    // payloads switched to the chunked form.
    const pinned = [
      [
        new DataI(MAGNITUDE_65),
        "070000d0bf539f244914ca89acbb89eb1a436a97e8517dba65829e45a66354f6",
        68n,
        69n,
      ],
      [
        new DataI(-MAGNITUDE_65 - 1n),
        "a0fb7417edbbff3cfa53acc61525c3ecb237ebd1da237b6c4b8ee9bfa5953224",
        68n,
        69n,
      ],
      [
        new DataI(MAGNITUDE_129),
        "51fc462c28acd3c5034be00f7245fe05db25903063ac5bad9bf7c0179287a406",
        132n,
        134n,
      ],
      [
        new DataConstr(0n, [
          new DataI(MAGNITUDE_65),
          new DataList([new DataI(-MAGNITUDE_65 - 1n)]),
        ]),
        "42ddbdf3015800405f36b3122c5a75c9be71a5249aec6d69655386aaf98c08a4",
        142n,
        146n,
      ],
    ] as const;
    for (const [data, root, cborLength, memory] of pinned) {
      const tree = commitMidgardCekDataTree(data);
      expect(Buffer.from(tree.root).toString("hex")).toBe(root);
      expect(tree.cborLength).toBe(cborLength);
      expect(tree.memory).toBe(memory);
    }
  });

  it("writes a 64-byte magnitude identically in both layouts", () => {
    for (const value of [MAGNITUDE_64, -MAGNITUDE_64 - 1n]) {
      expect(encodeMidgardCekDataTreeInteger(value)).toEqual(
        encodeMidgardCekPlutusData(new DataI(value)),
      );
    }
  });

  it("charges serialiseData by the node's current Data sizing", () => {
    // Byte sizing is the measure the node and the on-chain checker share
    // today. Cardano's word sizing is the redeploy-1 measure: switch
    // ASSERTED_SIZING to "word" in that change.
    const BUDGETS = {
      byte: { cpu: 15_674_034n, memory: 138n },
      word: { cpu: 3_728_562n, memory: 26n },
    } as const;
    const ASSERTED_SIZING: keyof typeof BUDGETS = "byte";
    expect(
      midgardCekDirectBuiltinBudget(51n, [
        direct(UPLCConst.data(new DataB(Buffer.alloc(65, 7)))),
      ]),
    ).toEqual(BUDGETS[ASSERTED_SIZING]);
  });
});
