/**
 * The envelope entry of the iterative Plutus Data reader: the Cardano rules of
 * `plutusDataFromCborIterative` without the 64-byte bound on a definite byte
 * string or on an indefinite chunk. Everything the Data entry accepts, the
 * envelope entry accepts with the same value; the only additional inputs it
 * accepts are those whose byte strings or chunks are longer than 64 bytes.
 */
import { encodeCborBytes } from "@al-ft/midgard-core/codec/cbor";
import { compactPlutusDataCarriageCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  makeFuzzRng,
  randomPlutusDataCbor,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  midgardEnvelopeDataFromCborIterative,
  plutusDataFromCborIterative,
} from "../src/plutus-data-iterative.decode.js";
import { encodeMidgardCekPlutusData } from "../src/plutus-data-iterative.encode.js";

const SEEDED_TIMEOUT_MS = 120_000;
const REFUSED = "refused";

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const render = (value: Data): string => {
  if (value instanceof DataI) return `I ${value.int.toString()}`;
  if (value instanceof DataB) return `B ${hex(value.bytes)}`;
  if (value instanceof DataList)
    return `L[${value.list.map(render).join(",")}]`;
  if (value instanceof DataMap) {
    return `M[${value.map
      .map(
        (entry) => `${render(entry.fst as Data)}=${render(entry.snd as Data)}`,
      )
      .join(",")}]`;
  }
  if (value instanceof DataConstr) {
    return `C ${value.constr.toString()}[${value.fields.map(render).join(",")}]`;
  }
  throw new Error("unrecognised Data value");
};

const outcome = (read: (bytes: Uint8Array) => Data, bytes: Uint8Array) => {
  try {
    return render(read(bytes));
  } catch {
    return REFUSED;
  }
};

const envelope = (vector: string): string =>
  outcome(midgardEnvelopeDataFromCborIterative, Buffer.from(vector, "hex"));
const data = (vector: string): string =>
  outcome(plutusDataFromCborIterative, Buffer.from(vector, "hex"));

const AB64 = "ab".repeat(64);
const AB65 = "ab".repeat(65);
const AB300 = "ab".repeat(300);

describe("midgardEnvelopeDataFromCborIterative", () => {
  it("accepts byte strings and chunks longer than 64 bytes", () => {
    const cases: readonly (readonly [string, string])[] = [
      [`5841${AB65}`, `B ${AB65}`],
      [`59012c${AB300}`, `B ${AB300}`],
      [`5f5841${AB65}ff`, `B ${AB65}`],
      [`5f5841${AB65}5840${AB64}ff`, `B ${AB65}${AB64}`],
      [`c25841${AB65}`, `I ${BigInt(`0x${AB65}`).toString()}`],
      [`c35841${AB65}`, `I ${(-1n - BigInt(`0x${AB65}`)).toString()}`],
      [`d8798159012c${AB300}`, `C 0[B ${AB300}]`],
      [`a15841${AB65}00`, `M[B ${AB65}=I 0]`],
    ];
    for (const [vector, expected] of cases) {
      expect(data(vector), vector).toBe(REFUSED);
      expect(envelope(vector), vector).toBe(expected);
    }
  });

  it("keeps every other refusal of the Data entry", () => {
    const refused = [
      "",
      "0000",
      `5841${AB65}00`,
      "5f5f4101ffff",
      `5f5f5841${AB65}ffff`,
      "5f6101ff",
      "61ff",
      "f6",
      "f97e00",
      "1c",
      "5c",
      "ff",
      "d87b4101",
      "d8024101",
      "d9000241ff",
      "c26101",
      "c280",
      "d86682c2410180",
      "d866823880",
      "d866820083",
      "d8669f008000ff",
      "d8664101",
      "d8798000",
      "9f00",
      "a1",
      "5a00000001",
      "58",
    ];
    for (const vector of refused) {
      expect(envelope(vector), vector).toBe(REFUSED);
    }
  });

  it(
    "agrees with the Data entry wherever the Data entry accepts, on 20,000 seeded inputs",
    () => {
      let dataAccepted = 0;
      let envelopeAccepted = 0;
      for (let seed = 1; seed <= 20_000; seed += 1) {
        const rng = makeFuzzRng(seed);
        const bytes = randomPlutusDataCbor(rng, {
          maxDepth: 1 + rng.int(7),
          maxWidth: 1 + rng.int(5),
          malformedRate: seed % 2 === 0 ? 0 : 0.05,
        });
        const fromData = outcome(plutusDataFromCborIterative, bytes);
        const fromEnvelope = outcome(
          midgardEnvelopeDataFromCborIterative,
          bytes,
        );
        if (fromData !== REFUSED) {
          dataAccepted += 1;
          expect(fromEnvelope, `seed ${seed.toString()}`).toBe(fromData);
        }
        if (fromEnvelope !== REFUSED) envelopeAccepted += 1;
      }
      expect(dataAccepted).toBe(12_911);
      expect(envelopeAccepted).toBe(14_423);
    },
    SEEDED_TIMEOUT_MS,
  );

  it("reads back what the envelope writers produce", () => {
    for (const length of [0, 1, 64, 65, 255, 256, 1_000, 70_000]) {
      const payload = Uint8Array.from(
        { length },
        (_, index) => (index * 7 + length) & 0xff,
      );
      expect(
        render(midgardEnvelopeDataFromCborIterative(encodeCborBytes(payload))),
        `length ${length.toString()}`,
      ).toBe(`B ${hex(payload)}`);
    }

    const carried = new DataConstr(1n, [
      new DataB(Buffer.alloc(200, 0x5a)),
      new DataList([new DataI(-(1n << 80n)), new DataB(Buffer.alloc(64, 1))]),
      new DataMap([
        { fst: new DataB(Buffer.alloc(65, 2)), snd: new DataConstr(300n, []) },
      ]),
    ]);
    const canonical = encodeMidgardCekPlutusData(carried);
    const compact = Buffer.from(
      compactPlutusDataCarriageCbor(hex(canonical)),
      "hex",
    );
    expect(render(plutusDataFromCborIterative(canonical))).toBe(
      render(carried),
    );
    expect(outcome(plutusDataFromCborIterative, compact)).toBe(REFUSED);
    expect(render(midgardEnvelopeDataFromCborIterative(compact))).toBe(
      render(carried),
    );
  });
});
