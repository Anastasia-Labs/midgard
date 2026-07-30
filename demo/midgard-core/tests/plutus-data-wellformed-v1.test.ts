import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { decodeMidgardDatum } from "../src/codec/datum.js";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  assertMidgardPlutusDataWellFormedV1,
} from "../src/plutus-data-cbor.js";

const unaryConstructorDataCborHex = (depth: number): string =>
  "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

const gateAccepts = (hex: string): boolean => {
  try {
    assertMidgardPlutusDataWellFormedV1(Buffer.from(hex, "hex"));
    return true;
  } catch {
    return false;
  }
};

describe("assertMidgardPlutusDataWellFormedV1", () => {
  it("accepts every well-formed Plutus Data shape CML accepts", () => {
    const wellFormed = [
      // canonical atoms and containers
      "00",
      "20",
      "1bffffffffffffffff",
      "3bffffffffffffffff",
      "40",
      "4101",
      "80",
      "a0",
      "9fff",
      "9f00ff",
      "a1004101",
      "bf004101ff",
      "d87980",
      "d87a80",
      "d8799f00ff",
      "d905789f00ff",
      "d9057880",
      "d8668218809f00ff",
      "d866820080",
      "c249010000000000000000",
      "c349010000000000000000",
      "c240",
      `5f5840${"aa".repeat(64)}41aaff`,
      // accepted by CML, later rejected only by the canonicity gate
      "5fff",
      "8100",
      "d8798100",
      "bf0000ff",
      `c25f41aaff`,
      "1800",
      "d86682180080",
    ];
    expect(wellFormed.filter((hex) => !gateAccepts(hex))).toEqual([]);
  });

  it("rejects every Plutus Data shape the ledger domain excludes", () => {
    const malformed = [
      // majors outside the Data domain
      "f93c00",
      "f5",
      "f6",
      "f7",
      "6161",
      // out-of-range constructor tags
      "d8789f00ff",
      "d8809f00ff",
      "d903e89f00ff",
      "d905799f00ff",
      "d9057980",
      "d81e820102",
      "d81f80",
      // bignum payload violations
      "c2f5",
      "c25841" + "aa".repeat(65),
      // byte-string chunk violations
      "5841" + "aa".repeat(65),
      "5f8101ff",
      `5f5841${"aa".repeat(65)}ff`,
      // constructor framing violations
      "d87900",
      "d866824080",
      "d8669f0080ff",
      "d866839f00ff9f00ff9f00ff",
      "d8668180",
      // truncation
      "",
      "d879",
      "d8799f",
      "9f",
      "bf00",
      "58",
      "5820",
      "1b00",
      // stray or misplaced break markers
      "ff",
      "bf00ff",
      // trailing content (stricter than CML; canonicity also rejects)
      "0000",
      "d8799f0102ffff",
      "9f00ff00",
    ];
    expect(malformed.filter((hex) => gateAccepts(hex))).toEqual([]);
  });

  it("admits the exact Cardano-derived maximum unary depth and beyond", () => {
    const start = Date.now();
    for (const depth of [1, 1_024, 1_523, 2_048, 4_043, 4_044, 16_000]) {
      expect(() =>
        assertMidgardPlutusDataWellFormedV1(
          Buffer.from(unaryConstructorDataCborHex(depth), "hex"),
        ),
      ).not.toThrow();
    }
    // The recursive probe this replaced needed multiple seconds per call at
    // these depths; the iterative pass must stay well under that in total.
    expect(Date.now() - start).toBeLessThan(5_000);
  });

  it("matches the composite CML verdict on a differential corpus", () => {
    const fixedCorpus = [
      "d8799f00ff",
      "00",
      "20",
      "40",
      "80",
      "a0",
      "9fff",
      "9f00ff",
      "d8668218809f00ff",
      "d866820080",
      "d86682180080",
      "d903e89f00ff",
      "d8789f00ff",
      "d8809f00ff",
      "d905799f00ff",
      "d9057880",
      "c249010000000000000000",
      "c349010000000000000000",
      "c240",
      "c2f5",
      "c25f41aaff",
      "d81e820102",
      "f93c00",
      "f5",
      "f6",
      "f7",
      "8100",
      "d8798100",
      "bf0000ff",
      `5841${"aa".repeat(65)}`,
      `5f5840${"aa".repeat(64)}41aaff`,
      "0000",
      "d87980",
      "d87a80",
      "d8799f0102ffff",
      "5fff",
      "5f40ff",
      "1800",
      "1b8000000000000000",
      "1bffffffffffffffff",
      "3b7fffffffffffffff",
      "3b8000000000000000",
      "3bffffffffffffffff",
      "a10001",
      "bf00ff",
      "d86682189f00ff",
      "d866824080",
      "d8669f0080ff",
      "d866839f00ff9f00ff9f00ff",
      "d81f80",
      "d87b80",
      "d87f80",
      "d9050080",
      "d9057980",
      "9f9f9fff0000ffff",
    ];
    let seed = 0x2468_ace0;
    const next = (): number => {
      seed = (seed * 1_103_515_245 + 12_345) & 0x7fff_ffff;
      return seed / 0x7fff_ffff;
    };
    const leaves = ["00", "20", "40", "4101", "c24101", "5fff", "80", "a0"];
    const tags = [121, 122, 127, 1_280, 1_400, 120, 128, 1_401, 1_000];
    const build = (budget: number): string => {
      if (budget <= 0 || next() < 0.3) {
        return leaves[Math.floor(next() * leaves.length)]!;
      }
      const pick = Math.floor(next() * 6);
      if (pick === 0) {
        const tag = tags[Math.floor(next() * tags.length)]!;
        const tagHex =
          tag < 256
            ? `d8${tag.toString(16).padStart(2, "0")}`
            : `d9${tag.toString(16).padStart(4, "0")}`;
        return `${tagHex}9f${build(budget - 1)}ff`;
      }
      if (pick === 1) return `9f${build(budget - 1)}${build(budget - 1)}ff`;
      if (pick === 2) return `82${build(budget - 1)}${build(budget - 1)}`;
      if (pick === 3) return `a1${build(budget - 1)}${build(budget - 1)}`;
      if (pick === 4) return `bf${build(budget - 1)}${build(budget - 1)}ff`;
      const alternative = ["1880", "1881", "00"][Math.floor(next() * 3)]!;
      return `d86682${alternative}9f${build(budget - 1)}ff`;
    };
    const corpus = [
      ...fixedCorpus,
      ...Array.from({ length: 6_000 }, () => build(1 + Math.floor(next() * 4))),
    ];

    const canonicityAccepts = (hex: string): boolean => {
      try {
        return (
          aikenSerialisedPlutusDataCborPreservingMapOrder(hex) ===
          hex.toLowerCase()
        );
      } catch {
        return false;
      }
    };
    const divergences = corpus.filter((hex) => {
      let cmlAccepts: boolean;
      try {
        Data.from(hex);
        cmlAccepts = true;
      } catch {
        cmlAccepts = false;
      }
      const canonical = canonicityAccepts(hex);
      const previousComposite = cmlAccepts && canonical;
      const proposedComposite = gateAccepts(hex) && canonical;
      return previousComposite !== proposedComposite;
    });
    expect(divergences).toEqual([]);
    expect(corpus.length).toBeGreaterThan(6_000);
  }, 120_000);
});

describe("decodeMidgardDatum with the recursion-free gate", () => {
  it("accepts the exact Cardano-derived maximum unary datum", () => {
    for (const depth of [1_024, 4_043, 4_044]) {
      const hex = unaryConstructorDataCborHex(depth);
      const decoded = decodeMidgardDatum(Buffer.from(hex, "hex"));
      expect(decoded.kind).toBe("inline");
      expect(decoded.cbor.toString("hex")).toBe(hex);
    }
  });

  it("keeps the exact rejection classes", () => {
    const expectFailure = (hex: string, message: string): void => {
      expect(() => decodeMidgardDatum(Buffer.from(hex, "hex")), hex).toThrow(
        message,
      );
    };
    expectFailure("", "PlutusData datum must not be empty");
    // domain violations reject at the well-formedness gate
    expectFailure("f93c00", "Invalid PlutusData datum CBOR");
    expectFailure("d8789f00ff", "Invalid PlutusData datum CBOR");
    expectFailure("d903e89f00ff", "Invalid PlutusData datum CBOR");
    expectFailure(`5841${"aa".repeat(65)}`, "Invalid PlutusData datum CBOR");
    expectFailure("d866824080", "Invalid PlutusData datum CBOR");
    expectFailure("0000", "Invalid PlutusData datum CBOR");
    expectFailure("d879", "Invalid PlutusData datum CBOR");
    // well-formed but noncanonical encodings reject at the canonicity gate
    expectFailure("5fff", "PlutusData datum is not canonical");
    expectFailure("8100", "PlutusData datum is not canonical");
    expectFailure("d8798100", "PlutusData datum is not canonical");
    expectFailure("bf0000ff", "PlutusData datum is not canonical");
    expectFailure("1800", "PlutusData datum is not canonical");
    expectFailure("c25f41aaff", "PlutusData datum is not canonical");
  });
});
