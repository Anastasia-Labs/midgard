/**
 * C26 INVESTIGATION SCAFFOLDING — throwaway, not evidence.
 *
 * Originally validated a prototype of the recursion-free Plutus Data gate
 * proposed in `docs/exec-plans/evidence/c26-cml-investigation.md`. That gate
 * now ships in production as `assertMidgardPlutusDataWellFormedV1`
 * (`demo/midgard-core/src/plutus-data-cbor.ts`) behind `decodeMidgardDatum`
 * and the native-redeemer entry validation, so this suite exercises the
 * production export directly. The permanent, ungated coverage lives in
 * `demo/midgard-core/tests/plutus-data-wellformed-v1.test.ts`,
 * `demo/midgard-core/tests/plutus-data-deep-datum-retained-v1.test.ts`, and
 * `demo/midgard-core/tests/native-redeemer-deep-data-v1.test.ts`; this file
 * remains only as the investigation's cross-package differential harness.
 *
 * Skipped unless `MIDGARD_C26_INVESTIGATION=1`.
 *
 * Run with:
 *   MIDGARD_C26_INVESTIGATION=1 pnpm --dir demo/midgard-validation test -- \
 *     c26-investigation-iterative-gate
 */
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  assertMidgardPlutusDataWellFormedV1,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

const investigationEnabled =
  process.env.MIDGARD_C26_INVESTIGATION === "1";
const describeInvestigation = investigationEnabled
  ? describe
  : describe.skip;

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

const normalizerAcceptsExactly = (hex: string): boolean => {
  try {
    return (
      aikenSerialisedPlutusDataCborPreservingMapOrder(hex) === hex.toLowerCase()
    );
  } catch {
    return false;
  }
};

type CmlVerdict = "accept" | "reject" | "beyondCmlLimit";

const cmlVerdict = (hex: string): CmlVerdict => {
  try {
    Data.from(hex);
    return "accept";
  } catch (cause) {
    if (
      cause instanceof WebAssembly.RuntimeError ||
      String((cause as Error).message).includes("Maximum call stack")
    ) {
      return "beyondCmlLimit";
    }
    return "reject";
  }
};

const fixedCorpus = [
  "d8799f00ff",
  "00",
  "20",
  "40",
  "80",
  "a0",
  "9f00ff",
  "d8668218809f00ff",
  "d903e89f00ff",
  "d8789f00ff",
  "d8809f00ff",
  "d905799f00ff",
  "d9057880",
  "c249010000000000000000",
  "c349010000000000000000",
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
  "9fff",
  "5fff",
  "5f40ff",
  "1b0000000000000001",
  "3b0000000000000001",
  "a10001",
  "bf00ff",
  "d86682189f00ff",
  "d866839f00ff9f00ff9f00ff",
  "c240",
  "c2f5",
  "c25f41aaff",
  "d866824080",
  "d81f80",
  "d87b80",
  "d87f80",
  "d9050080",
  "d9057980",
  "9f9f9fff0000ffff",
] as const;

const pseudoRandomCorpus = (count: number): readonly string[] => {
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
  return Array.from({ length: count }, () =>
    build(1 + Math.floor(next() * 4)),
  );
};

describeInvestigation("C26 investigation: iterative Plutus Data gate", () => {
  it("accepts the exact C26 maximum and adjacent depths that trap CML", () => {
    for (const depth of [1, 1_024, 1_523, 2_048, 4_043, 4_044, 16_000]) {
      expect(() =>
        assertMidgardPlutusDataWellFormedV1(
          Buffer.from(unaryConstructorDataCborHex(depth), "hex"),
        ),
      ).not.toThrow();
    }
  });

  it("stays linear where lucid Data.from is superlinear", () => {
    const start = Date.now();
    for (const depth of [1_024, 2_048, 4_043, 4_044, 16_000]) {
      assertMidgardPlutusDataWellFormedV1(
        Buffer.from(unaryConstructorDataCborHex(depth), "hex"),
      );
    }
    // Measured at well under 50ms total; the bound only has to exclude the
    // multi-second per-call cost of the recursive CML path it replaced.
    expect(Date.now() - start).toBeLessThan(5_000);
  });

  it("rejects every malformed Plutus Data shape the ledger rejects", () => {
    const mustReject = [
      "f93c00",
      "f5",
      "f6",
      "f7",
      "d903e89f00ff",
      "d8789f00ff",
      "d8809f00ff",
      "d905799f00ff",
      "d81e820102",
      `5841${"aa".repeat(65)}`,
      "0000",
      "d879",
      "d8799f",
      "c2f5",
      "d866824080",
    ];
    expect(mustReject.filter((hex) => gateAccepts(hex))).toEqual([]);
  });

  it("accepts every well-formed canonical Plutus Data shape", () => {
    const mustAccept = [
      "d8799f00ff",
      "00",
      "20",
      "40",
      "80",
      "a0",
      "9f00ff",
      "d8668218809f00ff",
      "c249010000000000000000",
      "c349010000000000000000",
      `5f5840${"aa".repeat(64)}41aaff`,
      "d87980",
      "d87a80",
      "5fff",
      "c25f41aaff",
      "d9057880",
    ];
    expect(mustAccept.filter((hex) => !gateAccepts(hex))).toEqual([]);
  });

  it("matches the current composite verdict on a differential corpus", () => {
    const corpus = [...fixedCorpus, ...pseudoRandomCorpus(4_000)];
    const divergences: readonly string[] = corpus.flatMap((hex) => {
      const cml = cmlVerdict(hex);
      if (cml === "beyondCmlLimit") {
        return [];
      }
      const canonical = normalizerAcceptsExactly(hex);
      // Recursive-probe composite pair vs shipped gate composite pair.
      const previous = cml === "accept" && canonical;
      const proposed = gateAccepts(hex) && canonical;
      return previous === proposed ? [] : [hex];
    });
    expect(divergences).toEqual([]);
    expect(corpus.length).toBeGreaterThan(4_000);
  }, 300_000);
});
