import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  mergeSplitFitLedgerParts,
  type SplitFitLedger,
  type StoredSplitFitPart,
} from "./support/split-fit-ledger.js";

const ledger: SplitFitLedger = {
  path: "/nowhere/example-fit-ledger.json",
  category: "example",
  compilerVersion: "aiken test",
  caseCount: 3,
  parts: ["big", "rest"],
  source: "/nowhere/example-cases.ts",
};

const row = (stem: string, signedBytes: number) => ({
  stem,
  kind: "lifecycle" as const,
  maximumShape: stem,
  signedBytes,
  memoryUnits: "10",
  cpuUnits: "20",
});

const part = (
  name: string,
  cases: StoredSplitFitPart["cases"],
  blueprintSha256 = "ab".repeat(32),
  sourceSha256 = "cd".repeat(32),
): StoredSplitFitPart => ({
  schemaVersion: "midgard-split-fit-ledger-part-v1",
  ledger: "example-fit-ledger",
  category: "example",
  compilerVersion: "aiken test",
  blueprintSha256,
  sourceSha256,
  caseCount: 3,
  part: name,
  cases,
});

const big = part("big", [{ ordinal: 1, rows: [row("b", 2), row("b", 3)] }]);
const rest = part("rest", [
  { ordinal: 0, rows: [row("a", 1)] },
  { ordinal: 2, rows: [row("c", 4)] },
]);

describe("split fit ledger merge", () => {
  it("numbers rows across parts as one file running every case in order", () => {
    const unsplit = buildVanRossemFitLedger({
      category: "example",
      blueprintSha256: "ab".repeat(32),
      compilerVersion: "aiken test",
      measurements: [
        ["a", 1],
        ["b", 2],
        ["b", 3],
        ["c", 4],
      ].map(([stem, signedBytes], index) => ({
        name: `${stem as string}:${index.toString()}`,
        kind: "lifecycle" as const,
        maximumShape: stem as string,
        signedBytes: signedBytes as number,
        memoryUnits: 10n,
        cpuUnits: 20n,
      })),
    });
    expect(JSON.stringify(mergeSplitFitLedgerParts(ledger, [big, rest]))).toBe(
      JSON.stringify(unsplit),
    );
  });

  it("refuses to merge a partial run", () => {
    expect(() => mergeSplitFitLedgerParts(ledger, [big])).toThrow(
      "needs every part",
    );
    expect(() =>
      mergeSplitFitLedgerParts(ledger, [
        big,
        part("rest", [{ ordinal: 0, rows: [row("a", 1)] }]),
      ]),
    ).toThrow("case 2 is missing");
  });

  it("refuses parts that disagree or overlap", () => {
    expect(() => mergeSplitFitLedgerParts(ledger, [rest, big])).toThrow(
      "does not belong",
    );
    expect(() =>
      mergeSplitFitLedgerParts(ledger, [
        part("big", big.cases, "cd".repeat(32)),
        rest,
      ]),
    ).toThrow("does not belong");
    expect(() =>
      mergeSplitFitLedgerParts(ledger, [
        part("big", big.cases, "ab".repeat(32), "ef".repeat(32)),
        rest,
      ]),
    ).toThrow("does not belong");
    expect(() =>
      mergeSplitFitLedgerParts(ledger, [
        part("big", [...big.cases, { ordinal: 2, rows: [] }]),
        rest,
      ]),
    ).toThrow("case 2 is duplicated");
    expect(() =>
      mergeSplitFitLedgerParts(ledger, [
        part("big", [...big.cases, { ordinal: 3, rows: [] }]),
        rest,
      ]),
    ).toThrow("unknown cases");
  });
});
