import { describe, expect, it } from "vitest";
import { Data } from "@lucid-evolution/lucid";
import {
  SchedulerDatum,
  normalizeRootIndefiniteArrayEncoding,
} from "@al-ft/midgard-sdk";

describe("normalizeRootIndefiniteArrayEncoding", () => {
  it("rewrites a root indefinite array to a definite array", () => {
    expect(normalizeRootIndefiniteArrayEncoding("9f0102ff")).toBe("820102");
  });

  it("rewrites a tag-102 root wrapper without touching inner bytes", () => {
    expect(normalizeRootIndefiniteArrayEncoding("d8669f0001ff")).toBe(
      "d866820001",
    );
  });

  it("rewrites a constructor root from Data.to without re-encoding inner fields", () => {
    const cborHex = Data.to(
      {
        ActiveOperator: {
          operator: "aa",
          start_time: 1n,
        },
      },
      SchedulerDatum,
    );

    expect(cborHex.startsWith("d87a9f")).toBe(true);
    expect(cborHex.endsWith("ff")).toBe(true);
    expect(normalizeRootIndefiniteArrayEncoding(cborHex)).toBe(
      `d87a82${cborHex.slice(6, -2)}`,
    );
  });

  it("preserves nested indefinite encodings while rewriting only the root array", () => {
    const cborHex = "d8799f9f0102ff03ff";

    expect(normalizeRootIndefiniteArrayEncoding(cborHex)).toBe(
      "d879829f0102ff03",
    );
  });

  it("returns unchanged data when the root array is already definite", () => {
    expect(normalizeRootIndefiniteArrayEncoding("d879820102")).toBe(
      "d879820102",
    );
  });

  it("throws on malformed root indefinite arrays", () => {
    expect(() => normalizeRootIndefiniteArrayEncoding("d8799f0102")).toThrow(
      "Failed to normalize root CBOR array encoding",
    );
  });
});
