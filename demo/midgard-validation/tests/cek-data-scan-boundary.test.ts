import { describe, expect, it, vi } from "vitest";

const unaryDataCbor = (depth: number): Buffer =>
  Buffer.from("d8799f".repeat(depth) + "00" + "ff".repeat(depth), "hex");

describe("V1 data scanner admitted-depth boundary", () => {
  it("rejects the first unary depth outside the source envelope", async () => {
    const { buildMidgardCekDataScanTraceV1 } = await import(
      "../src/cek-data-scan.js"
    );
    const raw = unaryDataCbor(2_304);
    expect(raw.length).toBe(9_217);
    expect(() => buildMidgardCekDataScanTraceV1(raw)).toThrow(/1\.\.9215/u);
  });

  it("does not call the whole-tree commitment for visited nodes", async () => {
    vi.resetModules();
    let calls = 0;
    const mockedCommit = vi.fn(() => {
      calls += 1;
      throw new Error("whole-tree commitment invoked");
    });
    vi.doMock("../src/cek-data-tree.js", async () => {
      const actual = await vi.importActual<
        typeof import("../src/cek-data-tree.js")
      >("../src/cek-data-tree.js");
      return { ...actual, commitMidgardCekDataTreeV1: mockedCommit };
    });

    try {
      const { buildMidgardCekDataScanTraceV1 } = await import(
        "../src/cek-data-scan.js"
      );
      const trace = buildMidgardCekDataScanTraceV1(unaryDataCbor(32));
      expect(trace.terminal.result).not.toBeNull();
      expect(calls).toBe(0);
    } finally {
      vi.doUnmock("../src/cek-data-tree.js");
      vi.resetModules();
    }
  });
});
