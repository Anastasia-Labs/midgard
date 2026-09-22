import { DataB, DataConstr, DataI } from "@harmoniclabs/plutus-data";
import { describe, expect, it, vi } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";

/**
 * The largest source envelope the V1 data scanner admits, in bytes.
 *
 * This is the span the on-chain twin can carry in one proof, so it is a
 * contract bound rather than a measurement: the scanner must accept a preimage
 * of exactly this length and refuse the first byte past it.
 */
const MAXIMUM_SCAN_SOURCE_BYTES = 9_215;

/**
 * A structured datum whose canonical encoding is exactly `sourceBytes` long.
 *
 * `d8 79 9f ‖ enc(bytes) ‖ ff` is three framing bytes plus the leaf plus the
 * break, and a byte string of `64n + r` bytes encodes as an indefinite string
 * of `n` 64-byte chunks plus an `r`-byte tail. Choosing `r` therefore selects
 * the total to the byte, which is what a boundary pair needs.
 */
const scanSourceOfExactly = (
  sourceBytes: number,
): { readonly data: DataConstr; readonly cbor: Buffer } => {
  const chunkCount = 139;
  const tail = sourceBytes - (3 + 1 + 2 + 66 * chunkCount + 2);
  const data = new DataConstr(0n, [
    new DataB(Buffer.alloc(64 * chunkCount + tail, 0xab)),
  ]);
  const cbor = encodeMidgardCekPlutusData(data);
  if (cbor.length !== sourceBytes) {
    throw new Error(
      `scan source builder produced ${cbor.length} bytes, not ${sourceBytes}`,
    );
  }
  return { data, cbor };
};

const unaryDataCbor = (depth: number): Buffer =>
  Buffer.from("d8799f".repeat(depth) + "00" + "ff".repeat(depth), "hex");

const unaryData = (depth: number): DataConstr => {
  let data: DataConstr = new DataConstr(0n, [new DataI(0n)]);
  for (let level = 1; level < depth; level += 1) {
    data = new DataConstr(0n, [data]);
  }
  return data;
};

describe("V1 data scanner admitted-source boundary", () => {
  it("scans the largest admitted source envelope to its whole-tree summary", async () => {
    const { buildMidgardCekDataScanTrace } = await import(
      "../src/cek-data-scan.js"
    );
    const { data, cbor } = scanSourceOfExactly(MAXIMUM_SCAN_SOURCE_BYTES);
    expect(cbor.length).toBe(MAXIMUM_SCAN_SOURCE_BYTES);

    const trace = buildMidgardCekDataScanTrace(cbor);

    // The scanner's result is decided against the independent whole-tree
    // commitment of the same datum, not against its own previous output.
    const wholeTree = commitMidgardCekDataTree(data);
    expect(trace.terminal.result).toEqual({
      root: Buffer.from(wholeTree.root),
      cborLength: wholeTree.cborLength,
      memory: wholeTree.memory,
    });
    expect(trace.terminal.result?.cborLength).toBe(
      BigInt(MAXIMUM_SCAN_SOURCE_BYTES),
    );
    // Every source byte is consumed: the whole envelope is scanned, not a
    // prefix of it.
    expect(trace.initial.offset).toBe(0);
    expect(trace.terminal.offset).toBe(MAXIMUM_SCAN_SOURCE_BYTES);
    expect(trace.terminal.rawLength).toBe(MAXIMUM_SCAN_SOURCE_BYTES);
    expect(trace.steps.map(({ step }) => step.kind)).toEqual([
      "openConstructor",
      "revealLeaf",
      "closeSequence",
      "foldList",
      "finalizeFrame",
    ]);
  });

  it("refuses the first source byte past the admitted envelope", async () => {
    const { buildMidgardCekDataScanTrace } = await import(
      "../src/cek-data-scan.js"
    );
    const { cbor } = scanSourceOfExactly(MAXIMUM_SCAN_SOURCE_BYTES + 1);
    expect(cbor.length).toBe(MAXIMUM_SCAN_SOURCE_BYTES + 1);
    expect(() => buildMidgardCekDataScanTrace(cbor)).toThrow(/1\.\.9215/u);
  });

  it("refuses an oversized source before decoding its structure", async () => {
    const { buildMidgardCekDataScanTrace } = await import(
      "../src/cek-data-scan.js"
    );
    // A depth-2,304 unary datum is 9,217 bytes and would overflow the
    // structural decoder long before its shape could be judged; the length
    // refusal has to come first.
    const raw = unaryDataCbor(2_304);
    expect(raw.length).toBe(9_217);
    expect(() => buildMidgardCekDataScanTrace(raw)).toThrow(/1\.\.9215/u);
  });

  it("refuses an empty source envelope", async () => {
    const { buildMidgardCekDataScanTrace } = await import(
      "../src/cek-data-scan.js"
    );
    expect(() => buildMidgardCekDataScanTrace(Buffer.alloc(0))).toThrow(
      /1\.\.9215/u,
    );
  });

  it("derives the terminal summary itself rather than delegating to the whole-tree commitment", async () => {
    // Behavioural reason for the interaction check: the scanner exists because
    // the on-chain twin cannot commit a whole Data tree in one step — it folds
    // frame by frame within the bounded envelope above. A scanner that reached
    // for `commitMidgardCekDataTree` would produce the right answer while
    // performing exactly the unbounded work the machine cannot perform, so the
    // absence of that call is part of the contract, and the summary must still
    // be correct without it.
    const data = unaryData(32);
    const cbor = encodeMidgardCekPlutusData(data);
    const wholeTree = commitMidgardCekDataTree(data);

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
      return { ...actual, commitMidgardCekDataTree: mockedCommit };
    });

    try {
      const { buildMidgardCekDataScanTrace } = await import(
        "../src/cek-data-scan.js"
      );
      const trace = buildMidgardCekDataScanTrace(cbor);
      expect(calls).toBe(0);
      expect(trace.terminal.result).toEqual({
        root: Buffer.from(wholeTree.root),
        cborLength: wholeTree.cborLength,
        memory: wholeTree.memory,
      });
      expect(trace.terminal.offset).toBe(cbor.length);
    } finally {
      vi.doUnmock("../src/cek-data-tree.js");
      vi.resetModules();
    }
  });
});
