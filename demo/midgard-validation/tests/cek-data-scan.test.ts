import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekPlutusDataV1,
} from "../src/cek-constant.js";
import {
  buildMidgardCekDataScanTraceV1,
  encodeMidgardCekDataScanControlV1,
  hashMidgardCekDataScanControlV1,
} from "../src/cek-data-scan.js";
import {
  commitMidgardCekDataTreeV1,
} from "../src/cek-data-tree.js";

describe("V1 content-addressed Data scanner", () => {
  it("reconstructs nested constructors, lists, and maps one bounded step at a time", () => {
    const data = new DataConstr(0n, [
      new DataList([new DataI(1n), new DataI(2n)]),
      new DataMap([
        new DataPair(new DataI(3n), new DataB(Buffer.from("abcd", "hex"))),
      ]),
    ]);
    const raw = encodeMidgardCekPlutusDataV1(data);
    const trace = buildMidgardCekDataScanTraceV1(raw);
    const expected = commitMidgardCekDataTreeV1(data);

    expect(trace.initial.offset).toBe(0);
    expect(trace.terminal.offset).toBe(raw.length);
    expect(trace.terminal.result).toEqual({
      root: Buffer.from(expected.root),
      cborLength: expected.cborLength,
      memory: expected.memory,
    });
    expect(trace.steps.map(({ step }) => step.kind)).toEqual([
      "openConstructor",
      "openList",
      "revealLeaf",
      "revealLeaf",
      "closeSequence",
      "foldList",
      "foldList",
      "finalizeFrame",
      "openMap",
      "revealLeaf",
      "revealLeaf",
      "foldMap",
      "finalizeFrame",
      "closeSequence",
      "foldList",
      "foldList",
      "finalizeFrame",
    ]);
    expect(
      trace.steps.every(
        ({ control }) =>
          encodeMidgardCekDataScanControlV1(control).length < 256 &&
          hashMidgardCekDataScanControlV1(control).length === 32,
      ),
    ).toBe(true);
  });

  it("reveals a large scalar as one field-bounded leaf without a value cap", () => {
    const data = new DataB(Buffer.alloc(8_800, 0x6a));
    const raw = encodeMidgardCekPlutusDataV1(data);
    const trace = buildMidgardCekDataScanTraceV1(raw);
    expect(trace.steps).toHaveLength(1);
    expect(trace.steps[0]!.step).toMatchObject({
      kind: "revealLeaf",
      itemLength: raw.length,
    });
    expect(trace.terminal.result?.cborLength).toBe(BigInt(raw.length));
  });
});
