import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import {
  buildMidgardCekDataScanTrace,
  encodeMidgardCekDataScanControl,
  hashMidgardCekDataScanChild,
  hashMidgardCekDataScanControl,
  hashMidgardCekDataScanFrame,
} from "../src/cek-data-scan.js";
import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";

describe("V1 content-addressed Data scanner", () => {
  it("reconstructs nested constructors, lists, and maps one bounded step at a time", () => {
    const data = new DataConstr(0n, [
      new DataList([new DataI(1n), new DataI(2n)]),
      new DataMap([
        new DataPair(new DataI(3n), new DataB(Buffer.from("abcd", "hex"))),
      ]),
    ]);
    const raw = encodeMidgardCekPlutusData(data);
    const trace = buildMidgardCekDataScanTrace(raw);
    const expected = commitMidgardCekDataTree(data);

    expect(trace.initial.offset).toBe(0);
    expect(trace.terminal.offset).toBe(raw.length);
    expect(trace.terminal.result).toEqual({
      root: Buffer.from(expected.root),
      cborLength: expected.cborLength,
      memory: expected.memory,
    });
    expect(hashMidgardCekDataScanControl(trace.initial).toString("hex")).toBe(
      "6b5258f74c54a3932194e3087c5ce5652fb1bbffb042b71572aab47bea7d07e4",
    );
    expect(hashMidgardCekDataScanControl(trace.terminal).toString("hex")).toBe(
      "865b9c51d15222a3b33dbb422e212aa302d37e4a791268c0f22a8ff6b0a538fa",
    );
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
          encodeMidgardCekDataScanControl(control).length < 256 &&
          hashMidgardCekDataScanControl(control).length === 32,
      ),
    ).toBe(true);
  });

  it("reveals a large scalar as one field-bounded leaf without a value cap", () => {
    const data = new DataB(Buffer.alloc(8_800, 0x6a));
    const raw = encodeMidgardCekPlutusData(data);
    const trace = buildMidgardCekDataScanTrace(raw);
    expect(trace.steps).toHaveLength(1);
    expect(trace.steps[0]!.step).toMatchObject({
      kind: "revealLeaf",
      itemLength: raw.length,
    });
    expect(trace.terminal.result?.cborLength).toBe(BigInt(raw.length));
  });

  it("summarizes large constructor alternatives after folding their fields", () => {
    const data = new DataConstr(128n, [new DataI(1n)]);
    const raw = encodeMidgardCekPlutusData(data);
    const trace = buildMidgardCekDataScanTrace(raw);
    const expected = commitMidgardCekDataTree(data);

    expect(trace.steps.map(({ step }) => step.kind)).toEqual([
      "openConstructor",
      "revealLeaf",
      "closeSequence",
      "foldList",
      "finalizeFrame",
    ]);
    expect(trace.terminal.result).toEqual({
      root: Buffer.from(expected.root),
      cborLength: expected.cborLength,
      memory: expected.memory,
    });
  });

  it("rejects malformed controls, frames, and child summaries before hashing", () => {
    const data = new DataList([new DataI(1n)]);
    const trace = buildMidgardCekDataScanTrace(
      encodeMidgardCekPlutusData(data),
    );
    expect(() =>
      encodeMidgardCekDataScanControl({
        ...trace.initial,
        rawHash: Buffer.alloc(31),
      }),
    ).toThrow(/exactly 32 bytes/u);
    expect(() =>
      encodeMidgardCekDataScanControl({
        ...trace.terminal,
        offset: trace.terminal.offset - 1,
      }),
    ).toThrow(/canonical terminal state/u);

    const frameStep = trace.steps.find(
      ({ step }) => step.kind === "closeSequence",
    );
    const frame =
      frameStep?.step.kind === "closeSequence"
        ? frameStep.step.frame
        : undefined;
    expect(frame).toBeDefined();
    if (frame === undefined) return;
    expect(() =>
      hashMidgardCekDataScanFrame({
        ...frame,
        childCount: frame.childCount + 1,
      }),
    ).toThrow(/child_count|authenticated frontier/u);
    expect(() =>
      hashMidgardCekDataScanChild(0, {
        root: Buffer.alloc(31),
        cborLength: 1n,
        memory: 1n,
      }),
    ).toThrow(/exactly 32 bytes/u);
  });
});
