import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekDataTraverse,
  appendMidgardCekDataFrameChild,
  buildMidgardCekDataTraverseTrace,
  buildMidgardValidationMerkleMembership,
  encodeCborBytes,
  encodeCborInteger,
  encodeCborMapRaw,
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataBytes,
  finalizeMidgardCekDataFrame,
  finalizeMidgardCekDataInteger,
  finalizeMidgardCekDataTraverse,
  finalizeMidgardCekSourceBlob,
  foldMidgardCekDataFrameListChild,
  hashMidgardCekDataFrame,
  hashMidgardCekDataFrameChild,
  hashMidgardCekDataTraverseControl,
  initialMidgardCekDataLargeConstrFrame,
  initialMidgardCekDataListFrame,
  initialMidgardCekDataSmallConstrFrame,
  initialMidgardCekDataTraverseControl,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  type MidgardCekDataFrame,
  type MidgardCekDataSummary,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
  nextMidgardCekDataTraverseSpan,
} from "../src/index.js";

type Harness = {
  control: MidgardCekDataTraverseControl;
  readonly source: Buffer;
  readonly sourceStart: number;
  readonly reveals: Buffer[];
};

const transition = (
  harness: Harness,
  action: MidgardCekDataTraverseAction,
): void => {
  const span = nextMidgardCekDataTraverseSpan(harness.control);
  const sourceBytes =
    span === null
      ? null
      : harness.source.subarray(
          span.absoluteStart - harness.sourceStart,
          span.absoluteStart - harness.sourceStart + span.length,
        );
  if (sourceBytes !== null) {
    harness.reveals.push(Buffer.from(sourceBytes));
  }
  const next = advanceMidgardCekDataTraverse({
    control: harness.control,
    sourceBytes,
    action,
  });
  expect(next).not.toBeNull();
  harness.control = next!;
};

const scalarSummary = (
  control: MidgardCekDataTraverseControl,
): MidgardCekDataSummary => {
  const summary =
    control.integer !== null
      ? finalizeMidgardCekDataInteger(control.integer)
      : finalizeMidgardCekDataBytes(control.bytes!);
  expect(summary).not.toBeNull();
  return summary!;
};

const finishScalar = (
  harness: Harness,
  parent: MidgardCekDataFrame | null,
): MidgardCekDataSummary => {
  while (
    (harness.control.stage === MidgardCekDataTraverseStages.Integer &&
      harness.control.integer!.stage !== 2) ||
    (harness.control.stage === MidgardCekDataTraverseStages.Bytes &&
      harness.control.bytes!.stage !== 3)
  ) {
    transition(harness, null);
  }
  const summary = scalarSummary(harness.control);
  transition(harness, { kind: "attachScalar", parent });
  return summary;
};

const appendChild = (
  frame: MidgardCekDataFrame,
  child: MidgardCekDataSummary,
): MidgardCekDataFrame => {
  const next = appendMidgardCekDataFrameChild(frame, child);
  expect(next).not.toBeNull();
  return next!;
};

const foldList = (
  harness: Harness,
  initial: MidgardCekDataFrame,
  children: readonly MidgardCekDataSummary[],
): MidgardCekDataFrame => {
  const leaves = children.map((child, index) =>
    hashMidgardCekDataFrameChild(index, child),
  );
  let frame = initial;
  for (let childIndex = children.length - 1; childIndex >= 0; childIndex -= 1) {
    const membership = buildMidgardValidationMerkleMembership(
      leaves,
      childIndex,
    );
    transition(harness, {
      kind: "foldList",
      frame,
      childIndex,
      child: children[childIndex]!,
      siblings: membership.siblings,
    });
    frame = foldMidgardCekDataFrameListChild({
      frame,
      childIndex,
      child: children[childIndex]!,
      siblings: membership.siblings,
    })!;
    expect(frame).not.toBeNull();
  }
  return frame;
};

const harness = (source: Uint8Array, sourceStart = 17): Harness => ({
  control: initialMidgardCekDataTraverseControl({
    sourceStart,
    sourceLength: source.length,
  }),
  source: Buffer.from(source),
  sourceStart,
  reveals: [],
});

const encodeCardanoDataBytes = (content: Uint8Array): Buffer => {
  const bytes = Buffer.from(content);
  if (bytes.length <= 64) return encodeCborBytes(bytes);
  const chunks: Buffer[] = [];
  for (let offset = 0; offset < bytes.length; offset += 64) {
    chunks.push(encodeCborBytes(bytes.subarray(offset, offset + 64)));
  }
  return Buffer.concat([Buffer.from([0x5f]), ...chunks, Buffer.from([0xff])]);
};

describe("authenticated CEK Data traversal V1", () => {
  it("streams a maximum-transaction-sized scalar root", () => {
    const magnitude = Buffer.alloc(16_380);
    magnitude[0] = 1;
    const source = Buffer.concat([
      Buffer.from([0xc2, 0x59, 0x3f, 0xfc]),
      magnitude,
    ]);
    const trace = harness(source);

    transition(trace, {
      kind: "headScalar",
      itemLength: source.length,
    });
    const summary = finishScalar(trace, null);

    expect(finalizeMidgardCekDataTraverse(trace.control)).toStrictEqual(
      summary,
    );
    expect(
      Math.max(...trace.reveals.map((reveal) => reveal.length)),
    ).toBeLessThanOrEqual(MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN);
  });

  it("authenticates list heads, children, closure, folds, and finalization", () => {
    const bytes = encodeCardanoDataBytes(Buffer.alloc(65, 0x6a));
    const source = Buffer.concat([
      Buffer.from([0x9f, 0x01]),
      bytes,
      Buffer.from([0xff]),
    ]);
    const trace = harness(source);
    let frame = initialMidgardCekDataListFrame({
      expectedChildren: 2,
    });
    const children: MidgardCekDataSummary[] = [];

    transition(trace, {
      kind: "headSequence",
      expectedChildren: 2,
    });
    transition(trace, { kind: "headScalar", itemLength: 1 });
    const integer = finishScalar(trace, frame);
    children.push(integer);
    frame = appendChild(frame, integer);
    transition(trace, {
      kind: "headScalar",
      itemLength: bytes.length,
    });
    const byteSummary = finishScalar(trace, frame);
    children.push(byteSummary);
    frame = appendChild(frame, byteSummary);
    transition(trace, null);
    frame = foldList(trace, frame, children);
    const expected = finalizeMidgardCekDataFrame(frame)!;
    transition(trace, {
      kind: "finalizeFrame",
      frame,
      parent: null,
    });

    expect(finalizeMidgardCekDataTraverse(trace.control)).toStrictEqual(
      expected,
    );
    expect(trace.control.offset).toBe(source.length);
  });

  it("pops an authenticated child container into its parent", () => {
    const source = Buffer.from("d8799f80ff", "hex");
    const trace = harness(source);
    let parent = initialMidgardCekDataSmallConstrFrame({
      constructor: 0n,
      expectedChildren: 1,
    });

    transition(trace, {
      kind: "headSequence",
      expectedChildren: 1,
    });
    const child = initialMidgardCekDataListFrame({
      tail: hashMidgardCekDataFrame(parent),
      expectedChildren: 0,
    });
    transition(trace, {
      kind: "headSequence",
      expectedChildren: 0,
    });
    const childSummary = finalizeMidgardCekDataFrame(child)!;
    transition(trace, {
      kind: "finalizeFrame",
      frame: child,
      parent,
    });
    parent = appendChild(parent, childSummary);
    transition(trace, null);
    parent = foldList(trace, parent, [childSummary]);
    const expected = finalizeMidgardCekDataFrame(parent)!;
    transition(trace, {
      kind: "finalizeFrame",
      frame: parent,
      parent: null,
    });

    expect(finalizeMidgardCekDataTraverse(trace.control)).toStrictEqual(
      expected,
    );
  });

  it("streams a large constructor alternative before its fields", () => {
    const constructorCbor = Buffer.from("c249010000000000000000", "hex");
    const source = Buffer.concat([
      Buffer.from("d86682", "hex"),
      constructorCbor,
      Buffer.from("9f01ff", "hex"),
    ]);
    const trace = harness(source);

    transition(trace, {
      kind: "headLargeConstructor",
      constructorCborLength: constructorCbor.length,
      expectedChildren: 1,
    });
    while (
      trace.control.stage === MidgardCekDataTraverseStages.LargeConstructor
    ) {
      transition(trace, null);
    }
    const integer = trace.control.integer!;
    const constructorCborRoot = finalizeMidgardCekSourceBlob(integer.blob!)!;
    let frame = initialMidgardCekDataLargeConstrFrame({
      constructorCborRoot,
      constructorCborLength: BigInt(constructorCbor.length),
      constructorMemory: integer.memory,
      expectedChildren: 1,
    });
    transition(trace, null);
    transition(trace, { kind: "headScalar", itemLength: 1 });
    const field = finishScalar(trace, frame);
    frame = appendChild(frame, field);
    transition(trace, null);
    frame = foldList(trace, frame, [field]);
    const expected = finalizeMidgardCekDataFrame(frame)!;
    transition(trace, {
      kind: "finalizeFrame",
      frame,
      parent: null,
    });

    expect(finalizeMidgardCekDataTraverse(trace.control)).toStrictEqual(
      expected,
    );
    expect(
      encodeMidgardCekDataTraverseControl(trace.control).toString("hex"),
    ).toBe(
      "8a010711111140d87a80d87a80d87a80d8799f835820844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c441109ff",
    );
    expect(
      hashMidgardCekDataTraverseControl(trace.control).toString("hex"),
    ).toBe("173ab9eb57665546414d5c286c55bc1fcd939a1784a7b800863e9970b82f6c16");
  });

  it("pins active nested integer and byte controls for Aiken decoding", () => {
    const integer = harness(Buffer.from("c249010000000000000000", "hex"));
    transition(integer, {
      kind: "headScalar",
      itemLength: integer.source.length,
    });
    expect(
      encodeMidgardCekDataTraverseControl(integer.control).toString("hex"),
    ).toBe("8a0101110b0040d87a80d8799f860100110b00d87a80ffd87a80d87a80");

    const bytes = harness(encodeCardanoDataBytes(Buffer.alloc(65, 0x6a)));
    transition(bytes, {
      kind: "headScalar",
      itemLength: bytes.source.length,
    });
    expect(
      encodeMidgardCekDataTraverseControl(bytes.control).toString("hex"),
    ).toBe("8a01021118460040d87a80d87a80d8799f86010011184600d87a80ffd87a80");
  });

  it("fails closed for wrong counts, trailing bytes, and small large constructors", () => {
    const wrongCount = harness(Buffer.from("9f01ff", "hex"));
    transition(wrongCount, {
      kind: "headSequence",
      expectedChildren: 2,
    });
    transition(wrongCount, {
      kind: "headScalar",
      itemLength: 1,
    });
    let frame = initialMidgardCekDataListFrame({
      expectedChildren: 2,
    });
    const child = finishScalar(wrongCount, frame);
    frame = appendChild(frame, child);
    const closeWindow = nextMidgardCekDataTraverseSpan(wrongCount.control)!;

    expect(closeWindow.length).toBe(1);
    expect(
      advanceMidgardCekDataTraverse({
        control: wrongCount.control,
        sourceBytes: Buffer.from([0xff]),
        action: { kind: "headScalar", itemLength: 1 },
      }),
    ).toBeNull();

    const trailing = harness(Buffer.from("0102", "hex"));
    transition(trailing, { kind: "headScalar", itemLength: 1 });
    while (trailing.control.integer!.stage !== 2) {
      transition(trailing, null);
    }
    expect(
      advanceMidgardCekDataTraverse({
        control: trailing.control,
        sourceBytes: null,
        action: { kind: "attachScalar", parent: null },
      }),
    ).toBeNull();

    const smallLarge = harness(Buffer.from("d86682187f80", "hex"));
    transition(smallLarge, {
      kind: "headLargeConstructor",
      constructorCborLength: 2,
      expectedChildren: 0,
    });
    expect(
      advanceMidgardCekDataTraverse({
        control: smallLarge.control,
        sourceBytes: Buffer.from("187f", "hex"),
        action: null,
      }),
    ).toBeNull();
  });

  it("automatically constructs every reveal and witness for nested canonical Data", () => {
    const byteLeaf = encodeCardanoDataBytes(Buffer.alloc(65, 0x71));
    const source = Buffer.concat([
      Buffer.from("a2019fd87980ff", "hex"),
      byteLeaf,
      Buffer.from("d8668218809f00ff", "hex"),
    ]);
    const trace = buildMidgardCekDataTraverseTrace({
      sourceStart: 31,
      source,
    });
    const summary = finalizeMidgardCekDataTraverse(trace.terminal);

    expect(summary).not.toBeNull();
    expect(trace.terminal.offset).toBe(source.length);
    expect(
      Math.max(...trace.steps.map((step) => step.sourceBytes?.length ?? 0)),
    ).toBeLessThanOrEqual(MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN);
    expect(trace.steps.map((step) => step.action?.kind)).toContain("foldMap");
    expect(trace.steps.map((step) => step.action?.kind)).toContain(
      "headLargeConstructor",
    );
    for (const step of trace.steps) {
      expect(
        advanceMidgardCekDataTraverse({
          control: step.control,
          sourceBytes: step.sourceBytes,
          action: step.action,
        }),
      ).toStrictEqual(step.next);
    }
  });

  it("reuses exact membership paths across broad nested list and map frames", () => {
    const listChildCount = 65;
    const mapPairCount = 33;
    const broadList = Buffer.concat([
      Buffer.from([0x9f]),
      ...Array.from({ length: listChildCount }, (_, index) =>
        encodeCborInteger(BigInt(index)),
      ),
      Buffer.from([0xff]),
    ]);
    const broadMap = encodeCborMapRaw(
      Array.from({ length: mapPairCount }, (_, index) => [
        encodeCborInteger(BigInt(index + 100)),
        encodeCborInteger(BigInt(index + 1_000)),
      ]),
    );
    const source = Buffer.concat([
      Buffer.from([0x9f]),
      broadList,
      broadMap,
      Buffer.from([0xff]),
    ]);
    const trace = buildMidgardCekDataTraverseTrace({
      sourceStart: 43,
      source,
    });
    const listFolds = trace.steps.flatMap(({ action }) =>
      action?.kind === "foldList" ? [action] : [],
    );
    const mapFolds = trace.steps.flatMap(({ action }) =>
      action?.kind === "foldMap" ? [action] : [],
    );

    expect(listFolds.map(({ childIndex }) => childIndex)).toStrictEqual([
      ...Array.from(
        { length: listChildCount },
        (_, index) => listChildCount - index - 1,
      ),
      1,
      0,
    ]);
    expect(mapFolds.map(({ pairIndex }) => pairIndex)).toStrictEqual(
      Array.from(
        { length: mapPairCount },
        (_, index) => mapPairCount - index - 1,
      ),
    );

    const listChildren = new Array<MidgardCekDataSummary>(listChildCount);
    for (const action of listFolds.slice(0, listChildCount)) {
      listChildren[action.childIndex] = action.child;
    }
    const listLeaves = listChildren.map((child, index) =>
      hashMidgardCekDataFrameChild(index, child),
    );
    for (const action of listFolds.slice(0, listChildCount)) {
      expect(action.siblings).toStrictEqual(
        buildMidgardValidationMerkleMembership(listLeaves, action.childIndex)
          .siblings,
      );
    }

    const mapChildren = new Array<MidgardCekDataSummary>(mapPairCount * 2);
    for (const action of mapFolds) {
      mapChildren[action.pairIndex * 2] = action.key;
      mapChildren[action.pairIndex * 2 + 1] = action.value;
    }
    const mapLeaves = mapChildren.map((child, index) =>
      hashMidgardCekDataFrameChild(index, child),
    );
    for (const action of mapFolds) {
      expect(action.keySiblings).toStrictEqual(
        buildMidgardValidationMerkleMembership(mapLeaves, action.pairIndex * 2)
          .siblings,
      );
      expect(action.valueSiblings).toStrictEqual(
        buildMidgardValidationMerkleMembership(
          mapLeaves,
          action.pairIndex * 2 + 1,
        ).siblings,
      );
    }

    for (const step of trace.steps) {
      expect(
        advanceMidgardCekDataTraverse({
          control: step.control,
          sourceBytes: step.sourceBytes,
          action: step.action,
        }),
      ).toStrictEqual(step.next);
    }
    expect(finalizeMidgardCekDataTraverse(trace.terminal)).not.toBeNull();
  });

  it("constructs deeply nested evidence without a JavaScript call-stack limit", () => {
    const depth = 3_000;
    const source = Buffer.concat([
      Buffer.from("d8799f".repeat(depth), "hex"),
      Buffer.from([0x01]),
      Buffer.from("ff".repeat(depth), "hex"),
    ]);
    const trace = buildMidgardCekDataTraverseTrace({
      sourceStart: 0,
      source,
    });

    expect(source.length).toBe(12_001);
    expect(finalizeMidgardCekDataTraverse(trace.terminal)).not.toBeNull();
  });

  it("makes the automatic constructor reject noncanonical and trailing bytes", () => {
    const malformed = ["8101", "9fff", "d86682187f80", "0102"];
    for (const source of malformed) {
      expect(() =>
        buildMidgardCekDataTraverseTrace({
          sourceStart: 0,
          source: Buffer.from(source, "hex"),
        }),
      ).toThrow();
    }
  });
});
