import { describe, expect, it } from "vitest";

import {
  appendMidgardCekDataFrameChild,
  buildMidgardValidationMerkleMembership,
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  encodeMidgardCekDataFrame,
  finalizeMidgardCekDataFrame,
  foldMidgardCekDataFrameListChild,
  foldMidgardCekDataFrameMapPair,
  hashMidgardCekDataFrame,
  hashMidgardCekDataFrameChild,
  initialMidgardCekDataLargeConstrFrame,
  initialMidgardCekDataListFrame,
  initialMidgardCekDataMapFrame,
  initialMidgardCekDataSmallConstrFrame,
  type MidgardCekDataFrame,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "../src/index.js";

const summary = (
  fill: number,
  cborLength: bigint,
  memory: bigint,
): MidgardCekDataSummary => ({
  root: Buffer.alloc(32, fill),
  cborLength,
  memory,
});

const appendAll = (
  initial: MidgardCekDataFrame,
  children: readonly MidgardCekDataSummary[],
): MidgardCekDataFrame =>
  children.reduce((frame, child) => {
    const next = appendMidgardCekDataFrameChild(frame, child);
    expect(next).not.toBeNull();
    return next!;
  }, initial);

const childMembership = (
  children: readonly MidgardCekDataSummary[],
  childIndex: number,
) =>
  buildMidgardValidationMerkleMembership(
    children.map((child, index) => hashMidgardCekDataFrameChild(index, child)),
    childIndex,
  );

describe("authenticated CEK Data frames V1", () => {
  it("folds a large constructor without embedding its integer", () => {
    const constructorCborRoot = Buffer.alloc(32, 0xc7);
    const children = [summary(0x11, 3n, 5n), summary(0x22, 67n, 68n)];
    const initial = initialMidgardCekDataLargeConstrFrame({
      constructorCborRoot,
      constructorCborLength: 16_384n,
      constructorMemory: 16_388n,
      tail: Buffer.alloc(32, 0x42),
      expectedChildren: children.length,
    });
    let frame = appendAll(initial, children);

    for (const childIndex of [1, 0]) {
      const membership = childMembership(children, childIndex);
      frame = foldMidgardCekDataFrameListChild({
        frame,
        childIndex,
        child: children[childIndex]!,
        siblings: membership.siblings,
      })!;
      expect(frame).not.toBeNull();
    }

    const fields = prependMidgardCekDataListSummary(
      children[0]!,
      prependMidgardCekDataListSummary(
        children[1]!,
        emptyMidgardCekDataListSummary(),
      ),
    );
    expect(finalizeMidgardCekDataFrame(frame)).toStrictEqual(
      summarizeMidgardCekLargeConstrData({
        constructorCborRoot,
        constructorCborLength: 16_384n,
        constructorMemory: 16_388n,
        fields,
      }),
    );
    expect(encodeMidgardCekDataFrame(frame).length).toBeLessThan(256);
  });

  it("folds map pairs in original order through authenticated children", () => {
    const children = [
      summary(0x10, 1n, 5n),
      summary(0x11, 2n, 6n),
      summary(0x20, 3n, 7n),
      summary(0x21, 4n, 8n),
    ];
    let frame = appendAll(
      initialMidgardCekDataMapFrame({
        expectedChildren: children.length,
      }),
      children,
    );

    for (const pairIndex of [1, 0]) {
      const keyIndex = pairIndex * 2;
      const valueIndex = keyIndex + 1;
      frame = foldMidgardCekDataFrameMapPair({
        frame,
        pairIndex,
        key: children[keyIndex]!,
        value: children[valueIndex]!,
        keySiblings: childMembership(children, keyIndex).siblings,
        valueSiblings: childMembership(children, valueIndex).siblings,
      })!;
      expect(frame).not.toBeNull();
    }

    const entries = prependMidgardCekDataPairSummary(
      children[0]!,
      children[1]!,
      prependMidgardCekDataPairSummary(
        children[2]!,
        children[3]!,
        emptyMidgardCekDataPairSummary(),
      ),
    );
    expect(finalizeMidgardCekDataFrame(frame)).toStrictEqual(
      summarizeMidgardCekMapData(entries),
    );
  });

  it("finalizes exact empty and small-container summaries", () => {
    const emptyList = initialMidgardCekDataListFrame({
      expectedChildren: 0,
    });
    const emptyMap = initialMidgardCekDataMapFrame({
      expectedChildren: 0,
    });
    const emptyConstr = initialMidgardCekDataSmallConstrFrame({
      constructor: 127n,
      expectedChildren: 0,
    });

    expect(finalizeMidgardCekDataFrame(emptyList)).toStrictEqual(
      summarizeMidgardCekListData(emptyMidgardCekDataListSummary()),
    );
    expect(finalizeMidgardCekDataFrame(emptyMap)).toStrictEqual(
      summarizeMidgardCekMapData(emptyMidgardCekDataPairSummary()),
    );
    expect(finalizeMidgardCekDataFrame(emptyConstr)).toStrictEqual(
      summarizeMidgardCekSmallConstrData(
        127n,
        emptyMidgardCekDataListSummary(),
      ),
    );
  });

  it("fails closed for forged frames and wrong fold order", () => {
    const children = [summary(0x31, 1n, 5n), summary(0x32, 2n, 6n)];
    const initial = initialMidgardCekDataListFrame({
      expectedChildren: children.length,
    });
    const full = appendAll(initial, children);
    const firstMembership = childMembership(children, 0);
    const forgedEmpty = {
      ...initial,
      sequence: {
        ...initial.sequence,
        root: Buffer.alloc(32, 0xff),
      },
    };
    const mismatchedFrontier = {
      ...full,
      childCount: 1,
    };

    expect(() => encodeMidgardCekDataFrame(forgedEmpty)).toThrow(
      /exact empty sequence/u,
    );
    expect(() => encodeMidgardCekDataFrame(mismatchedFrontier)).toThrow(
      /frontier count/u,
    );
    expect(
      foldMidgardCekDataFrameListChild({
        frame: full,
        childIndex: 0,
        child: children[0]!,
        siblings: firstMembership.siblings,
      }),
    ).toBeNull();
    expect(
      appendMidgardCekDataFrameChild(initial, summary(0x44, 0n, 4n)),
    ).toBeNull();
    expect(finalizeMidgardCekDataFrame(full)).toBeNull();
  });

  it("binds the fixed-size large-constructor frame vector", () => {
    const frame = initialMidgardCekDataLargeConstrFrame({
      constructorCborRoot: Buffer.alloc(32, 0xc7),
      constructorCborLength: 16_384n,
      constructorMemory: 16_388n,
      tail: Buffer.alloc(32, 0x42),
      expectedChildren: 2,
    });

    expect(encodeMidgardCekDataFrame(frame).toString("hex")).toBe(
      `8b01005820${"c7".repeat(32)}1940001940045820${"42".repeat(32)}020080008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000`,
    );
    expect(hashMidgardCekDataFrame(frame).toString("hex")).toBe(
      "f1f01b15e143b47b513a5be7c071a57709fa88b183a9220a9a81a1307b5334db",
    );
  });
});
