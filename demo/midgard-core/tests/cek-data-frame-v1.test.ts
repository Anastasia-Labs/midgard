import { describe, expect, it } from "vitest";

import {
  appendMidgardCekDataFrameChildV1,
  buildMidgardValidationMerkleMembershipV1,
  emptyMidgardCekDataListSummaryV1,
  emptyMidgardCekDataPairSummaryV1,
  encodeMidgardCekDataFrameV1,
  finalizeMidgardCekDataFrameV1,
  foldMidgardCekDataFrameListChildV1,
  foldMidgardCekDataFrameMapPairV1,
  hashMidgardCekDataFrameChildV1,
  hashMidgardCekDataFrameV1,
  initialMidgardCekDataLargeConstrFrameV1,
  initialMidgardCekDataListFrameV1,
  initialMidgardCekDataMapFrameV1,
  initialMidgardCekDataSmallConstrFrameV1,
  type MidgardCekDataFrameV1,
  type MidgardCekDataSummaryV1,
  prependMidgardCekDataListSummaryV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekLargeConstrDataV1,
  summarizeMidgardCekListDataV1,
  summarizeMidgardCekMapDataV1,
  summarizeMidgardCekSmallConstrDataV1,
} from "../src/index.js";

const summary = (
  fill: number,
  cborLength: bigint,
  memory: bigint,
): MidgardCekDataSummaryV1 => ({
  root: Buffer.alloc(32, fill),
  cborLength,
  memory,
});

const appendAll = (
  initial: MidgardCekDataFrameV1,
  children: readonly MidgardCekDataSummaryV1[],
): MidgardCekDataFrameV1 =>
  children.reduce((frame, child) => {
    const next = appendMidgardCekDataFrameChildV1(frame, child);
    expect(next).not.toBeNull();
    return next!;
  }, initial);

const childMembership = (
  children: readonly MidgardCekDataSummaryV1[],
  childIndex: number,
) =>
  buildMidgardValidationMerkleMembershipV1(
    children.map((child, index) =>
      hashMidgardCekDataFrameChildV1(index, child),
    ),
    childIndex,
  );

describe("authenticated CEK Data frames V1", () => {
  it("folds a large constructor without embedding its integer", () => {
    const constructorCborRoot = Buffer.alloc(32, 0xc7);
    const children = [summary(0x11, 3n, 5n), summary(0x22, 67n, 68n)];
    const initial = initialMidgardCekDataLargeConstrFrameV1({
      constructorCborRoot,
      constructorCborLength: 16_384n,
      constructorMemory: 16_388n,
      tail: Buffer.alloc(32, 0x42),
      expectedChildren: children.length,
    });
    let frame = appendAll(initial, children);

    for (const childIndex of [1, 0]) {
      const membership = childMembership(children, childIndex);
      frame = foldMidgardCekDataFrameListChildV1({
        frame,
        childIndex,
        child: children[childIndex]!,
        siblings: membership.siblings,
      })!;
      expect(frame).not.toBeNull();
    }

    const fields = prependMidgardCekDataListSummaryV1(
      children[0]!,
      prependMidgardCekDataListSummaryV1(
        children[1]!,
        emptyMidgardCekDataListSummaryV1(),
      ),
    );
    expect(finalizeMidgardCekDataFrameV1(frame)).toStrictEqual(
      summarizeMidgardCekLargeConstrDataV1({
        constructorCborRoot,
        constructorCborLength: 16_384n,
        constructorMemory: 16_388n,
        fields,
      }),
    );
    expect(encodeMidgardCekDataFrameV1(frame).length).toBeLessThan(256);
  });

  it("folds map pairs in original order through authenticated children", () => {
    const children = [
      summary(0x10, 1n, 5n),
      summary(0x11, 2n, 6n),
      summary(0x20, 3n, 7n),
      summary(0x21, 4n, 8n),
    ];
    let frame = appendAll(
      initialMidgardCekDataMapFrameV1({
        expectedChildren: children.length,
      }),
      children,
    );

    for (const pairIndex of [1, 0]) {
      const keyIndex = pairIndex * 2;
      const valueIndex = keyIndex + 1;
      frame = foldMidgardCekDataFrameMapPairV1({
        frame,
        pairIndex,
        key: children[keyIndex]!,
        value: children[valueIndex]!,
        keySiblings: childMembership(children, keyIndex).siblings,
        valueSiblings: childMembership(children, valueIndex).siblings,
      })!;
      expect(frame).not.toBeNull();
    }

    const entries = prependMidgardCekDataPairSummaryV1(
      children[0]!,
      children[1]!,
      prependMidgardCekDataPairSummaryV1(
        children[2]!,
        children[3]!,
        emptyMidgardCekDataPairSummaryV1(),
      ),
    );
    expect(finalizeMidgardCekDataFrameV1(frame)).toStrictEqual(
      summarizeMidgardCekMapDataV1(entries),
    );
  });

  it("finalizes exact empty and small-container summaries", () => {
    const emptyList = initialMidgardCekDataListFrameV1({
      expectedChildren: 0,
    });
    const emptyMap = initialMidgardCekDataMapFrameV1({
      expectedChildren: 0,
    });
    const emptyConstr = initialMidgardCekDataSmallConstrFrameV1({
      constructor: 127n,
      expectedChildren: 0,
    });

    expect(finalizeMidgardCekDataFrameV1(emptyList)).toStrictEqual(
      summarizeMidgardCekListDataV1(emptyMidgardCekDataListSummaryV1()),
    );
    expect(finalizeMidgardCekDataFrameV1(emptyMap)).toStrictEqual(
      summarizeMidgardCekMapDataV1(emptyMidgardCekDataPairSummaryV1()),
    );
    expect(finalizeMidgardCekDataFrameV1(emptyConstr)).toStrictEqual(
      summarizeMidgardCekSmallConstrDataV1(
        127n,
        emptyMidgardCekDataListSummaryV1(),
      ),
    );
  });

  it("fails closed for forged frames and wrong fold order", () => {
    const children = [summary(0x31, 1n, 5n), summary(0x32, 2n, 6n)];
    const initial = initialMidgardCekDataListFrameV1({
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

    expect(() => encodeMidgardCekDataFrameV1(forgedEmpty)).toThrow(
      /exact empty sequence/u,
    );
    expect(() => encodeMidgardCekDataFrameV1(mismatchedFrontier)).toThrow(
      /frontier count/u,
    );
    expect(
      foldMidgardCekDataFrameListChildV1({
        frame: full,
        childIndex: 0,
        child: children[0]!,
        siblings: firstMembership.siblings,
      }),
    ).toBeNull();
    expect(
      appendMidgardCekDataFrameChildV1(initial, summary(0x44, 0n, 4n)),
    ).toBeNull();
    expect(finalizeMidgardCekDataFrameV1(full)).toBeNull();
  });

  it("binds the fixed-size large-constructor frame vector", () => {
    const frame = initialMidgardCekDataLargeConstrFrameV1({
      constructorCborRoot: Buffer.alloc(32, 0xc7),
      constructorCborLength: 16_384n,
      constructorMemory: 16_388n,
      tail: Buffer.alloc(32, 0x42),
      expectedChildren: 2,
    });

    expect(encodeMidgardCekDataFrameV1(frame).toString("hex")).toBe(
      `8b01005820${"c7".repeat(32)}1940001940045820${"42".repeat(32)}020080008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000`,
    );
    expect(hashMidgardCekDataFrameV1(frame).toString("hex")).toBe(
      "f1f01b15e143b47b513a5be7c071a57709fa88b183a9220a9a81a1307b5334db",
    );
  });
});
