import {
  boundedCount,
  type Bytes,
  exactOptionalHash,
  exactSummary,
  hashMidgardCekDataFrameChild,
  type MidgardCekDataFrame,
  type MidgardCekDataFrameBase,
  validateMidgardCekDataFrame,
} from "./cek-data-frame.validate-midgard-cek-data-frame.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "./cek-semantic.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  appendMidgardValidationMerkleLeaf,
  emptyMidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "./validation-merkle.js";

const initialBase = ({
  tail,
  expectedChildren,
  sequence,
}: {
  readonly tail: Bytes;
  readonly expectedChildren: number;
  readonly sequence: MidgardCekDataSequenceSummary;
}): MidgardCekDataFrameBase => ({
  tail: exactOptionalHash(tail, "cek_data_frame.tail"),
  expectedChildren: boundedCount(
    expectedChildren,
    "cek_data_frame.expected_children",
  ),
  childCount: 0,
  childFrontier: emptyMidgardValidationMerkleFrontier(),
  foldCursor: 0,
  sequence,
});

export const initialMidgardCekDataSmallConstrFrame = ({
  constructor,
  tail = Buffer.alloc(0),
  expectedChildren,
}: {
  readonly constructor: bigint;
  readonly tail?: Bytes;
  readonly expectedChildren: number;
}): MidgardCekDataFrame => {
  const frame = {
    kind: "constrSmall",
    constructor,
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataListSummary(),
    }),
  } as const;
  validateMidgardCekDataFrame(frame);
  return frame;
};

export const initialMidgardCekDataLargeConstrFrame = ({
  constructorCborRoot,
  constructorCborLength,
  constructorMemory,
  tail = Buffer.alloc(0),
  expectedChildren,
}: {
  readonly constructorCborRoot: Bytes;
  readonly constructorCborLength: bigint;
  readonly constructorMemory: bigint;
  readonly tail?: Bytes;
  readonly expectedChildren: number;
}): MidgardCekDataFrame => {
  const frame = {
    kind: "constrLarge",
    constructorCborRoot,
    constructorCborLength,
    constructorMemory,
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataListSummary(),
    }),
  } as const;
  validateMidgardCekDataFrame(frame);
  return frame;
};

export const initialMidgardCekDataListFrame = ({
  tail = Buffer.alloc(0),
  expectedChildren,
}: {
  readonly tail?: Bytes;
  readonly expectedChildren: number;
}): MidgardCekDataFrame => {
  const frame = {
    kind: "list",
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataListSummary(),
    }),
  } as const;
  validateMidgardCekDataFrame(frame);
  return frame;
};

export const initialMidgardCekDataMapFrame = ({
  tail = Buffer.alloc(0),
  expectedChildren,
}: {
  readonly tail?: Bytes;
  readonly expectedChildren: number;
}): MidgardCekDataFrame => {
  const frame = {
    kind: "map",
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataPairSummary(),
    }),
  } as const;
  validateMidgardCekDataFrame(frame);
  return frame;
};

export const appendMidgardCekDataFrameChild = (
  frame: MidgardCekDataFrame,
  child: MidgardCekDataSummary,
): MidgardCekDataFrame | null => {
  try {
    validateMidgardCekDataFrame(frame);
    exactSummary(child, "cek_data_frame_child.summary");
    if (frame.foldCursor !== 0 || frame.childCount >= frame.expectedChildren) {
      return null;
    }
    const childFrontier = appendMidgardValidationMerkleLeaf(
      frame.childFrontier,
      hashMidgardCekDataFrameChild(frame.childCount, child),
    );
    const next = {
      ...frame,
      childCount: frame.childCount + 1,
      childFrontier,
    };
    validateMidgardCekDataFrame(next);
    return next;
  } catch {
    return null;
  }
};

export const foldMidgardCekDataFrameListChild = ({
  frame,
  childIndex,
  child,
  siblings,
}: {
  readonly frame: MidgardCekDataFrame;
  readonly childIndex: number;
  readonly child: MidgardCekDataSummary;
  readonly siblings: readonly Bytes[];
}): MidgardCekDataFrame | null => {
  try {
    validateMidgardCekDataFrame(frame);
    if (
      frame.kind === "map" ||
      frame.childCount !== frame.expectedChildren ||
      frame.foldCursor >= frame.expectedChildren
    ) {
      return null;
    }
    const expectedIndex = frame.expectedChildren - frame.foldCursor - 1;
    const leafHash = hashMidgardCekDataFrameChild(childIndex, child);
    if (
      childIndex !== expectedIndex ||
      !verifyMidgardValidationMerkleMembership({
        frontier: frame.childFrontier,
        leafIndex: childIndex,
        leafHash,
        siblings: siblings.map((sibling) =>
          ensureHash32(sibling, "cek_data_frame_child.sibling"),
        ),
      })
    ) {
      return null;
    }
    const next = {
      ...frame,
      foldCursor: frame.foldCursor + 1,
      sequence: prependMidgardCekDataListSummary(child, frame.sequence),
    };
    validateMidgardCekDataFrame(next);
    return next;
  } catch {
    return null;
  }
};

export const foldMidgardCekDataFrameMapPair = ({
  frame,
  pairIndex,
  key,
  value,
  keySiblings,
  valueSiblings,
}: {
  readonly frame: MidgardCekDataFrame;
  readonly pairIndex: number;
  readonly key: MidgardCekDataSummary;
  readonly value: MidgardCekDataSummary;
  readonly keySiblings: readonly Bytes[];
  readonly valueSiblings: readonly Bytes[];
}): MidgardCekDataFrame | null => {
  try {
    validateMidgardCekDataFrame(frame);
    if (frame.kind !== "map" || frame.childCount !== frame.expectedChildren) {
      return null;
    }
    const pairCount = frame.expectedChildren / 2;
    if (frame.foldCursor >= pairCount) return null;
    const expectedPairIndex = pairCount - frame.foldCursor - 1;
    const keyIndex = pairIndex * 2;
    const valueIndex = keyIndex + 1;
    const keyLeafHash = hashMidgardCekDataFrameChild(keyIndex, key);
    const valueLeafHash = hashMidgardCekDataFrameChild(valueIndex, value);
    if (
      pairIndex !== expectedPairIndex ||
      !verifyMidgardValidationMerkleMembership({
        frontier: frame.childFrontier,
        leafIndex: keyIndex,
        leafHash: keyLeafHash,
        siblings: keySiblings.map((sibling) =>
          ensureHash32(sibling, "cek_data_frame_map.key_sibling"),
        ),
      }) ||
      !verifyMidgardValidationMerkleMembership({
        frontier: frame.childFrontier,
        leafIndex: valueIndex,
        leafHash: valueLeafHash,
        siblings: valueSiblings.map((sibling) =>
          ensureHash32(sibling, "cek_data_frame_map.value_sibling"),
        ),
      })
    ) {
      return null;
    }
    const next = {
      ...frame,
      foldCursor: frame.foldCursor + 1,
      sequence: prependMidgardCekDataPairSummary(key, value, frame.sequence),
    };
    validateMidgardCekDataFrame(next);
    return next;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataFrame = (
  frame: MidgardCekDataFrame,
): MidgardCekDataSummary | null => {
  try {
    validateMidgardCekDataFrame(frame);
    const expectedFoldCursor =
      frame.kind === "map"
        ? frame.expectedChildren / 2
        : frame.expectedChildren;
    if (
      frame.childCount !== frame.expectedChildren ||
      frame.foldCursor !== expectedFoldCursor
    ) {
      return null;
    }
    switch (frame.kind) {
      case "constrSmall":
        return summarizeMidgardCekSmallConstrData(
          frame.constructor,
          frame.sequence,
        );
      case "constrLarge":
        return summarizeMidgardCekLargeConstrData({
          constructorCborRoot: frame.constructorCborRoot,
          constructorCborLength: frame.constructorCborLength,
          constructorMemory: frame.constructorMemory,
          fields: frame.sequence,
        });
      case "list":
        return summarizeMidgardCekListData(frame.sequence);
      case "map":
        return summarizeMidgardCekMapData(frame.sequence);
    }
  } catch {
    return null;
  }
};
