import { blake2b } from "@noble/hashes/blake2.js";

import {
  emptyMidgardCekDataListSummaryV1,
  emptyMidgardCekDataPairSummaryV1,
  type MidgardCekDataSequenceSummaryV1,
  type MidgardCekDataSummaryV1,
  prependMidgardCekDataListSummaryV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekLargeConstrDataV1,
  summarizeMidgardCekListDataV1,
  summarizeMidgardCekMapDataV1,
  summarizeMidgardCekSmallConstrDataV1,
} from "./cek-semantic.js";
import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  appendMidgardValidationMerkleLeafV1,
  emptyMidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleFrontierV1,
  validateMidgardValidationMerkleFrontierV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "./validation-merkle.js";

const FRAME_DOMAIN = Buffer.from("MidgardCekDataFrameV1", "ascii");
const CHILD_DOMAIN = Buffer.from("MidgardCekDataFrameChildV1", "ascii");

const UINT32_MAX = 0xffff_ffffn;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

type Bytes = Uint8Array;

type MidgardCekDataFrameBaseV1 = {
  readonly tail: Bytes;
  readonly expectedChildren: number;
  readonly childCount: number;
  readonly childFrontier: MidgardValidationMerkleFrontierV1;
  readonly foldCursor: number;
  readonly sequence: MidgardCekDataSequenceSummaryV1;
};

export type MidgardCekDataFrameV1 =
  | (MidgardCekDataFrameBaseV1 & {
      readonly kind: "constrSmall";
      readonly constructor: bigint;
    })
  | (MidgardCekDataFrameBaseV1 & {
      readonly kind: "constrLarge";
      readonly constructorCborRoot: Bytes;
      readonly constructorCborLength: bigint;
      readonly constructorMemory: bigint;
    })
  | (MidgardCekDataFrameBaseV1 & {
      readonly kind: "list";
    })
  | (MidgardCekDataFrameBaseV1 & {
      readonly kind: "map";
    });

export const MidgardCekDataFrameTagsV1 = Object.freeze({
  ConstrSmall: 0n,
  ConstrLarge: 1n,
  List: 2n,
  Map: 3n,
} as const);

const hash32 = (domain: Bytes, preimage: Bytes): Hash32 =>
  ensureHash32(
    blake2b(Buffer.concat([Buffer.from(domain), Buffer.from(preimage)]), {
      dkLen: 32,
    }),
    "cek_data_frame_hash",
  );

const boundedBigInt = (
  value: bigint,
  maximum: bigint,
  fieldName: string,
): bigint => {
  if (value < 0n || value > maximum) {
    throw new RangeError(
      `${fieldName} must be between 0 and ${maximum.toString(10)}`,
    );
  }
  return value;
};

const boundedCount = (value: number, fieldName: string): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > Number(UINT32_MAX)) {
    throw new RangeError(`${fieldName} must fit uint32`);
  }
  return value;
};

const exactOptionalHash = (value: Bytes, fieldName: string): Buffer => {
  if (value.length === 0) return Buffer.alloc(0);
  return Buffer.from(ensureHash32(value, fieldName));
};

const exactSummary = (
  summary: MidgardCekDataSummaryV1,
  fieldName: string,
): void => {
  ensureHash32(summary.root, `${fieldName}.root`);
  boundedBigInt(summary.cborLength, UINT64_MAX, `${fieldName}.cbor_length`);
  boundedBigInt(summary.memory, UINT64_MAX, `${fieldName}.memory`);
  if (summary.cborLength === 0n || summary.memory < 4n) {
    throw new RangeError(
      `${fieldName} must describe a nonempty canonical Data item`,
    );
  }
};

const frameTag = (frame: MidgardCekDataFrameV1): bigint => {
  switch (frame.kind) {
    case "constrSmall":
      return MidgardCekDataFrameTagsV1.ConstrSmall;
    case "constrLarge":
      return MidgardCekDataFrameTagsV1.ConstrLarge;
    case "list":
      return MidgardCekDataFrameTagsV1.List;
    case "map":
      return MidgardCekDataFrameTagsV1.Map;
  }
};

const constructorFields = (
  frame: MidgardCekDataFrameV1,
): readonly [bigint, Buffer, bigint, bigint] => {
  switch (frame.kind) {
    case "constrSmall":
      return [frame.constructor, Buffer.alloc(0), 0n, 0n];
    case "constrLarge":
      return [
        0n,
        Buffer.from(
          ensureHash32(
            frame.constructorCborRoot,
            "cek_data_frame.constructor_cbor_root",
          ),
        ),
        frame.constructorCborLength,
        frame.constructorMemory,
      ];
    case "list":
    case "map":
      return [0n, Buffer.alloc(0), 0n, 0n];
  }
};

const expectedEmptySequence = (
  frame: MidgardCekDataFrameV1,
): MidgardCekDataSequenceSummaryV1 =>
  frame.kind === "map"
    ? emptyMidgardCekDataPairSummaryV1()
    : emptyMidgardCekDataListSummaryV1();

export const validateMidgardCekDataFrameV1 = (
  frame: MidgardCekDataFrameV1,
): void => {
  const expectedChildren = boundedCount(
    frame.expectedChildren,
    "cek_data_frame.expected_children",
  );
  const childCount = boundedCount(
    frame.childCount,
    "cek_data_frame.child_count",
  );
  const foldCursor = boundedCount(
    frame.foldCursor,
    "cek_data_frame.fold_cursor",
  );
  exactOptionalHash(frame.tail, "cek_data_frame.tail");
  if (frame.kind === "constrSmall") {
    if (frame.constructor < 0n || frame.constructor > 127n) {
      throw new RangeError(
        "cek_data_frame.constructor must be between 0 and 127",
      );
    }
  } else if (frame.kind === "constrLarge") {
    ensureHash32(
      frame.constructorCborRoot,
      "cek_data_frame.constructor_cbor_root",
    );
    boundedBigInt(
      frame.constructorCborLength,
      UINT32_MAX,
      "cek_data_frame.constructor_cbor_length",
    );
    boundedBigInt(
      frame.constructorMemory,
      UINT64_MAX,
      "cek_data_frame.constructor_memory",
    );
    if (frame.constructorCborLength === 0n || frame.constructorMemory < 5n) {
      throw new RangeError("large-constructor frame summary is not canonical");
    }
  }
  if (frame.kind === "map" && expectedChildren % 2 !== 0) {
    throw new RangeError(
      "cek_data_frame.expected_children must contain complete map pairs",
    );
  }
  if (childCount > expectedChildren) {
    throw new RangeError(
      "cek_data_frame.child_count exceeds expected_children",
    );
  }
  if (frame.childFrontier.count !== childCount) {
    throw new Error("cek_data_frame frontier count does not match child_count");
  }
  validateMidgardValidationMerkleFrontierV1(frame.childFrontier);
  const maximumFoldCursor =
    frame.kind === "map" ? expectedChildren / 2 : expectedChildren;
  if (foldCursor > maximumFoldCursor) {
    throw new RangeError(
      "cek_data_frame.fold_cursor exceeds its sequence length",
    );
  }
  ensureHash32(frame.sequence.root, "cek_data_frame.sequence.root");
  boundedBigInt(
    frame.sequence.length,
    UINT32_MAX,
    "cek_data_frame.sequence.length",
  );
  boundedBigInt(
    frame.sequence.payloadCborLength,
    UINT64_MAX,
    "cek_data_frame.sequence.payload_cbor_length",
  );
  boundedBigInt(
    frame.sequence.memory,
    UINT64_MAX,
    "cek_data_frame.sequence.memory",
  );
  if (frame.sequence.length !== BigInt(foldCursor)) {
    throw new Error(
      "cek_data_frame sequence length does not match fold_cursor",
    );
  }
  if (foldCursor === 0) {
    const empty = expectedEmptySequence(frame);
    if (
      !Buffer.from(frame.sequence.root).equals(empty.root) ||
      frame.sequence.payloadCborLength !== 0n ||
      frame.sequence.memory !== 0n
    ) {
      throw new Error(
        "cek_data_frame zero cursor must use the exact empty sequence",
      );
    }
  } else if (childCount !== expectedChildren) {
    throw new Error(
      "cek_data_frame cannot fold before all children are committed",
    );
  }
};

export const encodeMidgardCekDataFrameV1 = (
  frame: MidgardCekDataFrameV1,
): Buffer => {
  validateMidgardCekDataFrameV1(frame);
  const [
    constructor,
    constructorCborRoot,
    constructorCborLength,
    constructorMemory,
  ] = constructorFields(frame);
  return encodeCbor([
    frameTag(frame),
    constructor,
    constructorCborRoot,
    constructorCborLength,
    constructorMemory,
    exactOptionalHash(frame.tail, "cek_data_frame.tail"),
    BigInt(frame.expectedChildren),
    BigInt(frame.childCount),
    frame.childFrontier.peaks.map((peak) => [
      BigInt(peak.height),
      Buffer.from(peak.hash),
    ]),
    BigInt(frame.foldCursor),
    [
      Buffer.from(frame.sequence.root),
      frame.sequence.length,
      frame.sequence.payloadCborLength,
      frame.sequence.memory,
    ],
  ]);
};

export const hashMidgardCekDataFrameV1 = (
  frame: MidgardCekDataFrameV1,
): Hash32 => hash32(FRAME_DOMAIN, encodeMidgardCekDataFrameV1(frame));

export const hashMidgardCekDataFrameChildV1 = (
  childIndex: number,
  child: MidgardCekDataSummaryV1,
): Hash32 => {
  boundedCount(childIndex, "cek_data_frame_child.index");
  exactSummary(child, "cek_data_frame_child.summary");
  return hash32(
    CHILD_DOMAIN,
    encodeCbor([
      BigInt(childIndex),
      Buffer.from(child.root),
      child.cborLength,
      child.memory,
    ]),
  );
};

const initialBase = ({
  tail,
  expectedChildren,
  sequence,
}: {
  readonly tail: Bytes;
  readonly expectedChildren: number;
  readonly sequence: MidgardCekDataSequenceSummaryV1;
}): MidgardCekDataFrameBaseV1 => ({
  tail: exactOptionalHash(tail, "cek_data_frame.tail"),
  expectedChildren: boundedCount(
    expectedChildren,
    "cek_data_frame.expected_children",
  ),
  childCount: 0,
  childFrontier: emptyMidgardValidationMerkleFrontierV1(),
  foldCursor: 0,
  sequence,
});

export const initialMidgardCekDataSmallConstrFrameV1 = ({
  constructor,
  tail = Buffer.alloc(0),
  expectedChildren,
}: {
  readonly constructor: bigint;
  readonly tail?: Bytes;
  readonly expectedChildren: number;
}): MidgardCekDataFrameV1 => {
  const frame = {
    kind: "constrSmall",
    constructor,
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataListSummaryV1(),
    }),
  } as const;
  validateMidgardCekDataFrameV1(frame);
  return frame;
};

export const initialMidgardCekDataLargeConstrFrameV1 = ({
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
}): MidgardCekDataFrameV1 => {
  const frame = {
    kind: "constrLarge",
    constructorCborRoot,
    constructorCborLength,
    constructorMemory,
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataListSummaryV1(),
    }),
  } as const;
  validateMidgardCekDataFrameV1(frame);
  return frame;
};

export const initialMidgardCekDataListFrameV1 = ({
  tail = Buffer.alloc(0),
  expectedChildren,
}: {
  readonly tail?: Bytes;
  readonly expectedChildren: number;
}): MidgardCekDataFrameV1 => {
  const frame = {
    kind: "list",
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataListSummaryV1(),
    }),
  } as const;
  validateMidgardCekDataFrameV1(frame);
  return frame;
};

export const initialMidgardCekDataMapFrameV1 = ({
  tail = Buffer.alloc(0),
  expectedChildren,
}: {
  readonly tail?: Bytes;
  readonly expectedChildren: number;
}): MidgardCekDataFrameV1 => {
  const frame = {
    kind: "map",
    ...initialBase({
      tail,
      expectedChildren,
      sequence: emptyMidgardCekDataPairSummaryV1(),
    }),
  } as const;
  validateMidgardCekDataFrameV1(frame);
  return frame;
};

export const appendMidgardCekDataFrameChildV1 = (
  frame: MidgardCekDataFrameV1,
  child: MidgardCekDataSummaryV1,
): MidgardCekDataFrameV1 | null => {
  try {
    validateMidgardCekDataFrameV1(frame);
    exactSummary(child, "cek_data_frame_child.summary");
    if (frame.foldCursor !== 0 || frame.childCount >= frame.expectedChildren) {
      return null;
    }
    const childFrontier = appendMidgardValidationMerkleLeafV1(
      frame.childFrontier,
      hashMidgardCekDataFrameChildV1(frame.childCount, child),
    );
    const next = {
      ...frame,
      childCount: frame.childCount + 1,
      childFrontier,
    };
    validateMidgardCekDataFrameV1(next);
    return next;
  } catch {
    return null;
  }
};

export const foldMidgardCekDataFrameListChildV1 = ({
  frame,
  childIndex,
  child,
  siblings,
}: {
  readonly frame: MidgardCekDataFrameV1;
  readonly childIndex: number;
  readonly child: MidgardCekDataSummaryV1;
  readonly siblings: readonly Bytes[];
}): MidgardCekDataFrameV1 | null => {
  try {
    validateMidgardCekDataFrameV1(frame);
    if (
      frame.kind === "map" ||
      frame.childCount !== frame.expectedChildren ||
      frame.foldCursor >= frame.expectedChildren
    ) {
      return null;
    }
    const expectedIndex = frame.expectedChildren - frame.foldCursor - 1;
    const leafHash = hashMidgardCekDataFrameChildV1(childIndex, child);
    if (
      childIndex !== expectedIndex ||
      !verifyMidgardValidationMerkleMembershipV1({
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
      sequence: prependMidgardCekDataListSummaryV1(child, frame.sequence),
    };
    validateMidgardCekDataFrameV1(next);
    return next;
  } catch {
    return null;
  }
};

export const foldMidgardCekDataFrameMapPairV1 = ({
  frame,
  pairIndex,
  key,
  value,
  keySiblings,
  valueSiblings,
}: {
  readonly frame: MidgardCekDataFrameV1;
  readonly pairIndex: number;
  readonly key: MidgardCekDataSummaryV1;
  readonly value: MidgardCekDataSummaryV1;
  readonly keySiblings: readonly Bytes[];
  readonly valueSiblings: readonly Bytes[];
}): MidgardCekDataFrameV1 | null => {
  try {
    validateMidgardCekDataFrameV1(frame);
    if (frame.kind !== "map" || frame.childCount !== frame.expectedChildren) {
      return null;
    }
    const pairCount = frame.expectedChildren / 2;
    if (frame.foldCursor >= pairCount) return null;
    const expectedPairIndex = pairCount - frame.foldCursor - 1;
    const keyIndex = pairIndex * 2;
    const valueIndex = keyIndex + 1;
    const keyLeafHash = hashMidgardCekDataFrameChildV1(keyIndex, key);
    const valueLeafHash = hashMidgardCekDataFrameChildV1(valueIndex, value);
    if (
      pairIndex !== expectedPairIndex ||
      !verifyMidgardValidationMerkleMembershipV1({
        frontier: frame.childFrontier,
        leafIndex: keyIndex,
        leafHash: keyLeafHash,
        siblings: keySiblings.map((sibling) =>
          ensureHash32(sibling, "cek_data_frame_map.key_sibling"),
        ),
      }) ||
      !verifyMidgardValidationMerkleMembershipV1({
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
      sequence: prependMidgardCekDataPairSummaryV1(key, value, frame.sequence),
    };
    validateMidgardCekDataFrameV1(next);
    return next;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataFrameV1 = (
  frame: MidgardCekDataFrameV1,
): MidgardCekDataSummaryV1 | null => {
  try {
    validateMidgardCekDataFrameV1(frame);
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
        return summarizeMidgardCekSmallConstrDataV1(
          frame.constructor,
          frame.sequence,
        );
      case "constrLarge":
        return summarizeMidgardCekLargeConstrDataV1({
          constructorCborRoot: frame.constructorCborRoot,
          constructorCborLength: frame.constructorCborLength,
          constructorMemory: frame.constructorMemory,
          fields: frame.sequence,
        });
      case "list":
        return summarizeMidgardCekListDataV1(frame.sequence);
      case "map":
        return summarizeMidgardCekMapDataV1(frame.sequence);
    }
  } catch {
    return null;
  }
};
