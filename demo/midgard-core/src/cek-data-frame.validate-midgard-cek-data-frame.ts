import { blake2b } from "@noble/hashes/blake2.js";

import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
} from "./cek-semantic.js";
import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  type MidgardValidationMerkleFrontier,
  validateMidgardValidationMerkleFrontier,
} from "./validation-merkle.js";

const FRAME_DOMAIN = Buffer.from("MidgardCekDataFrameV1", "ascii");

const CHILD_DOMAIN = Buffer.from("MidgardCekDataFrameChildV1", "ascii");

const UINT32_MAX = 0xffff_ffffn;

const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export type Bytes = Uint8Array;

/**
 * Only a map frame carries a child count fixed by its header
 * (`expectedChildren`). Every other frame is open-ended: its
 * `expectedChildren` is pinned to zero and its children are exactly the ones
 * the traversal attached before the authenticated close, so `childCount` is
 * the count every fold and the finalization use.
 */
export type MidgardCekDataFrameBase = {
  readonly tail: Bytes;
  readonly expectedChildren: number;
  readonly childCount: number;
  readonly childFrontier: MidgardValidationMerkleFrontier;
  readonly foldCursor: number;
  readonly sequence: MidgardCekDataSequenceSummary;
};

export type MidgardCekDataFrame =
  | (MidgardCekDataFrameBase & {
      readonly kind: "constrSmall";
      readonly constructor: bigint;
    })
  | (MidgardCekDataFrameBase & {
      readonly kind: "constrLarge";
      readonly constructorCborRoot: Bytes;
      readonly constructorCborLength: bigint;
      readonly constructorMemory: bigint;
    })
  | (MidgardCekDataFrameBase & {
      readonly kind: "list";
    })
  | (MidgardCekDataFrameBase & {
      readonly kind: "map";
    });

export const MidgardCekDataFrameTags = Object.freeze({
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

export const boundedCount = (value: number, fieldName: string): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > Number(UINT32_MAX)) {
    throw new RangeError(`${fieldName} must fit uint32`);
  }
  return value;
};

export const exactOptionalHash = (value: Bytes, fieldName: string): Buffer => {
  if (value.length === 0) return Buffer.alloc(0);
  return Buffer.from(ensureHash32(value, fieldName));
};

export const exactSummary = (
  summary: MidgardCekDataSummary,
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

const frameTag = (frame: MidgardCekDataFrame): bigint => {
  switch (frame.kind) {
    case "constrSmall":
      return MidgardCekDataFrameTags.ConstrSmall;
    case "constrLarge":
      return MidgardCekDataFrameTags.ConstrLarge;
    case "list":
      return MidgardCekDataFrameTags.List;
    case "map":
      return MidgardCekDataFrameTags.Map;
  }
};

const constructorFields = (
  frame: MidgardCekDataFrame,
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

/** The number of children a frame folds: the header count of a map, the
 * attached count of every open-ended frame. */
export const midgardCekDataFrameSequenceChildren = (
  frame: MidgardCekDataFrame,
): number => (frame.kind === "map" ? frame.expectedChildren : frame.childCount);

/** The fold cursor at which every child has been folded. */
export const midgardCekDataFrameCompleteFoldCursor = (
  frame: MidgardCekDataFrame,
): number =>
  frame.kind === "map" ? frame.expectedChildren / 2 : frame.childCount;

const expectedEmptySequence = (
  frame: MidgardCekDataFrame,
): MidgardCekDataSequenceSummary =>
  frame.kind === "map"
    ? emptyMidgardCekDataPairSummary()
    : emptyMidgardCekDataListSummary();

export const validateMidgardCekDataFrame = (
  frame: MidgardCekDataFrame,
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
  if (frame.kind === "map") {
    if (expectedChildren % 2 !== 0) {
      throw new RangeError(
        "cek_data_frame.expected_children must contain complete map pairs",
      );
    }
    if (childCount > expectedChildren) {
      throw new RangeError(
        "cek_data_frame.child_count exceeds expected_children",
      );
    }
  } else if (expectedChildren !== 0) {
    throw new RangeError(
      "cek_data_frame.expected_children is pinned to zero for an open-ended frame",
    );
  }
  if (frame.childFrontier.count !== childCount) {
    throw new Error("cek_data_frame frontier count does not match child_count");
  }
  validateMidgardValidationMerkleFrontier(frame.childFrontier);
  if (foldCursor > midgardCekDataFrameCompleteFoldCursor(frame)) {
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
  } else if (childCount !== midgardCekDataFrameSequenceChildren(frame)) {
    throw new Error(
      "cek_data_frame cannot fold before all children are committed",
    );
  }
};

export const encodeMidgardCekDataFrame = (
  frame: MidgardCekDataFrame,
): Buffer => {
  validateMidgardCekDataFrame(frame);
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

export const hashMidgardCekDataFrame = (frame: MidgardCekDataFrame): Hash32 =>
  hash32(FRAME_DOMAIN, encodeMidgardCekDataFrame(frame));

export const hashMidgardCekDataFrameChild = (
  childIndex: number,
  child: MidgardCekDataSummary,
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
