import { blake2b } from "@noble/hashes/blake2.js";

import { nextMidgardCekDataBytesSpan } from "./cek-data-bytes.js";
import { type MidgardCekDataFrame } from "./cek-data-frame.js";
import { nextMidgardCekDataIntegerSpan } from "./cek-data-integer.js";
import {
  type CborArgument,
  CONTROL_DOMAIN,
  isWellFormedMidgardCekDataTraverseControl,
  MIDGARD_CEK_DATA_TRAVERSE_HEAD_BYTES,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
  optionalControlCbor,
  type SmallConstructorHead,
  UINT32_MAX,
  UINT64_MAX,
  type WideCborArgument,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";
import { type MidgardCekSourceBlobSpan } from "./cek-source-blob.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

const optionalSummaryCbor = (summary: MidgardCekDataSummary | null): Buffer =>
  summary === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeCbor([
          Buffer.from(summary.root),
          summary.cborLength,
          summary.memory,
        ]),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardCekDataTraverseControl = (
  control: MidgardCekDataTraverseControl,
): Buffer => {
  if (!isWellFormedMidgardCekDataTraverseControl(control)) {
    throw new Error("Invalid V1 CEK Data traversal control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(control.version)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeCbor(BigInt(control.offset)),
    encodeCbor(control.frameRoot),
    optionalControlCbor(control.integer),
    optionalControlCbor(control.bytes),
    optionalSummaryCbor(control.result),
  ]);
};

export const hashMidgardCekDataTraverseControl = (
  control: MidgardCekDataTraverseControl,
): Hash32 =>
  ensureHash32(
    blake2b(
      Buffer.concat([
        CONTROL_DOMAIN,
        encodeMidgardCekDataTraverseControl(control),
      ]),
      { dkLen: 32 },
    ),
    "cek_data_traverse_control_hash",
  );

export const nextMidgardCekDataTraverseSpan = (
  control: MidgardCekDataTraverseControl,
): MidgardCekSourceBlobSpan | null => {
  if (!isWellFormedMidgardCekDataTraverseControl(control)) {
    return null;
  }
  switch (control.stage) {
    case MidgardCekDataTraverseStages.Head:
    case MidgardCekDataTraverseStages.Close:
      return {
        absoluteStart: control.sourceStart + control.offset,
        length: Math.min(
          MIDGARD_CEK_DATA_TRAVERSE_HEAD_BYTES,
          control.sourceLength - control.offset,
        ),
      };
    case MidgardCekDataTraverseStages.Integer:
    case MidgardCekDataTraverseStages.LargeConstructor:
      return nextMidgardCekDataIntegerSpan(
        control.integer!,
        control.sourceStart + control.sourceLength,
      );
    case MidgardCekDataTraverseStages.Bytes:
      return nextMidgardCekDataBytesSpan(
        control.bytes!,
        control.sourceStart + control.sourceLength,
      );
    case MidgardCekDataTraverseStages.LargeFields:
      return {
        absoluteStart: control.sourceStart + control.offset,
        length: 1,
      };
    case MidgardCekDataTraverseStages.Fold:
    case MidgardCekDataTraverseStages.Terminal:
      return null;
  }
};

export const readCanonicalCborArgument = (
  bytes: Uint8Array,
  offset: number,
): CborArgument | null => {
  if (offset < 0 || offset >= bytes.length) return null;
  const initial = bytes[offset]!;
  const major = initial >>> 5;
  const additional = initial & 0x1f;
  if (additional < 24) {
    return {
      major,
      value: additional,
      nextOffset: offset + 1,
    };
  }
  const byteLength =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : additional === 27
            ? 8
            : null;
  if (byteLength === null || offset + 1 + byteLength > bytes.length) {
    return null;
  }
  // An 8-byte argument may exceed 2^53; every consumer bounds it far below
  // that (uint32), so the rounded value still compares the same way.
  let value = 0;
  for (let index = 0; index < byteLength; index += 1) {
    value = value * 256 + bytes[offset + 1 + index]!;
  }
  if (
    (additional === 24 && value < 24) ||
    (additional === 25 && value <= 0xff) ||
    (additional === 26 && value <= 0xffff) ||
    (additional === 27 && value <= UINT32_MAX)
  ) {
    return null;
  }
  return { major, value, nextOffset: offset + 1 + byteLength };
};

export const parseSmallConstructorHead = (
  bytes: Uint8Array,
): SmallConstructorHead | null => {
  if (
    bytes.length >= 2 &&
    bytes[0] === 0xd8 &&
    bytes[1]! >= 121 &&
    bytes[1]! <= 127
  ) {
    return {
      constructor: BigInt(bytes[1]! - 121),
      prefixLength: 2,
    };
  }
  if (bytes.length < 3 || bytes[0] !== 0xd9) return null;
  const tag = bytes[1]! * 256 + bytes[2]!;
  const constructor = tag - 1_280 + 7;
  return tag >= 1_280 && tag <= 1_400 && constructor <= 127
    ? { constructor: BigInt(constructor), prefixLength: 3 }
    : null;
};

export type ParsedDataNode =
  | {
      readonly kind: "scalar";
      readonly start: number;
      end: number;
      readonly children: number[];
    }
  | {
      readonly kind: "list";
      readonly start: number;
      end: number;
      readonly children: number[];
      readonly closesWithBreak: boolean;
    }
  | {
      readonly kind: "map";
      readonly start: number;
      end: number;
      readonly children: number[];
    }
  | {
      readonly kind: "constrSmall";
      readonly start: number;
      end: number;
      readonly children: number[];
      readonly constructor: bigint;
      readonly closesWithBreak: boolean;
    }
  | {
      readonly kind: "constrLarge";
      readonly start: number;
      end: number;
      readonly children: number[];
      readonly constructorCborLength: number;
      readonly closesWithBreak: boolean;
    };

type ParsedContainerHead = {
  readonly refusalOffset?: number;
  readonly node: Exclude<ParsedDataNode, { readonly kind: "scalar" }>;
  readonly nextOffset: number;
  readonly remainingChildren: number | null;
};

export type ParsedNodeHead =
  | {
      readonly node: Extract<ParsedDataNode, { readonly kind: "scalar" }>;
      readonly nextOffset: number;
      readonly remainingChildren: 0;
    }
  | ParsedContainerHead;

export type DataParserFrame = {
  readonly nodeIndex: number;
  remainingChildren: number | null;
};

export type DataTraceFrame = {
  frame: MidgardCekDataFrame;
  readonly childSummaries: MidgardCekDataSummary[];
  readonly parent: DataTraceFrame | null;
  readonly node: Exclude<ParsedDataNode, { readonly kind: "scalar" }>;
};

export type DataTraceOperation =
  | {
      readonly kind: "visit";
      readonly nodeIndex: number;
      readonly parent: DataTraceFrame | null;
    }
  | {
      readonly kind: "finish";
      readonly context: DataTraceFrame;
    };

const readCanonicalCborArgumentWide = (
  bytes: Uint8Array,
  offset: number,
): WideCborArgument | null => {
  if (offset < 0 || offset >= bytes.length) return null;
  const initial = bytes[offset]!;
  const major = initial >>> 5;
  const additional = initial & 0x1f;
  if (additional < 24) {
    return {
      major,
      value: BigInt(additional),
      nextOffset: offset + 1,
    };
  }
  const byteLength =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : additional === 27
            ? 8
            : null;
  if (byteLength === null || offset + 1 + byteLength > bytes.length) {
    return null;
  }
  let value = 0n;
  for (let index = 0; index < byteLength; index += 1) {
    value = (value << 8n) | BigInt(bytes[offset + 1 + index]!);
  }
  if (
    (additional === 24 && value < 24n) ||
    (additional === 25 && value <= 0xffn) ||
    (additional === 26 && value <= 0xffffn) ||
    (additional === 27 && value <= 0xffff_ffffn)
  ) {
    return null;
  }
  return { major, value, nextOffset: offset + 1 + byteLength };
};

export const parseIntegerEnd = (
  bytes: Buffer,
  start: number,
): number | null => {
  const first = bytes[start];
  if (first === undefined) return null;
  if (first >>> 5 <= 1) {
    const argument = readCanonicalCborArgumentWide(bytes, start);
    if (
      argument === null ||
      argument.major > 1 ||
      argument.value > UINT64_MAX
    ) {
      return null;
    }
    return argument.nextOffset;
  }
  if (first !== 0xc2 && first !== 0xc3) return null;
  if (bytes[start + 1] === 0x5f) {
    let cursor = start + 2;
    let magnitudeLength = 0;
    let previousLength = 64;
    while (cursor < bytes.length) {
      if (bytes[cursor] === 0xff)
        return magnitudeLength > 64 ? cursor + 1 : null;
      if (previousLength !== 64) return null;
      const chunk = readCanonicalCborArgumentWide(bytes, cursor);
      if (
        chunk === null ||
        chunk.major !== 2 ||
        chunk.value < 1n ||
        chunk.value > 64n
      )
        return null;
      const length = Number(chunk.value);
      if (
        chunk.nextOffset + length >= bytes.length ||
        (magnitudeLength === 0 &&
          (length !== 64 || bytes[chunk.nextOffset] === 0))
      )
        return null;
      magnitudeLength += length;
      previousLength = length;
      cursor = chunk.nextOffset + length;
    }
    return null;
  }
  const magnitude = readCanonicalCborArgumentWide(bytes, start + 1);
  if (
    magnitude === null ||
    magnitude.major !== 2 ||
    magnitude.value < 9n ||
    magnitude.value > 64n
  ) {
    return null;
  }
  const endBigInt = BigInt(magnitude.nextOffset) + magnitude.value;
  if (
    endBigInt > BigInt(bytes.length) ||
    endBigInt > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    return null;
  }
  const end = Number(endBigInt);
  if (bytes[magnitude.nextOffset] === 0) return null;
  return end;
};
