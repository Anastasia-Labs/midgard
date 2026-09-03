import { blake2b } from "@noble/hashes/blake2.js";

import {
  advanceMidgardCekDataBytes,
  encodeMidgardCekDataBytesControl,
  finalizeMidgardCekDataBytes,
  initialMidgardCekDataBytesControl,
  isWellFormedMidgardCekDataBytesControl,
  MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
  type MidgardCekDataBytesControl,
  MidgardCekDataBytesStages,
  nextMidgardCekDataBytesSpan,
  parseMidgardCekDataBytesSyntax,
} from "./cek-data-bytes-v1.js";
import {
  appendMidgardCekDataFrameChild,
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
} from "./cek-data-frame-v1.js";
import {
  advanceMidgardCekDataInteger,
  encodeMidgardCekDataIntegerControl,
  finalizeMidgardCekDataInteger,
  initialMidgardCekDataIntegerControl,
  isWellFormedMidgardCekDataIntegerControl,
  MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
  type MidgardCekDataIntegerControl,
  MidgardCekDataIntegerStages,
  nextMidgardCekDataIntegerSpan,
  parseMidgardCekDataIntegerSyntax,
  parseMidgardCekDataLargeConstructorSyntax,
} from "./cek-data-integer-v1.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";
import {
  finalizeMidgardCekSourceBlob,
  type MidgardCekSourceBlobSpan,
} from "./cek-source-blob-v1.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import { buildMidgardValidationMerkleMembershipIndex } from "./validation-merkle.js";

export const MIDGARD_CEK_DATA_TRAVERSE_VERSION = 1 as const;
export const MIDGARD_CEK_DATA_TRAVERSE_HEAD_BYTES = 14;
export const MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN = 132;

const CONTROL_DOMAIN = Buffer.from("MidgardCekDataTraverseControlV1", "ascii");
const UINT32_MAX = 0xffff_ffff;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export const MidgardCekDataTraverseStages = Object.freeze({
  Head: 0,
  Integer: 1,
  Bytes: 2,
  LargeConstructor: 3,
  LargeFields: 4,
  Close: 5,
  Fold: 6,
  Terminal: 7,
} as const);

export type MidgardCekDataTraverseStage =
  (typeof MidgardCekDataTraverseStages)[keyof typeof MidgardCekDataTraverseStages];

export type MidgardCekDataTraverseControl = {
  readonly version: typeof MIDGARD_CEK_DATA_TRAVERSE_VERSION;
  readonly stage: MidgardCekDataTraverseStage;
  readonly sourceStart: number;
  readonly sourceLength: number;
  readonly offset: number;
  readonly frameRoot: Buffer;
  readonly pendingLargeExpectedChildren: number | null;
  readonly integer: MidgardCekDataIntegerControl | null;
  readonly bytes: MidgardCekDataBytesControl | null;
  readonly result: MidgardCekDataSummary | null;
};

export type MidgardCekDataTraverseAction =
  | {
      readonly kind: "headScalar";
      readonly itemLength: number;
    }
  | {
      readonly kind: "headSequence";
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "headMap";
    }
  | {
      readonly kind: "headLargeConstructor";
      readonly constructorCborLength: number;
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "attachScalar";
      readonly parent: MidgardCekDataFrame | null;
    }
  | {
      readonly kind: "foldList";
      readonly frame: MidgardCekDataFrame;
      readonly childIndex: number;
      readonly child: MidgardCekDataSummary;
      readonly siblings: readonly Uint8Array[];
    }
  | {
      readonly kind: "foldMap";
      readonly frame: MidgardCekDataFrame;
      readonly pairIndex: number;
      readonly key: MidgardCekDataSummary;
      readonly value: MidgardCekDataSummary;
      readonly keySiblings: readonly Uint8Array[];
      readonly valueSiblings: readonly Uint8Array[];
    }
  | {
      readonly kind: "finalizeFrame";
      readonly frame: MidgardCekDataFrame;
      readonly parent: MidgardCekDataFrame | null;
    }
  | null;

export type MidgardCekDataTraverseTraceStep = {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes: Buffer | null;
  readonly action: MidgardCekDataTraverseAction;
  readonly next: MidgardCekDataTraverseControl;
};

export type MidgardCekDataTraverseTrace = {
  readonly initial: MidgardCekDataTraverseControl;
  readonly steps: readonly MidgardCekDataTraverseTraceStep[];
  readonly terminal: MidgardCekDataTraverseControl;
};

type CborArgument = {
  readonly major: number;
  readonly value: number;
  readonly nextOffset: number;
};

type WideCborArgument = {
  readonly major: number;
  readonly value: bigint;
  readonly nextOffset: number;
};

type SmallConstructorHead = {
  readonly constructor: bigint;
  readonly prefixLength: number;
};

const exactUint32 = (value: number, fieldName: string): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > UINT32_MAX) {
    throw new RangeError(`${fieldName} must fit uint32`);
  }
  return value;
};

const optionalHashIsWellFormed = (value: Uint8Array): boolean =>
  value.length === 0 || value.length === 32;

const summaryIsWellFormed = (summary: MidgardCekDataSummary): boolean => {
  try {
    ensureHash32(summary.root, "cek_data_traverse.result.root");
    return (
      summary.cborLength > 0n &&
      summary.cborLength <= UINT64_MAX &&
      summary.memory >= 4n &&
      summary.memory <= UINT64_MAX
    );
  } catch {
    return false;
  }
};

const nestedIntegerFits = (
  control: MidgardCekDataTraverseControl,
  integer: MidgardCekDataIntegerControl,
  startsAtCursor: boolean,
): boolean => {
  const absoluteCursor = control.sourceStart + control.offset;
  return (
    isWellFormedMidgardCekDataIntegerControl(integer) &&
    (startsAtCursor
      ? integer.sourceStart === absoluteCursor
      : integer.sourceStart + integer.sourceLength === absoluteCursor) &&
    integer.sourceStart >= control.sourceStart &&
    integer.sourceStart + integer.sourceLength <=
      control.sourceStart + control.sourceLength
  );
};

export const isWellFormedMidgardCekDataTraverseControl = (
  control: MidgardCekDataTraverseControl,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_DATA_TRAVERSE_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataTraverseStages.Head ||
      control.stage > MidgardCekDataTraverseStages.Terminal ||
      exactUint32(control.sourceStart, "cek_data_traverse.source_start") !==
        control.sourceStart ||
      exactUint32(control.sourceLength, "cek_data_traverse.source_length") !==
        control.sourceLength ||
      control.sourceLength === 0 ||
      !Number.isSafeInteger(control.sourceStart + control.sourceLength) ||
      exactUint32(control.offset, "cek_data_traverse.offset") !==
        control.offset ||
      control.offset > control.sourceLength ||
      !optionalHashIsWellFormed(control.frameRoot) ||
      (control.pendingLargeExpectedChildren !== null &&
        exactUint32(
          control.pendingLargeExpectedChildren,
          "cek_data_traverse.pending_large_children",
        ) !== control.pendingLargeExpectedChildren) ||
      (control.result !== null && !summaryIsWellFormed(control.result))
    ) {
      return false;
    }
    switch (control.stage) {
      case MidgardCekDataTraverseStages.Head:
        return (
          control.offset < control.sourceLength &&
          (control.frameRoot.length === 32 || control.offset === 0) &&
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes === null &&
          control.result === null
        );
      case MidgardCekDataTraverseStages.Integer:
        return (
          control.pendingLargeExpectedChildren === null &&
          control.integer !== null &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, true)
        );
      case MidgardCekDataTraverseStages.Bytes:
        return (
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes !== null &&
          control.result === null &&
          isWellFormedMidgardCekDataBytesControl(control.bytes) &&
          control.bytes.sourceStart === control.sourceStart + control.offset &&
          control.offset + control.bytes.sourceLength <= control.sourceLength
        );
      case MidgardCekDataTraverseStages.LargeConstructor:
        return (
          control.pendingLargeExpectedChildren !== null &&
          control.integer !== null &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, true) &&
          control.offset + control.integer.sourceLength < control.sourceLength
        );
      case MidgardCekDataTraverseStages.LargeFields:
        return (
          control.pendingLargeExpectedChildren !== null &&
          control.integer !== null &&
          control.integer.stage === MidgardCekDataIntegerStages.Terminal &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, false) &&
          control.offset < control.sourceLength
        );
      case MidgardCekDataTraverseStages.Close:
      case MidgardCekDataTraverseStages.Fold:
        return (
          control.frameRoot.length === 32 &&
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes === null &&
          control.result === null &&
          (control.stage !== MidgardCekDataTraverseStages.Close ||
            control.offset < control.sourceLength)
        );
      case MidgardCekDataTraverseStages.Terminal:
        return (
          control.offset === control.sourceLength &&
          control.frameRoot.length === 0 &&
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes === null &&
          control.result !== null
        );
    }
  } catch {
    return false;
  }
};

export const initialMidgardCekDataTraverseControl = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekDataTraverseControl => {
  const control = {
    version: MIDGARD_CEK_DATA_TRAVERSE_VERSION,
    stage: MidgardCekDataTraverseStages.Head,
    sourceStart,
    sourceLength,
    offset: 0,
    frameRoot: Buffer.alloc(0),
    pendingLargeExpectedChildren: null,
    integer: null,
    bytes: null,
    result: null,
  } satisfies MidgardCekDataTraverseControl;
  if (!isWellFormedMidgardCekDataTraverseControl(control)) {
    throw new Error("Invalid V1 CEK Data traversal source");
  }
  return control;
};

const optionalIntCbor = (value: number | null): Buffer =>
  value === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeCbor(BigInt(value)),
        Buffer.from([0xff]),
      ]);

const optionalControlCbor = (
  control: MidgardCekDataIntegerControl | MidgardCekDataBytesControl | null,
): Buffer => {
  if (control === null) return Buffer.from("d87a80", "hex");
  const nested =
    "memory" in control
      ? encodeMidgardCekDataIntegerControl(control)
      : encodeMidgardCekDataBytesControl(control);
  return Buffer.concat([
    Buffer.from("d8799f", "hex"),
    nested,
    Buffer.from([0xff]),
  ]);
};

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
    optionalIntCbor(control.pendingLargeExpectedChildren),
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
      return {
        absoluteStart: control.sourceStart + control.offset,
        length: Math.min(
          MIDGARD_CEK_DATA_TRAVERSE_HEAD_BYTES,
          control.sourceLength - control.offset,
        ),
      };
    case MidgardCekDataTraverseStages.Integer:
    case MidgardCekDataTraverseStages.LargeConstructor:
      return nextMidgardCekDataIntegerSpan(control.integer!);
    case MidgardCekDataTraverseStages.Bytes:
      return nextMidgardCekDataBytesSpan(control.bytes!);
    case MidgardCekDataTraverseStages.LargeFields:
    case MidgardCekDataTraverseStages.Close:
      return {
        absoluteStart: control.sourceStart + control.offset,
        length: 1,
      };
    case MidgardCekDataTraverseStages.Fold:
    case MidgardCekDataTraverseStages.Terminal:
      return null;
  }
};

const readCanonicalCborArgument = (
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
          : null;
  if (byteLength === null || offset + 1 + byteLength > bytes.length) {
    return null;
  }
  let value = 0;
  for (let index = 0; index < byteLength; index += 1) {
    value = value * 256 + bytes[offset + 1 + index]!;
  }
  if (
    (additional === 24 && value < 24) ||
    (additional === 25 && value <= 0xff) ||
    (additional === 26 && value <= 0xffff)
  ) {
    return null;
  }
  return { major, value, nextOffset: offset + 1 + byteLength };
};

const parseSmallConstructorHead = (
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

type ParsedDataNode =
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
  readonly node: Exclude<ParsedDataNode, { readonly kind: "scalar" }>;
  readonly nextOffset: number;
  readonly remainingChildren: number | null;
};

type ParsedNodeHead =
  | {
      readonly node: Extract<ParsedDataNode, { readonly kind: "scalar" }>;
      readonly nextOffset: number;
      readonly remainingChildren: 0;
    }
  | ParsedContainerHead;

type DataParserFrame = {
  readonly nodeIndex: number;
  remainingChildren: number | null;
};

type DataTraceFrame = {
  frame: MidgardCekDataFrame;
  readonly childSummaries: MidgardCekDataSummary[];
  readonly parent: DataTraceFrame | null;
  readonly node: Exclude<ParsedDataNode, { readonly kind: "scalar" }>;
};

type DataTraceOperation =
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

const parseIntegerEnd = (bytes: Buffer, start: number): number | null => {
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
  const magnitude = readCanonicalCborArgumentWide(bytes, start + 1);
  if (
    magnitude === null ||
    magnitude.major !== 2 ||
    magnitude.value < 9n ||
    magnitude.value > BigInt(UINT32_MAX)
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

const parseBytesEnd = (bytes: Buffer, start: number): number | null => {
  const first = bytes[start];
  if (first === undefined) return null;
  if (first >= 0x40 && first <= 0x57) {
    const end = start + 1 + first - 0x40;
    return end <= bytes.length ? end : null;
  }
  if (first === 0x58) {
    const length = bytes[start + 1];
    if (length === undefined || length < 24 || length > 64) {
      return null;
    }
    const end = start + 2 + length;
    return end <= bytes.length ? end : null;
  }
  if (first !== 0x5f) return null;
  let cursor = start + 1;
  let contentLength = 0;
  let previousChunkLength: number | null = null;
  while (cursor < bytes.length && bytes[cursor] !== 0xff) {
    if (previousChunkLength !== null && previousChunkLength !== 64) {
      return null;
    }
    const chunkFirst = bytes[cursor]!;
    let chunkLength: number;
    let headerLength: number;
    if (chunkFirst >= 0x41 && chunkFirst <= 0x57) {
      chunkLength = chunkFirst - 0x40;
      headerLength = 1;
    } else if (
      chunkFirst === 0x58 &&
      bytes[cursor + 1] !== undefined &&
      bytes[cursor + 1]! >= 24 &&
      bytes[cursor + 1]! <= 64
    ) {
      chunkLength = bytes[cursor + 1]!;
      headerLength = 2;
    } else {
      return null;
    }
    const next = cursor + headerLength + chunkLength;
    if (next > bytes.length) return null;
    contentLength += chunkLength;
    previousChunkLength = chunkLength;
    cursor = next;
  }
  if (
    bytes[cursor] !== 0xff ||
    contentLength <= 64 ||
    previousChunkLength === null
  ) {
    return null;
  }
  return cursor + 1;
};

const scalarEnd = (bytes: Buffer, start: number): number | null => {
  const first = bytes[start];
  if (first === undefined) return null;
  const end =
    first >>> 5 <= 1 || first === 0xc2 || first === 0xc3
      ? parseIntegerEnd(bytes, start)
      : first >>> 5 === 2
        ? parseBytesEnd(bytes, start)
        : null;
  if (end === null) return null;
  const sourceLength = end - start;
  const syntaxBytes = bytes.subarray(
    start,
    Math.min(
      end,
      start +
        (first >>> 5 <= 1 || first === 0xc2 || first === 0xc3
          ? MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES
          : MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES),
    ),
  );
  const valid =
    first >>> 5 <= 1 || first === 0xc2 || first === 0xc3
      ? parseMidgardCekDataIntegerSyntax({
          syntaxBytes,
          sourceLength,
        }) !== null
      : parseMidgardCekDataBytesSyntax({
          syntaxBytes,
          sourceLength,
        }) !== null;
  return valid ? end : null;
};

const parseSequenceHead = ({
  bytes,
  start,
  prefixLength,
}: {
  readonly bytes: Buffer;
  readonly start: number;
  readonly prefixLength: number;
}): {
  readonly nextOffset: number;
  readonly remainingChildren: number | null;
  readonly closesWithBreak: boolean;
} | null => {
  const sequence = bytes[start + prefixLength];
  if (sequence === 0x80) {
    return {
      nextOffset: start + prefixLength + 1,
      remainingChildren: 0,
      closesWithBreak: false,
    };
  }
  if (sequence === 0x9f) {
    return {
      nextOffset: start + prefixLength + 1,
      remainingChildren: null,
      closesWithBreak: true,
    };
  }
  return null;
};

const parseDataNodeHead = (bytes: Buffer, start: number): ParsedNodeHead => {
  const scalar = scalarEnd(bytes, start);
  if (scalar !== null) {
    return {
      node: {
        kind: "scalar",
        start,
        end: scalar,
        children: [],
      },
      nextOffset: scalar,
      remainingChildren: 0,
    };
  }

  const small = parseSmallConstructorHead(bytes.subarray(start));
  if (small !== null) {
    const sequence = parseSequenceHead({
      bytes,
      start,
      prefixLength: small.prefixLength,
    });
    if (sequence !== null) {
      return {
        node: {
          kind: "constrSmall",
          start,
          end: 0,
          children: [],
          constructor: small.constructor,
          closesWithBreak: sequence.closesWithBreak,
        },
        nextOffset: sequence.nextOffset,
        remainingChildren: sequence.remainingChildren,
      };
    }
  }

  if (
    start + 3 <= bytes.length &&
    bytes.subarray(start, start + 3).equals(Buffer.from("d86682", "hex"))
  ) {
    const constructorStart = start + 3;
    const constructorEnd = parseIntegerEnd(bytes, constructorStart);
    if (constructorEnd !== null) {
      const constructorCborLength = constructorEnd - constructorStart;
      const syntaxBytes = bytes.subarray(
        constructorStart,
        Math.min(
          constructorEnd,
          constructorStart + MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
        ),
      );
      const sequence = parseSequenceHead({
        bytes,
        start: constructorEnd,
        prefixLength: 0,
      });
      if (
        parseMidgardCekDataLargeConstructorSyntax({
          syntaxBytes,
          sourceLength: constructorCborLength,
        }) !== null &&
        sequence !== null
      ) {
        return {
          node: {
            kind: "constrLarge",
            start,
            end: 0,
            children: [],
            constructorCborLength,
            closesWithBreak: sequence.closesWithBreak,
          },
          nextOffset: sequence.nextOffset,
          remainingChildren: sequence.remainingChildren,
        };
      }
    }
  }

  const first = bytes[start];
  if (first === 0x80 || first === 0x9f) {
    const sequence = parseSequenceHead({
      bytes,
      start,
      prefixLength: 0,
    })!;
    return {
      node: {
        kind: "list",
        start,
        end: 0,
        children: [],
        closesWithBreak: sequence.closesWithBreak,
      },
      nextOffset: sequence.nextOffset,
      remainingChildren: sequence.remainingChildren,
    };
  }

  const map = readCanonicalCborArgument(bytes, start);
  if (
    map !== null &&
    map.major === 5 &&
    map.value <= Math.floor(UINT32_MAX / 2)
  ) {
    return {
      node: {
        kind: "map",
        start,
        end: 0,
        children: [],
      },
      nextOffset: map.nextOffset,
      remainingChildren: map.value * 2,
    };
  }

  throw new Error(
    `V1 CEK Data traversal rejected syntax at byte ${start.toString(10)}`,
  );
};

const parseMidgardCekDataNodes = (
  source: Buffer,
): readonly ParsedDataNode[] => {
  if (source.length === 0 || source.length > UINT32_MAX) {
    throw new Error("V1 CEK Data traversal source must fit uint32");
  }
  const nodes: ParsedDataNode[] = [];
  const frames: DataParserFrame[] = [];
  const root = parseDataNodeHead(source, 0);
  nodes.push(root.node);
  let cursor = root.nextOffset;
  if (root.remainingChildren !== 0) {
    frames.push({
      nodeIndex: 0,
      remainingChildren: root.remainingChildren,
    });
  } else if (root.node.kind !== "scalar") {
    root.node.end = cursor;
  }

  while (frames.length > 0) {
    const frame = frames[frames.length - 1]!;
    const node = nodes[frame.nodeIndex]!;
    if (node.kind === "scalar") {
      throw new Error("V1 CEK Data parser frame cannot be scalar");
    }
    const closesWithBreak = node.kind !== "map" && node.closesWithBreak;
    const isComplete =
      frame.remainingChildren === 0 ||
      (frame.remainingChildren === null && source[cursor] === 0xff);
    if (isComplete) {
      if (closesWithBreak) {
        if (
          frame.remainingChildren !== null ||
          node.children.length === 0 ||
          source[cursor] !== 0xff
        ) {
          throw new Error(
            "V1 CEK Data traversal rejected a noncanonical sequence",
          );
        }
        cursor += 1;
      }
      node.end = cursor;
      frames.pop();
      continue;
    }
    if (cursor >= source.length || source[cursor] === 0xff) {
      throw new Error("V1 CEK Data traversal rejected an incomplete container");
    }
    const child = parseDataNodeHead(source, cursor);
    const childIndex = nodes.length;
    nodes.push(child.node);
    node.children.push(childIndex);
    if (frame.remainingChildren !== null) {
      frame.remainingChildren -= 1;
    }
    cursor = child.nextOffset;
    if (child.remainingChildren !== 0) {
      frames.push({
        nodeIndex: childIndex,
        remainingChildren: child.remainingChildren,
      });
    } else if (child.node.kind !== "scalar") {
      child.node.end = cursor;
    }
  }

  if (nodes[0]!.kind === "scalar") {
    cursor = nodes[0]!.end;
  }
  if (cursor !== source.length) {
    throw new Error("V1 CEK Data traversal rejected trailing source bytes");
  }
  return nodes;
};

const exactSourceBytes = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
}): Buffer | null => {
  const span = nextMidgardCekDataTraverseSpan(control);
  if (
    span === null ||
    sourceBytes === null ||
    sourceBytes === undefined ||
    sourceBytes.length !== span.length
  ) {
    return null;
  }
  return Buffer.from(sourceBytes);
};

const advanced = (
  control: MidgardCekDataTraverseControl,
): MidgardCekDataTraverseControl | null =>
  isWellFormedMidgardCekDataTraverseControl(control) ? control : null;

const nextParentStage = (
  frame: MidgardCekDataFrame,
): MidgardCekDataTraverseStage => {
  if (frame.childCount < frame.expectedChildren) {
    return MidgardCekDataTraverseStages.Head;
  }
  return frame.kind === "map"
    ? MidgardCekDataTraverseStages.Fold
    : MidgardCekDataTraverseStages.Close;
};

const attachSummary = ({
  control,
  summary,
  parent,
  offset,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly summary: MidgardCekDataSummary;
  readonly parent: MidgardCekDataFrame | null;
  readonly offset: number;
}): MidgardCekDataTraverseControl | null => {
  if (!summaryIsWellFormed(summary)) return null;
  if (control.frameRoot.length === 0) {
    if (parent !== null || offset !== control.sourceLength) {
      return null;
    }
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Terminal,
      offset,
      frameRoot: Buffer.alloc(0),
      pendingLargeExpectedChildren: null,
      integer: null,
      bytes: null,
      result: summary,
    });
  }
  if (
    parent === null ||
    !hashMidgardCekDataFrame(parent).equals(control.frameRoot)
  ) {
    return null;
  }
  const nextParent = appendMidgardCekDataFrameChild(parent, summary);
  if (nextParent === null) return null;
  return advanced({
    ...control,
    stage: nextParentStage(nextParent),
    offset,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(nextParent)),
    pendingLargeExpectedChildren: null,
    integer: null,
    bytes: null,
    result: null,
  });
};

const stepHeadScalar = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "headScalar" }
  >;
}): MidgardCekDataTraverseControl | null => {
  const itemLength = exactUint32(
    action.itemLength,
    "cek_data_traverse.scalar_length",
  );
  if (itemLength === 0 || control.offset + itemLength > control.sourceLength) {
    return null;
  }
  const first = bytes[0]!;
  if (first >>> 5 <= 1 || first === 0xc2 || first === 0xc3) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Integer,
      integer: initialMidgardCekDataIntegerControl({
        sourceStart: control.sourceStart + control.offset,
        sourceLength: itemLength,
      }),
    });
  }
  if (first >>> 5 === 2) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Bytes,
      bytes: initialMidgardCekDataBytesControl({
        sourceStart: control.sourceStart + control.offset,
        sourceLength: itemLength,
      }),
    });
  }
  return null;
};

const stepHeadSequence = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "headSequence" }
  >;
}): MidgardCekDataTraverseControl | null => {
  const expectedChildren = exactUint32(
    action.expectedChildren,
    "cek_data_traverse.expected_children",
  );
  const sequenceHeader = expectedChildren === 0 ? 0x80 : 0x9f;
  const small = parseSmallConstructorHead(bytes);
  let frame: MidgardCekDataFrame;
  let headLength: number;
  if (small !== null) {
    if (bytes[small.prefixLength] !== sequenceHeader) return null;
    frame = initialMidgardCekDataSmallConstrFrame({
      constructor: small.constructor,
      tail: control.frameRoot,
      expectedChildren,
    });
    headLength = small.prefixLength + 1;
  } else {
    if (bytes[0] !== sequenceHeader) return null;
    frame = initialMidgardCekDataListFrame({
      tail: control.frameRoot,
      expectedChildren,
    });
    headLength = 1;
  }
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStages.Fold
        : MidgardCekDataTraverseStages.Head,
    offset: control.offset + headLength,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
  });
};

const stepHeadMap = ({
  control,
  bytes,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
}): MidgardCekDataTraverseControl | null => {
  const argument = readCanonicalCborArgument(bytes, 0);
  if (
    argument === null ||
    argument.major !== 5 ||
    argument.value > Math.floor(UINT32_MAX / 2)
  ) {
    return null;
  }
  const expectedChildren = argument.value * 2;
  const frame = initialMidgardCekDataMapFrame({
    tail: control.frameRoot,
    expectedChildren,
  });
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStages.Fold
        : MidgardCekDataTraverseStages.Head,
    offset: control.offset + argument.nextOffset,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
  });
};

const stepHeadLargeConstructor = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "headLargeConstructor" }
  >;
}): MidgardCekDataTraverseControl | null => {
  const constructorCborLength = exactUint32(
    action.constructorCborLength,
    "cek_data_traverse.constructor_cbor_length",
  );
  const expectedChildren = exactUint32(
    action.expectedChildren,
    "cek_data_traverse.expected_children",
  );
  if (
    constructorCborLength === 0 ||
    bytes.length < 3 ||
    !bytes.subarray(0, 3).equals(Buffer.from("d86682", "hex")) ||
    control.offset + 3 + constructorCborLength >= control.sourceLength
  ) {
    return null;
  }
  const offset = control.offset + 3;
  return advanced({
    ...control,
    stage: MidgardCekDataTraverseStages.LargeConstructor,
    offset,
    pendingLargeExpectedChildren: expectedChildren,
    integer: initialMidgardCekDataIntegerControl({
      sourceStart: control.sourceStart + offset,
      sourceLength: constructorCborLength,
    }),
  });
};

const stepHead = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const bytes = exactSourceBytes({ control, sourceBytes });
  if (bytes === null || action === null) return null;
  // Non-head actions are rejected by this phase-specific dispatcher.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (action.kind) {
    case "headScalar":
      return stepHeadScalar({ control, bytes, action });
    case "headSequence":
      return stepHeadSequence({ control, bytes, action });
    case "headMap":
      return stepHeadMap({ control, bytes });
    case "headLargeConstructor":
      return stepHeadLargeConstructor({
        control,
        bytes,
        action,
      });
    default:
      return null;
  }
};

const stepInteger = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const integer = control.integer!;
  if (integer.stage === MidgardCekDataIntegerStages.Terminal) {
    if (sourceBytes !== null && sourceBytes !== undefined) {
      return null;
    }
    if (action === null || action.kind !== "attachScalar") {
      return null;
    }
    const summary = finalizeMidgardCekDataInteger(integer);
    return summary === null
      ? null
      : attachSummary({
          control,
          summary,
          parent: action.parent,
          offset: control.offset + integer.sourceLength,
        });
  }
  if (action !== null) return null;
  const nextInteger = advanceMidgardCekDataInteger({
    control: integer,
    sourceBytes,
  });
  return nextInteger === null
    ? null
    : advanced({ ...control, integer: nextInteger });
};

const stepBytes = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const byteControl = control.bytes!;
  if (byteControl.stage === MidgardCekDataBytesStages.Terminal) {
    if (sourceBytes !== null && sourceBytes !== undefined) {
      return null;
    }
    if (action === null || action.kind !== "attachScalar") {
      return null;
    }
    const summary = finalizeMidgardCekDataBytes(byteControl);
    return summary === null
      ? null
      : attachSummary({
          control,
          summary,
          parent: action.parent,
          offset: control.offset + byteControl.sourceLength,
        });
  }
  if (action !== null) return null;
  const nextBytes = advanceMidgardCekDataBytes({
    control: byteControl,
    sourceBytes,
  });
  return nextBytes === null ? null : advanced({ ...control, bytes: nextBytes });
};

const stepLargeConstructor = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const integer = control.integer!;
  if (integer.stage === MidgardCekDataIntegerStages.Terminal) {
    if (
      action !== null ||
      (sourceBytes !== null && sourceBytes !== undefined)
    ) {
      return null;
    }
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.LargeFields,
      offset: control.offset + integer.sourceLength,
    });
  }
  if (action !== null) return null;
  if (
    integer.stage === MidgardCekDataIntegerStages.Syntax &&
    (sourceBytes === null ||
      sourceBytes === undefined ||
      parseMidgardCekDataLargeConstructorSyntax({
        syntaxBytes: sourceBytes,
        sourceLength: integer.sourceLength,
      }) === null)
  ) {
    return null;
  }
  const nextInteger = advanceMidgardCekDataInteger({
    control: integer,
    sourceBytes,
  });
  return nextInteger === null
    ? null
    : advanced({ ...control, integer: nextInteger });
};

const stepLargeFields = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if (action !== null) return null;
  const bytes = exactSourceBytes({ control, sourceBytes });
  const expectedChildren = control.pendingLargeExpectedChildren!;
  const sequenceHeader = expectedChildren === 0 ? 0x80 : 0x9f;
  const integer = control.integer!;
  const constructorCborRoot = finalizeMidgardCekSourceBlob(integer.blob!);
  if (
    bytes === null ||
    bytes[0] !== sequenceHeader ||
    constructorCborRoot === null
  ) {
    return null;
  }
  const frame = initialMidgardCekDataLargeConstrFrame({
    constructorCborRoot,
    constructorCborLength: BigInt(integer.sourceLength),
    constructorMemory: integer.memory,
    tail: control.frameRoot,
    expectedChildren,
  });
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStages.Fold
        : MidgardCekDataTraverseStages.Head,
    offset: control.offset + 1,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
    pendingLargeExpectedChildren: null,
    integer: null,
  });
};

const stepClose = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if (action !== null) return null;
  const bytes = exactSourceBytes({ control, sourceBytes });
  return bytes !== null && bytes[0] === 0xff
    ? advanced({
        ...control,
        stage: MidgardCekDataTraverseStages.Fold,
        offset: control.offset + 1,
      })
    : null;
};

const stepFinalizeFrame = ({
  control,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "finalizeFrame" }
  >;
}): MidgardCekDataTraverseControl | null => {
  if (!hashMidgardCekDataFrame(action.frame).equals(control.frameRoot)) {
    return null;
  }
  const summary = finalizeMidgardCekDataFrame(action.frame);
  if (summary === null) return null;
  if (action.frame.tail.length === 0) {
    return attachSummary({
      control: { ...control, frameRoot: Buffer.alloc(0) },
      summary,
      parent: action.parent,
      offset: control.offset,
    });
  }
  if (
    action.parent === null ||
    !hashMidgardCekDataFrame(action.parent).equals(action.frame.tail)
  ) {
    return null;
  }
  return attachSummary({
    control: {
      ...control,
      frameRoot: Buffer.from(action.frame.tail),
    },
    summary,
    parent: action.parent,
    offset: control.offset,
  });
};

const stepFold = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if ((sourceBytes !== null && sourceBytes !== undefined) || action === null) {
    return null;
  }
  if (
    "frame" in action &&
    !hashMidgardCekDataFrame(action.frame).equals(control.frameRoot)
  ) {
    return null;
  }
  if (action.kind === "foldList") {
    const frame = foldMidgardCekDataFrameListChild({
      frame: action.frame,
      childIndex: action.childIndex,
      child: action.child,
      siblings: action.siblings,
    });
    return frame === null
      ? null
      : advanced({
          ...control,
          frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
        });
  }
  if (action.kind === "foldMap") {
    const frame = foldMidgardCekDataFrameMapPair({
      frame: action.frame,
      pairIndex: action.pairIndex,
      key: action.key,
      value: action.value,
      keySiblings: action.keySiblings,
      valueSiblings: action.valueSiblings,
    });
    return frame === null
      ? null
      : advanced({
          ...control,
          frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
        });
  }
  return action.kind === "finalizeFrame"
    ? stepFinalizeFrame({ control, action })
    : null;
};

export const advanceMidgardCekDataTraverse = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if (!isWellFormedMidgardCekDataTraverseControl(control)) {
    return null;
  }
  try {
    switch (control.stage) {
      case MidgardCekDataTraverseStages.Head:
        return stepHead({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Integer:
        return stepInteger({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Bytes:
        return stepBytes({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.LargeConstructor:
        return stepLargeConstructor({
          control,
          sourceBytes,
          action,
        });
      case MidgardCekDataTraverseStages.LargeFields:
        return stepLargeFields({
          control,
          sourceBytes,
          action,
        });
      case MidgardCekDataTraverseStages.Close:
        return stepClose({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Fold:
        return stepFold({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Terminal:
        return null;
    }
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataTraverse = (
  control: MidgardCekDataTraverseControl,
): MidgardCekDataSummary | null =>
  isWellFormedMidgardCekDataTraverseControl(control) &&
  control.stage === MidgardCekDataTraverseStages.Terminal
    ? control.result
    : null;

export const buildMidgardCekDataTraverseTrace = ({
  sourceStart,
  source,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekDataTraverseTrace => {
  const bytes = Buffer.from(source);
  const nodes = parseMidgardCekDataNodes(bytes);
  const initial = initialMidgardCekDataTraverseControl({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekDataTraverseTraceStep[] = [];
  let control = initial;
  const currentStage = (): MidgardCekDataTraverseStage => control.stage;

  const emit = (action: MidgardCekDataTraverseAction): void => {
    const span = nextMidgardCekDataTraverseSpan(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceMidgardCekDataTraverse({
      control,
      sourceBytes,
      action,
    });
    if (next === null || !isWellFormedMidgardCekDataTraverseControl(next)) {
      throw new Error("V1 CEK Data traversal evidence failed closed");
    }
    steps.push({
      control,
      sourceBytes: sourceBytes === null ? null : Buffer.from(sourceBytes),
      action,
      next,
    });
    control = next;
  };

  const appendToParent = (
    parent: DataTraceFrame | null,
    summary: MidgardCekDataSummary,
  ): void => {
    if (parent === null) return;
    const next = appendMidgardCekDataFrameChild(parent.frame, summary);
    if (next === null) {
      throw new Error("V1 CEK Data traversal rejected a child summary");
    }
    parent.frame = next;
    parent.childSummaries.push(summary);
  };

  const initialFrame = (
    node: Exclude<ParsedDataNode, { readonly kind: "scalar" }>,
    parent: DataTraceFrame | null,
    largeConstructor: {
      readonly root: Buffer;
      readonly memory: bigint;
    } | null,
  ): MidgardCekDataFrame => {
    const tail =
      parent === null
        ? Buffer.alloc(0)
        : Buffer.from(hashMidgardCekDataFrame(parent.frame));
    switch (node.kind) {
      case "list":
        return initialMidgardCekDataListFrame({
          tail,
          expectedChildren: node.children.length,
        });
      case "map":
        return initialMidgardCekDataMapFrame({
          tail,
          expectedChildren: node.children.length,
        });
      case "constrSmall":
        return initialMidgardCekDataSmallConstrFrame({
          constructor: node.constructor,
          tail,
          expectedChildren: node.children.length,
        });
      case "constrLarge":
        if (largeConstructor === null) {
          throw new Error("V1 CEK Data traversal lost a large constructor");
        }
        return initialMidgardCekDataLargeConstrFrame({
          constructorCborRoot: largeConstructor.root,
          constructorCborLength: BigInt(node.constructorCborLength),
          constructorMemory: largeConstructor.memory,
          tail,
          expectedChildren: node.children.length,
        });
    }
  };

  const operations: DataTraceOperation[] = [
    { kind: "visit", nodeIndex: 0, parent: null },
  ];
  while (operations.length > 0) {
    const operation = operations.pop()!;
    if (operation.kind === "visit") {
      const node = nodes[operation.nodeIndex]!;
      if (
        control.stage !== MidgardCekDataTraverseStages.Head ||
        control.offset !== node.start
      ) {
        throw new Error("V1 CEK Data traversal evidence lost source position");
      }
      if (node.kind === "scalar") {
        emit({
          kind: "headScalar",
          itemLength: node.end - node.start,
        });
        while (
          (currentStage() === MidgardCekDataTraverseStages.Integer &&
            control.integer!.stage !== MidgardCekDataIntegerStages.Terminal) ||
          (currentStage() === MidgardCekDataTraverseStages.Bytes &&
            control.bytes!.stage !== MidgardCekDataBytesStages.Terminal)
        ) {
          emit(null);
        }
        const summary =
          control.integer !== null
            ? finalizeMidgardCekDataInteger(control.integer)
            : finalizeMidgardCekDataBytes(control.bytes!);
        if (summary === null) {
          throw new Error("V1 CEK Data traversal rejected a scalar");
        }
        emit({
          kind: "attachScalar",
          parent: operation.parent?.frame ?? null,
        });
        appendToParent(operation.parent, summary);
        continue;
      }

      let largeConstructor: {
        readonly root: Buffer;
        readonly memory: bigint;
      } | null = null;
      if (node.kind === "map") {
        emit({ kind: "headMap" });
      } else if (node.kind === "constrLarge") {
        emit({
          kind: "headLargeConstructor",
          constructorCborLength: node.constructorCborLength,
          expectedChildren: node.children.length,
        });
        while (
          currentStage() === MidgardCekDataTraverseStages.LargeConstructor
        ) {
          emit(null);
        }
        if (
          currentStage() !== MidgardCekDataTraverseStages.LargeFields ||
          control.integer === null ||
          control.integer.blob === null
        ) {
          throw new Error("V1 CEK Data traversal rejected a large constructor");
        }
        const root = finalizeMidgardCekSourceBlob(control.integer.blob);
        if (root === null) {
          throw new Error("V1 CEK Data traversal lost constructor bytes");
        }
        largeConstructor = {
          root: Buffer.from(root),
          memory: control.integer.memory,
        };
      } else {
        emit({
          kind: "headSequence",
          expectedChildren: node.children.length,
        });
      }
      const frame = initialFrame(node, operation.parent, largeConstructor);
      if (node.kind === "constrLarge") {
        emit(null);
      }
      const context: DataTraceFrame = {
        frame,
        childSummaries: [],
        parent: operation.parent,
        node,
      };
      operations.push({ kind: "finish", context });
      for (let index = node.children.length - 1; index >= 0; index -= 1) {
        operations.push({
          kind: "visit",
          nodeIndex: node.children[index]!,
          parent: context,
        });
      }
      continue;
    }

    const { context } = operation;
    const { node, childSummaries } = context;
    if (
      context.frame.childCount !== node.children.length ||
      childSummaries.length !== node.children.length
    ) {
      throw new Error("V1 CEK Data traversal evidence lost container children");
    }
    if (node.kind !== "map" && node.closesWithBreak) {
      emit(null);
    }
    const leaves = childSummaries.map((child, index) =>
      hashMidgardCekDataFrameChild(index, child),
    );
    const memberships = buildMidgardValidationMerkleMembershipIndex(leaves);
    let frame = context.frame;
    if (node.kind === "map") {
      for (
        let pairIndex = childSummaries.length / 2 - 1;
        pairIndex >= 0;
        pairIndex -= 1
      ) {
        const keyIndex = pairIndex * 2;
        const valueIndex = keyIndex + 1;
        const key = childSummaries[keyIndex]!;
        const value = childSummaries[valueIndex]!;
        const keySiblings = memberships.membershipAt(keyIndex).siblings;
        const valueSiblings = memberships.membershipAt(valueIndex).siblings;
        emit({
          kind: "foldMap",
          frame,
          pairIndex,
          key,
          value,
          keySiblings,
          valueSiblings,
        });
        const next = foldMidgardCekDataFrameMapPair({
          frame,
          pairIndex,
          key,
          value,
          keySiblings,
          valueSiblings,
        });
        if (next === null) {
          throw new Error("V1 CEK Data traversal rejected a map fold");
        }
        frame = next;
      }
    } else {
      for (
        let childIndex = childSummaries.length - 1;
        childIndex >= 0;
        childIndex -= 1
      ) {
        const child = childSummaries[childIndex]!;
        const siblings = memberships.membershipAt(childIndex).siblings;
        emit({
          kind: "foldList",
          frame,
          childIndex,
          child,
          siblings,
        });
        const next = foldMidgardCekDataFrameListChild({
          frame,
          childIndex,
          child,
          siblings,
        });
        if (next === null) {
          throw new Error("V1 CEK Data traversal rejected a sequence fold");
        }
        frame = next;
      }
    }
    const summary = finalizeMidgardCekDataFrame(frame);
    if (summary === null) {
      throw new Error("V1 CEK Data traversal rejected container finalization");
    }
    emit({
      kind: "finalizeFrame",
      frame,
      parent: context.parent?.frame ?? null,
    });
    appendToParent(context.parent, summary);
  }

  if (
    currentStage() !== MidgardCekDataTraverseStages.Terminal ||
    finalizeMidgardCekDataTraverse(control) === null
  ) {
    throw new Error("V1 CEK Data traversal evidence did not terminate");
  }
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    terminal: control,
  });
};
