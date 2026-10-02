import {
  indefiniteMidgardCekDataBytesLength,
  MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
  parseMidgardCekDataBytesSyntax,
} from "./cek-data-bytes.js";
import {
  MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
  parseMidgardCekDataIntegerSyntax,
  parseMidgardCekDataLargeConstructorSyntax,
} from "./cek-data-integer.js";
import { UINT32_MAX } from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import {
  type ParsedNodeHead,
  parseIntegerEnd,
  parseSmallConstructorHead,
  readCanonicalCborArgument,
} from "./cek-data-traverse.read-canonical-cbor-argument-wide.js";

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
      : first === 0x5f
        ? indefiniteMidgardCekDataBytesLength(sourceLength) !== null
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

export const parseDataNodeHead = (
  bytes: Buffer,
  start: number,
): ParsedNodeHead => {
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
