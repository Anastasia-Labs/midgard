import { fromHex, toHex } from "@lucid-evolution/lucid";
import { CborDeserializationError } from "./common.js";

/**
 * Utilities for walking CBOR without fully decoding it into a typed
 * representation.
 *
 * Midgard uses this to normalize only one specific encoding quirk: some
 * upstream encoders emit the root array with indefinite-length encoding while
 * the rest of the payload is already canonical enough for our use. The helpers
 * below operate as a structural cursor so the original bytes stay untouched
 * except for the root array header.
 */
type CborHeader = {
  readonly majorType: number;
  readonly additionalInfo: number;
  readonly isIndefinite: boolean;
  readonly value: number | null;
  readonly nextOffset: number;
};

/**
 * Reads a single byte and converts an out-of-range access into a stable
 * deserialization error instead of leaking `undefined` into later offset math.
 */
const readByte = (bytes: Uint8Array, offset: number): number => {
  if (offset >= bytes.length) {
    throw new CborDeserializationError({
      message: "Failed to normalize root CBOR array encoding",
      cause: `Unexpected end of CBOR at byte ${offset}`,
    });
  }
  return bytes[offset];
};

/**
 * Decodes the integer payload attached to a CBOR header.
 *
 * Offsets in this file are tracked as JavaScript numbers, so lengths larger
 * than `Number.MAX_SAFE_INTEGER` are rejected early rather than silently
 * truncating cursor calculations.
 */
const readUint = (
  bytes: Uint8Array,
  offset: number,
  width: number,
): { readonly value: number; readonly nextOffset: number } => {
  if (offset + width > bytes.length) {
    throw new CborDeserializationError({
      message: "Failed to normalize root CBOR array encoding",
      cause: `Unexpected end of CBOR while reading ${width}-byte integer at byte ${offset}`,
    });
  }

  let value = 0n;
  for (let i = 0; i < width; i += 1) {
    value = (value << 8n) | BigInt(bytes[offset + i]!);
  }

  if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new CborDeserializationError({
      message: "Failed to normalize root CBOR array encoding",
      cause: `CBOR length exceeds Number.MAX_SAFE_INTEGER: ${value.toString()}`,
    });
  }

  return { value: Number(value), nextOffset: offset + width };
};

/**
 * Parses one CBOR initial byte plus any additional integer payload needed to
 * describe its length/value.
 */
const readHeader = (bytes: Uint8Array, offset: number): CborHeader => {
  const initialByte = readByte(bytes, offset);
  const majorType = initialByte >> 5;
  const additionalInfo = initialByte & 0x1f;
  const next = offset + 1;

  if (additionalInfo < 24) {
    return {
      majorType,
      additionalInfo,
      isIndefinite: false,
      value: additionalInfo,
      nextOffset: next,
    };
  }

  switch (additionalInfo) {
    case 24: {
      const { value, nextOffset } = readUint(bytes, next, 1);
      return {
        majorType,
        additionalInfo,
        isIndefinite: false,
        value,
        nextOffset,
      };
    }
    case 25: {
      const { value, nextOffset } = readUint(bytes, next, 2);
      return {
        majorType,
        additionalInfo,
        isIndefinite: false,
        value,
        nextOffset,
      };
    }
    case 26: {
      const { value, nextOffset } = readUint(bytes, next, 4);
      return {
        majorType,
        additionalInfo,
        isIndefinite: false,
        value,
        nextOffset,
      };
    }
    case 27: {
      const { value, nextOffset } = readUint(bytes, next, 8);
      return {
        majorType,
        additionalInfo,
        isIndefinite: false,
        value,
        nextOffset,
      };
    }
    case 31:
      return {
        majorType,
        additionalInfo,
        isIndefinite: true,
        value: null,
        nextOffset: next,
      };
    default:
      throw new CborDeserializationError({
        message: "Failed to normalize root CBOR array encoding",
        cause: `Unsupported CBOR additional information value: ${additionalInfo}`,
      });
  }
};

/**
 * Advances across the payload of a definite-length byte or text string.
 */
const skipDefiniteByteOrTextString = (
  bytes: Uint8Array,
  offset: number,
  byteLength: number,
): number => {
  const nextOffset = offset + byteLength;
  if (nextOffset > bytes.length) {
    throw new CborDeserializationError({
      message: "Failed to normalize root CBOR array encoding",
      cause: `Unexpected end of CBOR while skipping ${byteLength} bytes at byte ${offset}`,
    });
  }
  return nextOffset;
};

/**
 * Walks an indefinite-length byte or text string chunk-by-chunk until the
 * terminating break byte is reached.
 */
const skipIndefiniteByteOrTextString = (
  bytes: Uint8Array,
  offset: number,
  expectedMajorType: 2 | 3,
): number => {
  let cursor = offset;
  while (true) {
    const current = readByte(bytes, cursor);
    if (current === 0xff) {
      return cursor + 1;
    }

    const chunkHeader = readHeader(bytes, cursor);
    if (chunkHeader.majorType !== expectedMajorType || chunkHeader.isIndefinite) {
      throw new CborDeserializationError({
        message: "Failed to normalize root CBOR array encoding",
        cause: `Invalid CBOR chunk inside indefinite string at byte ${cursor}`,
      });
    }

    cursor = skipDefiniteByteOrTextString(
      bytes,
      chunkHeader.nextOffset,
      chunkHeader.value ?? 0,
    );
  }
};

/**
 * Skips one complete CBOR item starting at `offset`, recursively traversing
 * nested arrays, maps, and semantic tags.
 */
const skipItem = (bytes: Uint8Array, offset: number): number => {
  const header = readHeader(bytes, offset);

  switch (header.majorType) {
    case 0:
    case 1:
      return header.nextOffset;
    case 2:
    case 3:
      return header.isIndefinite
        ? skipIndefiniteByteOrTextString(bytes, header.nextOffset, header.majorType)
        : skipDefiniteByteOrTextString(
            bytes,
            header.nextOffset,
            header.value ?? 0,
          );
    case 4: {
      let cursor = header.nextOffset;
      if (header.isIndefinite) {
        while (readByte(bytes, cursor) !== 0xff) {
          cursor = skipItem(bytes, cursor);
        }
        return cursor + 1;
      }
      for (let i = 0; i < (header.value ?? 0); i += 1) {
        cursor = skipItem(bytes, cursor);
      }
      return cursor;
    }
    case 5: {
      let cursor = header.nextOffset;
      if (header.isIndefinite) {
        while (readByte(bytes, cursor) !== 0xff) {
          cursor = skipItem(bytes, cursor);
          cursor = skipItem(bytes, cursor);
        }
        return cursor + 1;
      }
      for (let i = 0; i < (header.value ?? 0); i += 1) {
        cursor = skipItem(bytes, cursor);
        cursor = skipItem(bytes, cursor);
      }
      return cursor;
    }
    case 6:
      return skipItem(bytes, header.nextOffset);
    case 7:
      if (header.isIndefinite) {
        throw new CborDeserializationError({
          message: "Failed to normalize root CBOR array encoding",
          cause: `Unexpected break marker at byte ${offset}`,
        });
      }
      return header.nextOffset;
    default:
      throw new CborDeserializationError({
        message: "Failed to normalize root CBOR array encoding",
        cause: `Unsupported CBOR major type: ${header.majorType}`,
      });
  }
};

/**
 * Encodes a definite-length CBOR array header using the smallest legal width
 * for the provided item count.
 */
const encodeDefiniteArrayHeader = (length: number): Uint8Array => {
  if (length < 24) {
    return Uint8Array.from([0x80 | length]);
  }
  if (length < 0x100) {
    return Uint8Array.from([0x98, length]);
  }
  if (length < 0x1_0000) {
    return Uint8Array.from([0x99, (length >> 8) & 0xff, length & 0xff]);
  }
  if (length < 0x1_0000_0000) {
    return Uint8Array.from([
      0x9a,
      (length >>> 24) & 0xff,
      (length >>> 16) & 0xff,
      (length >>> 8) & 0xff,
      length & 0xff,
    ]);
  }

  const big = BigInt(length);
  return Uint8Array.from([
    0x9b,
    Number((big >> 56n) & 0xffn),
    Number((big >> 48n) & 0xffn),
    Number((big >> 40n) & 0xffn),
    Number((big >> 32n) & 0xffn),
    Number((big >> 24n) & 0xffn),
    Number((big >> 16n) & 0xffn),
    Number((big >> 8n) & 0xffn),
    Number(big & 0xffn),
  ]);
};

/**
 * Concatenates byte slices without re-encoding any inner CBOR items.
 */
const concatBytes = (...parts: Uint8Array[]): Uint8Array => {
  const totalLength = parts.reduce((sum, part) => sum + part.length, 0);
  const result = new Uint8Array(totalLength);
  let offset = 0;
  for (const part of parts) {
    result.set(part, offset);
    offset += part.length;
  }
  return result;
};

/**
 * Rewrites only the root indefinite-length array header into a definite-length
 * header.
 *
 * Tagged wrappers preceding the root array are preserved exactly. If the input
 * is already definite, the original hex is returned unchanged. Any trailing
 * bytes after the root value are rejected because they would make the payload
 * ambiguous for downstream hashing and decoding.
 */
export const normalizeRootIndefiniteArrayEncoding = (
  cborHex: string,
): string => {
  const bytes = fromHex(cborHex);

  let cursor = 0;
  while (true) {
    const header = readHeader(bytes, cursor);
    if (header.majorType === 6) {
      cursor = header.nextOffset;
      continue;
    }

    if (header.majorType !== 4 || !header.isIndefinite) {
      return cborHex;
    }

    const prefix = bytes.slice(0, cursor);
    const innerStart = header.nextOffset;

    let innerCursor = innerStart;
    let itemCount = 0;
    while (readByte(bytes, innerCursor) !== 0xff) {
      innerCursor = skipItem(bytes, innerCursor);
      itemCount += 1;
    }

    const breakOffset = innerCursor;
    const rewritten = concatBytes(
      prefix,
      encodeDefiniteArrayHeader(itemCount),
      bytes.slice(innerStart, breakOffset),
    );

    if (breakOffset + 1 !== bytes.length) {
      throw new CborDeserializationError({
        message: "Failed to normalize root CBOR array encoding",
        cause: `Trailing bytes after root CBOR value at byte ${breakOffset + 1}`,
      });
    }

    return toHex(rewritten);
  }
};
