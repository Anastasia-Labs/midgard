import { fromHex, toHex } from "@lucid-evolution/lucid";

import { CborDeserializationError } from "./errors.js";

type CborHeader = {
  readonly majorType: number;
  readonly additionalInfo: number;
  readonly isIndefinite: boolean;
  readonly value: number | null;
  readonly nextOffset: number;
};

const readByte = (bytes: Uint8Array, offset: number): number => {
  if (offset >= bytes.length) {
    throw new CborDeserializationError({
      message: "Failed to normalize root CBOR array encoding",
      cause: `Unexpected end of CBOR at byte ${offset}`,
    });
  }
  return bytes[offset];
};

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
    value = (value << 8n) | BigInt(bytes[offset + i]);
  }
  if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new CborDeserializationError({
      message: "Failed to normalize root CBOR array encoding",
      cause: `CBOR length exceeds Number.MAX_SAFE_INTEGER: ${value.toString()}`,
    });
  }
  return { value: Number(value), nextOffset: offset + width };
};

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
    if (
      chunkHeader.majorType !== expectedMajorType ||
      chunkHeader.isIndefinite
    ) {
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

const skipItem = (bytes: Uint8Array, offset: number): number => {
  const header = readHeader(bytes, offset);

  switch (header.majorType) {
    case 0:
    case 1:
      return header.nextOffset;
    case 2:
    case 3:
      return header.isIndefinite
        ? skipIndefiniteByteOrTextString(
            bytes,
            header.nextOffset,
            header.majorType,
          )
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
    if (breakOffset + 1 !== bytes.length) {
      throw new CborDeserializationError({
        message: "Failed to normalize root CBOR array encoding",
        cause: `Trailing bytes after root CBOR value at byte ${breakOffset + 1}`,
      });
    }

    return toHex(
      concatBytes(
        prefix,
        encodeDefiniteArrayHeader(itemCount),
        bytes.slice(innerStart, breakOffset),
      ),
    );
  }
};

/**
 * Normalizes Lucid's parameterized-script output, which contains exactly two
 * definite bytestring layers. The outer wrapper is stripped while the inner
 * wrapper is retained as the canonical one-layer ledger script.
 */
export const normalizeAikenParameterizedPlutusScript = (
  cborHex: string,
): string => {
  if (!/^[0-9a-fA-F]+$/.test(cborHex) || cborHex.length % 2 !== 0) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Script must be a non-empty even-length hexadecimal string",
    });
  }
  const bytes = fromHex(cborHex);
  const first = bytes[0];
  if (first === undefined || first >> 5 !== 2 || (first & 0x1f) < 24) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Expected a two-layer definite CBOR bytestring script",
    });
  }
  const outerLengthBytes = first & 0x1f;
  const outerHeaderLength =
    outerLengthBytes === 24
      ? 2
      : outerLengthBytes === 25
        ? 3
        : outerLengthBytes === 26
          ? 5
          : 0;
  if (outerHeaderLength === 0) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Unsupported outer CBOR bytestring length header",
    });
  }
  const readLength = (offset: number, width: number): number => {
    let value = 0;
    for (let index = 0; index < width; index += 1) {
      value = value * 256 + (bytes[offset + index] ?? 0);
    }
    return value;
  };
  const outerLength = readLength(1, outerHeaderLength - 1);
  if (outerLength <= 0 || outerHeaderLength + outerLength !== bytes.length) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Outer CBOR bytestring length does not match script payload",
    });
  }
  const inner = bytes.slice(outerHeaderLength);
  if (inner[0] === undefined || inner[0] >> 5 !== 2 || (inner[0] & 0x1f) < 24) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Expected exactly one retained inner CBOR bytestring layer",
    });
  }
  const innerHeaderLength =
    (inner[0] & 0x1f) === 24
      ? 2
      : (inner[0] & 0x1f) === 25
        ? 3
        : (inner[0] & 0x1f) === 26
          ? 5
          : 0;
  if (innerHeaderLength === 0) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Unsupported inner CBOR bytestring length header",
    });
  }
  const innerLength = (() => {
    let value = 0;
    for (let index = 1; index < innerHeaderLength; index += 1) {
      value = value * 256 + (inner[index] ?? 0);
    }
    return value;
  })();
  if (innerLength <= 0 || innerHeaderLength + innerLength !== inner.length) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Inner CBOR bytestring length does not match script payload",
    });
  }
  if (inner[innerHeaderLength]! >> 5 === 2) {
    throw new CborDeserializationError({
      message: "Failed to normalize parameterized Aiken Plutus script",
      cause: "Script contains more than two CBOR bytestring layers",
    });
  }
  return toHex(inner);
};
