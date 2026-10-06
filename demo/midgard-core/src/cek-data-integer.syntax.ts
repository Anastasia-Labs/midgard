import { parseChunkedMagnitudeSyntax } from "./cek-data-integer.chunked-magnitude.js";

export const MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES = 14;

export const UINT32_MAX = 0xffff_ffff;
export const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

type CborArgument = {
  readonly major: number;
  readonly value: bigint;
  readonly nextOffset: number;
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
  return {
    major,
    value,
    nextOffset: offset + 1 + byteLength,
  };
};

const unsignedByteLength = (value: bigint): bigint => {
  let size = 1n;
  let remaining = value;
  while (remaining >= 256n) {
    size += 1n;
    remaining >>= 8n;
  }
  return size;
};

/** Memory derived from the integer's authenticated prefix and canonical extent. */
export const parseMidgardCekDataIntegerSyntax = ({
  syntaxBytes,
  sourceLength,
}: {
  readonly syntaxBytes: Uint8Array;
  readonly sourceLength: number;
}): bigint | null => {
  try {
    if (
      !Number.isInteger(sourceLength) ||
      sourceLength < 1 ||
      sourceLength > UINT32_MAX ||
      syntaxBytes.length !==
        Math.min(sourceLength, MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES)
    ) {
      return null;
    }
    const first = syntaxBytes[0]!;
    if (first >>> 5 <= 1) {
      const argument = readCanonicalCborArgument(syntaxBytes, 0);
      if (
        argument === null ||
        argument.major > 1 ||
        argument.value > UINT64_MAX ||
        argument.nextOffset !== sourceLength
      ) {
        return null;
      }
      return 4n + unsignedByteLength(argument.value * 2n);
    }
    if (first !== 0xc2 && first !== 0xc3) return null;
    if (syntaxBytes[1] === 0x5f) {
      return parseChunkedMagnitudeSyntax(syntaxBytes, sourceLength);
    }
    const magnitude = readCanonicalCborArgument(syntaxBytes, 1);
    if (
      magnitude === null ||
      magnitude.major !== 2 ||
      magnitude.value < 9n ||
      magnitude.value > 64n ||
      BigInt(magnitude.nextOffset) + magnitude.value !== BigInt(sourceLength) ||
      magnitude.nextOffset >= syntaxBytes.length
    ) {
      return null;
    }
    const firstMagnitudeByte = syntaxBytes[magnitude.nextOffset]!;
    if (firstMagnitudeByte === 0) return null;
    return 4n + magnitude.value + (firstMagnitudeByte >= 0x80 ? 1n : 0n);
  } catch {
    return null;
  }
};

/**
 * Restricts the integer grammar to a canonical nonnegative constructor
 * alternative above 127. Positive bignums are accepted without materializing
 * their value; major-one and tag-three encodings fail closed.
 */
export const parseMidgardCekDataLargeConstructorSyntax = ({
  syntaxBytes,
  sourceLength,
}: {
  readonly syntaxBytes: Uint8Array;
  readonly sourceLength: number;
}): bigint | null => {
  const memory = parseMidgardCekDataIntegerSyntax({
    syntaxBytes,
    sourceLength,
  });
  if (memory === null) return null;
  const first = syntaxBytes[0]!;
  if (first === 0xc2) return memory;
  const argument = readCanonicalCborArgument(syntaxBytes, 0);
  return argument !== null &&
    argument.major === 0 &&
    argument.value > 127n &&
    argument.nextOffset === sourceLength
    ? memory
    : null;
};
