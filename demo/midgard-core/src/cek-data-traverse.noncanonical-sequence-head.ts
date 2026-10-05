import { readCanonicalCborArgument } from "./cek-data-traverse.read-canonical-cbor-argument-wide.js";

/** A nonempty list or compact constructor's fields must use the 9f opener. */
export const hasNonCanonicalDefiniteSequenceHead = (
  source: Uint8Array,
): boolean => {
  const bytes = Buffer.from(source);
  let prefix = 0;
  if (
    bytes.length >= 2 &&
    bytes[0] === 0xd8 &&
    bytes[1]! >= 121 &&
    bytes[1]! <= 127
  )
    prefix = 2;
  else if (bytes.length >= 3 && bytes[0] === 0xd9) {
    const tag = bytes[1]! * 256 + bytes[2]!;
    if (tag < 1280 || tag > 1400) return false;
    prefix = 3;
  }
  const first = bytes[prefix];
  return (
    first !== undefined && first >>> 5 === 4 && first !== 0x80 && first !== 0x9f
  );
};

/** A complete nonminimal Data argument is invalid at its authenticated head. */
export const hasNonCanonicalDataHead = (source: Uint8Array): boolean => {
  if (hasNonCanonicalDefiniteSequenceHead(source)) return true;
  const first = source[0];
  if (first === undefined) return false;
  if (first === 0xbf) return true;
  const major = first >>> 5;
  if (major > 2 && major !== 5 && major !== 6) return false;
  const additional = first & 31;
  const width =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : additional === 27
            ? 8
            : null;
  if (width === null || source.length < 1 + width) return false;
  return readCanonicalCborArgument(source, 0) === null;
};
