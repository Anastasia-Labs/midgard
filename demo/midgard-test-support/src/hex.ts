/**
 * Fixture bytes: a hash, policy id, or key hash whose every byte is the same
 * value, which is how nearly every suite in this workspace spells a
 * distinguishable-but-arbitrary identifier.
 *
 * These exist because roughly fifty test files each defined their own `h28` /
 * `h32`, under argument conventions that disagree with each other while sharing
 * a name:
 *
 *   const h32 = (byte: string) => byte.repeat(32);   // "71" -> 32 bytes
 *   const h32 = (byte: string) => byte.repeat(64);   // "a"  -> 32 bytes
 *   const h32 = (byte: number) => ...padStart(2, "0").repeat(32);
 *   const h32 = (byte: number): Buffer => Buffer.alloc(32, byte);
 *
 * All four produce 32 bytes, so a value copied between two files silently
 * becomes a different length, or a Buffer where a hex string was expected, with
 * no type error to catch it — `"abc".repeat(32)` is a perfectly good string.
 * The definitions here take a byte *value* and a byte *length*, and reject
 * anything that is not one, so the same call means the same bytes everywhere.
 */

const assertByte = (byte: number): void => {
  if (!Number.isInteger(byte) || byte < 0 || byte > 0xff) {
    throw new RangeError(
      `fixture byte must be an integer in 0..255, got ${String(byte)}`,
    );
  }
};

const assertByteLength = (byteLength: number): void => {
  if (!Number.isInteger(byteLength) || byteLength < 1) {
    throw new RangeError(
      `fixture byte length must be a positive integer, got ${String(byteLength)}`,
    );
  }
};

/** `byteLength` copies of `byte`, as a buffer. */
export const fixtureBytes = (byte: number, byteLength: number): Buffer => {
  assertByte(byte);
  assertByteLength(byteLength);
  return Buffer.alloc(byteLength, byte);
};

/** `byteLength` copies of `byte`, lower-case hex, no `0x` prefix. */
export const fixtureHex = (byte: number, byteLength: number): string =>
  fixtureBytes(byte, byteLength).toString("hex");

/** A 28-byte hash — a policy id, key hash, or script hash. */
export const h28 = (byte: number): string => fixtureHex(byte, 28);

/** A 32-byte hash — a transaction id, header hash, or Merkle root. */
export const h32 = (byte: number): string => fixtureHex(byte, 32);

/**
 * Even-length lower-case hex for a fixture *ordinal* — a position in a list —
 * rather than a byte value.
 *
 * The idiom this replaces is `(index + 1).toString(16).padStart(2, "0")`, used
 * to give every entry of a canonical list its own distinguishable script or
 * hash. That spelling silently assumed the list would never pass 255 entries:
 * ordinal 256 renders as `"100"`, which is three hex digits and therefore not
 * bytes at all. The canonical deployment manifest crossed that line (287
 * contract names, 280 reference-script roles) and the assumption failed as an
 * unpadded-hex decode error inside Lucid, far from the fixture that produced
 * it.
 *
 * Ordinals 0..255 keep their historical one-byte spelling, so fixture values
 * and every hash derived from them stay put; wider ordinals grow to two bytes,
 * which can never collide with a one-byte value because the lengths differ.
 */
export const ordinalHex = (ordinal: number): string => {
  if (!Number.isInteger(ordinal) || ordinal < 0) {
    throw new RangeError(
      `fixture ordinal must be a non-negative integer, got ${String(ordinal)}`,
    );
  }
  const digits = ordinal.toString(16);
  return digits.length % 2 === 0 ? digits : `0${digits}`;
};

/**
 * A distinct 32-byte fixture hash for an ordinal that may run past a single
 * byte — the `h32` of a list position rather than of a byte value.
 *
 * Ordinals 0..255 are exactly `h32(ordinal)`, the repeated-byte spelling every
 * suite already uses. Wider ordinals cannot repeat a byte, so they carry their
 * big-endian encoding in the trailing bytes over a zero prefix; that shape can
 * only equal a repeated-byte hash if every byte is zero, which ordinal 0
 * already holds, so the two families stay disjoint.
 */
export const h32ForOrdinal = (ordinal: number): string => {
  if (ordinal <= 0xff) {
    return h32(ordinal);
  }
  const suffix = ordinalHex(ordinal);
  return `${"0".repeat(64 - suffix.length)}${suffix}`;
};
