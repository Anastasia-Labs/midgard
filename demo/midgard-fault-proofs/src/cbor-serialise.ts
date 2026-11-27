/**
 * Off-chain twins of the CBOR encodings fault-proof checkpoints hash: Aiken
 * `cbor.serialise` on an integer (Plutus `serialiseData`) and
 * `encode_definite_bytes`.
 */

const HEAD_LIMIT = 1n << 64n;
const BIGNUM_CHUNK_BYTES = 64;

/** A CBOR head: major type `major` with an argument below 2^64. */
export const cborHead = (major: number, input: bigint | number): Buffer => {
  const value = BigInt(input);
  if (value < 0n || value >= HEAD_LIMIT)
    throw new Error("CBOR head argument is outside 0..2^64-1");
  if (value < 24n) return Buffer.from([(major << 5) | Number(value)]);
  const [width, additional] =
    value <= 0xffn
      ? [1, 24]
      : value <= 0xffffn
        ? [2, 25]
        : value <= 0xffffffffn
          ? [4, 26]
          : [8, 27];
  return Buffer.concat([
    Buffer.from([(major << 5) | additional]),
    Buffer.from(value.toString(16).padStart(width * 2, "0"), "hex"),
  ]);
};

/** Twin of Aiken `encode_definite_bytes`: a definite byte string. */
export const cborDefiniteBytes = (bytes: Uint8Array): Buffer =>
  Buffer.concat([cborHead(2, bytes.length), bytes]);

const magnitudeBytes = (magnitude: bigint): Buffer => {
  const hex = magnitude.toString(16);
  return Buffer.from(hex.length % 2 === 0 ? hex : `0${hex}`, "hex");
};

/**
 * Twin of Aiken `cbor.serialise` on an integer of any width. Below 2^64 in
 * magnitude it is a major-0 or major-1 head; beyond, a tag-2 or tag-3 bignum
 * whose byte string is definite up to 64 bytes and 64-byte chunks after.
 */
export const cborSerialiseInteger = (value: bigint): Buffer => {
  const negative = value < 0n;
  const magnitude = negative ? -1n - value : value;
  if (magnitude < HEAD_LIMIT) return cborHead(negative ? 1 : 0, magnitude);
  const bytes = magnitudeBytes(magnitude);
  const tag = Buffer.from([negative ? 0xc3 : 0xc2]);
  if (bytes.length <= BIGNUM_CHUNK_BYTES)
    return Buffer.concat([tag, cborDefiniteBytes(bytes)]);
  const chunks: Buffer[] = [];
  for (let offset = 0; offset < bytes.length; offset += BIGNUM_CHUNK_BYTES)
    chunks.push(
      cborDefiniteBytes(bytes.subarray(offset, offset + BIGNUM_CHUNK_BYTES)),
    );
  return Buffer.concat([
    tag,
    Buffer.from([0x5f]),
    ...chunks,
    Buffer.from([0xff]),
  ]);
};
