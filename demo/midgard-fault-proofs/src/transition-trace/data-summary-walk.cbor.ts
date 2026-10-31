/** The canonical CBOR heads and byte strings the data summary walk reads. */
export const byteAt = (bytes: Uint8Array, at: number, end: number) =>
  at < end ? bytes[at]! : -1;

export const bigEndian = (bytes: Uint8Array) =>
  bytes.reduce((value, byte) => (value << 8n) | BigInt(byte), 0n);

/** A minimal head: [major, argument (-1 when indefinite), next]. */
export const head = (
  bytes: Uint8Array,
  at: number,
  end: number,
): readonly [number, bigint, number] | null => {
  const first = byteAt(bytes, at, end);
  if (first < 0) return null;
  const major = first >> 5;
  const info = first & 31;
  if (info < 24) return [major, BigInt(info), at + 1];
  if (info <= 27) {
    const size = [1, 2, 4, 8][info - 24]!;
    if (at + 1 + size > end) return null;
    const value = bigEndian(bytes.subarray(at + 1, at + 1 + size));
    const minimal = value.toString(16).length;
    if (value >= 24n && 2 * Math.ceil(minimal / 2) > size)
      return [major, value, at + 1 + size];
    return null;
  }
  return info === 31 ? [major, -1n, at + 1] : null;
};

const chunks = (
  bytes: Uint8Array,
  at: number,
  end: number,
  content: Buffer,
): readonly [Buffer, number] | null => {
  const read = head(bytes, at, end);
  if (read === null || read[0] !== 2) return null;
  const length = Number(read[1]);
  const next = read[2];
  if (!(length >= 1 && length <= 64 && next + length <= end)) return null;
  const joined = Buffer.concat([content, bytes.subarray(next, next + length)]);
  const after = next + length;
  if (byteAt(bytes, after, end) === 255) return [joined, after + 1];
  return length === 64 ? chunks(bytes, after, end, joined) : null;
};

export const byteString = (
  bytes: Uint8Array,
  at: number,
  end: number,
): readonly [Buffer, number] | null => {
  const read = head(bytes, at, end);
  if (read === null || read[0] !== 2) return null;
  const [, length, next] = read;
  if (length === -1n) {
    const joined = chunks(bytes, next, end, Buffer.alloc(0));
    return joined !== null && joined[0].length > 64 ? joined : null;
  }
  if (length <= 64n && next + Number(length) <= end)
    return [
      Buffer.from(bytes.subarray(next, next + Number(length))),
      next + Number(length),
    ];
  return null;
};
