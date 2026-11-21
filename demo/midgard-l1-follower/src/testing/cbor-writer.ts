/** A tiny CBOR writer for hand-built test blocks, including non-canonical forms. */
export type Cbor = Buffer;

export const head = (
  major: number,
  value: number | bigint,
  width?: 0 | 1 | 2 | 4 | 8,
): Buffer => {
  const n = BigInt(value);
  const size =
    width ??
    (n < 24n
      ? 0
      : n < 0x100n
        ? 1
        : n < 0x10000n
          ? 2
          : n < 0x100000000n
            ? 4
            : 8);
  if (size === 0) return Buffer.from([(major << 5) | Number(n)]);
  const info = { 1: 24, 2: 25, 4: 26, 8: 27 }[size];
  const out = Buffer.alloc(1 + size);
  out[0] = (major << 5) | info;
  if (size === 8) out.writeBigUInt64BE(n, 1);
  else out.writeUIntBE(Number(n), 1, size);
  return out;
};

export const uint = (value: number | bigint, width?: 0 | 1 | 2 | 4 | 8): Cbor =>
  head(0, value, width);
export const nint = (value: number | bigint): Cbor =>
  head(1, -BigInt(value) - 1n);
export const bytes = (data: Buffer): Cbor =>
  Buffer.concat([head(2, data.length), data]);
/** An indefinite-length byte string split into chunks. */
export const bytesIndef = (...chunks: Buffer[]): Cbor =>
  Buffer.concat([
    Buffer.from([0x5f]),
    ...chunks.map(bytes),
    Buffer.from([0xff]),
  ]);
export const array = (...items: Cbor[]): Cbor =>
  Buffer.concat([head(4, items.length), ...items]);
export const arrayIndef = (...items: Cbor[]): Cbor =>
  Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);
export const map = (...entries: [Cbor, Cbor][]): Cbor =>
  Buffer.concat([head(5, entries.length), ...entries.flat()]);
export const mapIndef = (...entries: [Cbor, Cbor][]): Cbor =>
  Buffer.concat([Buffer.from([0xbf]), ...entries.flat(), Buffer.from([0xff])]);
export const tag = (number: number, item: Cbor): Cbor =>
  Buffer.concat([head(6, number), item]);
export const nul: Cbor = Buffer.from([0xf6]);
export const bool = (value: boolean): Cbor =>
  Buffer.from([value ? 0xf5 : 0xf4]);
