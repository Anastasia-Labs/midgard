// The minimal CBOR codec the fake sidecar speaks: unsigned and negative
// integers (bigint), byte and text strings, arrays, maps and tags. Kept apart
// from the sidecar so each module stays under the module-size cap.

const head = (major, value) => {
  const n = BigInt(value);
  if (n < 24n) return Buffer.from([(major << 5) | Number(n)]);
  if (n < 0x100n) return Buffer.from([(major << 5) | 24, Number(n)]);
  if (n < 0x10000n) {
    const b = Buffer.alloc(3);
    b[0] = (major << 5) | 25;
    b.writeUInt16BE(Number(n), 1);
    return b;
  }
  if (n < 0x100000000n) {
    const b = Buffer.alloc(5);
    b[0] = (major << 5) | 26;
    b.writeUInt32BE(Number(n), 1);
    return b;
  }
  const b = Buffer.alloc(9);
  b[0] = (major << 5) | 27;
  b.writeBigUInt64BE(n, 1);
  return b;
};

/** Encodes naturals, text, bytes, booleans, null, arrays and text-keyed objects. */
export const encodeCbor = (value) => {
  if (value === null) return Buffer.from([0xf6]);
  if (value === true) return Buffer.from([0xf5]);
  if (value === false) return Buffer.from([0xf4]);
  if (typeof value === "number" || typeof value === "bigint") {
    if (BigInt(value) < 0n) throw new TypeError("negative CBOR integer");
    return head(0, value);
  }
  if (typeof value === "string") {
    const text = Buffer.from(value, "utf8");
    return Buffer.concat([head(3, text.length), text]);
  }
  if (value instanceof Uint8Array)
    return Buffer.concat([head(2, value.length), value]);
  if (Array.isArray(value))
    return Buffer.concat([head(4, value.length), ...value.map(encodeCbor)]);
  if (value instanceof Map)
    return Buffer.concat([
      head(5, value.size),
      ...[...value].flatMap(([k, v]) => [encodeCbor(k), encodeCbor(v)]),
    ]);
  if (typeof value === "object") {
    const entries = Object.entries(value).filter(([, v]) => v !== undefined);
    return Buffer.concat([
      head(5, entries.length),
      ...entries.flatMap(([k, v]) => [encodeCbor(k), encodeCbor(v)]),
    ]);
  }
  throw new TypeError(`cannot encode ${typeof value} as CBOR`);
};

/** Decodes the subset `encodeCbor` writes; maps decode as objects. */
export const decodeCbor = (bytes) => {
  const buffer = Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let offset = 0;
  const read = () => {
    const initial = buffer[offset++];
    if (initial === undefined) throw new Error("truncated CBOR");
    const major = initial >> 5;
    const info = initial & 31;
    if (major === 7) {
      if (info === 20) return false;
      if (info === 21) return true;
      if (info === 22) return null;
      throw new Error("unsupported CBOR simple value");
    }
    let n;
    if (info < 24) n = BigInt(info);
    else if (info === 24) (n = BigInt(buffer.readUInt8(offset))), (offset += 1);
    else if (info === 25)
      (n = BigInt(buffer.readUInt16BE(offset))), (offset += 2);
    else if (info === 26)
      (n = BigInt(buffer.readUInt32BE(offset))), (offset += 4);
    else if (info === 27) (n = buffer.readBigUInt64BE(offset)), (offset += 8);
    else throw new Error("unsupported CBOR length");
    const length = Number(n);
    switch (major) {
      case 0:
        return n <= BigInt(Number.MAX_SAFE_INTEGER) ? Number(n) : n;
      case 2: {
        const value = new Uint8Array(buffer.subarray(offset, offset + length));
        offset += length;
        return value;
      }
      case 3: {
        const value = buffer.toString("utf8", offset, offset + length);
        offset += length;
        return value;
      }
      case 4:
        return Array.from({ length }, read);
      case 5: {
        const record = {};
        for (let i = 0; i < length; i++) {
          const key = read();
          if (typeof key !== "string") throw new Error("non-text map key");
          record[key] = read();
        }
        return record;
      }
      default:
        throw new Error(`unsupported CBOR major type ${major}`);
    }
  };
  const value = read();
  if (offset !== buffer.length) throw new Error("trailing CBOR bytes");
  return value;
};
