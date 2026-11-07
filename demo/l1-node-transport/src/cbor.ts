/**
 * The small CBOR codec the transport needs: frame headers, and walking the
 * node's raw ledger answers. Integers decode as `number` when safe and as
 * `bigint` otherwise. Maps decode as {@link CborMap} (ordered entries), so
 * non-text keys such as stake credentials survive.
 */

export class CborTag {
  constructor(
    readonly tag: number | bigint,
    readonly value: CborValue,
  ) {}
}

export class CborMap {
  constructor(
    readonly entries: ReadonlyArray<readonly [CborValue, CborValue]>,
  ) {}

  /** The map as a text-keyed record; any other key, or a duplicate, throws. */
  textRecord(subject: string): Record<string, CborValue> {
    const record: Record<string, CborValue> = Object.create(null) as Record<
      string,
      CborValue
    >;
    for (const [key, value] of this.entries) {
      if (typeof key !== "string")
        throw new CborError(`${subject} has a non-text key`);
      if (Object.prototype.hasOwnProperty.call(record, key))
        throw new CborError(`${subject} repeats key ${key}`);
      record[key] = value;
    }
    return record;
  }
}

export type CborValue =
  | number
  | bigint
  | string
  | Uint8Array
  | boolean
  | null
  | undefined
  | CborValue[]
  | CborMap
  | CborTag;

/** A value to encode. Plain objects encode as text-keyed maps. */
export type CborInput =
  | number
  | bigint
  | string
  | Uint8Array
  | boolean
  | null
  | readonly CborInput[]
  | CborMap
  | CborTag
  | { readonly [key: string]: CborInput | undefined };

export class CborError extends Error {
  override readonly name = "CborError";
}

const MAX_DEPTH = 1024;

const head = (major: number, value: number | bigint, out: number[]): void => {
  const n = BigInt(value);
  if (n < 0n || n > 0xffff_ffff_ffff_ffffn)
    throw new CborError("integer is outside the CBOR range");
  const m = major << 5;
  if (n < 24n) out.push(m | Number(n));
  else if (n < 0x100n) out.push(m | 24, Number(n));
  else if (n < 0x1_0000n) out.push(m | 25, Number(n >> 8n), Number(n & 0xffn));
  else if (n < 0x1_0000_0000n) {
    out.push(m | 26);
    for (let shift = 24n; shift >= 0n; shift -= 8n)
      out.push(Number((n >> shift) & 0xffn));
  } else {
    out.push(m | 27);
    for (let shift = 56n; shift >= 0n; shift -= 8n)
      out.push(Number((n >> shift) & 0xffn));
  }
};

const encodeInto = (value: CborInput, out: number[], depth: number): void => {
  if (depth > MAX_DEPTH) throw new CborError("value nests too deeply");
  if (typeof value === "number" || typeof value === "bigint") {
    if (typeof value === "number" && !Number.isSafeInteger(value))
      throw new CborError("only safe integers encode as numbers");
    const n = BigInt(value);
    if (n >= 0n) head(0, n, out);
    else head(1, -1n - n, out);
  } else if (typeof value === "string") {
    const bytes = new TextEncoder().encode(value);
    head(3, bytes.length, out);
    for (const byte of bytes) out.push(byte);
  } else if (value instanceof Uint8Array) {
    head(2, value.length, out);
    for (const byte of value) out.push(byte);
  } else if (typeof value === "boolean") {
    out.push(value ? 0xf5 : 0xf4);
  } else if (value === null) {
    out.push(0xf6);
  } else if (Array.isArray(value)) {
    head(4, value.length, out);
    for (const item of value as readonly CborInput[])
      encodeInto(item, out, depth + 1);
  } else if (value instanceof CborMap) {
    head(5, value.entries.length, out);
    for (const [key, item] of value.entries) {
      encodeInto(key as CborInput, out, depth + 1);
      encodeInto(item as CborInput, out, depth + 1);
    }
  } else if (value instanceof CborTag) {
    head(6, value.tag, out);
    encodeInto(value.value as CborInput, out, depth + 1);
  } else {
    const entries = Object.entries(value).filter(
      (entry): entry is [string, CborInput] => entry[1] !== undefined,
    );
    head(5, entries.length, out);
    for (const [key, item] of entries) {
      encodeInto(key, out, depth + 1);
      encodeInto(item, out, depth + 1);
    }
  }
};

export const encodeCbor = (value: CborInput): Uint8Array => {
  const out: number[] = [];
  encodeInto(value, out, 0);
  return Uint8Array.from(out);
};

/** Reads items from one buffer, keeping each item's exact byte slice. */
export class CborReader {
  #offset = 0;
  readonly bytes: Uint8Array;

  constructor(bytes: Uint8Array) {
    // A plain view, so every slice is a Uint8Array (never a Buffer).
    this.bytes = new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.length);
  }

  get offset(): number {
    return this.#offset;
  }

  get done(): boolean {
    return this.#offset >= this.bytes.length;
  }

  #byte(): number {
    if (this.#offset >= this.bytes.length)
      throw new CborError("CBOR item is truncated");
    return this.bytes[this.#offset++]!;
  }

  #view(size: number): DataView {
    if (this.#offset + size > this.bytes.length)
      throw new CborError("CBOR item is truncated");
    const view = new DataView(
      this.bytes.buffer,
      this.bytes.byteOffset + this.#offset,
      size,
    );
    this.#offset += size;
    return view;
  }

  #argument(info: number): bigint | null {
    if (info < 24) return BigInt(info);
    const size =
      info === 24 ? 1 : info === 25 ? 2 : info === 26 ? 4 : info === 27 ? 8 : 0;
    if (info === 31) return null;
    if (size === 0) throw new CborError("CBOR item has a reserved length");
    let n = 0n;
    for (let i = 0; i < size; i++) n = (n << 8n) | BigInt(this.#byte());
    return n;
  }

  #length(argument: bigint | null): number | null {
    if (argument === null) return null;
    if (argument > BigInt(this.bytes.length - this.#offset))
      throw new CborError("CBOR length exceeds the input");
    return Number(argument);
  }

  #isBreak(): boolean {
    if (this.#offset >= this.bytes.length)
      throw new CborError("indefinite CBOR item is unterminated");
    if (this.bytes[this.#offset] === 0xff) {
      this.#offset++;
      return true;
    }
    return false;
  }

  #chunks(major: number, length: number | null): Uint8Array {
    if (length !== null) {
      const slice = this.bytes.subarray(this.#offset, this.#offset + length);
      this.#offset += length;
      return slice;
    }
    const parts: Uint8Array[] = [];
    while (!this.#isBreak()) {
      const initial = this.#byte();
      if (initial >> 5 !== major)
        throw new CborError("indefinite string has a foreign chunk");
      const chunkLength = this.#length(this.#argument(initial & 0x1f));
      if (chunkLength === null)
        throw new CborError("indefinite string chunk is itself indefinite");
      parts.push(this.#chunks(major, chunkLength));
    }
    const joined = new Uint8Array(
      parts.reduce((n, part) => n + part.length, 0),
    );
    let at = 0;
    for (const part of parts) {
      joined.set(part, at);
      at += part.length;
    }
    return joined;
  }

  /** Decodes the next item. */
  read(depth = 0): CborValue {
    if (depth > MAX_DEPTH) throw new CborError("CBOR item nests too deeply");
    const initial = this.#byte();
    const major = initial >> 5;
    const info = initial & 0x1f;
    if (major === 7) {
      switch (info) {
        case 20:
          return false;
        case 21:
          return true;
        case 22:
          return null;
        case 23:
          return undefined;
        case 25: {
          const view = this.#view(2);
          return halfFloat(view.getUint16(0));
        }
        case 26: {
          const view = this.#view(4);
          return view.getFloat32(0);
        }
        case 27: {
          const view = this.#view(8);
          return view.getFloat64(0);
        }
        default:
          if (info < 20) return info;
          if (info === 24) return this.#byte();
          throw new CborError("CBOR simple value is unsupported");
      }
    }
    const argument = this.#argument(info);
    switch (major) {
      case 0:
      case 1: {
        if (argument === null)
          throw new CborError("integer cannot be indefinite");
        const n = major === 0 ? argument : -1n - argument;
        return n >= BigInt(Number.MIN_SAFE_INTEGER) &&
          n <= BigInt(Number.MAX_SAFE_INTEGER)
          ? Number(n)
          : n;
      }
      case 2:
        // A copy: a decoded value never pins the frame it came from.
        return this.#chunks(2, this.#length(argument)).slice();
      case 3:
        return new TextDecoder("utf-8", { fatal: true }).decode(
          this.#chunks(3, this.#length(argument)),
        );
      case 4: {
        const items: CborValue[] = [];
        const length = this.#length(argument);
        if (length === null)
          while (!this.#isBreak()) items.push(this.read(depth + 1));
        else for (let i = 0; i < length; i++) items.push(this.read(depth + 1));
        return items;
      }
      case 5: {
        const entries: Array<readonly [CborValue, CborValue]> = [];
        const length = this.#length(argument);
        const pair = () =>
          [this.read(depth + 1), this.read(depth + 1)] as const;
        if (length === null) while (!this.#isBreak()) entries.push(pair());
        else for (let i = 0; i < length; i++) entries.push(pair());
        return new CborMap(entries);
      }
      default: {
        if (argument === null) throw new CborError("tag cannot be indefinite");
        const tag =
          argument <= BigInt(Number.MAX_SAFE_INTEGER)
            ? Number(argument)
            : argument;
        return new CborTag(tag, this.read(depth + 1));
      }
    }
  }

  /** Skips the next item and returns its exact bytes. */
  readRaw(): Uint8Array {
    const start = this.#offset;
    this.read();
    return this.bytes.subarray(start, this.#offset);
  }

  /** Reads an array head; null for an indefinite array. */
  readArrayHeader(): number | null {
    const initial = this.#byte();
    if (initial >> 5 !== 4) throw new CborError("CBOR item is not an array");
    return this.#length(this.#argument(initial & 0x1f));
  }

  /** Reads a map head; null for an indefinite map. */
  readMapHeader(): number | null {
    const initial = this.#byte();
    if (initial >> 5 !== 5) throw new CborError("CBOR item is not a map");
    return this.#length(this.#argument(initial & 0x1f));
  }

  /** For an indefinite container: whether its break comes next (consumed). */
  atBreak(): boolean {
    return this.#isBreak();
  }
}

const halfFloat = (bits: number): number => {
  const exponent = (bits >> 10) & 0x1f;
  const fraction = bits & 0x3ff;
  const sign = bits & 0x8000 ? -1 : 1;
  if (exponent === 0) return sign * 2 ** -14 * (fraction / 1024);
  if (exponent === 31) return fraction === 0 ? sign * Infinity : NaN;
  return sign * 2 ** (exponent - 15) * (1 + fraction / 1024);
};

/** Decodes exactly one item; trailing bytes throw. */
export const decodeCbor = (bytes: Uint8Array): CborValue => {
  const reader = new CborReader(bytes);
  const value = reader.read();
  if (!reader.done) throw new CborError("CBOR item has trailing bytes");
  return value;
};
