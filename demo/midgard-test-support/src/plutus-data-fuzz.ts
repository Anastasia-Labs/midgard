/**
 * Seeded generators for Plutus Data differential tests.
 *
 * Every generator is a pure function of its `FuzzRng`, so a failing case is
 * reproduced from its seed alone. Three families are provided:
 *
 * - `randomPlutusDataCbor`: raw CBOR bytes in every encoding a Plutus Data
 *   reader meets (definite and indefinite containers, chunked bytes, bignum
 *   tags, compact and general constructor tags, duplicate and unsorted map
 *   keys), optionally mixed with malformed items so rejections are exercised;
 * - `randomDataTree`: a value tree built through caller-supplied constructors,
 *   so one generator serves every in-memory Data shape (Lucid, semantic, ...);
 * - `deepPlutusDataCbor`: one-path chains of a given depth, for depth tests.
 */

export type FuzzRng = {
  /** A float in [0, 1). */
  readonly next: () => number;
  /** An integer in [0, bound). */
  readonly int: (bound: number) => number;
  readonly chance: (probability: number) => boolean;
  readonly pick: <T>(items: readonly T[]) => T;
};

/** mulberry32: small, fast and good enough for test-case selection. */
export const makeFuzzRng = (seed: number): FuzzRng => {
  let state = seed >>> 0;
  const next = (): number => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const int = (bound: number): number => Math.floor(next() * bound);
  return {
    next,
    int,
    chance: (probability) => next() < probability,
    pick: (items) => items[int(items.length)]!,
  };
};

const TWO_POW_64 = 1n << 64n;

/** Integer edge values every Data reader must round-trip. */
export const PLUTUS_DATA_EDGE_INTEGERS: readonly bigint[] = [
  0n,
  1n,
  -1n,
  23n,
  24n,
  -24n,
  -25n,
  255n,
  256n,
  65535n,
  65536n,
  (1n << 32n) - 1n,
  1n << 32n,
  1n << 63n,
  -(1n << 63n),
  TWO_POW_64 - 1n,
  TWO_POW_64,
  TWO_POW_64 + 1n,
  -TWO_POW_64,
  -TWO_POW_64 - 1n,
  -TWO_POW_64 + 1n,
  1n << 128n,
  -(1n << 128n),
  (1n << 520n) + 12345n,
];

/** Byte-string lengths straddling the 64-byte chunk boundary. */
export const PLUTUS_DATA_EDGE_BYTE_LENGTHS: readonly number[] = [
  0, 1, 63, 64, 65, 128, 1000,
];

export type CborHeadWidth = "minimal" | "non-minimal";

/** Appends a CBOR head; `non-minimal` widens it by one size step. */
export const pushCborHead = (
  out: number[],
  major: number,
  value: bigint,
  width: CborHeadWidth = "minimal",
): void => {
  const prefix = major << 5;
  let size =
    value < 24n
      ? 0
      : value < 0x100n
        ? 1
        : value < 0x10000n
          ? 2
          : value < 0x100000000n
            ? 4
            : 8;
  if (width === "non-minimal" && size < 8) {
    size = size === 0 ? 1 : size * 2;
  }
  if (size === 0) {
    out.push(prefix | Number(value));
    return;
  }
  out.push(prefix | { 1: 24, 2: 25, 4: 26, 8: 27 }[size]!);
  for (let i = size - 1; i >= 0; i -= 1) {
    out.push(Number((value >> BigInt(i * 8)) & 0xffn));
  }
};

const bigintBytes = (value: bigint, leadingZeros: number): number[] => {
  const bytes: number[] = [];
  let rest = value;
  while (rest > 0n) {
    bytes.unshift(Number(rest & 0xffn));
    rest >>= 8n;
  }
  for (let i = 0; i < leadingZeros; i += 1) {
    bytes.unshift(0);
  }
  return bytes;
};

const pushBoundedBytes = (
  rng: FuzzRng,
  out: number[],
  bytes: readonly number[],
): void => {
  const form = rng.int(4);
  if (form === 0 || (form === 1 && bytes.length <= 64)) {
    pushCborHead(out, 2, BigInt(bytes.length));
    out.push(...bytes);
    return;
  }
  // Indefinite: split into chunks, sometimes with empty or oversize chunks.
  out.push(0x5f);
  let offset = 0;
  while (offset < bytes.length) {
    const size = form === 2 ? 64 : rng.chance(0.1) ? 65 : 1 + rng.int(64);
    const chunk = bytes.slice(offset, offset + size);
    if (rng.chance(0.1)) {
      out.push(0x40);
    }
    pushCborHead(out, 2, BigInt(chunk.length));
    out.push(...chunk);
    offset += chunk.length;
  }
  out.push(0xff);
};

const randomInteger = (rng: FuzzRng): bigint =>
  rng.chance(0.6)
    ? rng.pick(PLUTUS_DATA_EDGE_INTEGERS)
    : BigInt(rng.int(2_000_001) - 1_000_000);

const pushInteger = (rng: FuzzRng, out: number[], value: bigint): void => {
  const magnitude = value < 0n ? -1n - value : value;
  const useTag = magnitude >= TWO_POW_64 || rng.chance(0.08);
  if (!useTag) {
    pushCborHead(
      out,
      value < 0n ? 1 : 0,
      magnitude,
      rng.chance(0.05) ? "non-minimal" : "minimal",
    );
    return;
  }
  pushCborHead(out, 6, value < 0n ? 3n : 2n);
  pushBoundedBytes(
    rng,
    out,
    bigintBytes(magnitude, rng.chance(0.1) ? 1 + rng.int(3) : 0),
  );
};

const MALFORMED_ITEMS: readonly (readonly number[])[] = [
  [0x61, 0x61], // text "a"
  [0xf9, 0x3c, 0x00], // float16 1.0
  [0xfb, 0x3f, 0xf0, 0, 0, 0, 0, 0, 0], // float64 1.0
  [0xf4],
  [0xf5],
  [0xf6],
  [0xf7],
  [0xe0], // simple(0)
  [0xf8, 0x20], // simple(32)
  [0x1c], // reserved additional info 28
  [0x1d],
  [0x1e],
  [0xff], // stray break
  [0xd9, 0x01, 0x02, 0x80], // tag 258 set
  [0xd8, 0x78, 0x80], // tag 120 (below compact range)
  [0xd8, 0x80, 0x80], // tag 128
  [0xd9, 0x04, 0xff, 0x80], // tag 1279
  [0xd9, 0x05, 0x79, 0x80], // tag 1401
  [0xd8, 0x65, 0x82, 0x00, 0x80], // tag 101
  [0x18, 0x17], // non-minimal 23
  [0x19, 0x00, 0x17],
  [0x5f, 0x5f, 0xff, 0xff], // nested indefinite chunk
  [0x5f, 0x01, 0xff], // non-bytes chunk
  [0xd8, 0x79, 0x00], // constr fields not a list
  [0xd8, 0x66, 0x81, 0x00], // tag 102 with one element
  [0xa1, 0xd8, 0x79, 0x80], // map key with no value
];

const CONSTR_INDEXES: readonly bigint[] = [
  0n,
  1n,
  6n,
  7n,
  8n,
  127n,
  128n,
  129n,
  1000n,
  TWO_POW_64 - 1n,
];

type ByteGenOptions = {
  readonly maxDepth: number;
  readonly maxWidth: number;
  readonly malformedRate: number;
};

/** Appends without spreading, which would overflow for large arrays. */
const appendBytes = (out: number[], bytes: readonly number[]): void => {
  for (const byte of bytes) out.push(byte);
};

const pushConstr = (
  rng: FuzzRng,
  out: number[],
  depth: number,
  options: ByteGenOptions,
): void => {
  const index = rng.pick(CONSTR_INDEXES);
  const width = rng.int(options.maxWidth + 1);
  const indefinite = rng.chance(0.4);
  const pushFields = (): void => {
    if (indefinite) {
      out.push(0x9f);
    } else {
      pushCborHead(out, 4, BigInt(width));
    }
    for (let i = 0; i < width; i += 1) {
      pushDataItem(rng, out, depth + 1, options);
    }
    if (indefinite) {
      out.push(0xff);
    }
  };
  const general = index > 127n || rng.chance(0.2);
  if (!general) {
    const tag = index <= 6n ? 121n + index : 1280n + index - 7n;
    pushCborHead(out, 6, tag, rng.chance(0.05) ? "non-minimal" : "minimal");
    pushFields();
    return;
  }
  pushCborHead(out, 6, 102n);
  const extra = rng.chance(0.05);
  if (rng.chance(0.2)) {
    out.push(0x9f);
    pushInteger(rng, out, index);
    pushFields();
    if (extra) {
      out.push(0x00);
    }
    out.push(0xff);
    return;
  }
  pushCborHead(out, 4, extra ? 3n : 2n);
  pushInteger(rng, out, index);
  pushFields();
  if (extra) {
    out.push(0x00);
  }
};

const pushDataItem = (
  rng: FuzzRng,
  out: number[],
  depth: number,
  options: ByteGenOptions,
): void => {
  if (rng.chance(options.malformedRate)) {
    out.push(...rng.pick(MALFORMED_ITEMS));
    return;
  }
  const leaf = depth >= options.maxDepth || rng.chance(0.35);
  const kind = leaf ? rng.int(2) : 2 + rng.int(3);
  switch (kind) {
    case 0:
      pushInteger(rng, out, randomInteger(rng));
      return;
    case 1: {
      const length = rng.chance(0.5)
        ? rng.pick(PLUTUS_DATA_EDGE_BYTE_LENGTHS)
        : rng.int(8);
      const bytes = Array.from({ length }, () => rng.int(256));
      pushBoundedBytes(rng, out, bytes);
      return;
    }
    case 2:
    case 3: {
      const isMap = kind === 3;
      const width = rng.int(options.maxWidth + 1);
      const indefinite = rng.chance(0.4);
      if (indefinite) {
        out.push(isMap ? 0xbf : 0x9f);
      } else {
        pushCborHead(out, isMap ? 5 : 4, BigInt(width));
      }
      const keys: number[][] = [];
      for (let i = 0; i < width; i += 1) {
        if (isMap) {
          const key: number[] = [];
          if (keys.length > 0 && rng.chance(0.15)) {
            appendBytes(key, rng.pick(keys));
          } else {
            pushDataItem(rng, key, depth + 1, options);
          }
          keys.push(key);
          appendBytes(out, key);
        }
        pushDataItem(rng, out, depth + 1, options);
      }
      if (indefinite) {
        out.push(0xff);
      }
      return;
    }
    default:
      pushConstr(rng, out, depth, options);
  }
};

/**
 * Random Plutus Data CBOR. With `malformedRate > 0`, malformed items are
 * mixed in and the whole input is sometimes truncated, extended with
 * trailing bytes, or has one byte replaced.
 */
export const randomPlutusDataCbor = (
  rng: FuzzRng,
  options: Partial<ByteGenOptions> = {},
): Uint8Array => {
  const resolved: ByteGenOptions = {
    maxDepth: options.maxDepth ?? 6,
    maxWidth: options.maxWidth ?? 4,
    malformedRate: options.malformedRate ?? 0,
  };
  const out: number[] = [];
  pushDataItem(rng, out, 0, resolved);
  if (resolved.malformedRate > 0 && out.length > 0) {
    const mutation = rng.int(8);
    if (mutation === 0) {
      out.length = rng.int(out.length);
    } else if (mutation === 1) {
      out.push(rng.int(256));
    } else if (mutation === 2) {
      out[rng.int(out.length)] = rng.int(256);
    }
  }
  return Uint8Array.from(out);
};

export type DataTreeBuilders<T> = {
  readonly integer: (value: bigint) => T;
  readonly bytes: (hex: string) => T;
  readonly list: (items: T[]) => T;
  readonly map: (entries: [T, T][]) => T;
  readonly constr: (index: bigint, fields: T[]) => T;
};

/**
 * A random Data tree built bottom-up through `builders`. Map keys repeat an
 * earlier sibling key with some probability, so duplicate-key handling is
 * exercised.
 */
export const randomDataTree = <T>(
  rng: FuzzRng,
  builders: DataTreeBuilders<T>,
  options: { readonly maxDepth?: number; readonly maxWidth?: number } = {},
): T => {
  const maxDepth = options.maxDepth ?? 6;
  const maxWidth = options.maxWidth ?? 4;
  const build = (depth: number): T => {
    const leaf = depth >= maxDepth || rng.chance(0.35);
    const kind = leaf ? rng.int(2) : 2 + rng.int(3);
    if (kind === 0) {
      return builders.integer(randomInteger(rng));
    }
    if (kind === 1) {
      const length = rng.chance(0.3)
        ? rng.pick(PLUTUS_DATA_EDGE_BYTE_LENGTHS)
        : rng.int(8);
      return builders.bytes(
        Array.from({ length }, () =>
          rng.int(256).toString(16).padStart(2, "0"),
        ).join(""),
      );
    }
    const width = rng.int(maxWidth + 1);
    if (kind === 2) {
      return builders.list(
        Array.from({ length: width }, () => build(depth + 1)),
      );
    }
    if (kind === 3) {
      const entries: [T, T][] = [];
      for (let i = 0; i < width; i += 1) {
        const key =
          entries.length > 0 && rng.chance(0.15)
            ? rng.pick(entries)[0]
            : build(depth + 1);
        entries.push([key, build(depth + 1)]);
      }
      return builders.map(entries);
    }
    return builders.constr(
      rng.pick(CONSTR_INDEXES),
      Array.from({ length: width }, () => build(depth + 1)),
    );
  };
  return build(0);
};

export type DeepDataShape =
  | "definite-list"
  | "indefinite-list"
  | "map"
  | "constr";

/**
 * A single path of `depth` nested containers around the integer 0:
 * `81`/`9f..ff`/`a1 00`/`d879 81` per level (1, 2, 2 and 3 bytes).
 */
export const deepPlutusDataCbor = (
  shape: DeepDataShape,
  depth: number,
): Uint8Array => {
  const open =
    shape === "definite-list"
      ? [0x81]
      : shape === "indefinite-list"
        ? [0x9f]
        : shape === "map"
          ? [0xa1, 0x00]
          : [0xd8, 0x79, 0x81];
  const close = shape === "indefinite-list" ? [0xff] : [];
  const out = new Uint8Array(depth * (open.length + close.length) + 1);
  let offset = 0;
  for (let i = 0; i < depth; i += 1) {
    out.set(open, offset);
    offset += open.length;
  }
  out[offset] = 0x00;
  offset += 1;
  for (let i = 0; i < depth; i += 1) {
    out.set(close, offset);
    offset += close.length;
  }
  return out;
};

/** Protocol maximum depth per shape and carrier byte cap (plan header table). */
export const PLUTUS_DATA_PROTOCOL_MAX_DEPTHS = {
  redeemer: {
    "definite-list": 32_742,
    "indefinite-list": 16_371,
    map: 16_371,
    constr: 10_914,
  },
  inlineDatum: {
    "definite-list": 16_338,
    "indefinite-list": 8_169,
    map: 8_169,
    constr: 5_446,
  },
  cekConstant: {
    "definite-list": 9_214,
    "indefinite-list": 4_607,
    map: 4_607,
    constr: 3_071,
  },
} as const satisfies Record<string, Record<DeepDataShape, number>>;
