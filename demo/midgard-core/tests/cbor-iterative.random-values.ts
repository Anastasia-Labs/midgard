import {
  type FuzzRng,
  pushCborHead,
  randomPlutusDataCbor,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";

import { legacyEncodeCbor } from "./cbor-iterative.legacy-codec.js";

/**
 * Seeded inputs for the generic CBOR codec differential: JS values for the
 * encoder (every type the reference encoder classifies, supported or not) and
 * byte strings for the decoder (canonical items, near-misses and garbage).
 */

const TWO_POW_64 = 1n << 64n;

const NUMBERS: readonly number[] = [
  0,
  1,
  -1,
  23,
  24,
  -24,
  -25,
  255,
  256,
  65535,
  65536,
  4294967295,
  4294967296,
  Number.MAX_SAFE_INTEGER,
  Number.MIN_SAFE_INTEGER,
  2 ** 53,
  -(2 ** 53),
  2 ** 64,
  0.5,
  -1.5,
  1e300,
  5e-324,
  -0,
  NaN,
  Infinity,
  -Infinity,
];

const BIGINTS: readonly bigint[] = [
  0n,
  1n,
  -1n,
  23n,
  24n,
  -25n,
  255n,
  256n,
  (1n << 32n) - 1n,
  1n << 32n,
  2n ** 53n,
  TWO_POW_64 - 1n,
  TWO_POW_64,
  -TWO_POW_64,
  -TWO_POW_64 - 1n,
  1n << 100n,
];

const STRINGS: readonly string[] = [
  "",
  "a",
  "key",
  "x".repeat(23),
  "y".repeat(24),
  "z".repeat(300),
  "été",
  "\u{1f600}",
  "\ud800",
  "a\udc00b",
  "\ud83d",
  "﻿bom",
  "mixed é and \u{1f600} and \ud800 tail".repeat(3),
];

const randomBytes = (rng: FuzzRng, length: number): Uint8Array =>
  Uint8Array.from({ length }, () => rng.int(256));

const randomByteLike = (rng: FuzzRng): unknown => {
  const bytes = randomBytes(rng, rng.pick([0, 1, 5, 23, 24, 300]));
  switch (rng.int(9)) {
    case 0:
      return bytes;
    case 1:
      return Buffer.from(bytes);
    case 2:
      return new DataView(bytes.buffer, 0, bytes.length);
    case 3:
      return new Uint16Array(bytes.buffer, 0, bytes.length >> 1);
    case 4:
      return new Int8Array(bytes);
    case 5:
      return new Float64Array([1.5, -2]);
    case 6:
      return new BigInt64Array([1n, -1n]);
    case 7:
      return bytes.buffer;
    default:
      return new Uint8ClampedArray(bytes).subarray(1);
  }
};

const unsupportedValue = (rng: FuzzRng): unknown =>
  rng.pick<() => unknown>([
    () => Symbol("s"),
    () => () => 1,
    () => new Date(0),
    () => /x/,
    () => new Set([1]),
    () => new Error("e"),
    () => new WeakMap(),
    () => new URL("http://example.invalid/"),
    () => new SharedArrayBuffer(2),
    () => ({ [Symbol.toStringTag]: "Date" }),
  ])();

const randomScalar = (rng: FuzzRng, allowUnsupported: boolean): unknown => {
  const roll = rng.int(allowUnsupported ? 10 : 9);
  switch (roll) {
    case 0:
      return rng.pick(NUMBERS);
    case 1:
      return rng.int(100_000) - 50_000;
    case 2:
      return rng.pick(BIGINTS);
    case 3:
      return rng.pick(STRINGS);
    case 4:
      return randomByteLike(rng);
    case 5:
      return rng.pick([null, undefined, true, false]);
    case 6:
      return rng.pick([[], new Map(), {}]);
    case 7:
      return BigInt(rng.int(1000) - 500);
    case 8:
      return String.fromCharCode(...randomBytes(rng, rng.int(6)));
    default:
      return unsupportedValue(rng);
  }
};

/** A random value for the encoder; about one in five is not encodable. */
export const randomEncodableValue = (
  rng: FuzzRng,
  depth = 0,
  allowUnsupported = true,
): unknown => {
  if (depth >= 5 || rng.chance(0.4)) {
    return randomScalar(rng, allowUnsupported && rng.chance(0.1));
  }
  const width = rng.int(5);
  switch (rng.int(5)) {
    case 0:
    case 1: {
      const items = Array.from({ length: width }, () =>
        randomEncodableValue(rng, depth + 1, allowUnsupported),
      );
      if (rng.chance(0.05)) {
        items.push(items);
      }
      return items;
    }
    case 2:
    case 3: {
      const map = new Map<unknown, unknown>();
      for (let i = 0; i < width; i += 1) {
        const key =
          rng.chance(0.1) && allowUnsupported
            ? randomEncodableValue(rng, depth + 1, allowUnsupported)
            : randomScalar(rng, false);
        map.set(key, randomEncodableValue(rng, depth + 1, allowUnsupported));
      }
      if (rng.chance(0.05)) {
        map.set("self", map);
      }
      return map;
    }
    default: {
      const target: Record<string, unknown> = rng.chance(0.2)
        ? (Object.create(null) as Record<string, unknown>)
        : {};
      for (let i = 0; i < width; i += 1) {
        target[rng.pick(STRINGS)] = randomEncodableValue(
          rng,
          depth + 1,
          allowUnsupported,
        );
      }
      return target;
    }
  }
};

/** Canonical-looking CBOR built by hand, with local defects mixed in. */
const pushRawItem = (rng: FuzzRng, out: number[], depth: number): void => {
  const leaf = depth >= 5 || rng.chance(0.4);
  const roll = leaf ? rng.int(8) : 8 + rng.int(2);
  switch (roll) {
    case 0:
      pushCborHead(out, rng.int(2), BigInt(rng.pick([0, 23, 24, 256, 70000])));
      return;
    case 1:
      pushCborHead(
        out,
        0,
        rng.pick(BIGINTS.filter((v) => v >= 0n && v < TWO_POW_64)),
      );
      return;
    case 2: {
      const bytes = randomBytes(rng, rng.int(4));
      pushCborHead(out, 2, BigInt(bytes.length));
      out.push(...bytes);
      return;
    }
    case 3: {
      const text = rng.pick<readonly number[]>([
        [0x61],
        [0xc3, 0xa9],
        [0xef, 0xbb, 0xbf, 0x61],
        [0xff],
        [0xc3],
        [0xed, 0xa0, 0x80],
      ]);
      pushCborHead(out, 3, BigInt(text.length));
      out.push(...text);
      return;
    }
    case 4:
      out.push(rng.pick([0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xe0, 0xfb]));
      return;
    case 5:
      out.push(rng.pick([0x18, 0x5f, 0x9f, 0xbf, 0xff, 0x1c, 0x3b, 0x7b]));
      out.push(...randomBytes(rng, rng.int(9)));
      return;
    case 6:
      pushCborHead(out, 6, BigInt(rng.pick([2, 24, 121, 258])));
      pushRawItem(rng, out, depth + 1);
      return;
    case 7:
      pushCborHead(out, rng.pick([2, 3, 4, 5]), 1n << 60n);
      return;
    default: {
      const isMap = roll === 9;
      const width = rng.int(5);
      pushCborHead(out, isMap ? 5 : 4, BigInt(width));
      const keys: number[][] = [];
      for (let i = 0; i < width; i += 1) {
        if (isMap) {
          const key: number[] = [];
          if (keys.length > 0 && rng.chance(0.2)) {
            key.push(...rng.pick(keys));
          } else {
            pushRawItem(rng, key, depth + 1);
          }
          keys.push(key);
          out.push(...key);
        }
        pushRawItem(rng, out, depth + 1);
      }
    }
  }
};

/** A random decoder input: encoder output, hand-built items or Data CBOR. */
export const randomDecoderInput = (rng: FuzzRng): Uint8Array => {
  let bytes: number[];
  switch (rng.int(4)) {
    case 0:
    case 1: {
      try {
        bytes = [...legacyEncodeCbor(randomEncodableValue(rng, 0, false))];
      } catch {
        bytes = [0x80];
      }
      break;
    }
    case 2:
      bytes = [];
      pushRawItem(rng, bytes, 0);
      break;
    default:
      bytes = [...randomPlutusDataCbor(rng, { malformedRate: 0.05 })];
  }
  if (rng.chance(0.3) && bytes.length > 0) {
    const mutation = rng.int(4);
    if (mutation === 0) {
      bytes.length = rng.int(bytes.length);
    } else if (mutation === 1) {
      bytes.push(rng.int(256));
    } else if (mutation === 2) {
      bytes[rng.int(bytes.length)] = rng.int(256);
    } else {
      bytes.splice(rng.int(bytes.length), 0, rng.int(256));
    }
  }
  return Uint8Array.from(bytes);
};

/**
 * A type-exact rendering of a decoded or encodable value: numbers, bigints,
 * `Uint8Array` versus `Buffer`, and map entry order all show.
 */
export const describeValue = (value: unknown): string => {
  if (typeof value === "bigint") {
    return `${value}n`;
  }
  if (typeof value === "number") {
    return Object.is(value, -0) ? "num:-0" : `num:${value}`;
  }
  if (typeof value === "string") {
    return `str:${JSON.stringify(value)}`;
  }
  if (value === null || value === undefined || typeof value === "boolean") {
    return String(value);
  }
  if (value instanceof Uint8Array) {
    return `${value.constructor.name}:${Buffer.from(value).toString("hex")}`;
  }
  if (Array.isArray(value)) {
    return `[${value.map(describeValue).join(",")}]`;
  }
  if (value instanceof Map) {
    return `Map{${[...value]
      .map(([k, v]) => `${describeValue(k)}=>${describeValue(v)}`)
      .join(",")}}`;
  }
  return `other:${Object.prototype.toString.call(value)}`;
};
