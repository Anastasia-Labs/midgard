/**
 * Bulk decoding of byte strings, strings, Data and Value constants for the
 * script-decode step machine, as in
 * `onchain/aiken/lib/midgard/script-decode-bulk-v1.ak`.
 *
 * Each decoder accepts exactly the values the one-node ticks accept, or
 * stops; a stop is never a verdict, the one-node ticks then decode the value
 * and raise any refusal. Work is metered in the same units as on-chain.
 */
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekMapData,
} from "@al-ft/midgard-core";

import {
  integerSummary,
  nextSetBit,
  readBits,
  shortBytesSummary,
} from "./script-decode.step-bits.js";
import type { Bits, Quick } from "./script-decode.step-bulk.js";
import { constrSummary } from "./script-decode.step-data.js";
import type { SequenceSummary, Summary } from "./script-decode.step-types.js";

/** Units per byte string or string, past those of its chunks. */
export const STRING_UNITS = 10;
/** Units per Flat chunk read or stepped over past a value's first. */
export const CHUNK_UNITS = 1;
/** Units per Data constant: its filler and its closing chunk. */
export const DATA_UNITS = 4;
/** Units per CBOR head or break read. */
export const HEAD_UNITS = 1;
/** Units per integer item. */
export const INTEGER_UNITS = 6;
/** Units per item of a Data list or constructor. */
export const ITEM_UNITS = 2;
/** Units per entry of a Data map. */
export const ENTRY_UNITS = 4;
/** Units per Data list, map or constructor node. */
export const NODE_UNITS = 4;
/** Units per currency and per token of a Value, past a quantity's groups. */
export const CURRENCY_UNITS = 16;
export const TOKEN_UNITS = 16;

/** The most bytes a byte string decoded here may have: one blob chunk. */
export const MAX_STRING_BYTES = 4_095;
const MAX_DATA_BYTES_CHUNK = 64n;
const MAX_KEY_BYTES = 32;
const MAX_INTEGER_GROUPS = 20;
const CBOR_BREAK = 255;
const MIN_QUANTITY = -(1n << 127n);
const MAX_QUANTITY = (1n << 127n) - 1n;

/** A stop inside a leaf, which takes no step down. */
export const stop = (units: number): Quick => ({ k: "stop", units, path: [] });

export const ok = (
  after: number,
  summary: Summary,
  memory: bigint,
  units: number,
): Quick => ({ k: "ok", after, summary, memory, units });

/** The byte index after a value's filler from bit `position`, if aligned. */
export const afterFiller = (
  bits: Bits,
  position: number,
): number | undefined => {
  const q = nextSetBit(bits.window, position, bits.flatLength);
  if (q === undefined || (q + 1) % 8 !== 0) return undefined;
  return (q + 1) / 8;
};

// ## Data

/** A read position in the Flat chunks of a Data constant, and units left. */
type Reader = { position: number; left: number; units: number };

const charge = (r: Reader, units: number): boolean => {
  r.units -= units;
  return r.units >= 0;
};

/** Opens the next Flat chunk when the current one is spent. */
const open = (bits: Bits, r: Reader): boolean => {
  if (r.left > 0) return true;
  const p = r.position;
  if (p >= bits.flatLength) return false;
  const n = bits.window[p]!;
  if (n === 0 || p + 1 + n > bits.flatLength) return false;
  if (!charge(r, CHUNK_UNITS)) return false;
  r.position = p + 1;
  r.left = n;
  return true;
};

const nextByte = (bits: Bits, r: Reader): number | undefined => {
  if (!open(bits, r)) return undefined;
  const value = bits.window[r.position]!;
  r.position += 1;
  r.left -= 1;
  return value;
};

/** `count` CBOR bytes, across Flat chunks. */
const takeBytes = (
  bits: Bits,
  r: Reader,
  count: number,
): Buffer | undefined => {
  const parts: Uint8Array[] = [];
  for (let rest = count; rest > 0; ) {
    if (!open(bits, r)) return undefined;
    const k = Math.min(rest, r.left);
    parts.push(bits.window.subarray(r.position, r.position + k));
    r.position += k;
    r.left -= k;
    rest -= k;
  }
  return Buffer.concat(parts);
};

type Head = readonly [major: number, additional: number, argument: bigint];

/** The head with initial byte `initial`; argument -1 for additional 31. */
const headOf = (bits: Bits, r: Reader, initial: number): Head | undefined => {
  const major = initial >> 5;
  const additional = initial & 31;
  if (additional < 24) return [major, additional, BigInt(additional)];
  if (additional === 31) return [major, additional, -1n];
  if (additional > 27) return undefined;
  let argument = 0n;
  for (let width = 1 << (additional - 24); width > 0; width -= 1) {
    const value = nextByte(bits, r);
    if (value === undefined) return undefined;
    argument = argument * 256n + BigInt(value);
  }
  return [major, additional, argument];
};

const readHead = (bits: Bits, r: Reader): Head | undefined => {
  if (!charge(r, HEAD_UNITS)) return undefined;
  const initial = nextByte(bits, r);
  return initial === undefined ? undefined : headOf(bits, r, initial);
};

/** Reads a break, or the head of the next chunk of an indefinite string. */
const nextPiece = (bits: Bits, r: Reader): "break" | number | undefined => {
  if (!charge(r, HEAD_UNITS)) return undefined;
  const initial = nextByte(bits, r);
  if (initial === undefined) return undefined;
  if (initial === CBOR_BREAK) return "break";
  const head = headOf(bits, r, initial);
  if (head === undefined) return undefined;
  const [major, , length] = head;
  if (major !== 2 || length < 0n || length > MAX_DATA_BYTES_CHUNK) {
    return undefined;
  }
  return Number(length);
};

/** The bytes of a byte string item of `length` (-1 when indefinite). */
const stringBytes = (
  bits: Bits,
  r: Reader,
  length: bigint,
  limit: number,
): Buffer | undefined => {
  if (length >= 0n) return takeBytes(bits, r, Number(length));
  const parts: Buffer[] = [];
  let total = 0;
  for (;;) {
    const piece = nextPiece(bits, r);
    if (piece === undefined) return undefined;
    if (piece === "break") return Buffer.concat(parts);
    total += piece;
    if (total > limit) return undefined;
    const part = takeBytes(bits, r, piece);
    if (part === undefined) return undefined;
    parts.push(part);
  }
};

type Item = Summary | "break" | undefined;

const item = (bits: Bits, r: Reader): Item => {
  if (!charge(r, HEAD_UNITS)) return undefined;
  const initial = nextByte(bits, r);
  if (initial === undefined) return undefined;
  if (initial === CBOR_BREAK) return "break";
  const head = headOf(bits, r, initial);
  if (head === undefined) return undefined;
  const [major, additional, argument] = head;
  switch (major) {
    case 0:
    case 1:
      if (argument < 0n || !charge(r, INTEGER_UNITS)) return undefined;
      return integerSummary(major === 0 ? argument : -1n - argument);
    case 2: {
      if (argument > MAX_DATA_BYTES_CHUNK) return undefined;
      const bytes = stringBytes(bits, r, argument, MAX_STRING_BYTES);
      if (bytes === undefined) return undefined;
      if (!charge(r, STRING_UNITS + (bytes.length >> 5))) return undefined;
      return shortBytesSummary(bytes);
    }
    case 4:
      return items(bits, r, argument, -1n);
    case 5:
      return entries(bits, r, argument);
    case 6:
      return tagged(bits, r, additional, argument);
    default:
      return undefined;
  }
};

/** `length` items (-1 until a break) under `constructor` (-1 a list). */
const items = (
  bits: Bits,
  r: Reader,
  length: bigint,
  constructor: bigint,
): Summary | undefined => {
  const heads: Summary[] = [];
  for (let index = 0n; length < 0n || index < length; index += 1n) {
    if (!charge(r, ITEM_UNITS)) return undefined;
    const got = item(bits, r);
    if (got === undefined) return undefined;
    if (got === "break") {
      if (length >= 0n) return undefined;
      break;
    }
    heads.push(got);
  }
  if (!charge(r, NODE_UNITS)) return undefined;
  let sequence: SequenceSummary = emptyMidgardCekDataListSummary();
  for (let index = heads.length - 1; index >= 0; index -= 1) {
    sequence = prependMidgardCekDataListSummary(heads[index]!, sequence);
  }
  return constrSummary(constructor, sequence);
};

/** `length` map entries (-1 until a break in place of a key). */
const entries = (
  bits: Bits,
  r: Reader,
  length: bigint,
): Summary | undefined => {
  const pairs: (readonly [Summary, Summary])[] = [];
  for (let index = 0n; length < 0n || index < length; index += 1n) {
    if (!charge(r, ENTRY_UNITS)) return undefined;
    const key = item(bits, r);
    if (key === undefined) return undefined;
    if (key === "break") {
      if (length >= 0n) return undefined;
      break;
    }
    const value = item(bits, r);
    if (value === undefined || value === "break") return undefined;
    pairs.push([key, value]);
  }
  if (!charge(r, NODE_UNITS)) return undefined;
  return mapSummary(pairs);
};

const mapSummary = (pairs: readonly (readonly [Summary, Summary])[]) => {
  let sequence: SequenceSummary = emptyMidgardCekDataPairSummary();
  for (let index = pairs.length - 1; index >= 0; index -= 1) {
    const [key, value] = pairs[index]!;
    sequence = prependMidgardCekDataPairSummary(key, value, sequence);
  }
  return summarizeMidgardCekMapData(sequence);
};

/** The fields list of a constructor. */
const fields = (
  bits: Bits,
  r: Reader,
  constructor: bigint,
): Summary | undefined => {
  const head = readHead(bits, r);
  if (head === undefined || head[0] !== 4) return undefined;
  return items(bits, r, head[2], constructor);
};

const tagged = (
  bits: Bits,
  r: Reader,
  additional: number,
  tag: bigint,
): Summary | undefined => {
  if (tag < 0n) return undefined;
  if (additional === 2 || additional === 3) {
    const head = readHead(bits, r);
    if (head === undefined) return undefined;
    const [major, , length] = head;
    if (major !== 2 || length > MAX_DATA_BYTES_CHUNK) return undefined;
    const bytes = stringBytes(bits, r, length, Number(MAX_DATA_BYTES_CHUNK));
    if (bytes === undefined || !charge(r, INTEGER_UNITS)) return undefined;
    const m = bytes.length === 0 ? 0n : BigInt(`0x${bytes.toString("hex")}`);
    return integerSummary(additional === 3 ? -1n - m : m);
  }
  if (tag === 102n) {
    const head = readHead(bits, r);
    if (head === undefined) return undefined;
    const [major, , length] = head;
    if (major !== 4 || (length !== 2n && length !== -1n)) return undefined;
    const index = readHead(bits, r);
    if (index === undefined || index[0] !== 0 || index[2] < 0n) {
      return undefined;
    }
    const summary = fields(bits, r, index[2]);
    if (summary === undefined || length === 2n) return summary;
    if (!charge(r, HEAD_UNITS)) return undefined;
    return nextByte(bits, r) === CBOR_BREAK ? summary : undefined;
  }
  if (tag >= 121n && tag <= 127n) return fields(bits, r, tag - 121n);
  if (tag >= 1280n && tag <= 1400n) return fields(bits, r, tag - 1273n);
  return undefined;
};

/**
 * A Data constant from bit `position`: the filler, the first CBOR item, and
 * the Flat chunks after it up to the empty one.
 */
export const dataConstant = (
  bits: Bits,
  position: number,
  units: number,
): Quick => {
  const { window, flatLength } = bits;
  const start = afterFiller(bits, position);
  if (start === undefined) return stop(units);
  const r: Reader = { position: start, left: 0, units: units - DATA_UNITS };
  if (r.units < 0) return stop(r.units);
  const got = item(bits, r);
  if (got === undefined || got === "break") return stop(r.units);
  let p = r.position + r.left;
  for (;;) {
    if (p >= flatLength) return stop(r.units);
    const n = window[p]!;
    if (n === 0) break;
    if (!charge(r, CHUNK_UNITS)) return stop(r.units);
    p += 1 + n;
  }
  return ok(8 * (p + 1), got, got.memory, r.units);
};

// ## Values

/** A Value key from bit `position`: at most 32 bytes over its chunks. */
const valueKey = (
  bits: Bits,
  position: number,
  r: Reader,
): { readonly key: Buffer; readonly after: number } | undefined => {
  const { window, flatLength } = bits;
  const index = afterFiller(bits, position);
  if (index === undefined) return undefined;
  const parts: Uint8Array[] = [];
  let total = 0;
  for (let i = index; ; ) {
    if (i >= flatLength) return undefined;
    const n = window[i]!;
    if (n === 0) return { key: Buffer.concat(parts), after: 8 * (i + 1) };
    if (i !== index && !charge(r, CHUNK_UNITS)) return undefined;
    total += n;
    if (total > MAX_KEY_BYTES) return undefined;
    parts.push(window.subarray(i + 1, i + 1 + n));
    i += 1 + n;
  }
};

const follows = (previous: Buffer | undefined, key: Buffer): boolean =>
  previous === undefined || Buffer.compare(previous, key) < 0;

/** A Flat natural of at most 20 groups: its value and groups. */
export const naturalAt = (
  bits: Bits,
  position: number,
): readonly [after: number, groups: number, value: bigint] | undefined => {
  let p = position;
  let acc = 0n;
  let scale = 1n;
  for (let groups = 0; ; ) {
    if (groups >= MAX_INTEGER_GROUPS || p + 8 > 8 * bits.flatLength) {
      return undefined;
    }
    const v = readBits(bits.window, p, 8);
    acc += BigInt(v % 128) * scale;
    groups += 1;
    p += 8;
    if (v < 128) return [p, groups, acc];
    scale *= 128n;
  }
};

export const zigzag = (z: bigint): bigint =>
  z % 2n === 0n ? z / 2n : -(z / 2n) - 1n;

type Entries = (readonly [Summary, Summary])[];

/** The tokens of one currency from the cons bit at `position`. */
const tokens = (
  bits: Bits,
  position: number,
  r: Reader,
  out: Entries,
): number | undefined => {
  let previous: Buffer | undefined;
  let p = position;
  for (;;) {
    if (p >= 8 * bits.flatLength) return undefined;
    if (readBits(bits.window, p, 1) === 0)
      return out.length === 0 ? undefined : p + 1;
    if (!charge(r, TOKEN_UNITS)) return undefined;
    const token = valueKey(bits, p + 1, r);
    if (token === undefined || !follows(previous, token.key)) return undefined;
    const natural = naturalAt(bits, token.after);
    if (natural === undefined || !charge(r, natural[1])) return undefined;
    const n = zigzag(natural[2]);
    if (n === 0n || n < MIN_QUANTITY || n > MAX_QUANTITY) return undefined;
    out.push([shortBytesSummary(token.key), integerSummary(n)]);
    previous = token.key;
    p = natural[0];
  }
};

/** A non-empty Value from its first cons bit at `position`. */
export const valueConstant = (
  bits: Bits,
  position: number,
  units: number,
): Quick => {
  const r: Reader = { position: 0, left: 0, units };
  const currencies: Entries = [];
  let previous: Buffer | undefined;
  let total = 0n;
  let p = position;
  for (;;) {
    if (p >= 8 * bits.flatLength) return stop(r.units);
    if (readBits(bits.window, p, 1) === 0) break;
    if (!charge(r, CURRENCY_UNITS)) return stop(r.units);
    const currency = valueKey(bits, p + 1, r);
    if (currency === undefined || !follows(previous, currency.key)) {
      return stop(r.units);
    }
    const inner: Entries = [];
    const after = tokens(bits, currency.after, r, inner);
    if (after === undefined) return stop(r.units);
    currencies.push([shortBytesSummary(currency.key), mapSummary(inner)]);
    total += BigInt(inner.length);
    previous = currency.key;
    p = after;
  }
  return ok(p + 1, mapSummary(currencies), total, r.units);
};
