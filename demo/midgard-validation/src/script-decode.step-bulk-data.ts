/**
 * Bulk decoding of byte strings, strings and Value constants for the
 * script-decode step machine, as in
 * `onchain/aiken/lib/midgard/script-decode-bulk-v1.ak`.
 *
 * Each decoder accepts exactly the values the one-node ticks accept, or
 * stops; a stop is never a verdict, the one-node ticks then decode the value
 * and raise any refusal. Work is metered in the same units as on-chain.
 */
import {
  emptyMidgardCekDataPairSummary,
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
import type { SequenceSummary, Summary } from "./script-decode.step-types.js";

/** Units per byte string or string, past those of its chunks. */
export const STRING_UNITS = 10;
/** Units per Flat chunk read or stepped over past a value's first. */
export const CHUNK_UNITS = 1;
/** Units per currency and per token of a Value, past a quantity's groups. */
export const CURRENCY_UNITS = 16;
export const TOKEN_UNITS = 16;

/** The most bytes a byte string decoded here may have: one blob chunk. */
export const MAX_STRING_BYTES = 4_095;
const MAX_KEY_BYTES = 32;
const MAX_INTEGER_GROUPS = 20;
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

// ## Values

/** Units left to a Value decode. */
type Reader = { units: number };

const charge = (r: Reader, units: number): boolean => {
  r.units -= units;
  return r.units >= 0;
};

const mapSummary = (pairs: readonly (readonly [Summary, Summary])[]) => {
  let sequence: SequenceSummary = emptyMidgardCekDataPairSummary();
  for (let index = pairs.length - 1; index >= 0; index -= 1) {
    const [key, value] = pairs[index]!;
    sequence = prependMidgardCekDataPairSummary(key, value, sequence);
  }
  return summarizeMidgardCekMapData(sequence);
};

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
  const r: Reader = { units };
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
