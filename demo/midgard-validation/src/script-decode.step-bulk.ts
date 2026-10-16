/**
 * Bulk decoding of constant values for the script-decode step machine, as in
 * `onchain/aiken/lib/midgard/script-decode-bulk-v1.ak`.
 *
 * A value whose bits allow it is decoded in one call: a run of list
 * elements, or a pair component. Anything this module stops on is left to the
 * one-node ticks, which raise every refusal. Work is metered in the same
 * units as on-chain, so runs and stops match it exactly.
 */
import {
  emptyMidgardCekDataListSummary,
  midgardCekBytesMemoryWords,
  midgardCekIntegerMemoryWords,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekListData,
} from "@al-ft/midgard-core";

import {
  bytesSummary,
  emptySink,
  integerSummary,
  readBits,
  shortBytesSummary,
  sinkRoot,
  utf8Push,
} from "./script-decode.step-bits.js";
import {
  afterFiller,
  CHUNK_UNITS,
  dataConstant,
  MAX_STRING_BYTES,
  naturalAt,
  ok,
  stop,
  STRING_UNITS,
  valueConstant,
  zigzag,
} from "./script-decode.step-bulk-data.js";
import {
  ELEMENT_UNITS,
  type Fixed,
  type Kind,
  never,
  PAIR_UNITS,
  pairSummary,
  type Shape,
  UNITS_PER_TICK,
  UNMETERED,
  wrapSummary,
} from "./script-decode.step-shape.js";
import type { SequenceSummary, Summary } from "./script-decode.step-types.js";

export type Quick =
  | {
      readonly k: "ok";
      readonly after: number;
      readonly summary: Summary;
      readonly memory: bigint;
      readonly units: number;
    }
  | { readonly k: "stop"; readonly units: number };

export type Bits = { readonly window: Uint8Array; readonly flatLength: number };

export type Leaf =
  | { readonly k: "fixed"; readonly after: number; readonly value: Fixed }
  | { readonly k: "gen"; readonly kind: Kind }
  | { readonly k: "truncated" };

/**
 * Follows the bits of `shape` from `position` to a leaf, as `leaf_v1`: a
 * fixed leaf with the position after the bits read, a general leaf, or
 * `truncated` when the Flat bits end first.
 */
export const leafOf = (bits: Bits, shape: Shape, position: number): Leaf => {
  let current = shape;
  let p = position;
  while (current.k === "bit") {
    if (p >= 8 * bits.flatLength) return { k: "truncated" };
    current = readBits(bits.window, p, 1) === 0 ? current.zero : current.one;
    p += 1;
  }
  if (current.k === "fixed")
    return { k: "fixed", after: p, value: current.value };
  return { k: "gen", kind: current.kind };
};

/** Decodes the value of `shape` at bit `position`, as `quick_v1`. */
export const quick = (
  bits: Bits,
  shape: Shape,
  position: number,
  units: number,
): Quick => {
  if (units < 0) return stop(units);
  const leaf = leafOf(bits, shape, position);
  switch (leaf.k) {
    case "fixed":
      return {
        k: "ok",
        after: leaf.after,
        summary: leaf.value.summary,
        memory: leaf.value.memory,
        units,
      };
    case "gen":
      return general(bits, leaf.kind, position, units);
    case "truncated":
      return stop(units);
  }
};

/** Decodes a value of `kind` at bit `position`, as `general_v1`. */
export const general = (
  bits: Bits,
  kind: Kind,
  position: number,
  units: number,
): Quick => {
  switch (kind.k) {
    case "integer": {
      const natural = naturalAt(bits, position);
      if (natural === undefined) return stop(units);
      const [after, groups, acc] = natural;
      const left = units - 5 - groups;
      if (left < 0) return stop(left);
      const n = zigzag(acc);
      return {
        k: "ok",
        after,
        summary: integerSummary(n),
        memory: midgardCekIntegerMemoryWords(n),
        units: left,
      };
    }
    case "bytes":
    case "string":
      return byteChunks(bits, position, kind.k === "string", units);
    case "data":
      return dataConstant(bits, position, units);
    case "value":
      return valueConstant(bits, position, units);
    case "list": {
      const listed = items(bits, kind.element, position, units - 1);
      if (listed.k === "stop") return listed;
      return {
        k: "ok",
        after: listed.after,
        summary: summarizeMidgardCekListData(listed.sequence),
        memory: kind.array ? BigInt(listed.count) : listed.memory,
        units: listed.units,
      };
    }
    case "pair": {
      if (kind.stops) return stop(units);
      const a = quick(bits, kind.first, position, units - PAIR_UNITS);
      if (a.k === "stop") return a;
      const b = quick(bits, kind.second, a.after, a.units);
      if (b.k === "stop") return b;
      return {
        k: "ok",
        after: b.after,
        summary: pairSummary(a.summary, b.summary),
        memory: a.memory + b.memory,
        units: b.units,
      };
    }
    case "wrap": {
      const inner = general(bits, kind.inner, position, units - kind.units);
      if (inner.k === "stop") return inner;
      return {
        ...inner,
        summary: kind.layers.reduce(
          (summary, layer) => wrapSummary(layer, summary),
          inner.summary,
        ),
        memory: inner.memory + kind.memory,
      };
    }
    case "stop":
      return stop(units);
  }
};

/**
 * A byte string or string from bit `position`: the filler, its chunks and
 * the empty chunk ending them.
 */
export const byteChunks = (
  bits: Bits,
  position: number,
  string: boolean,
  units: number,
): Quick => {
  const { window, flatLength } = bits;
  const index = afterFiller(bits, position);
  if (index === undefined || index >= flatLength) return stop(units);
  if (window[index] === 0) {
    const left = units - 2;
    if (left < 0) return stop(left);
    return ok(
      8 * (index + 1),
      bytesSummary(0, sinkRoot(emptySink())),
      string ? 0n : 1n,
      left,
    );
  }
  const chunks: Uint8Array[] = [];
  let left = units - STRING_UNITS;
  let total = 0;
  let utf8 = 0;
  let i = index;
  for (;;) {
    if (i >= flatLength) return stop(left);
    const n = window[i]!;
    if (n === 0) break;
    left -= (i === index ? 0 : CHUNK_UNITS) + (string ? n : n >> 5);
    if (left < 0) return stop(left);
    // A chunk running past the Flat bytes leaves the next chunk head past
    // them, where the check above stops.
    total += n;
    if (total > MAX_STRING_BYTES) return stop(left);
    const chunk = window.subarray(i + 1, i + 1 + n);
    if (string) {
      utf8 = utf8Push(utf8, chunk);
      if (utf8 < 0) return stop(left);
    }
    chunks.push(chunk);
    i += 1 + n;
  }
  if (utf8 !== 0) return stop(left);
  return ok(
    8 * (i + 1),
    shortBytesSummary(Buffer.concat(chunks)),
    string ? BigInt(total >> 2) : midgardCekBytesMemoryWords(total),
    left,
  );
};

type Items =
  | {
      readonly k: "ok";
      readonly after: number;
      readonly sequence: SequenceSummary;
      readonly count: number;
      readonly memory: bigint;
      readonly units: number;
    }
  | { readonly k: "stop"; readonly units: number };

/** The elements of a list from the cons bit at `position` to its end. */
const items = (
  bits: Bits,
  element: Shape,
  position: number,
  units: number,
): Items => {
  const heads: Summary[] = [];
  let memory = 0n;
  let p = position;
  let left = units;
  for (;;) {
    if (left < 0) return { k: "stop", units: left };
    if (p >= 8 * bits.flatLength) return { k: "stop", units: left };
    if (readBits(bits.window, p, 1) === 0) break;
    const head = quick(bits, element, p + 1, left - ELEMENT_UNITS);
    if (head.k === "stop") return head;
    heads.push(head.summary);
    memory += head.memory;
    p = head.after;
    left = head.units;
  }
  let sequence = emptyMidgardCekDataListSummary();
  for (let index = heads.length - 1; index >= 0; index -= 1) {
    sequence = prependMidgardCekDataListSummary(heads[index]!, sequence);
  }
  return {
    k: "ok",
    after: p + 1,
    sequence,
    count: heads.length,
    memory,
    units: left,
  };
};

// ## Runs

/**
 * The run of list elements from the cons bit at `position` that a tick
 * decodes, as `scan_run_v1`: its element count, the position after it, the
 * elements' memory and the units it spent.
 */
export const scanRun = (
  bits: Bits,
  element: Shape,
  position: number,
): readonly [number, number, bigint, number] => {
  if (never(element)) return [0, position, 0n, 0];
  let count = 0;
  let memory = 0n;
  let p = position;
  let units = UNITS_PER_TICK;
  for (;;) {
    if (p >= 8 * bits.flatLength) break;
    if (readBits(bits.window, p, 1) === 0) break;
    const next = quick(bits, element, p + 1, units - ELEMENT_UNITS);
    if (next.k === "stop") break;
    count += 1;
    memory += next.memory;
    p = next.after;
    units = next.units;
  }
  return [count, p, memory, UNITS_PER_TICK - units];
};

/**
 * Prepends the `count` elements of a scanned run, from the cons bit at
 * `position`, to `sequence`, as `fold_run_v1`.
 */
export const foldRun = (
  bits: Bits,
  element: Shape,
  position: number,
  count: number,
  sequence: SequenceSummary,
): SequenceSummary => {
  const heads: Summary[] = [];
  let p = position;
  for (let index = 0; index < count; index += 1) {
    const next = quick(bits, element, p + 1, UNMETERED);
    if (next.k === "stop") {
      throw new Error("script decode: a scanned run element no longer decodes");
    }
    heads.push(next.summary);
    p = next.after;
  }
  let folded = sequence;
  for (let index = heads.length - 1; index >= 0; index -= 1) {
    folded = prependMidgardCekDataListSummary(heads[index]!, folded);
  }
  return folded;
};
