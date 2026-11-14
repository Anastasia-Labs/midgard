/**
 * The off-chain twin of `midgard/cek_write_bits_loop_v1`: the hops of
 * writeBits (tag 80) over an index list of any length, beside an input of at
 * most 4,096 bytes.
 *
 * The roots hop commits the index list compacted: its payload is the Data
 * summary of a list node whose items root heads a chain of list nodes, one
 * per index. The first hop reveals that node and the input's bytes. Every hop
 * opens the next indices: they, prepended in order onto the summary of the
 * rest of the list the hop names, must give the root the previous hop left.
 *
 * A success sets the indices' bits hop by hop; the hop that opens the end of
 * the list checks the result and gives the charge, which the step's
 * post-state must post. A failure walks the list the same way and ends,
 * uncharged, on the hop whose indices hold one outside the input.
 *
 * {@link advanceMidgardCekWriteBitsHop} and
 * {@link advanceMidgardCekWriteBitsFailureHop} take exactly the decisions
 * `advance_write_bits_v1` and `advance_write_bits_failure_v1` take, from the
 * same inputs; the post-state is the caller's to check.
 */
import {
  hashMidgardCekDataNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  midgardCekBytesMemoryWords,
  midgardCekDataBytesCborLength,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  midgardCekFlatRoot,
  prependMidgardCekDataListSummary,
} from "@al-ft/midgard-core";
import { Constr, type Data } from "@lucid-evolution/lucid";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
import { computeMidgardCekBuiltinBudget } from "./cek-cost.js";
import { buildMidgardCekDataIntegerNode } from "./cek-data-tree.js";
import {
  type Compacted,
  compactValue,
  sameCompacted,
} from "./cek-staged-builtin.verify.js";
import { UPLCConst } from "./uplc-constant.js";

/** Protocol version 11 fails writeBits on an input over 4,096 bytes. */
export const MIDGARD_CEK_WRITE_BITS_MAX_INPUT = 4_096;

/** What the first hop reveals: the input's bytes and the items of the index
 * list's node (its CBOR length and memory are the committed summary's). */
export type MidgardCekWriteBitsStart = {
  readonly input: Buffer;
  readonly itemsCount: bigint;
  readonly itemsRoot: Buffer;
};

/** One hop: the start on the first hop, the indices it opens, in order, and
 * the summary of the rest of the list. */
export type MidgardCekWriteBitsHop = {
  readonly start: MidgardCekWriteBitsStart | null;
  readonly items: readonly bigint[];
  readonly tail: MidgardCekDataSequenceSummary;
};

/** A success's state between hops: the bytes so far, the root of the rest
 * of the list and the list's index count. */
export type MidgardCekWriteBitsState = {
  readonly out: Buffer;
  readonly root: Buffer;
  readonly count: bigint;
};

/** A failure's state between hops: the input's length and the root of the
 * rest of the list. */
export type MidgardCekWriteBitsWalk = {
  readonly length: number;
  readonly root: Buffer;
};

/** The facts a success hop reads: the step's compacted arguments and
 * result. */
export type MidgardCekWriteBitsFacts = {
  readonly arguments: readonly MidgardCekDirectValueWitness[];
  readonly result: MidgardCekDirectValueWitness;
};

export type MidgardCekWriteBitsAdvance =
  | { readonly kind: "next"; readonly state: MidgardCekWriteBitsState }
  | { readonly kind: "done"; readonly budget: MidgardCekBuiltinBudget }
  | { readonly kind: "rejected"; readonly reason: string };

export type MidgardCekWriteBitsWalkAdvance =
  | { readonly kind: "next"; readonly walk: MidgardCekWriteBitsWalk }
  | { readonly kind: "failed" }
  | { readonly kind: "rejected"; readonly reason: string };

/** The canonical CBOR of the ByteString and list<integer> constant types. */
const BYTES_TYPE = Buffer.from("9f01ff", "hex");
const INDICES_TYPE = Buffer.from("9f0500ff", "hex");

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const same = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

/** The summary of integer `value` as a list item. */
export const midgardCekWriteBitsItemSummary = (
  value: bigint,
): MidgardCekDataSummary => {
  const node = buildMidgardCekDataIntegerNode(value, midgardCekFlatRoot);
  return {
    root: hashMidgardCekDataNode(node),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

/** The summary of `items` prepended in order onto `tail`. */
export const midgardCekWriteBitsOpened = (
  items: readonly bigint[],
  tail: MidgardCekDataSequenceSummary,
): MidgardCekDataSequenceSummary =>
  items.reduceRight(
    (rest, value) =>
      prependMidgardCekDataListSummary(
        midgardCekWriteBitsItemSummary(value),
        rest,
      ),
    tail,
  );

/** The byte-string payload of `bytes`: the summary of its Data leaf. */
export const midgardCekWriteBitsBytesPayload = (
  bytes: Uint8Array,
): Compacted["payload"] => {
  const length = BigInt(bytes.length);
  const node = {
    kind: "bytes" as const,
    bytesRoot: midgardCekFlatRoot(bytes),
    bytesLength: length,
    cborLength: midgardCekDataBytesCborLength(length),
    memory: 4n + midgardCekBytesMemoryWords(length),
  };
  return {
    root: hashMidgardCekDataNode(node),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

/** The compacted byte string `bytes`, as the result of a success. */
export const midgardCekWriteBitsBytesValue = (
  bytes: Uint8Array,
): Compacted => ({
  typeCbor: BYTES_TYPE,
  payload: midgardCekWriteBitsBytesPayload(bytes),
  memory: midgardCekBytesMemoryWords(bytes.length),
});

/** The charge of writeBits over an input of `length` bytes and `count`
 * indices: a Boolean's memory is 1. */
export const midgardCekWriteBitsCharge = (
  length: number,
  count: bigint,
): MidgardCekBuiltinBudget =>
  computeMidgardCekBuiltinBudget(80, [
    midgardCekBytesMemoryWords(length),
    count,
    1n,
  ]);

/** Whether `index` names a bit of an input of `length` bytes. */
const inside = (index: bigint, length: number): boolean =>
  index >= 0n && index < 8n * BigInt(length);

/** writeBits over `bytes`: bit `i` is bit `i % 8` of the byte `i / 8` from
 * the end. Null when an index is outside the input. */
export const midgardCekWriteBits = (
  bytes: Uint8Array,
  indices: readonly bigint[],
  set: boolean,
): Buffer | null => {
  if (bytes.length > MIDGARD_CEK_WRITE_BITS_MAX_INPUT) return null;
  const out = Buffer.from(bytes);
  for (const index of indices) {
    if (!inside(index, out.length)) return null;
    const at = out.length - 1 - Number(index / 8n);
    const mask = 1 << Number(index % 8n);
    out[at] = set ? out[at]! | mask : out[at]! & ~mask;
  }
  return out;
};

const semantic = (value: MidgardCekDirectValueWitness | undefined) =>
  value?.kind === "semanticConstant" ? value.witness : null;

/** `bytes` are the input's committed bytes (its payload alone). */
const inputIs = (
  input: MidgardCekDirectValueWitness | undefined,
  bytes: Uint8Array,
): boolean => {
  const committed = semantic(input);
  if (committed === null) return false;
  const payload = midgardCekWriteBitsBytesPayload(bytes);
  return (
    same(committed.payload.root, payload.root) &&
    committed.payload.cborLength === payload.cborLength &&
    committed.payload.memory === payload.memory
  );
};

/** The index list's node is the one the list's payload commits. */
const indicesOpen = (
  indices: MidgardCekDirectValueWitness | undefined,
  start: MidgardCekWriteBitsStart,
): boolean => {
  const committed = semantic(indices);
  if (committed === null) return false;
  return same(
    committed.payload.root,
    hashMidgardCekDataNode({
      kind: "list",
      itemsCount: start.itemsCount,
      itemsRoot: start.itemsRoot,
      cborLength: committed.payload.cborLength,
      memory: committed.payload.memory,
    }),
  );
};

const typeOf = (value: MidgardCekDirectValueWitness | undefined) =>
  semantic(value)?.typeCbor ?? null;

/** The hop opens the next indices past `cursor`, at least one, and ends
 * after them, onto the root it was handed. */
const opens = (
  cursor: number,
  hop: MidgardCekWriteBitsHop,
  end: number,
  root: Uint8Array,
): boolean =>
  hop.items.length > 0 &&
  end === cursor + hop.items.length &&
  same(midgardCekWriteBitsOpened(hop.items, hop.tail).root, root);

const TRUE = compactValue(UPLCConst.bool(true));
const FALSE = compactValue(UPLCConst.bool(false));

const rejected = (reason: string) => ({ kind: "rejected", reason }) as const;

/**
 * One hop of a success over indices [cursor, end) from `from` (null on the
 * first hop), as `advance_write_bits_v1` runs it: the state to carry on
 * with, done with the charge once the list ends and the result holds, or
 * the check that fails.
 */
export const advanceMidgardCekWriteBitsHop = (
  facts: MidgardCekWriteBitsFacts,
  hop: MidgardCekWriteBitsHop,
  from: {
    readonly cursor: number;
    readonly state: MidgardCekWriteBitsState;
  } | null,
  end: number,
): MidgardCekWriteBitsAdvance => {
  const [input, indices, value, ...others] = facts.arguments;
  if (others.length !== 0 || value === undefined) return rejected("arguments");
  let cursor = 0;
  let state: MidgardCekWriteBitsState;
  if (from === null) {
    const { start } = hop;
    if (start === null) return rejected("start");
    const inputType = typeOf(input);
    if (inputType === null || !same(inputType, BYTES_TYPE))
      return rejected("input type");
    if (!inputIs(input, start.input)) return rejected("input");
    const indicesType = typeOf(indices);
    if (indicesType === null || !same(indicesType, INDICES_TYPE))
      return rejected("indices type");
    if (!indicesOpen(indices, start)) return rejected("indices");
    state = {
      out: start.input,
      root: start.itemsRoot,
      count: start.itemsCount,
    };
  } else {
    ({ cursor, state } = from);
  }
  if (!opens(cursor, hop, end, state.root)) return rejected("opened");
  const committed = semantic(value);
  const set = committed !== null && sameCompacted(committed, TRUE);
  if (!set && (committed === null || !sameCompacted(committed, FALSE)))
    return rejected("value");
  const out = midgardCekWriteBits(state.out, hop.items, set);
  if (out === null) return rejected("index");
  if (!same(hop.tail.root, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT))
    return {
      kind: "next",
      state: { out, root: Buffer.from(hop.tail.root), count: state.count },
    };
  const result = semantic(facts.result);
  if (
    result === null ||
    !sameCompacted(result, midgardCekWriteBitsBytesValue(out))
  )
    return rejected("result");
  return {
    kind: "done",
    budget: midgardCekWriteBitsCharge(out.length, state.count),
  };
};

/**
 * One hop of a failure over indices [cursor, end) from `from` (null on the
 * first hop), as `advance_write_bits_failure_v1` runs it over the step's
 * compacted arguments: the walk to carry on with, failed on the hop whose
 * indices hold one outside the input, or the check that fails.
 */
export const advanceMidgardCekWriteBitsFailureHop = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  hop: MidgardCekWriteBitsHop,
  from: {
    readonly cursor: number;
    readonly walk: MidgardCekWriteBitsWalk;
  } | null,
  end: number,
): MidgardCekWriteBitsWalkAdvance => {
  let cursor = 0;
  let walk: MidgardCekWriteBitsWalk;
  if (from === null) {
    const [input, indices, value, ...others] = arguments_;
    if (others.length !== 0 || value === undefined)
      return rejected("arguments");
    const { start } = hop;
    if (start === null) return rejected("start");
    if (!inputIs(input, start.input)) return rejected("input");
    if (!indicesOpen(indices, start)) return rejected("indices");
    walk = { length: start.input.length, root: start.itemsRoot };
  } else {
    ({ cursor, walk } = from);
  }
  if (!opens(cursor, hop, end, walk.root)) return rejected("opened");
  if (hop.items.some((index) => !inside(index, walk.length)))
    return { kind: "failed" };
  if (same(hop.tail.root, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT))
    return rejected("no index outside the input");
  return {
    kind: "next",
    walk: { length: walk.length, root: Buffer.from(hop.tail.root) },
  };
};

// ## Data

/** `WriteBitsHopV1` as the hop's redeemer carries it. */
export const midgardCekWriteBitsHopData = (hop: MidgardCekWriteBitsHop): Data =>
  new Constr(0, [
    hop.start === null
      ? new Constr(1, [])
      : new Constr(0, [
          new Constr(0, [
            hex(hop.start.input),
            hop.start.itemsCount,
            hex(hop.start.itemsRoot),
          ]),
        ]),
    [...hop.items],
    new Constr(0, [
      hex(hop.tail.root),
      hop.tail.length,
      hop.tail.payloadCborLength,
      hop.tail.memory,
    ]),
  ]);

/** `WriteBitsStateV1` as a success hop's output carries it. */
export const midgardCekWriteBitsStateData = (
  state: MidgardCekWriteBitsState,
): Data => new Constr(0, [hex(state.out), hex(state.root), state.count]);

/** `WriteBitsWalkV1` as a failure hop's output carries it. */
export const midgardCekWriteBitsWalkData = (
  walk: MidgardCekWriteBitsWalk,
): Data => new Constr(0, [BigInt(walk.length), hex(walk.root)]);
