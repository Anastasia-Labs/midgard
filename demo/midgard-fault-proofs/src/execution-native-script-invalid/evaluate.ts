/** Off-chain twin of `execution_native_script_invalid/evaluate.page_v1`: the
 * paged pre-order token walk over one committed native source, its
 * hash-linked frame stack, its signer answers and its chunk window. A page
 * spends one unit of budget per token read and per frame pop, exactly as the
 * validator does, so the planned next state is the one the validator
 * computes. Malformed bytes throw: such a source is the decoding family's
 * fault, never this family's. */
import {
  buildMidgardBoundedItemChunkProof,
  hashMidgardNativeScriptScanFrame,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
  type MidgardBoundedItem,
  type MidgardNativeScriptScanFrame,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";

import { nativeSignerQuery, type NativeSignerSet } from "./signers.js";

const SIGNATURE = 0;
const ALL = 1;
const ANY = 2;
const AT_LEAST = 3;
const AFTER = 4;
const BEFORE = 5;

const fail = (message: string): never => {
  throw new Error(`execution-native-script-invalid evaluation: ${message}`);
};

type Head = { readonly next: number; readonly value: bigint };

/** `canonical_head`: a minimally encoded CBOR head of `major`. */
const canonicalHead = (bytes: Buffer, offset: number, major: number): Head => {
  if (offset < 0 || offset >= bytes.length) return fail("head out of range");
  const first = bytes[offset]!;
  if (first >> 5 !== major) return fail("unexpected major type");
  const additional = first & 31;
  if (additional < 24) return { next: offset + 1, value: BigInt(additional) };
  const widths: Record<number, readonly [number, bigint]> = {
    24: [1, 24n],
    25: [2, 256n],
    26: [4, 65_536n],
    27: [8, 4_294_967_296n],
  };
  const width = widths[additional];
  if (width === undefined) return fail("indefinite or reserved head");
  const [size, minimum] = width;
  if (offset + 1 + size > bytes.length) return fail("head out of range");
  let value = 0n;
  for (let index = 0; index < size; index += 1)
    value = value * 256n + BigInt(bytes[offset + 1 + index]!);
  if (value < minimum) return fail("non-minimal head");
  return { next: offset + 1 + size, value };
};

/** `versioned_script_header_v1` of a native script. */
export const nativeScriptPayloadOffset = (item: Buffer): number => {
  const outer = canonicalHead(item, 0, 4);
  if (outer.value !== 2n) fail("versioned script is not a pair");
  const language = canonicalHead(item, outer.next, 0);
  const payload = canonicalHead(item, language.next, 2);
  if (language.value !== 0n) fail("source is not a native script");
  if (BigInt(payload.next) + payload.value !== BigInt(item.length))
    fail("payload length differs from the item");
  return payload.next;
};

export type NativeScriptToken = {
  readonly kind: number;
  readonly next: number;
  readonly childCount: number;
  readonly required: bigint;
  readonly keyHash: Buffer;
  readonly slot: bigint;
};

/** `token_at_v1` at an absolute offset of the item. */
export const nativeScriptTokenAt = (
  item: Buffer,
  offset: number,
): NativeScriptToken => {
  const outer = canonicalHead(item, offset, 4);
  const tag = canonicalHead(item, outer.next, 0);
  const kind = Number(tag.value);
  if (tag.value > BigInt(BEFORE)) fail("unknown native script tag");
  if (outer.value !== (kind === AT_LEAST ? 3n : 2n))
    fail("native script node has the wrong arity");
  const empty = {
    kind,
    childCount: 0,
    required: 0n,
    keyHash: Buffer.alloc(0),
    slot: 0n,
  };
  if (kind === SIGNATURE) {
    const key = canonicalHead(item, tag.next, 2);
    if (key.value !== 28n || key.next + 28 > item.length)
      fail("signature key hash is not 28 bytes");
    return {
      ...empty,
      next: key.next + 28,
      keyHash: Buffer.from(item.subarray(key.next, key.next + 28)),
    };
  }
  if (kind === AFTER || kind === BEFORE) {
    const slot = canonicalHead(item, tag.next, 0);
    return { ...empty, next: slot.next, slot: slot.value };
  }
  const required =
    kind === AT_LEAST ? canonicalHead(item, tag.next, 0) : undefined;
  const children = canonicalHead(item, required?.next ?? tag.next, 4);
  if (children.value > BigInt(MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES))
    fail("container has too many children");
  return {
    ...empty,
    next: children.next,
    childCount: Number(children.value),
    required: required?.value ?? 0n,
  };
};

type Frame = {
  readonly kind: number;
  readonly childCount: number;
  readonly remaining: number;
  readonly validCount: number;
  readonly required: bigint;
};

const frameHash = (frame: Frame, tail: Buffer): Buffer =>
  hashMidgardNativeScriptScanFrame({
    tail,
    kind: frame.kind as MidgardNativeScriptScanFrame["kind"],
    childCount: frame.childCount,
    remaining: frame.remaining,
    validCount: frame.validCount,
    required: frame.required,
  });

/** One walk position: the open frames bottom first, and the stack root of
 * every prefix (`roots[k]` commits the bottom `k` frames). */
export type NativeEvaluationPosition = {
  readonly cursor: number;
  readonly totalLength: number;
  readonly pending: boolean | null;
  readonly frames: readonly Frame[];
  readonly roots: readonly Buffer[];
};

export const INITIAL_NATIVE_EVALUATION_POSITION: NativeEvaluationPosition = {
  cursor: 0,
  totalLength: 0,
  pending: null,
  frames: [],
  roots: [Buffer.alloc(0)],
};

export type NativeEvaluationContext = {
  /** The committed source item, as its source descriptor commits it. */
  readonly item: MidgardBoundedItem;
  readonly signers: NativeSignerSet;
  /** -1 when the transaction carries no such bound. */
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
};

const done = (position: NativeEvaluationPosition) =>
  position.pending !== null && position.frames.length === 0;

/** One page from `position` of at most `budget` token reads and pops. */
export const nativeEvaluationPage = (
  context: NativeEvaluationContext,
  position: NativeEvaluationPosition,
  budget: number,
): {
  readonly page: Omit<
    SDK.ExecutionNativeScriptInvalidEvaluatePage,
    "input_index" | "output_index"
  >;
  readonly next: NativeEvaluationPosition;
  /** The root result once the walk ends on this page. */
  readonly satisfied: boolean | null;
} => {
  if (!Number.isSafeInteger(budget) || budget <= 0)
    fail("page budget must be positive");
  const bytes = context.item.bytes;
  let read: [number, number] | null = null;
  const touch = (from: number, to: number) => {
    read = read === null ? [from, to] : [Math.min(read[0], from), to];
  };
  let { cursor, totalLength, pending } = position;
  if (cursor === 0) {
    cursor = nativeScriptPayloadOffset(bytes);
    totalLength = bytes.length;
    touch(0, cursor);
  }
  const frames = [...position.frames];
  const roots = [...position.roots];
  let carried = frames.length;
  const carriedFrames: SDK.NativeScriptFrame[] = [];
  const signers: SDK.ExecutionNativeScriptInvalidSignerQuery[] = [];
  const push = (frame: Frame) => {
    roots[frames.length + 1] = frameHash(frame, roots[frames.length]!);
    frames.push(frame);
  };
  let steps = 0;
  while (steps < budget && !(pending !== null && frames.length === 0)) {
    steps += 1;
    if (pending !== null) {
      const top = frames.pop()!;
      roots.length = frames.length + 1;
      if (frames.length < carried) {
        carried = frames.length;
        carriedFrames.push({
          tail: roots[frames.length]!.toString("hex"),
          kind: BigInt(top.kind),
          child_count: BigInt(top.childCount),
          remaining: BigInt(top.remaining),
          valid_count: BigInt(top.validCount),
          required: top.required,
        });
      }
      const validCount = top.validCount + (pending ? 1 : 0);
      if (top.remaining === 1) {
        pending =
          top.kind === ALL
            ? validCount === top.childCount
            : top.kind === ANY
              ? validCount > 0
              : BigInt(validCount) >= top.required;
      } else {
        push({ ...top, remaining: top.remaining - 1, validCount });
        pending = null;
      }
      continue;
    }
    const token = nativeScriptTokenAt(bytes, cursor);
    touch(cursor, token.next);
    cursor = token.next;
    if (token.kind === SIGNATURE) {
      const query = nativeSignerQuery(context.signers, token.keyHash);
      signers.push(query.query);
      pending = query.present;
    } else if (token.kind === AFTER)
      pending =
        context.validityIntervalStart >= 0n &&
        context.validityIntervalStart >= token.slot;
    else if (token.kind === BEFORE)
      pending =
        context.validityIntervalEnd >= 0n &&
        context.validityIntervalEnd <= token.slot;
    else if (token.childCount === 0)
      pending =
        token.kind === ALL
          ? true
          : token.kind === ANY
            ? false
            : token.required === 0n;
    else
      push({
        kind: token.kind,
        childCount: token.childCount,
        remaining: token.childCount,
        validCount: 0,
        required: token.required,
      });
  }
  const range = read as [number, number] | null;
  const chunks =
    range === null
      ? []
      : Array.from(
          {
            length:
              Math.floor((range[1] - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES) -
              Math.floor(range[0] / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES) +
              1,
          },
          (_, offset) =>
            chunkProofData(
              buildMidgardBoundedItemChunkProof(
                context.item,
                Math.floor(range[0] / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES) +
                  offset,
              ),
            ),
        );
  const next: NativeEvaluationPosition = {
    cursor,
    totalLength,
    pending,
    frames,
    roots,
  };
  const finished = done(next);
  if (finished && cursor !== totalLength)
    fail("the root ends before the source does");
  return {
    page: {
      chunks,
      frames: carriedFrames,
      signers,
      budget: BigInt(budget),
    },
    next,
    satisfied: finished ? pending : null,
  };
};

const chunkProofData = (
  proof: ReturnType<typeof buildMidgardBoundedItemChunkProof>,
): SDK.BoundedItemChunkProof => ({
  version: BigInt(proof.version),
  field_index: BigInt(proof.fieldIndex),
  item_index: BigInt(proof.itemIndex),
  total_length: BigInt(proof.totalLength),
  chunk_index: BigInt(proof.chunkIndex),
  chunk: proof.chunk.toString("hex"),
  frontier: proof.frontier.peaks.map(({ height, hash }) => ({
    height: BigInt(height),
    hash: Buffer.from(hash).toString("hex"),
  })),
  siblings: proof.siblings.map((hash) => Buffer.from(hash).toString("hex")),
});

/** The datum fields a position fixes. */
export const nativeEvaluationStateFields = (
  position: NativeEvaluationPosition,
) => ({
  total_length: BigInt(position.totalLength),
  cursor: BigInt(position.cursor),
  stack_root: position.roots[position.frames.length]!.toString("hex"),
  stack_depth: BigInt(position.frames.length),
  pending: position.pending,
});

/** Every page of the whole walk at `budget`, and the root result. */
export const nativeEvaluationPages = (
  context: NativeEvaluationContext,
  budget: number,
) => {
  const pages: ReturnType<typeof nativeEvaluationPage>[] = [];
  let position = INITIAL_NATIVE_EVALUATION_POSITION;
  for (;;) {
    const page = nativeEvaluationPage(context, position, budget);
    pages.push(page);
    if (page.satisfied !== null) return { pages, satisfied: page.satisfied };
    position = page.next;
  }
};

/** The root result of the walk, the off-chain verdict the terminal reads. */
export const nativeEvaluationResult = (
  context: NativeEvaluationContext,
): boolean => nativeEvaluationPages(context, Number.MAX_SAFE_INTEGER).satisfied;

/** The position a thread holding `state` is at: the walk from its start,
 * one step at a time, to the step whose datum fields `state` holds. */
export const nativeEvaluationPositionOf = (
  context: NativeEvaluationContext,
  state: Pick<
    SDK.ExecutionNativeScriptInvalidEvalState,
    "cursor" | "stack_root" | "stack_depth" | "pending" | "total_length"
  >,
): NativeEvaluationPosition => {
  const matches = (position: NativeEvaluationPosition) => {
    const fields = nativeEvaluationStateFields(position);
    return (
      fields.cursor === state.cursor &&
      fields.stack_depth === state.stack_depth &&
      fields.pending === state.pending &&
      fields.stack_root === state.stack_root.toLowerCase() &&
      fields.total_length === state.total_length
    );
  };
  let position = INITIAL_NATIVE_EVALUATION_POSITION;
  for (;;) {
    if (matches(position)) return position;
    if (done(position)) return fail("no walk step holds the thread's state");
    position = nativeEvaluationPage(context, position, 1).next;
  }
};
