import {
  MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
  type MidgardDataStreamFrame,
  midgardDataStreamStackRoot,
  runMidgardDataStream,
  runMidgardDataStreamTracked,
} from "@al-ft/midgard-core";

/**
 * Plans a canonical-Data read as the resumable steps the on-chain stream
 * reader (`canonical_plutus_data_stream_v1.run_v1`) replays: each step reads
 * as many nodes as fit one transaction's execution budget over the window
 * the step authenticates, and reveals one open container more than it pops,
 * so its run equals a run with the whole stack revealed.
 *
 * The node budget is chosen per step from an execution-memory estimate that
 * bounds the step's measured cost from above: every node is charged by its
 * shape, read from its first bytes, and every popped container by its kind;
 * a step adds the family's fixed per-transaction cost and the frames it
 * reveals. The weights are measured per-shape costs of the on-chain reader
 * with a margin of about four percent, fitted so that no measured step of
 * a corpus covering every shape exceeds its estimate. Memory binds before
 * CPU for every node shape.
 */

/** Execution memory one data step may spend, estimate included. */
export const CANONICAL_DATA_STEP_MEMORY_TARGET = 12_000_000;

/** Revealing one stored frame: decoding it and hashing it into the root. */
const PER_REVEALED_FRAME = 13_500;

/** Closing a container: a list's or byte string's break, a map's last value. */
const POP_LIST = 15_000;
const POP_MAP = 8_500;
const POP_CHUNKS = 7_500;

/** One definite chunk of an indefinite byte string. */
const CHUNK = 34_500;

/** An integer head, by its additional information. */
const integerWeight = (additional: number): number =>
  additional < 24
    ? 24_500
    : additional === 24
      ? 29_000
      : additional === 25
        ? 34_000
        : additional === 26
          ? 35_000
          : 36_500;

/**
 * The cost of a tag node, which also opens the list or magnitude after it:
 * opening an indefinite list or byte string pushes a frame.
 */
const tagWeight = (bytes: Uint8Array, offset: number): number => {
  const first = bytes[offset];
  if (first === 0xc2 || first === 0xc3)
    return bytes[offset + 1] === 0x5f ? 52_000 : 50_500;
  if (first === 0xd8 && bytes[offset + 1] === 0x66) {
    // `d8 66 82 <alternative> <fields>`: the widest alternatives cost most.
    const alternative = bytes[offset + 3];
    const width =
      alternative === 0x18
        ? 1
        : alternative === 0x19
          ? 2
          : alternative === 0x1a
            ? 4
            : 8;
    const fields = bytes[offset + 4 + width];
    if (width === 1) return fields === 0x9f ? 62_000 : 59_000;
    return fields === 0x9f ? 71_000 : 66_500;
  }
  if (first === 0xd8) return bytes[offset + 2] === 0x9f ? 45_500 : 41_000;
  if (first === 0xd9) return bytes[offset + 3] === 0x9f ? 52_000 : 47_500;
  return 71_000;
};

/** The cost of reading the item whose first byte is at `offset`. */
const itemWeight = (bytes: Uint8Array, offset: number): number => {
  const first = bytes[offset];
  if (first === undefined) return 71_000;
  const major = first >> 5;
  const additional = first & 31;
  switch (major) {
    case 0:
    case 1:
      return integerWeight(additional);
    case 2:
      return first === 0x5f ? 31_500 : additional < 24 ? 29_500 : 34_500;
    case 4:
      return first === 0x9f ? 31_500 : 27_000;
    case 5:
      return first === 0xa0 ? 28_500 : additional < 24 ? 28_000 : 40_000;
    case 6:
      return tagWeight(bytes, offset);
    default:
      return 71_000;
  }
};

/** The cost of the node the reader takes next at `offset` under `top`. */
export const canonicalDataNodeWeight = (
  bytes: Uint8Array,
  offset: number,
  top: MidgardDataStreamFrame | undefined,
): number => {
  if (top === undefined) return itemWeight(bytes, offset);
  switch (top.kind) {
    case "map":
      return top.remaining === 0n ? POP_MAP : itemWeight(bytes, offset);
    case "chunks":
      return bytes[offset] === 0xff ? POP_CHUNKS : CHUNK;
    case "list":
      return bytes[offset] === 0xff ? POP_LIST : itemWeight(bytes, offset);
  }
};

/** The bytes one step reads, in the carrier's coordinates. */
export type CanonicalDataWindow = {
  /** Carrier offset of the window's first byte. */
  readonly start: number;
  /** Authenticated length of the window, before any cut at the span's end. */
  readonly length: number;
  /** The bytes the reader runs over, already cut at the span's end when `full`. */
  readonly bytes: Uint8Array;
  readonly full: boolean;
};

/** One data-step transaction: the state it spends and what it reveals. */
export type CanonicalDataStreamStep = {
  /** Carrier offset of the next node. */
  readonly cursor: number;
  /** Every open container, top first; the stored root is their hash chain. */
  readonly stack: readonly MidgardDataStreamFrame[];
  readonly stackRoot: string;
  readonly windowStart: number;
  readonly windowLength: number;
  /** How many of `stack`'s top frames the redeemer reveals. */
  readonly revealedCount: number;
  readonly tailRoot: string;
  readonly nodeBudget: number;
  /** The planner's execution-memory estimate for the step. */
  readonly estimatedMemory: number;
  /** The state the step continues to, or the verdict it finishes with. */
  readonly next:
    | {
        readonly kind: "continue";
        readonly cursor: number;
        readonly stack: readonly MidgardDataStreamFrame[];
        readonly stackRoot: string;
      }
    | { readonly kind: "finish"; readonly canonical: boolean };
};

export type CanonicalDataStreamPlan = {
  readonly canonical: boolean;
  readonly steps: readonly CanonicalDataStreamStep[];
};

/**
 * The node budget for one step from `cursor` with `stack` open: the most
 * nodes whose estimate fits the target, at least one, and enough to reach a
 * verdict that falls within it. Also returns the estimate.
 */
const chooseBudget = ({
  window,
  rootStart,
  offset: startOffset,
  stack,
  fixedMemory,
  target,
}: {
  readonly window: CanonicalDataWindow;
  readonly rootStart: number;
  readonly offset: number;
  readonly stack: readonly MidgardDataStreamFrame[];
  readonly fixedMemory: number;
  readonly target: number;
}): { readonly budget: number; readonly estimate: number } => {
  let frames = stack;
  let offset = startOffset;
  let original = stack.length;
  let pushed = 0;
  let lowest = stack.length;
  let nodes = 0;
  let spent = fixedMemory;
  const revealCost = (low: number) =>
    PER_REVEALED_FRAME * (Math.min(stack.length, stack.length - low + 1) + 1);
  for (;;) {
    const weight = canonicalDataNodeWeight(window.bytes, offset, frames[0]);
    const { outcome, consumed } = runMidgardDataStreamTracked({
      bytes: window.bytes,
      full: window.full,
      rootStart,
      offset,
      frames,
      tailRoot: MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
      budget: 1,
    });
    // A node that pops a frame of the stored stack widens the reveal, so
    // the node is charged the reveal it leaves behind.
    const popsOriginal = consumed > 0 && pushed === 0;
    const lowestAfter = popsOriginal ? Math.min(lowest, original - 1) : lowest;
    const estimateAfter = spent + weight + revealCost(lowestAfter);
    if (nodes > 0 && estimateAfter > target)
      return { budget: nodes, estimate: spent + revealCost(lowest) };
    if (outcome.kind !== "paused")
      return { budget: nodes + 1, estimate: estimateAfter };
    if (outcome.nodes === 0)
      return {
        budget: Math.max(nodes, 1),
        estimate: spent + revealCost(lowest),
      };
    spent += weight;
    nodes += 1;
    if (outcome.frames.length > frames.length) pushed += 1;
    else if (popsOriginal) {
      original -= 1;
      lowest = lowestAfter;
    } else if (outcome.frames.length < frames.length) pushed -= 1;
    frames = outcome.frames;
    offset = outcome.offset;
  }
};

/**
 * The honest step sequence reading the span from `spanStart` (carrier
 * offset of its first byte). `windowAt` gives the window a step at a cursor
 * authenticates, exactly as the on-chain step builds it.
 */
export const planCanonicalDataStream = ({
  spanStart,
  windowAt,
  fixedMemory,
  target = CANONICAL_DATA_STEP_MEMORY_TARGET,
  label,
}: {
  readonly spanStart: number;
  readonly windowAt: (cursor: number) => CanonicalDataWindow;
  readonly fixedMemory: number;
  readonly target?: number;
  readonly label: string;
}): CanonicalDataStreamPlan => {
  const steps: CanonicalDataStreamStep[] = [];
  let cursor = spanStart;
  let stack: readonly MidgardDataStreamFrame[] = [];
  for (;;) {
    const window = windowAt(cursor);
    const rootStart = spanStart - window.start;
    const offset = cursor - window.start;
    const { budget, estimate } = chooseBudget({
      window,
      rootStart,
      offset,
      stack,
      fixedMemory,
      target,
    });
    const common = {
      bytes: window.bytes,
      full: window.full,
      rootStart,
      offset,
      budget,
    };
    const { consumed } = runMidgardDataStreamTracked({
      ...common,
      frames: stack,
      tailRoot: MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
    });
    const revealedCount = Math.min(stack.length, consumed + 1);
    const revealed = stack.slice(0, revealedCount);
    const hidden = stack.slice(revealedCount);
    const tailRoot = midgardDataStreamStackRoot(
      hidden,
      MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
    );
    const outcome = runMidgardDataStream({
      ...common,
      frames: revealed,
      tailRoot,
    });
    const step = {
      cursor,
      stack,
      stackRoot: midgardDataStreamStackRoot(revealed, tailRoot).toString("hex"),
      windowStart: window.start,
      windowLength: window.length,
      revealedCount,
      tailRoot: tailRoot.toString("hex"),
      nodeBudget: budget,
      estimatedMemory: estimate,
    };
    if (outcome.kind !== "paused") {
      const canonical = outcome.kind === "canonical";
      steps.push(
        Object.freeze({
          ...step,
          next: Object.freeze({ kind: "finish" as const, canonical }),
        }),
      );
      return Object.freeze({ canonical, steps: Object.freeze(steps) });
    }
    if (outcome.nodes === 0)
      throw new Error(`${label}: data step made no progress`);
    const nextStack = Object.freeze([...outcome.frames, ...hidden]);
    const nextCursor = window.start + outcome.offset;
    steps.push(
      Object.freeze({
        ...step,
        next: Object.freeze({
          kind: "continue" as const,
          cursor: nextCursor,
          stack: nextStack,
          stackRoot: midgardDataStreamStackRoot(
            outcome.frames,
            tailRoot,
          ).toString("hex"),
        }),
      }),
    );
    cursor = nextCursor;
    stack = nextStack;
  }
};

/** A frame as the `StreamFrameV1` Data a step redeemer carries. */
export const canonicalDataStreamFrameData = (frame: MidgardDataStreamFrame) => {
  switch (frame.kind) {
    case "list":
      return { ListFrame: { started: frame.started } };
    case "map":
      return { MapFrame: { remaining: frame.remaining } };
    case "chunks":
      return {
        ChunksFrame: { seen: BigInt(frame.seen), leading: frame.leading },
      };
  }
};
