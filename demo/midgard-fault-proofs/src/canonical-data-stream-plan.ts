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
 * The node budget is chosen per step from an execution-memory estimate: every
 * node is charged by its shape, read from its first byte and the container
 * it sits in. The weights are measured upper bounds of the on-chain reader's
 * cost per node (pushes include hashing the new frame into the stored stack
 * root); a step's estimate adds the family's fixed per-transaction cost and
 * the revealed frames. Memory binds before CPU for every node shape.
 */

/** Execution memory one data step may spend, estimate included. */
export const CANONICAL_DATA_STEP_MEMORY_TARGET = 12_000_000;

const PER_REVEALED_FRAME = 10_000;

const ITEM_IN_MAP = 2_000;

const unsignedWeight = (additional: number): number =>
  additional < 24
    ? 25_000
    : additional === 24
      ? 30_000
      : additional === 25
        ? 34_500
        : additional === 26
          ? 35_500
          : 36_500;

/** The cost of reading the item whose first byte is at `offset`. */
const itemWeight = (bytes: Uint8Array, offset: number): number => {
  const first = bytes[offset];
  if (first === undefined) return 25_000;
  const major = first >> 5;
  const additional = first & 31;
  switch (major) {
    case 0:
    case 1:
      return unsignedWeight(additional);
    case 2:
      return first === 0x5f ? 33_000 : additional < 24 ? 30_000 : 35_000;
    case 4:
      return first === 0x9f ? 32_500 : 27_500;
    case 5:
      return first === 0xa0 ? 29_000 : 31_000;
    case 6: {
      if (first === 0xc2 || first === 0xc3) return 55_000;
      if (first === 0xd9) return 51_500;
      if (first === 0xd8) return bytes[offset + 1] === 0x66 ? 70_000 : 46_000;
      return 30_000;
    }
    default:
      return 25_000;
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
      return top.remaining === 0n
        ? 12_000
        : itemWeight(bytes, offset) + ITEM_IN_MAP;
    case "chunks":
      return bytes[offset] === 0xff ? 16_000 : 35_000;
    case "list":
      return bytes[offset] === 0xff ? 14_000 : itemWeight(bytes, offset);
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
  const revealCost = () =>
    PER_REVEALED_FRAME *
    (Math.min(stack.length, stack.length - lowest + 1) + 1);
  for (;;) {
    const weight = canonicalDataNodeWeight(window.bytes, offset, frames[0]);
    if (nodes > 0 && spent + weight + revealCost() > target)
      return { budget: nodes, estimate: spent + revealCost() };
    const { outcome } = runMidgardDataStreamTracked({
      bytes: window.bytes,
      full: window.full,
      rootStart,
      offset,
      frames,
      tailRoot: MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
      budget: 1,
    });
    if (outcome.kind !== "paused")
      return {
        budget: nodes + 1,
        estimate: spent + weight + revealCost(),
      };
    if (outcome.nodes === 0)
      return { budget: Math.max(nodes, 1), estimate: spent + revealCost() };
    spent += weight;
    nodes += 1;
    if (outcome.frames.length > frames.length) pushed += 1;
    else if (outcome.frames.length < frames.length) {
      if (pushed > 0) pushed -= 1;
      else {
        original -= 1;
        lowest = Math.min(lowest, original);
      }
    }
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
