/**
 * The hops of the writeBits loop (`cek_write_bits_loop_v1`): an honest
 * prover's plan for walking a writeBits step's index list, hop by hop,
 * within the hop budget.
 *
 * A hop opens as many indices as the hop cost model
 * (`cek-write-bits-loop.cost.ts`) fits in the hop budget with the loop
 * margin kept free, at least one. A success's last hop opens the end of the
 * list; a failure's walk ends on the first hop that opens an index outside
 * the input. Planning is deterministic, so a restart replays the plan to the
 * cursor the chain holds.
 */
import {
  emptyMidgardCekDataListSummary,
  type MidgardCekDataSequenceSummary,
  prependMidgardCekDataListSummary,
} from "@al-ft/midgard-core";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
import {
  MIDGARD_CEK_LOOP_HOP_BUDGET,
  type MidgardCekExUnits,
  midgardCekLoopHopFits,
} from "./cek-loop-hop-budget.js";
import {
  midgardCekWriteBitsHopCost,
  type MidgardCekWriteBitsHopShape,
} from "./cek-write-bits-loop.cost.js";
import {
  advanceMidgardCekWriteBitsFailureHop,
  advanceMidgardCekWriteBitsHop,
  type MidgardCekWriteBitsHop,
  midgardCekWriteBitsItemSummary,
  type MidgardCekWriteBitsState,
  type MidgardCekWriteBitsWalk,
} from "./cek-write-bits-loop.js";

/**
 * A writeBits step's loop: a success (with its compacted result) or a
 * failure, its compacted arguments, the input's bytes and the indices, and
 * the summary of every suffix of the indices, the whole list first and the
 * empty list last.
 */
export type MidgardCekWriteBitsLoop = {
  readonly failure: boolean;
  readonly arguments: readonly MidgardCekDirectValueWitness[];
  readonly result: MidgardCekDirectValueWitness | null;
  readonly input: Buffer;
  readonly indices: readonly bigint[];
  readonly suffixes: readonly MidgardCekDataSequenceSummary[];
  readonly itemBytes: readonly number[];
};

/** The state a hop hands on: a success's or a failure's. */
export type MidgardCekWriteBitsCarried =
  | MidgardCekWriteBitsState
  | MidgardCekWriteBitsWalk;

export type MidgardCekWriteBitsPlannedHop = {
  /** The cursor and state the hop starts from; null for the first hop. */
  readonly from: {
    readonly cursor: number;
    readonly state: MidgardCekWriteBitsCarried;
  } | null;
  readonly end: number;
  readonly hop: MidgardCekWriteBitsHop;
  /** The state it hands on; null for the hop that ends the loop. */
  readonly next: MidgardCekWriteBitsCarried | null;
  /** A success's charge, on the hop that ends it. */
  readonly charge: MidgardCekBuiltinBudget | null;
  readonly shape: MidgardCekWriteBitsHopShape;
  readonly cost: MidgardCekExUnits;
};

/** The loop of a writeBits step over `input` and `indices`. */
export const midgardCekWriteBitsLoopOf = (step: {
  readonly failure: boolean;
  readonly arguments: readonly MidgardCekDirectValueWitness[];
  readonly result: MidgardCekDirectValueWitness | null;
  readonly input: Uint8Array;
  readonly indices: readonly bigint[];
}): MidgardCekWriteBitsLoop => {
  // Built from the end of the list, then put in list order.
  const suffixes: MidgardCekDataSequenceSummary[] = [
    emptyMidgardCekDataListSummary(),
  ];
  const itemBytes: number[] = [];
  for (let index = step.indices.length - 1; index >= 0; index -= 1) {
    const item = midgardCekWriteBitsItemSummary(step.indices[index]!);
    itemBytes.push(Number(item.cborLength));
    suffixes.push(prependMidgardCekDataListSummary(item, suffixes.at(-1)!));
  }
  return {
    ...step,
    input: Buffer.from(step.input),
    suffixes: suffixes.reverse(),
    itemBytes: itemBytes.reverse(),
  };
};

const outside = (index: bigint, length: number): boolean =>
  index < 0n || index >= 8n * BigInt(length);

/**
 * The number of indices the hop from `cursor` opens: the most the model fits
 * in `budget` with the loop margin, at most `limit`, at least one; a
 * failure's hop ends at the first index outside the input it reaches.
 */
const hopSize = (
  loop: MidgardCekWriteBitsLoop,
  cursor: number,
  limit: number,
  budget: MidgardCekExUnits,
): { readonly size: number; readonly shape: MidgardCekWriteBitsHopShape } => {
  const shapeOf = (size: number, bytes: number, ends: boolean) => ({
    failure: loop.failure,
    first: cursor === 0,
    last: ends,
    items: size,
    itemBytes: bytes,
    inputBytes: loop.input.length,
  });
  let size = 0;
  let bytes = 0;
  let shape = shapeOf(0, 0, false);
  while (cursor + size < loop.indices.length && size < limit) {
    const index = loop.indices[cursor + size]!;
    const nextBytes = bytes + loop.itemBytes[cursor + size]!;
    const ends = loop.failure
      ? outside(index, loop.input.length)
      : cursor + size + 1 === loop.indices.length;
    const tried = shapeOf(size + 1, nextBytes, ends);
    if (
      size > 0 &&
      !midgardCekLoopHopFits(midgardCekWriteBitsHopCost(tried), budget)
    )
      break;
    size += 1;
    bytes = nextBytes;
    shape = tried;
    if (ends && loop.failure) break;
  }
  if (size === 0) throw new Error("writeBits hop cannot open an index");
  return { size, shape };
};

/** Every hop of `loop`, from the start, at most `limit` indices each. */
export const planMidgardCekWriteBitsHops = (
  loop: MidgardCekWriteBitsLoop,
  options: {
    readonly limit?: number;
    readonly budget?: MidgardCekExUnits;
  } = {},
): MidgardCekWriteBitsPlannedHop[] => {
  const limit = options.limit ?? Number.MAX_SAFE_INTEGER;
  const budget = options.budget ?? MIDGARD_CEK_LOOP_HOP_BUDGET;
  const hops: MidgardCekWriteBitsPlannedHop[] = [];
  let from: MidgardCekWriteBitsPlannedHop["from"] = null;
  let cursor = 0;
  for (;;) {
    const { size, shape } = hopSize(loop, cursor, limit, budget);
    const end = cursor + size;
    const whole = loop.suffixes[0]!;
    const hop: MidgardCekWriteBitsHop = {
      start:
        cursor === 0
          ? {
              input: loop.input,
              itemsCount: whole.length,
              itemsRoot: Buffer.from(whole.root),
            }
          : null,
      items: loop.indices.slice(cursor, end),
      tail: loop.suffixes[end]!,
    };
    const cost = midgardCekWriteBitsHopCost(shape);
    let next: MidgardCekWriteBitsCarried | null;
    let charge: MidgardCekBuiltinBudget | null = null;
    if (loop.failure) {
      const advanced = advanceMidgardCekWriteBitsFailureHop(
        loop.arguments,
        hop,
        from === null
          ? null
          : {
              cursor: from.cursor,
              walk: from.state as MidgardCekWriteBitsWalk,
            },
        end,
      );
      if (advanced.kind === "rejected")
        throw new Error(
          `writeBits failure hop is rejected: ${advanced.reason}`,
        );
      next = advanced.kind === "next" ? advanced.walk : null;
    } else {
      if (loop.result === null) throw new Error("a success has a result");
      const advanced = advanceMidgardCekWriteBitsHop(
        { arguments: loop.arguments, result: loop.result },
        hop,
        from === null
          ? null
          : {
              cursor: from.cursor,
              state: from.state as MidgardCekWriteBitsState,
            },
        end,
      );
      if (advanced.kind === "rejected")
        throw new Error(`writeBits hop is rejected: ${advanced.reason}`);
      if (advanced.kind === "next") next = advanced.state;
      else {
        next = null;
        charge = advanced.budget;
      }
    }
    hops.push({ from, end, hop, next, charge, shape, cost });
    if (next === null) return hops;
    from = { cursor: end, state: next };
    cursor = end;
  }
};
