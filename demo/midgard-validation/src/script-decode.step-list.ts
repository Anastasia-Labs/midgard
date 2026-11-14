/**
 * The list ticks of the script-decode step machine: cons bits, the cons bits
 * of a Value's maps, and the folds that close them, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
} from "@al-ft/midgard-core";

import { shortBytesSummary } from "./script-decode.step-bits.js";
import { foldRun, scanRun } from "./script-decode.step-bulk.js";
import {
  bitsOf,
  climb,
  descend,
  LIST_CLOSE_UNITS,
} from "./script-decode.step-const.js";
import {
  at,
  type DecodeEnv,
  type Exec,
  expectFrame,
  have,
  pop,
  push,
  read,
  refuse,
  ScriptDecodeClass,
  type Tick,
  to,
} from "./script-decode.step-exec.js";
import { shapeOf, UNITS_PER_TICK } from "./script-decode.step-shape.js";
import type {
  ConstantType,
  ConstFoldNext,
  Control,
  Fold,
  Frame,
} from "./script-decode.step-types.js";

const listBit = (
  env: DecodeEnv,
  ex: Exec,
  more: Control,
  close: (frame: Frame, rest: Exec, after: number) => Tick,
): Tick => {
  const cursor = ex.cursor;
  if (!have(env, cursor, 1)) {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, cursor);
  }
  if (read(env, cursor, 1) === 1) return at(ex, cursor + 1, more);
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  return close(popped[0], popped[1], cursor + 1);
};
/**
 * At a cons bit, decodes the run of elements that decode in bulk, or else the
 * next element as a value. At the nil bit the list's fold is a tick of its own.
 */
export const tickListBit = (
  env: DecodeEnv,
  ex: Exec,
  element: ConstantType,
): Tick => {
  const cursor = ex.cursor;
  if (have(env, cursor, 1) && read(env, cursor, 1) === 1) {
    const [run, after, runMemory, spent, path] = scanRun(
      bitsOf(env),
      shapeOf(element),
      cursor,
    );
    if (run === 0) {
      return descend(env, { ...ex, cursor: cursor + 1 }, element, path);
    }
    const popped = pop(ex);
    if (popped === undefined) return undefined;
    const list = expectFrame(popped[0], "valueList");
    return at(
      push(
        push(popped[1], {
          k: "valueRun",
          start: cursor,
          count: run,
          units: spent,
        }),
        {
          ...list,
          element,
          count: list.count + run,
          memory: list.memory + runMemory,
        },
      ),
      after,
      { k: "listBit", element },
    );
  }
  return listBit(env, ex, { k: "value", ty: element }, (frame, rest, after) => {
    const list = expectFrame(frame, "valueList");
    return at(rest, after, {
      k: "constFold",
      fold: {
        pairs: false,
        remaining: list.count,
        sequence: emptyMidgardCekDataListSummary(),
      },
      next: {
        k: "list",
        element,
        array: list.array,
        count: list.count,
        memory: list.memory,
      },
    });
  });
};

export const tickAssetOuterBit = (env: DecodeEnv, ex: Exec): Tick =>
  listBit(
    env,
    ex,
    { k: "filler", purpose: "currency" },
    (frame, rest, after) => {
      const outer = expectFrame(frame, "assetOuter");
      return at(rest, after, {
        k: "constFold",
        fold: {
          pairs: true,
          remaining: outer.count,
          sequence: emptyMidgardCekDataPairSummary(),
        },
        next: { k: "assetOuter", invalid: outer.invalid, total: outer.total },
      });
    },
  );

export const tickAssetInnerBit = (env: DecodeEnv, ex: Exec): Tick =>
  listBit(env, ex, { k: "filler", purpose: "token" }, (frame, rest, after) => {
    const inner = expectFrame(frame, "assetInner");
    return at(rest, after, {
      k: "constFold",
      fold: {
        pairs: true,
        remaining: inner.count,
        sequence: emptyMidgardCekDataPairSummary(),
      },
      next: {
        k: "assetInner",
        currency: inner.currency,
        count: inner.count,
        invalid: inner.invalid,
      },
    });
  });

/** Folds one popped entry of a Data list or map into the sequence. */
export const foldOne = (fold: Fold, frame: Frame): Fold => {
  if (fold.pairs) {
    const entry = expectFrame(frame, "pairEntry");
    return {
      ...fold,
      remaining: fold.remaining - 1,
      sequence: prependMidgardCekDataPairSummary(
        entry.key,
        entry.value,
        fold.sequence,
      ),
    };
  }
  const item = expectFrame(frame, "valueItem");
  return {
    ...fold,
    remaining: fold.remaining - 1,
    sequence: prependMidgardCekDataListSummary(item.summary, fold.sequence),
  };
};

/**
 * Units charged for folding one pair entry and one list item. Each entry is a
 * revealed frame, so its charge covers opening the frame as well as folding
 * it.
 */
const FOLD_PAIR_UNITS = 12;
const FOLD_ITEM_UNITS = 8;

/** The units folding `frame` costs. */
const entryUnits = (frame: Frame): number => {
  if (frame.k === "pairEntry") return FOLD_PAIR_UNITS;
  if (frame.k === "valueItem") return FOLD_ITEM_UNITS;
  if (frame.k === "valueRun") return frame.units;
  throw new Error("script decode: a non-entry frame in a constant fold");
};

/** Folds one entry into the sequence. */
const foldEntry = (
  env: DecodeEnv,
  fold: Fold,
  next: ConstFoldNext,
  frame: Frame,
): Fold => {
  if (frame.k === "pairEntry") {
    if (!fold.pairs) throw new Error("script decode: a pair in a list fold");
    return foldOne(fold, frame);
  }
  if (frame.k === "valueItem") {
    if (fold.pairs) throw new Error("script decode: an item in a map fold");
    return foldOne(fold, frame);
  }
  if (frame.k === "valueRun") {
    if (next.k !== "list") throw new Error("script decode: a run in a map");
    return {
      ...fold,
      remaining: fold.remaining - frame.count,
      sequence: foldRun(
        bitsOf(env),
        shapeOf(next.element),
        frame.start,
        frame.count,
        fold.sequence,
      ),
    };
  }
  throw new Error("script decode: a non-entry frame in a constant fold");
};

/**
 * Folds further entries while each fits in `units` and the frames are
 * revealed; a list whose entries are all folded closes, and its value climbs
 * while `units` last.
 */
const foldMore = (
  env: DecodeEnv,
  ex: Exec,
  fold: Fold,
  next: ConstFoldNext,
  units: number,
): Exec => {
  let folded = fold;
  let current = ex;
  let left = units;
  while (folded.remaining > 0) {
    const more = pop(current);
    if (more === undefined) break;
    const cost = entryUnits(more[0]);
    if (cost > left) break;
    folded = foldEntry(env, folded, next, more[0]);
    current = more[1];
    left -= cost;
  }
  if (folded.remaining === 0 && next.k === "list" && left >= LIST_CLOSE_UNITS) {
    return climb(
      env,
      current,
      summarizeMidgardCekListData(folded.sequence),
      next.array ? BigInt(next.count) : next.memory,
      left - LIST_CLOSE_UNITS,
    );
  }
  return { ...current, control: { k: "constFold", fold: folded, next } };
};

export const tickConstFold = (
  env: DecodeEnv,
  ex: Exec,
  control: Extract<Control, { k: "constFold" }>,
): Tick => {
  const { fold, next } = control;
  if (fold.remaining > 0) {
    const popped = pop(ex);
    if (popped === undefined) return undefined;
    return foldMore(
      env,
      popped[1],
      foldEntry(env, fold, next, popped[0]),
      next,
      UNITS_PER_TICK - entryUnits(popped[0]),
    );
  }
  switch (next.k) {
    case "list":
      return climb(
        env,
        ex,
        summarizeMidgardCekListData(fold.sequence),
        next.array ? BigInt(next.count) : next.memory,
        UNITS_PER_TICK - LIST_CLOSE_UNITS,
      );
    case "assetInner": {
      const popped = pop(ex);
      if (popped === undefined) return undefined;
      const outer = expectFrame(popped[0], "assetOuter");
      return to(
        push(
          push(popped[1], {
            k: "pairEntry",
            key: shortBytesSummary(next.currency),
            value: summarizeMidgardCekMapData(fold.sequence),
          }),
          {
            k: "assetOuter",
            count: outer.count + 1,
            previous: outer.previous,
            invalid: outer.invalid || next.invalid || next.count === 0,
            total: outer.total + next.count,
          },
        ),
        { k: "assetOuterBit" },
      );
    }
    case "assetOuter":
      if (next.invalid) {
        return refuse(env, ex, ScriptDecodeClass.nonCanonicalValue, ex.cursor);
      }
      return to(ex, {
        k: "valueReturn",
        summary: summarizeMidgardCekMapData(fold.sequence),
        memory: BigInt(next.total),
      });
  }
};
