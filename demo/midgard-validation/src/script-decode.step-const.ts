/**
 * The constant ticks of the script-decode step machine: the type tag list,
 * values and folds, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  hashMidgardCekBlobChunk,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

import { shortBytesSummary } from "./script-decode.step-bits.js";
import {
  intData,
  serialiseScriptDecodeData,
} from "./script-decode.step-data.js";
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
import type {
  ConstantType,
  Control,
  Fold,
  Frame,
  Summary,
} from "./script-decode.step-types.js";

const MAX_TYPE_TAGS = 32;
const TYPE_TAGS_PER_TICK = 8;

// ## Constant types

/**
 * One type tag through the kind grammar: `pending` types remain, the next
 * symbol sits under `top` (0 a type, 1 one application, 2 two).
 */
const kindStep = (
  pending: number,
  top: number,
  bad: boolean,
  tag: number,
): readonly [number, number, boolean] => {
  if (bad || pending === 0) return [pending, top, true];
  if (tag === 7)
    return top === 2 ? [pending, top, true] : [pending, top + 1, false];
  if (tag === 5 || tag === 12) {
    return top === 1 ? [pending, 0, false] : [pending, top, true];
  }
  if (tag === 6) {
    return top === 2 ? [pending + 1, 0, false] : [pending, top, true];
  }
  if (tag === 14 || tag === 15) return [pending, top, true];
  return top === 0 ? [pending - 1, 0, false] : [pending, top, true];
};

const SIMPLE_TYPES: Readonly<Record<number, ConstantType>> = {
  0: { k: "integer" },
  1: { k: "bytestring" },
  2: { k: "string" },
  3: { k: "unit" },
  4: { k: "boolean" },
  8: { k: "data" },
  9: { k: "g1" },
  10: { k: "g2" },
  11: { k: "ml" },
  13: { k: "value" },
};

const parseType = (
  tags: readonly number[],
  from: number,
): readonly [ConstantType, number] => {
  const tag = tags[from];
  if (tag === undefined) throw new Error("script decode: empty type tags");
  if (tag === 5 || tag === 12) {
    const [element, after] = parseType(tags, from + 1);
    return [{ k: tag === 5 ? "list" : "array", element }, after];
  }
  if (tag === 6) {
    const [first, middle] = parseType(tags, from + 1);
    const [second, after] = parseType(tags, middle);
    return [{ k: "pair", first, second }, after];
  }
  const simple = SIMPLE_TYPES[tag];
  if (simple === undefined) throw new Error("script decode: bad type tag");
  return [simple, from + 1];
};

export const tickTypes = (
  env: DecodeEnv,
  ex: Exec,
  control: Extract<Control, { k: "types" }>,
): Tick => {
  let { count, pending, top, bad, tags } = control;
  let c = ex.cursor;
  for (let budget = TYPE_TAGS_PER_TICK; budget > 0; budget -= 1) {
    if (!have(env, c, 1)) {
      return refuse(env, ex, ScriptDecodeClass.flatTruncated, c);
    }
    if (read(env, c, 1) === 0) {
      const after = c + 1;
      if (bad || pending !== 0) {
        return refuse(env, ex, ScriptDecodeClass.unknownConstantType, after);
      }
      if (count > MAX_TYPE_TAGS) {
        return refuse(env, ex, ScriptDecodeClass.constantTypeTooLarge, after);
      }
      const ordered = [...tags].reverse();
      const [ty] = parseType(ordered, 0);
      return at(
        push(ex, {
          k: "constant",
          typeHash: Buffer.from(
            hashMidgardCekBlobChunk(
              serialiseScriptDecodeData(ordered.map(intData)),
            ),
          ),
        }),
        after,
        { k: "value", ty },
      );
    }
    if (!have(env, c + 1, 4)) {
      return refuse(env, ex, ScriptDecodeClass.flatTruncated, c + 1);
    }
    const tag = read(env, c + 1, 4);
    [pending, top, bad] = kindStep(pending, top, bad, tag);
    if (tag !== 7 && count + 1 <= MAX_TYPE_TAGS) tags = [tag, ...tags];
    count += 1;
    c += 5;
  }
  return at(ex, c, { k: "types", count, pending, top, bad, tags });
};

// ## Constant values

const emptyConstr = (constructor: number): Summary =>
  summarizeMidgardCekSmallConstrData(
    BigInt(constructor),
    emptyMidgardCekDataListSummary(),
  );

export const tickValue = (env: DecodeEnv, ex: Exec, ty: ConstantType): Tick => {
  const cursor = ex.cursor;
  switch (ty.k) {
    case "integer":
      return to(ex, {
        k: "nat",
        purpose: { k: "integer" },
        start: cursor,
        groups: 0,
        acc: 0n,
        scale: 1n,
        big: false,
      });
    case "bytestring":
      return to(ex, { k: "filler", purpose: "plain" });
    case "string":
      return to(ex, { k: "filler", purpose: "string" });
    case "data":
      return to(ex, { k: "filler", purpose: "data" });
    case "unit":
      return to(ex, { k: "valueReturn", summary: emptyConstr(0), memory: 1n });
    case "boolean":
      if (!have(env, cursor, 1)) {
        return refuse(env, ex, ScriptDecodeClass.flatTruncated, cursor);
      }
      return at(ex, cursor + 1, {
        k: "valueReturn",
        summary: emptyConstr(read(env, cursor, 1)),
        memory: 1n,
      });
    case "list":
    case "array":
      return to(
        push(ex, {
          k: "valueList",
          element: ty.element,
          array: ty.k === "array",
          count: 0,
          memory: 0n,
        }),
        { k: "listBit", element: ty.element },
      );
    case "pair":
      return to(push(ex, { k: "valuePairFirst", second: ty.second }), {
        k: "value",
        ty: ty.first,
      });
    case "value":
      return to(
        push(ex, {
          k: "assetOuter",
          count: 0,
          previous: undefined,
          invalid: false,
          total: 0,
        }),
        { k: "assetOuterBit" },
      );
    case "g1":
    case "g2":
    case "ml":
      return refuse(env, ex, ScriptDecodeClass.blsConstant, cursor);
  }
};

export const tickValueReturn = (
  ex: Exec,
  summary: Summary,
  memory: bigint,
): Tick => {
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  const [frame, rest] = popped;
  switch (frame.k) {
    case "constant":
      return to(rest, {
        k: "return",
        hash: Buffer.from(
          hashMidgardCekTermNode({
            kind: "constant",
            value: hashMidgardCekValueNode({
              kind: "constant",
              typeRoot: frame.typeHash,
              payloadRoot: summary.root,
              payloadLength: summary.cborLength,
              semanticRoot: summary.root,
              memory,
            }),
          }),
        ),
      });
    case "valueList":
      return to(
        push(push(rest, { k: "valueItem", summary }), {
          ...frame,
          count: frame.count + 1,
          memory: frame.memory + memory,
        }),
        { k: "listBit", element: frame.element },
      );
    case "valuePairFirst":
      return to(push(rest, { k: "valuePairSecond", first: summary, memory }), {
        k: "value",
        ty: frame.second,
      });
    case "valuePairSecond":
      return to(rest, {
        k: "valueReturn",
        summary: summarizeMidgardCekSmallConstrData(
          0n,
          prependMidgardCekDataListSummary(
            frame.first,
            prependMidgardCekDataListSummary(
              summary,
              emptyMidgardCekDataListSummary(),
            ),
          ),
        ),
        memory: frame.memory + memory,
      });
    case "delay":
    case "lambda":
    case "force":
    case "applyFunction":
    case "caseScrutinee":
    case "dataRoot":
    case "applyArgument":
    case "constrFields":
    case "caseBranches":
    case "termChild":
    case "valueItem":
    case "assetOuter":
    case "assetInner":
    case "pairEntry":
    case "dataList":
    case "dataMap":
      throw new Error(
        "script decode: constant value under a non-constant frame",
      );
  }
};

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

export const tickListBit = (
  env: DecodeEnv,
  ex: Exec,
  element: ConstantType,
): Tick =>
  listBit(env, ex, { k: "value", ty: element }, (frame, rest, after) => {
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
        array: list.array,
        count: list.count,
        memory: list.memory,
      },
    });
  });

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

/** Folds one popped entry into the sequence. */
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

export const tickConstFold = (
  env: DecodeEnv,
  ex: Exec,
  control: Extract<Control, { k: "constFold" }>,
): Tick => {
  const { fold, next } = control;
  if (fold.remaining > 0) {
    const popped = pop(ex);
    if (popped === undefined) return undefined;
    return to(popped[1], { ...control, fold: foldOne(fold, popped[0]) });
  }
  switch (next.k) {
    case "list":
      return to(ex, {
        k: "valueReturn",
        summary: summarizeMidgardCekListData(fold.sequence),
        memory: next.array ? BigInt(next.count) : next.memory,
      });
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
