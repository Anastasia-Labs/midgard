/**
 * The constant ticks of the script-decode step machine: the type tag list and
 * values, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import {
  emptyMidgardCekDataListSummary,
  hashMidgardCekBlobChunk,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

import { general, leafOf } from "./script-decode.step-bulk.js";
import {
  intData,
  serialiseScriptDecodeData,
} from "./script-decode.step-data.js";
import {
  at,
  type DecodeEnv,
  type Exec,
  have,
  pop,
  push,
  read,
  refuse,
  ScriptDecodeClass,
  type Tick,
  to,
} from "./script-decode.step-exec.js";
import {
  PAIR_UNITS,
  pairSummary,
  type Shape,
  shapeOf,
  UNITS_PER_TICK,
} from "./script-decode.step-shape.js";
import type {
  ConstantType,
  Control,
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

export const bitsOf = (env: DecodeEnv) => ({
  window: env.window,
  flatLength: env.flatLength,
});

/**
 * Decodes the value of `ty` in bulk where its bits allow, and otherwise as one
 * node, after decoding what it can of a pair's components.
 */
export const tickValue = (env: DecodeEnv, ex: Exec, ty: ConstantType): Tick =>
  piece(
    env,
    ex,
    ex.cursor,
    ty,
    shapeOf(ty),
    UNITS_PER_TICK,
    (held) => held,
    (after, summary, memory, left) =>
      climb(env, { ...ex, cursor: after }, summary, memory, left),
  );

type PieceOk = (
  after: number,
  summary: Summary,
  memory: bigint,
  units: number,
) => Tick;

/**
 * Decodes the value of `ty` at `cursor` while `units` last, as `piece`. A pair
 * is decoded component by component; the first value that does not decode in
 * bulk is decoded as one node, under the frames `pending` pushes.
 */
const piece = (
  env: DecodeEnv,
  ex: Exec,
  cursor: number,
  ty: ConstantType,
  shape: Shape,
  units: number,
  pending: (held: Exec) => Exec,
  ok: PieceOk,
): Tick => {
  const node = () => nodeValue(env, pending({ ...ex, cursor }), ty);
  if (units < 0) return node();
  const leaf = leafOf(bitsOf(env), shape, cursor);
  if (leaf.k === "truncated") return node();
  if (leaf.k === "fixed") {
    return ok(leaf.after, leaf.value.summary, leaf.value.memory, units);
  }
  const kind = leaf.kind;
  if (kind.k === "pair") {
    if (ty.k !== "pair") {
      throw new Error("script decode: a pair shape of a non-pair type");
    }
    return piece(
      env,
      ex,
      cursor,
      ty.first,
      kind.first,
      units - PAIR_UNITS,
      (held) => push(pending(held), { k: "valuePairFirst", second: ty.second }),
      (middle, a, aMemory, left) =>
        piece(
          env,
          ex,
          middle,
          ty.second,
          kind.second,
          left,
          (held) =>
            push(pending(held), {
              k: "valuePairSecond",
              first: a,
              memory: aMemory,
            }),
          (after, b, bMemory, rest) =>
            ok(after, pairSummary(a, b), aMemory + bMemory, rest),
        ),
    );
  }
  const value = general(bitsOf(env), kind, cursor, units);
  if (value.k === "stop") return node();
  return ok(value.after, value.summary, value.memory, value.units);
};

/** One node of a constant value: a leaf, or the frame of a list, pair or value. */
const nodeValue = (env: DecodeEnv, ex: Exec, ty: ConstantType): Tick => {
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
  env: DecodeEnv,
  ex: Exec,
  summary: Summary,
  memory: bigint,
): Tick => {
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  return ascend(env, popped[1], popped[0], summary, memory, UNITS_PER_TICK);
};

/**
 * Returns a decoded value to the frames above it while `units` last and the
 * frames are revealed.
 */
const climb = (
  env: DecodeEnv,
  ex: Exec,
  summary: Summary,
  memory: bigint,
  units: number,
): Exec => {
  const held: Exec = { ...ex, control: { k: "valueReturn", summary, memory } };
  if (units < 0) return held;
  const popped = pop(ex);
  if (popped === undefined) return held;
  const next = ascend(env, popped[1], popped[0], summary, memory, units);
  if (next === undefined) throw new Error("script decode: a stuck ascent");
  return next;
};

/** Returns a decoded value to `frame`. */
const ascend = (
  env: DecodeEnv,
  ex: Exec,
  frame: Frame,
  summary: Summary,
  memory: bigint,
  units: number,
): Tick => {
  switch (frame.k) {
    case "constant":
      return to(ex, {
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
        push(push(ex, { k: "valueItem", summary }), {
          ...frame,
          count: frame.count + 1,
          memory: frame.memory + memory,
        }),
        { k: "listBit", element: frame.element },
      );
    case "valuePairFirst":
      return piece(
        env,
        ex,
        ex.cursor,
        frame.second,
        shapeOf(frame.second),
        units,
        (held) => push(held, { k: "valuePairSecond", first: summary, memory }),
        (after, second, secondMemory, left) =>
          climb(
            env,
            { ...ex, cursor: after },
            pairSummary(summary, second),
            memory + secondMemory,
            left - PAIR_UNITS,
          ),
      );
    case "valuePairSecond":
      return climb(
        env,
        ex,
        pairSummary(frame.first, summary),
        frame.memory + memory,
        units - PAIR_UNITS,
      );
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
    case "valueRun":
      throw new Error(
        "script decode: constant value under a non-constant frame",
      );
  }
};
