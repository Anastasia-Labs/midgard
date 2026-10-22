/**
 * The constant value ticks of the script-decode step machine, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import {
  emptyMidgardCekDataListSummary,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

import { general, leafOf, type Within } from "./script-decode.step-bulk.js";
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
  type Fixed,
  PAIR_UNITS,
  pairSummary,
  type Shape,
  shapeOf,
  UNITS_PER_TICK,
} from "./script-decode.step-shape.js";
import type {
  ConstantType,
  Frame,
  Summary,
} from "./script-decode.step-types.js";

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
    true,
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
 * bulk is decoded as one node, under the frames `pending` pushes. When the
 * decode began at the value with a whole tick's units (`full`), what it
 * decoded of the value is kept instead (see `descend`).
 */
const piece = (
  env: DecodeEnv,
  ex: Exec,
  cursor: number,
  ty: ConstantType,
  shape: Shape,
  units: number,
  full: boolean,
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
      full,
      (held) => push(pending(held), { k: "valuePairFirst", second: ty.second }),
      (middle, a, aMemory, left) =>
        piece(
          env,
          ex,
          middle,
          ty.second,
          kind.second,
          left,
          false,
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
  if (value.k === "stop") {
    return full
      ? descend(env, pending({ ...ex, cursor }), ty, value.path)
      : node();
  }
  return ok(value.after, value.summary, value.memory, value.units);
};

/** The value of a type no bits encode. */
const fixedValue = (ty: ConstantType): Fixed | undefined => {
  if (!noBits(ty)) return undefined;
  const shape = shapeOf(ty);
  if (shape.k !== "fixed") {
    throw new Error("script decode: a type without bits has a bit table");
  }
  return shape.value;
};

const noBits = (ty: ConstantType): boolean =>
  ty.k === "unit" || (ty.k === "pair" && noBits(ty.first) && noBits(ty.second));

/**
 * Keeps what a bulk decode of the value of `ty` at the cursor decoded before
 * it stopped, given as the steps `path` down from the value, as `descend`:
 * pushes the frames of the values it was in, outermost first, each list's
 * decoded elements as a run, and goes on at the cons bit of the innermost
 * list, or, where no step leads further, with the value there as one node. A
 * pair component no bits encode takes no step.
 */
export const descend = (
  env: DecodeEnv,
  ex: Exec,
  ty: ConstantType,
  path: readonly Within[],
): Tick => {
  const [step, ...rest] = path;
  if (ty.k === "list" || ty.k === "array") {
    if (step?.k !== "list") return nodeValue(env, ex, ty);
    const runs =
      step.count > 0
        ? push(ex, {
            k: "valueRun",
            start: ex.cursor,
            count: step.count,
            units: step.units,
          })
        : ex;
    const held = push(runs, {
      k: "valueList",
      element: ty.element,
      array: ty.k === "array",
      count: step.count,
      memory: step.memory,
    });
    if (rest.length === 0) {
      return at(held, step.at, { k: "listBit", element: ty.element });
    }
    return descend(env, { ...held, cursor: step.at + 1 }, ty.element, rest);
  }
  if (ty.k === "pair") {
    const first = fixedValue(ty.first);
    if (first !== undefined) {
      return descend(
        env,
        push(ex, {
          k: "valuePairSecond",
          first: first.summary,
          memory: first.memory,
        }),
        ty.second,
        path,
      );
    }
    if (fixedValue(ty.second) !== undefined) {
      return descend(
        env,
        push(ex, { k: "valuePairFirst", second: ty.second }),
        ty.first,
        path,
      );
    }
    if (step?.k === "first") {
      return descend(
        env,
        push(ex, { k: "valuePairFirst", second: ty.second }),
        ty.first,
        rest,
      );
    }
    // The first component decoded in the scan; it is decoded again in a tick
    // of its own and the second after it.
    if (step?.k === "second") {
      return to(push(ex, { k: "valuePairFirst", second: ty.second }), {
        k: "value",
        ty: ty.first,
      });
    }
    return nodeValue(env, ex, ty);
  }
  return nodeValue(env, ex, ty);
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
 * Units charged for closing a list: its summary, and returning it to the
 * frame above.
 */
export const LIST_CLOSE_UNITS = 32;

/**
 * Returns a decoded value to the frames above it while `units` last and the
 * frames are revealed.
 */
export const climb = (
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

/**
 * Returns a decoded value to `frame`, with `units` left. A pair's second
 * component is decoded in a tick of its own.
 */
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
    case "valueList": {
      const cursor = ex.cursor;
      if (
        units >= LIST_CLOSE_UNITS &&
        have(env, cursor, 1) &&
        read(env, cursor, 1) === 0
      ) {
        const sequence = prependMidgardCekDataListSummary(
          summary,
          emptyMidgardCekDataListSummary(),
        );
        if (frame.count === 0) {
          return climb(
            env,
            { ...ex, cursor: cursor + 1 },
            summarizeMidgardCekListData(sequence),
            frame.array ? 1n : frame.memory + memory,
            units - LIST_CLOSE_UNITS,
          );
        }
        return at(ex, cursor + 1, {
          k: "constFold",
          fold: { pairs: false, remaining: frame.count, sequence },
          next: {
            k: "list",
            element: frame.element,
            array: frame.array,
            count: frame.count + 1,
            memory: frame.memory + memory,
          },
        });
      }
      return to(
        push(push(ex, { k: "valueItem", summary }), {
          ...frame,
          count: frame.count + 1,
          memory: frame.memory + memory,
        }),
        { k: "listBit", element: frame.element },
      );
    }
    case "valuePairFirst":
      return to(push(ex, { k: "valuePairSecond", first: summary, memory }), {
        k: "value",
        ty: frame.second,
      });
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
