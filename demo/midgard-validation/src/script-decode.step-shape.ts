/**
 * The constant-type shapes of the script-decode bulk decoder, as in
 * `onchain/aiken/lib/midgard/script-decode-bulk-v1.ak`: the table a type is
 * read through, and the units a tick may spend on it.
 */
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

import type { ConstantType, Summary } from "./script-decode.step-types.js";

/** The units one tick may spend on bulk decoding. */
export const UNITS_PER_TICK = 512;

/** Units charged per list element and per pair. */
export const ELEMENT_UNITS = 2;
export const PAIR_UNITS = 3;

/** More units than any run spends. */
export const UNMETERED = 1_000_000_000;

export const pairSummary = (a: Summary, b: Summary): Summary =>
  summarizeMidgardCekSmallConstrData(
    0n,
    prependMidgardCekDataListSummary(
      a,
      prependMidgardCekDataListSummary(b, emptyMidgardCekDataListSummary()),
    ),
  );

const emptyConstr = (constructor: bigint): Summary =>
  summarizeMidgardCekSmallConstrData(
    constructor,
    emptyMidgardCekDataListSummary(),
  );

// ## Shapes

export type Fixed = { readonly summary: Summary; readonly memory: bigint };

/** A constant type as the bulk decoder reads it. */
export type Shape =
  /** A value its bits so far fix. */
  | { readonly k: "fixed"; readonly value: Fixed }
  /** Reads one bit and goes on with `zero` or `one`. */
  | { readonly k: "bit"; readonly zero: Shape; readonly one: Shape }
  /** Decodes `kind` from the bit where the shape began. */
  | { readonly k: "gen"; readonly kind: Kind };

export type Kind =
  | { readonly k: "integer" | "bytes" | "string" | "stop" }
  | { readonly k: "list"; readonly element: Shape; readonly array: boolean }
  /** `stops` when a component never decodes here. */
  | {
      readonly k: "pair";
      readonly first: Shape;
      readonly second: Shape;
      readonly stops: boolean;
    }
  /**
   * A value of `inner` as a component of pairs whose other component no bits
   * encode, the innermost pair first, charged `units` and adding `memory`.
   */
  | {
      readonly k: "wrap";
      readonly inner: Kind;
      readonly layers: readonly Layer[];
      readonly units: number;
      readonly memory: bigint;
    };

/** The other component of a pair a value is wrapped in, and whether it comes first. */
export type Layer = { readonly first: boolean; readonly summary: Summary };

export const wrapSummary = (layer: Layer, summary: Summary): Summary =>
  layer.first
    ? pairSummary(layer.summary, summary)
    : pairSummary(summary, layer.summary);

/** The most leaves a pair's bit table may have. */
const MAX_LEAVES = 4;

const fixed = (summary: Summary, memory: bigint): Shape => ({
  k: "fixed",
  value: { summary, memory },
});

const gen = (kind: Kind): Shape => ({ k: "gen", kind });

/** No value of the shape decodes here. */
export const never = (shape: Shape): boolean =>
  shape.k === "gen" &&
  (shape.kind.k === "stop" || (shape.kind.k === "pair" && shape.kind.stops));

const leaves = (shape: Shape): number =>
  shape.k === "bit" ? leaves(shape.zero) + leaves(shape.one) : 1;

const hasFixed = (shape: Shape): boolean =>
  shape.k === "fixed" ||
  (shape.k === "bit" && (hasFixed(shape.zero) || hasFixed(shape.one)));

const graftSecond = (a: Fixed, b: Shape, general: Shape): Shape => {
  switch (b.k) {
    case "fixed":
      return fixed(
        pairSummary(a.summary, b.value.summary),
        a.memory + b.value.memory,
      );
    case "bit":
      return {
        k: "bit",
        zero: graftSecond(a, b.zero, general),
        one: graftSecond(a, b.one, general),
      };
    case "gen":
      return general;
  }
};

const graftFirst = (a: Shape, b: Shape, general: Shape): Shape => {
  switch (a.k) {
    case "fixed":
      return graftSecond(a.value, b, general);
    case "bit":
      return {
        k: "bit",
        zero: graftFirst(a.zero, b, general),
        one: graftFirst(a.one, b, general),
      };
    case "gen":
      return general;
  }
};

/**
 * The shape of a pair: the other component's table wrapped in the pair when
 * one component is fixed and the other can decode here, a bit table over its
 * components' tables when both have a fixed leaf and the table stays small,
 * and otherwise decoded component by component.
 */
const pairShape = (a: Shape, b: Shape): Shape => {
  if (a.k === "fixed" && !never(b))
    return wrapShape(
      b,
      { first: true, summary: a.value.summary },
      a.value.memory,
    );
  if (b.k === "fixed" && !never(a))
    return wrapShape(
      a,
      { first: false, summary: b.value.summary },
      b.value.memory,
    );
  const general = gen({
    k: "pair",
    first: a,
    second: b,
    stops: never(a) || never(b),
  });
  if (hasFixed(a) && hasFixed(b) && leaves(a) * leaves(b) <= MAX_LEAVES) {
    return graftFirst(a, b, general);
  }
  return general;
};

/**
 * The shape of a pair of a value of `shape` and a value no bits encode, given
 * as `layer` and its constant `memory`: the same bit table, each leaf wrapped
 * in the pair.
 */
const wrapShape = (shape: Shape, layer: Layer, memory: bigint): Shape => {
  switch (shape.k) {
    case "fixed":
      return fixed(
        wrapSummary(layer, shape.value.summary),
        shape.value.memory + memory,
      );
    case "bit":
      return {
        k: "bit",
        zero: wrapShape(shape.zero, layer, memory),
        one: wrapShape(shape.one, layer, memory),
      };
    case "gen": {
      const kind = shape.kind;
      if (kind.k === "stop" || (kind.k === "pair" && kind.stops)) return shape;
      if (kind.k === "wrap")
        return gen({
          k: "wrap",
          inner: kind.inner,
          layers: [...kind.layers, layer],
          units: kind.units + PAIR_UNITS,
          memory: kind.memory + memory,
        });
      return gen({
        k: "wrap",
        inner: kind,
        layers: [layer],
        units: PAIR_UNITS,
        memory,
      });
    }
  }
};

/** The shape of `ty`. */
export const shapeOf = (ty: ConstantType): Shape => {
  switch (ty.k) {
    case "unit":
      return fixed(emptyConstr(0n), 1n);
    case "boolean":
      return {
        k: "bit",
        zero: fixed(emptyConstr(0n), 1n),
        one: fixed(emptyConstr(1n), 1n),
      };
    case "integer":
      return gen({ k: "integer" });
    case "bytestring":
      return gen({ k: "bytes" });
    case "string":
      return gen({ k: "string" });
    case "list":
    case "array":
      return {
        k: "bit",
        zero: fixed(
          summarizeMidgardCekListData(emptyMidgardCekDataListSummary()),
          0n,
        ),
        one: gen({
          k: "list",
          element: shapeOf(ty.element),
          array: ty.k === "array",
        }),
      };
    case "value":
      return {
        k: "bit",
        zero: fixed(
          summarizeMidgardCekMapData(emptyMidgardCekDataPairSummary()),
          0n,
        ),
        one: gen({ k: "stop" }),
      };
    case "pair":
      return pairShape(shapeOf(ty.first), shapeOf(ty.second));
    case "data":
    case "g1":
    case "g2":
    case "ml":
      return gen({ k: "stop" });
  }
};
