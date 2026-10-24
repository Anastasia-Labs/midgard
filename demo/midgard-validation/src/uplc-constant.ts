/**
 * UPLC constant types and the constant term.
 *
 * Derived from @harmoniclabs/uplc 2.0.7 (Copyright Harmonic Laboratories,
 * Apache License 2.0, https://github.com/HarmonicLabs/uplc). Changes: the
 * type tags are numbered as Cardano's Flat encoding numbers them (upstream
 * swaps array and Value), they are a frozen object rather than an enum, and a
 * constant checks its type but not the shape of its value, so the constant
 * class does not depend on the identity of the Data classes.
 */
import type { Data } from "./uplc-data.js";
import { UPLCTermTag } from "./uplc-term-tag.js";

/**
 * Constant type tags, numbered as in Cardano's Flat encoding (`DefaultUni`).
 * The committed constant type CBOR (`9f09ff` for a G1 element) spells these
 * numbers.
 */
export const ConstTyTag = Object.freeze({
  int: 0,
  byteStr: 1,
  str: 2,
  unit: 3,
  bool: 4,
  list: 5,
  pair: 6,
  data: 8,
  bls12_381_G1_element: 9,
  bls12_381_G2_element: 10,
  bls12_381_MlResult: 11,
  array: 12,
  value: 13,
} as const);
export type ConstTyTag = (typeof ConstTyTag)[keyof typeof ConstTyTag];

/**
 * A constant type in prefix order: `list` is followed by its element type,
 * `pair` by its two element types (the Flat type application tag 7 is not
 * spelled).
 */
export type ConstType = [ConstTyTag, ...ConstTyTag[]];

/** A pair constant's value. */
export interface Pair<First, Second> {
  fst: First;
  snd: Second;
}

/** A constant's value; BLS values are whatever the evaluator holds. */
export type ConstValue =
  | bigint
  | Uint8Array
  | string
  | undefined
  | boolean
  | readonly ConstValue[]
  | Pair<ConstValue, ConstValue>
  | Data
  | object;

const TAGS: ReadonlySet<number> = new Set(Object.values(ConstTyTag));

/** How many type arguments each tag takes. */
const typeArity = (tag: number): number =>
  tag === ConstTyTag.pair
    ? 2
    : tag === ConstTyTag.list || tag === ConstTyTag.array
      ? 1
      : 0;

/** Whether `type` spells exactly one complete constant type. */
export const isWellFormedConstType = (type: unknown): type is ConstType => {
  if (!Array.isArray(type) || type.length === 0) return false;
  let open = 1;
  for (let index = 0; index < type.length; index += 1) {
    const tag: unknown = type[index];
    if (typeof tag !== "number" || !TAGS.has(tag) || open === 0) return false;
    open += typeArity(tag) - 1;
  }
  return open === 0;
};

/** Whether two constant types spell the same tags. */
export const constTypeEq = (
  left: readonly ConstTyTag[],
  right: readonly ConstTyTag[],
): boolean =>
  left.length === right.length && left.every((tag, i) => tag === right[i]);

const wellFormed = (type: readonly ConstTyTag[], use: string): void => {
  if (!isWellFormedConstType(type))
    throw new Error(`${use} needs a well-formed constant type`);
};

/** Builders for well-formed constant types. */
export const constT = Object.freeze({
  int: Object.freeze([ConstTyTag.int]) as unknown as ConstType,
  byteStr: Object.freeze([ConstTyTag.byteStr]) as unknown as ConstType,
  str: Object.freeze([ConstTyTag.str]) as unknown as ConstType,
  unit: Object.freeze([ConstTyTag.unit]) as unknown as ConstType,
  bool: Object.freeze([ConstTyTag.bool]) as unknown as ConstType,
  data: Object.freeze([ConstTyTag.data]) as unknown as ConstType,
  bls12_381_G1_element: Object.freeze([
    ConstTyTag.bls12_381_G1_element,
  ]) as unknown as ConstType,
  bls12_381_G2_element: Object.freeze([
    ConstTyTag.bls12_381_G2_element,
  ]) as unknown as ConstType,
  bls12_381_MlResult: Object.freeze([
    ConstTyTag.bls12_381_MlResult,
  ]) as unknown as ConstType,
  listOf: (element: readonly ConstTyTag[]): ConstType => {
    wellFormed(element, "constT.listOf");
    return Object.freeze([ConstTyTag.list, ...element]) as unknown as ConstType;
  },
  pairOf: (
    first: readonly ConstTyTag[],
    second: readonly ConstTyTag[],
  ): ConstType => {
    wellFormed(first, "constT.pairOf");
    wellFormed(second, "constT.pairOf");
    return Object.freeze([
      ConstTyTag.pair,
      ...first,
      ...second,
    ]) as unknown as ConstType;
  },
});

/** The length of the complete type that starts at `start`. */
const typeLengthAt = (type: readonly ConstTyTag[], start: number): number => {
  let open = 1;
  let index = start;
  while (open > 0) {
    open += typeArity(type[index]!) - 1;
    index += 1;
  }
  return index - start;
};

/** The two element types of a well-formed pair type. */
export const constPairTypeArguments = (
  type: readonly ConstTyTag[],
): readonly [ConstType, ConstType] => {
  if (type[0] !== ConstTyTag.pair || !isWellFormedConstType(type))
    throw new Error("not a well-formed pair type");
  const firstLength = typeLengthAt(type, 1);
  return [
    type.slice(1, 1 + firstLength) as ConstType,
    type.slice(1 + firstLength) as ConstType,
  ];
};

/** A constant term: a type and a value of it. */
export class UPLCConst {
  readonly tag = UPLCTermTag.Const;
  type: ConstType;
  value: ConstValue;

  constructor(type: readonly ConstTyTag[], value?: ConstValue) {
    wellFormed(type, "a UPLC constant");
    let coerced = value;
    if (type.length === 1 && type[0] === ConstTyTag.int)
      coerced = BigInt(value as bigint | number);
    else if (
      type.length === 2 &&
      type[0] === ConstTyTag.list &&
      type[1] === ConstTyTag.int
    )
      coerced = (value as readonly (bigint | number)[]).map((n) => BigInt(n));
    else if (type[0] === ConstTyTag.pair) {
      const [first, second] = constPairTypeArguments(type);
      const pair = value as Pair<ConstValue, ConstValue>;
      if (constTypeEq(first, constT.int))
        pair.fst = BigInt(pair.fst as bigint | number);
      if (constTypeEq(second, constT.int))
        pair.snd = BigInt(pair.snd as bigint | number);
    }
    this.type = type as ConstType;
    this.value = coerced;
  }

  static int(int: bigint | number): UPLCConst {
    return new UPLCConst(constT.int, BigInt(int));
  }

  static byteString(bytes: Uint8Array): UPLCConst {
    return new UPLCConst(constT.byteStr, bytes);
  }

  static str(text: string): UPLCConst {
    return new UPLCConst(constT.str, text);
  }

  static get unit(): UPLCConst {
    return new UPLCConst(constT.unit, undefined);
  }

  static bool(bool: boolean): UPLCConst {
    return new UPLCConst(constT.bool, bool);
  }

  static listOf(
    element: readonly ConstTyTag[],
  ): (values: readonly ConstValue[]) => UPLCConst {
    return (values) => new UPLCConst(constT.listOf(element), values);
  }

  static pairOf(
    first: readonly ConstTyTag[],
    second: readonly ConstTyTag[],
  ): (fst: ConstValue, snd: ConstValue) => UPLCConst {
    return (fst, snd) =>
      new UPLCConst(constT.pairOf(first, second), { fst, snd });
  }

  static data(data: Data): UPLCConst {
    return new UPLCConst(constT.data, data);
  }
}
