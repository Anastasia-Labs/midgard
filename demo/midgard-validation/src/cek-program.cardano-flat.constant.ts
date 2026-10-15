/**
 * Flat constants as the Cardano ledger's decoder reads them (plutus-core
 * 1.71): the type tags (`decodeKindedUniFlat`, FlatInstances.hs:129-135, and
 * `withDecodedUni`, Default/Universe.hs:1098-1122), then the value
 * (`decodeDefaultUniValue`, Default/Universe.hs:168-187).
 */
import { type Data } from "@harmoniclabs/plutus-data";
import {
  type ConstType,
  ConstTyTag,
  ErrorUPLC,
  UPLCConst,
  type UPLCTerm,
} from "@harmoniclabs/uplc";

import type { CardanoFlatReader } from "./cek-program.cardano-flat.reader.js";
import { plutusDataFirstItemFromCborIterative } from "./plutus-data-iterative.decode.js";

/** `mbHeader` at protocol version 11 (Versions.hs:397-401). */
const MAX_CONSTANT_TYPE_TAGS = 32;
/** `maxKeyLen` (Value.hs:87-88). */
const MAX_VALUE_KEY_BYTES = 32;
/** `Quantity` bounds: a signed 128-bit integer (Value.hs:157-160). */
const MIN_QUANTITY = -(1n << 127n);
const MAX_QUANTITY = (1n << 127n) - 1n;

/** A star-kinded constant type, in the order its value is read. */
type ValueType =
  | {
      readonly kind:
        | "integer"
        | "bytestring"
        | "string"
        | "unit"
        | "bool"
        | "data"
        | "value";
    }
  | { readonly kind: "bls"; readonly tag: ConstTyTag }
  | { readonly kind: "list" | "array"; readonly element: ValueType }
  | {
      readonly kind: "pair";
      readonly first: ValueType;
      readonly second: ValueType;
    };

/** A type being built: a type constructor and the arguments applied so far. */
interface TypeNode {
  readonly tag: number;
  readonly args: ValueType[];
}

/** Arity of each wire type tag; tags 14 and 15 are unknown. */
const TYPE_ARITY: readonly number[] = [
  0, 0, 0, 0, 0, 1, 2, -1, 0, 0, 0, 0, 1, 0,
];
const LEAF_KINDS = new Map<number, ValueType>([
  [0, { kind: "integer" }],
  [1, { kind: "bytestring" }],
  [2, { kind: "string" }],
  [3, { kind: "unit" }],
  [4, { kind: "bool" }],
  [8, { kind: "data" }],
  [9, { kind: "bls", tag: ConstTyTag.bls12_381_G1_element }],
  [10, { kind: "bls", tag: ConstTyTag.bls12_381_G2_element }],
  [11, { kind: "bls", tag: ConstTyTag.bls12_381_MlResult }],
  [13, { kind: "value" }],
]);

const remaining = (node: TypeNode): number =>
  TYPE_ARITY[node.tag]! - node.args.length;

const saturate = (node: TypeNode): ValueType => {
  if (node.tag === 5) return { kind: "list", element: node.args[0]! };
  if (node.tag === 12) return { kind: "array", element: node.args[0]! };
  if (node.tag === 6)
    return { kind: "pair", first: node.args[0]!, second: node.args[1]! };
  return LEAF_KINDS.get(node.tag)!;
};

/**
 * The type a tag list spells, or undefined where `decodeKindedUni`
 * (Universe/Core.hs:479-482) fails: an unknown tag, an application whose
 * argument is not of the kind the function expects (`withApplicable`,
 * Universe/Core.hs:596-616), a missing tag, or a tag left over. Also undefined
 * for a type that is not star (`checkStar`). Parsed with an explicit stack.
 */
const parseType = (tags: readonly number[]): ValueType | undefined => {
  // Each pending application holds its function once that is parsed.
  const pending: (TypeNode | undefined)[] = [];
  let index = 0;
  for (;;) {
    const tag = tags[index++];
    if (tag === undefined) return undefined;
    if (tag === 7) {
      pending.push(undefined);
      continue;
    }
    if (tag > 13) return undefined;
    let node: TypeNode = { tag, args: [] };
    for (;;) {
      if (pending.length === 0) {
        if (index !== tags.length || remaining(node) !== 0) return undefined;
        return saturate(node);
      }
      const fn = pending[pending.length - 1];
      if (fn === undefined) {
        pending[pending.length - 1] = node;
        break;
      }
      if (remaining(fn) < 1 || remaining(node) !== 0) return undefined;
      pending.pop();
      node = { tag: fn.tag, args: [...fn.args, saturate(node)] };
    }
  }
};

/** The harmonic `ConstType` of a type inside the profile. */
const constType = (type: ValueType): ConstType => {
  switch (type.kind) {
    case "integer":
      return [ConstTyTag.int];
    case "bytestring":
      return [ConstTyTag.byteStr];
    case "string":
      return [ConstTyTag.str];
    case "unit":
      return [ConstTyTag.unit];
    case "bool":
      return [ConstTyTag.bool];
    case "data":
      return [ConstTyTag.data];
    case "list":
      return [ConstTyTag.list, ...constType(type.element)];
    case "pair":
      return [
        ConstTyTag.pair,
        ...constType(type.first),
        ...constType(type.second),
      ];
    case "bls":
      // Only as the element type of an empty list.
      return [type.tag];
    case "array":
    case "value":
      throw new Error(`no harmonic constant type for ${type.kind}`);
  }
};

/** Whether a type holds an array or a Value, which the profile excludes. */
const outsideProfile = (type: ValueType): boolean => {
  switch (type.kind) {
    case "array":
    case "value":
      return true;
    case "list":
      return outsideProfile(type.element);
    case "pair":
      return outsideProfile(type.first) || outsideProfile(type.second);
    case "integer":
    case "bytestring":
    case "string":
    case "unit":
    case "bool":
    case "data":
    case "bls":
      return false;
  }
};

const UTF8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

const compareBytes = (left: Uint8Array, right: Uint8Array): number => {
  const shared = Math.min(left.length, right.length);
  for (let index = 0; index < shared; index += 1) {
    const difference = left[index]! - right[index]!;
    if (difference !== 0) return difference;
  }
  return left.length - right.length;
};

/** `K`'s Flat decode (Value.hs:103-109): a byte string of at most 32 bytes. */
const valueKey = (reader: CardanoFlatReader): Uint8Array => {
  const start = reader.bitOffset;
  const key = reader.byteString();
  if (key.length > MAX_VALUE_KEY_BYTES)
    reader.refuse("invalidValue", "Value key over 32 bytes", start);
  return key;
};

/**
 * `Value`'s Flat decode (Value.hs:229-238) and `buildValueWith` (Value.hs:
 * 692-757). Its value has no harmonic form; the profile refuses it later.
 */
const skipLedgerValue = (reader: CardanoFlatReader): void => {
  const start = reader.bitOffset;
  const currencies: { key: Uint8Array; tokens: Uint8Array[] }[] = [];
  let zeroQuantity = false;
  while (reader.bit()) {
    const key = valueKey(reader);
    const tokens: Uint8Array[] = [];
    while (reader.bit()) {
      tokens.push(valueKey(reader));
      const quantityStart = reader.bitOffset;
      const quantity = reader.integer();
      if (quantity < MIN_QUANTITY || quantity > MAX_QUANTITY)
        reader.refuse(
          "invalidValue",
          "quantity outside signed 128 bits",
          quantityStart,
        );
      if (quantity === 0n) zeroQuantity = true;
    }
    currencies.push({ key, tokens });
  }
  const ascending = (keys: readonly Uint8Array[]): boolean =>
    keys.every(
      (key, index) => index === 0 || compareBytes(keys[index - 1]!, key) < 0,
    );
  if (
    zeroQuantity ||
    !ascending(currencies.map(({ key }) => key)) ||
    currencies.some(({ tokens }) => tokens.length === 0 || !ascending(tokens))
  )
    reader.refuse(
      "invalidValue",
      "Value keys not strictly ascending, an empty inner map, or a zero quantity",
      start,
    );
};

/**
 * Reads a value of the type. Recursion follows the type only, which is at
 * most 32 tags deep; list elements are read in a loop.
 */
const readValue = (reader: CardanoFlatReader, type: ValueType): unknown => {
  const start = reader.bitOffset;
  switch (type.kind) {
    case "integer":
      return reader.integer();
    case "bytestring":
      return reader.byteString();
    case "string": {
      const bytes = reader.byteString();
      try {
        return UTF8.decode(bytes);
      } catch {
        return reader.refuse("invalidUtf8", "string is not UTF-8", start);
      }
    }
    case "unit":
      return undefined;
    case "bool":
      return reader.bit();
    case "data": {
      // Cardano reads the first CBOR item and ignores any bytes after it
      // (`deserialiseOrFail`, serialise 0.2.6.1 Codec/Serialise.hs:134-143).
      // The constant is that item's value, so its witness and the term root
      // commit the value's canonical encoding, not these bytes.
      const bytes = reader.byteString();
      let data: Data;
      try {
        data = plutusDataFirstItemFromCborIterative(bytes);
      } catch (error) {
        return reader.refuse(
          "invalidData",
          error instanceof Error ? error.message : String(error),
          start,
        );
      }
      return data;
    }
    case "bls":
      return reader.refuse(
        "constantNotFlatDecodable",
        "BLS12-381 values have no Flat decoding",
      );
    case "value":
      skipLedgerValue(reader);
      return undefined;
    case "list":
    case "array": {
      const elements: unknown[] = [];
      while (reader.bit()) elements.push(readValue(reader, type.element));
      return elements;
    }
    case "pair": {
      const fst = readValue(reader, type.first);
      const snd = readValue(reader, type.second);
      return { fst, snd };
    }
  }
};

/** What a constant decodes to: the term, or a profile refusal to report. */
export interface DecodedConstant {
  readonly term: UPLCTerm;
  readonly outsideProfile?: string;
}

/**
 * Reads a constant term's type and value (UntypedPlutusCore Flat.hs, tag 4):
 * the tag list, the type (`decodeKindedUniFlat`), `checkStar`, the size bound
 * (`checkConstant`, SerialisedScript.hs:207-214), then the value. A constant
 * Cardano decodes but the profile cannot represent comes back as an error
 * term with `outsideProfile` set, so the caller can still find any later
 * Cardano refusal.
 */
export const readConstant = (reader: CardanoFlatReader): DecodedConstant => {
  const start = reader.bitOffset;
  const tags: number[] = [];
  while (reader.bit()) tags.push(reader.bits(4));
  const type = parseType(tags);
  if (type === undefined)
    reader.refuse(
      "badConstantType",
      `type tags [${tags.join(",")}] do not spell a star type`,
      start,
    );
  if (tags.length > MAX_CONSTANT_TYPE_TAGS)
    reader.refuse(
      "constantTypeTooLarge",
      `constant type of ${tags.length} tags`,
      start,
    );
  const value = readValue(reader, type);
  if (outsideProfile(type))
    return {
      term: new ErrorUPLC(),
      outsideProfile: "array or Value constant",
    };
  return { term: new UPLCConst(constType(type), value as never) };
};
