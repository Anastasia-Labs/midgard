import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  type DataPair,
} from "@harmoniclabs/plutus-data";

import { midgardDataPair } from "./plutus-data-iterative.pair.js";

/**
 * CBOR -> harmonic `Data` without recursion.
 *
 * This reader accepts and rejects exactly the inputs harmonic 1.2.6
 * `dataFromCbor` (`Cbor.parse` from `@harmoniclabs/cbor` 1.6.6 followed by
 * `dataFromCborObj`) accepts and rejects, and builds the same `Data` value, but
 * keeps its own frame stack so any byte-cap-legal nesting depth decodes. The
 * behaviour it reproduces, pinned by the differential test:
 *
 * - bytes after the first complete item are ignored;
 * - non-minimal heads are accepted; additional information 28-30 is refused,
 *   and 31 (indefinite) is refused for majors 0, 1 and 7;
 * - text (major 3) and simple/float (major 7) items are refused wherever they
 *   appear, because the harmonic conversion has no Data node for them;
 * - indefinite bytes take chunks that are themselves major-2 items (definite
 *   or indefinite), concatenated;
 * - tag 2 / tag 3 directly over a bytes item is a bignum (empty bytes are
 *   refused); tags 121-127 and 1280-1400 over an array are constructors, and
 *   tag 102 over `[uint, array]` is a constructor while tag 1375 is
 *   constructor 102 over its array; every other tag, and every recognised tag
 *   over a non-array, is transparent;
 * - map entries keep their order, duplicate keys included;
 * - a byte string is a copy of the class harmonic returns: the input's class
 *   (a Buffer input gives Buffers) for a definite string or an indefinite one
 *   with a single chunk, a plain `Uint8Array` otherwise.
 */

const enum Kind {
  UInt = 0,
  NegInt = 1,
  Bytes = 2,
  Array = 4,
  Map = 5,
  Tag = 6,
}

type Item = {
  readonly kind: Kind;
  /** Undefined only for a bytes item before it is placed in a Data parent. */
  data: Data | undefined;
  /** Raw payload of a bytes item. */
  readonly raw?: Uint8Array;
  /** Kinds of the first two elements of an array item of length two. */
  readonly firstKinds?: readonly Kind[];
};

type ArrayFrame = {
  readonly type: "array";
  readonly length: bigint;
  readonly values: Data[];
  readonly kinds: Kind[];
};

type MapFrame = {
  readonly type: "map";
  readonly length: bigint;
  readonly pairs: DataPair<Data, Data>[];
  key: Data | undefined;
};

type TagFrame = { readonly type: "tag"; readonly tag: bigint };

type BytesFrame = { readonly type: "bytes"; readonly chunks: Uint8Array[] };

type Frame = ArrayFrame | MapFrame | TagFrame | BytesFrame;

const INDEFINITE = -1n;

/**
 * A copy of `bytes` of the same class (a Buffer stays a Buffer), as
 * harmonic's `Uint8Array.prototype.slice.call` produces.
 */
const copyOf = (bytes: Uint8Array): Uint8Array =>
  Uint8Array.prototype.slice.call(bytes);

const itemData = (item: Item): Data => {
  if (item.data === undefined) {
    item.data = new DataB(copyOf(item.raw!));
  }
  return item.data;
};

/**
 * Harmonic `concatUint8Array`: a lone chunk is copied with its class, and no
 * chunk or several chunks give a plain `Uint8Array`.
 */
const concatChunks = (chunks: readonly Uint8Array[]): Uint8Array => {
  if (chunks.length === 1) return copyOf(chunks[0]!);
  let length = 0;
  for (const chunk of chunks) length += chunk.length;
  const out = new Uint8Array(length);
  let at = 0;
  for (const chunk of chunks) {
    out.set(chunk, at);
    at += chunk.length;
  }
  return out;
};

/** Harmonic's `cborTagToConstrNumber`: a negative result means "ignore". */
const constrNumberOfTag = (tag: bigint): bigint => {
  if (tag < 0n) return tag;
  if (tag >= 121n && tag <= 127n) return tag - 121n;
  if (tag >= 1280n && tag <= 1400n) return tag - 1273n;
  if (tag === 102n) return tag;
  return -1n;
};

const bignumOf = (raw: Uint8Array): bigint => {
  if (raw.length === 0) {
    throw new SyntaxError("CBOR bignum has no magnitude bytes");
  }
  return BigInt(`0x${Buffer.from(raw).toString("hex")}`);
};

const arrayItem = (frame: ArrayFrame): Item => ({
  kind: Kind.Array,
  data: new DataList(frame.values),
  firstKinds: frame.values.length === 2 ? frame.kinds : undefined,
});

const mapItem = (frame: MapFrame): Item => ({
  kind: Kind.Map,
  data: new DataMap(frame.pairs),
});

const closeItem = (frame: Frame): Item => {
  switch (frame.type) {
    case "bytes":
      return {
        kind: Kind.Bytes,
        data: undefined,
        raw: concatChunks(frame.chunks),
      };
    case "array":
      return arrayItem(frame);
    case "map":
      return mapItem(frame);
    case "tag":
      throw new Error("a CBOR tag cannot close without its item");
  }
};

/** Whether `frame` is open to a CBOR break (0xff) at this point. */
const acceptsBreak = (frame: Frame | undefined): frame is Frame =>
  frame !== undefined &&
  (frame.type === "bytes" ||
    (frame.type === "array" && frame.length === INDEFINITE) ||
    (frame.type === "map" &&
      frame.length === INDEFINITE &&
      frame.key === undefined));

const completeTag = (tag: bigint, child: Item): Item => {
  if ((tag === 2n || tag === 3n) && child.kind === Kind.Bytes) {
    const magnitude = bignumOf(child.raw!);
    return tag === 2n
      ? { kind: Kind.UInt, data: new DataI(magnitude) }
      : { kind: Kind.NegInt, data: new DataI(-1n - magnitude) };
  }
  const constr = constrNumberOfTag(tag);
  if (constr < 0n || child.kind !== Kind.Array) {
    return { kind: Kind.Tag, data: itemData(child) };
  }
  const values = (child.data as DataList).list;
  if (constr === 102n && tag !== 1375n) {
    const kinds = child.firstKinds;
    if (
      values.length !== 2 ||
      kinds === undefined ||
      kinds[0] !== Kind.UInt ||
      kinds[1] !== Kind.Array
    ) {
      throw new Error("invalid fields for CBOR tag 102 Plutus Data");
    }
    return {
      kind: Kind.Tag,
      data: new DataConstr(
        (values[0] as DataI).int,
        (values[1] as DataList).list,
      ),
    };
  }
  return { kind: Kind.Tag, data: new DataConstr(constr, values) };
};

export const plutusDataFromCborIterative = (bytes: Uint8Array): Data => {
  let offset = 0;
  const byteAt = (at: number): number => {
    if (at >= bytes.length) {
      throw new RangeError("CBOR Plutus Data ends before its item does");
    }
    return bytes[at]!;
  };
  const take = (length: number): Uint8Array => {
    if (bytes.length < offset + length) {
      throw new RangeError("CBOR Plutus Data ends before its item does");
    }
    offset += length;
    return bytes.subarray(offset - length, offset);
  };
  const argument = (additional: number): bigint => {
    if (additional < 24) return BigInt(additional);
    if (additional === 31) return INDEFINITE;
    const width =
      additional === 24
        ? 1
        : additional === 25
          ? 2
          : additional === 26
            ? 4
            : additional === 27
              ? 8
              : 0;
    if (width === 0) {
      throw new Error("invalid CBOR length encoding in Plutus Data");
    }
    let value = 0n;
    for (const byte of take(width)) value = (value << 8n) | BigInt(byte);
    return value;
  };

  const stack: Frame[] = [];
  for (;;) {
    let item: Item | undefined;
    const top = stack.at(-1);
    if (acceptsBreak(top) && byteAt(offset) === 0xff) {
      offset += 1;
      stack.pop();
      item = closeItem(top);
    } else {
      const head = byteAt(offset);
      offset += 1;
      const major = head >> 5;
      if (major === 3 || major === 7) {
        throw new Error("invalid CBOR major type for Plutus Data");
      }
      const length = argument(head & 0x1f);
      if (length === INDEFINITE && major < 2) {
        throw new Error("unexpected indefinite length CBOR element");
      }
      if (top?.type === "bytes" && major !== 2) {
        throw new Error("indefinite CBOR bytes contain a non-bytes chunk");
      }
      switch (major) {
        case 0:
          item = { kind: Kind.UInt, data: new DataI(length) };
          break;
        case 1:
          item = { kind: Kind.NegInt, data: new DataI(-1n - length) };
          break;
        case 2:
          if (length === INDEFINITE) {
            stack.push({ type: "bytes", chunks: [] });
          } else {
            item = {
              kind: Kind.Bytes,
              data: undefined,
              raw: take(Number(length)),
            };
          }
          break;
        case 4:
          if (length > BigInt(bytes.length - offset)) {
            throw new RangeError("CBOR Plutus Data array exceeds its input");
          }
          stack.push({ type: "array", length, values: [], kinds: [] });
          break;
        case 5:
          if (length > BigInt(bytes.length - offset)) {
            throw new RangeError("CBOR Plutus Data map exceeds its input");
          }
          stack.push({ type: "map", length, pairs: [], key: undefined });
          break;
        default:
          stack.push({
            type: "tag",
            tag: length === INDEFINITE ? INDEFINITE : BigInt(Number(length)),
          });
      }
    }
    // Close every definite container that is now full, then deliver the item
    // to its parents until one of them needs more input.
    for (;;) {
      const parent = stack.at(-1);
      if (item === undefined) {
        if (
          parent !== undefined &&
          (parent.type === "array" || parent.type === "map") &&
          parent.length === 0n
        ) {
          stack.pop();
          item = closeItem(parent);
          continue;
        }
        break;
      }
      if (parent === undefined) return itemData(item);
      if (parent.type === "tag") {
        stack.pop();
        item = completeTag(parent.tag, item);
        continue;
      }
      if (parent.type === "bytes") {
        parent.chunks.push(item.raw!);
        item = undefined;
        break;
      }
      if (parent.type === "array") {
        if (parent.values.length < 2) parent.kinds.push(item.kind);
        parent.values.push(itemData(item));
        item = undefined;
        if (BigInt(parent.values.length) !== parent.length) break;
        stack.pop();
        item = arrayItem(parent);
        continue;
      }
      if (parent.key === undefined) {
        parent.key = itemData(item);
        item = undefined;
        break;
      }
      parent.pairs.push(midgardDataPair(parent.key, itemData(item)));
      parent.key = undefined;
      item = undefined;
      if (BigInt(parent.pairs.length) !== parent.length) break;
      stack.pop();
      item = mapItem(parent);
    }
  }
};
