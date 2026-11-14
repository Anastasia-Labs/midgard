import {
  type CborNode,
  type CborRange,
  expectCborLength,
  parseCborNode,
  parseCborRange,
  readCborLength,
} from "./plutus-data-cbor.parse-cbor-node.js";

const parseArrayItemRanges = (
  bytes: Buffer,
  offset: number,
): { readonly items: readonly CborRange[]; readonly offset: number } => {
  const initial = bytes[offset];
  if (initial === undefined) {
    throw new Error("Unexpected end of CBOR input");
  }
  const major = initial >> 5;
  if (major !== 4) {
    throw new Error("Expected constructor fields to be a CBOR array");
  }

  const additional = initial & 0x1f;
  const length = readCborLength(bytes, offset + 1, additional);
  const items: CborRange[] = [];
  let cursor = length.offset;

  if (length.value === null) {
    while (bytes[cursor] !== 0xff) {
      const range = parseCborRange(bytes, cursor);
      items.push(range);
      cursor = range.end;
    }
    if (bytes[cursor] !== 0xff) {
      throw new Error("Unterminated indefinite CBOR array");
    }
    return { items, offset: cursor + 1 };
  }

  for (let i = 0n; i < length.value; i += 1n) {
    const range = parseCborRange(bytes, cursor);
    items.push(range);
    cursor = range.end;
  }
  return { items, offset: cursor };
};

export const constrFieldRanges = (
  bytes: Buffer,
): { readonly items: readonly CborRange[]; readonly offset: number } => {
  const initial = bytes[0];
  if (initial === undefined) {
    throw new Error("Unexpected empty PlutusData CBOR input");
  }
  if (initial >> 5 !== 6) {
    throw new Error("Expected constructor PlutusData CBOR");
  }

  const tag = readCborLength(bytes, 1, initial & 0x1f);
  const tagValue = expectCborLength(tag.value, "constructor tag");
  if (
    (121n <= tagValue && tagValue <= 127n) ||
    (1280n <= tagValue && tagValue <= 1400n)
  ) {
    return parseArrayItemRanges(bytes, tag.offset);
  }

  if (tagValue === 102n) {
    const constructorItems = parseArrayItemRanges(bytes, tag.offset);
    const fieldsRange = constructorItems.items[1];
    if (fieldsRange === undefined) {
      throw new Error("General constructor is missing its fields array");
    }
    return parseArrayItemRanges(
      bytes.subarray(fieldsRange.start, fieldsRange.end),
      0,
    );
  }

  throw new Error(`Unsupported PlutusData constructor tag ${tagValue}`);
};

export const encodeCborHeader = (
  major: number,
  value: bigint | null,
): Buffer => {
  if (value === null) {
    return Buffer.from([(major << 5) | 31]);
  }
  if (value < 24n) {
    return Buffer.from([(major << 5) | Number(value)]);
  }
  if (value <= 0xffn) {
    return Buffer.from([(major << 5) | 24, Number(value)]);
  }
  if (value <= 0xffffn) {
    const out = Buffer.alloc(3);
    out[0] = (major << 5) | 25;
    out.writeUInt16BE(Number(value), 1);
    return out;
  }
  if (value <= 0xffffffffn) {
    const out = Buffer.alloc(5);
    out[0] = (major << 5) | 26;
    out.writeUInt32BE(Number(value), 1);
    return out;
  }
  const out = Buffer.alloc(9);
  out[0] = (major << 5) | 27;
  out.writeBigUInt64BE(value, 1);
  return out;
};

/** Logical Data children exclude constructor framing and bignum magnitudes. */
const plutusDataChildren = function* (node: CborNode): Generator<CborNode> {
  if (node.kind === "array") yield* node.items;
  else if (node.kind === "map") {
    for (const [key, value] of node.entries) {
      yield key;
      yield value;
    }
  } else if (node.kind === "tag") {
    if (node.tag === 2n || node.tag === 3n) {
      if (node.value.kind !== "bytes")
        throw new Error("PlutusData bignum requires bytes");
      return;
    }
    if (node.tag === 102n) {
      if (
        node.value.kind !== "array" ||
        node.value.items.length !== 2 ||
        node.value.items[0]!.kind !== "uint" ||
        node.value.items[1]!.kind !== "array"
      )
        throw new Error("Invalid general PlutusData constructor");
      yield* node.value.items[1]!.items;
    } else if (
      (node.tag >= 121n && node.tag <= 127n) ||
      (node.tag >= 1280n && node.tag <= 1400n)
    ) {
      if (node.value.kind !== "array")
        throw new Error("PlutusData constructor requires fields");
      yield* node.value.items;
    } else throw new Error("Unsupported PlutusData tag");
  }
};

export const countPlutusDataNodes = (
  node: CborNode,
  maximum?: bigint,
): bigint => {
  const work: Iterator<CborNode>[] = [[node][Symbol.iterator]()];
  let count = 0n;
  while (work.length > 0) {
    const next = work[work.length - 1]!.next();
    if (next.done) {
      work.pop();
      continue;
    }
    ++count;
    if (maximum !== undefined && count > maximum)
      throw new Error("PlutusData exceeds the Data-node bound");
    work.push(plutusDataChildren(next.value));
  }
  return count;
};

export const parseCompletePlutusData = (cbor: string): CborNode => {
  if (!/^(?:[0-9a-fA-F]{2})+$/u.test(cbor))
    throw new Error("Expected complete PlutusData CBOR hex");
  const input = Buffer.from(cbor, "hex");
  const parsed = parseCborNode(input, 0);
  if (parsed.offset !== input.length)
    throw new Error("Unexpected trailing bytes in PlutusData CBOR");
  return parsed.node;
};

/** Count actual ordered map pairs, including repeated keys, without a JS Map
 * conversion. Constructors and lists count once; encoding wrappers do not. */
export const countPlutusDataCborNodes = (
  cbor: string,
  maximum: bigint,
): bigint => countPlutusDataNodes(parseCompletePlutusData(cbor), maximum);
