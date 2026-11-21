import {
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "@al-ft/midgard-core";

import {
  encodeCardanoBytes,
  encodeCardanoInteger,
  encodeSmallCborArgument,
} from "./cek-constant.semantic-data.js";
import {
  type Bytes,
  rootHex,
} from "./cek-executor.build-midgard-cek-execution-graph.js";

/**
 * Exact Cardano Data CBOR for structural builtin results, derived from the
 * bytes of the arguments the result is built from. A result whose bytes are
 * revealed directly is at most one direct payload long, so each splice costs
 * at most the bytes of the step's own arguments and result; nothing here
 * re-encodes a value from its semantic tree when its bytes are already known.
 */

const OPEN_LIST = Buffer.from([0x9f]);
const EMPTY_LIST = Buffer.from([0x80]);
const EMPTY_MAP = Buffer.from([0xa0]);
const BREAK = Buffer.from([0xff]);
const PAIR_OPEN = Buffer.from([0xd8, 0x79, 0x9f]);
const LARGE_CONSTR_OPEN = Buffer.from([0xd8, 0x66, 0x82]);

/** The offset just past the one CBOR data item that starts at `start`. */
export const midgardCekCborItemEnd = (bytes: Buffer, start: number): number => {
  let offset = start;
  // Items still owed by each open container; -1 marks an indefinite one.
  const owed: number[] = [1];
  while (owed.length > 0) {
    const top = owed.length - 1;
    if (owed[top] === 0) {
      owed.pop();
      continue;
    }
    if (offset >= bytes.length) {
      throw new Error("CEK Data CBOR ends inside an item");
    }
    const initial = bytes[offset]!;
    offset += 1;
    if (initial === 0xff) {
      if (owed[top] !== -1) {
        throw new Error("CEK Data CBOR has an unexpected break");
      }
      owed.pop();
      continue;
    }
    if (owed[top]! > 0) owed[top] = owed[top]! - 1;
    const major = initial >> 5;
    const info = initial & 0x1f;
    let argument: number;
    if (info < 24) {
      argument = info;
    } else if (info === 24) {
      argument = bytes[offset]!;
      offset += 1;
    } else if (info === 25) {
      argument = bytes.readUInt16BE(offset);
      offset += 2;
    } else if (info === 26) {
      argument = bytes.readUInt32BE(offset);
      offset += 4;
    } else if (info === 27) {
      argument = Number(bytes.readBigUInt64BE(offset));
      offset += 8;
    } else if (info === 31 && major >= 2 && major <= 5) {
      argument = -1;
    } else {
      throw new Error("CEK Data CBOR has an unsupported item header");
    }
    switch (major) {
      case 0:
      case 1:
      case 7:
        break;
      case 2:
      case 3:
        if (argument === -1) owed.push(-1);
        else offset += argument;
        break;
      case 4:
        owed.push(argument);
        break;
      case 5:
        owed.push(argument === -1 ? -1 : argument * 2);
        break;
      case 6:
        owed.push(1);
        break;
    }
  }
  if (offset > bytes.length) {
    throw new Error("CEK Data CBOR ends inside an item");
  }
  return offset;
};

/** The item that starts at `start`. */
const itemAt = (bytes: Buffer, start: number): Buffer =>
  bytes.subarray(start, midgardCekCborItemEnd(bytes, start));

/** The constructor tag header for `constructor` (fields follow it). */
const constrHeader = (constructor: bigint): Buffer =>
  constructor <= 6n
    ? encodeSmallCborArgument(6, 121n + constructor)
    : constructor <= 127n
      ? encodeSmallCborArgument(6, 1280n + constructor - 7n)
      : Buffer.concat([LARGE_CONSTR_OPEN, encodeCardanoInteger(constructor)]);

/** The offset of a constructor's fields list inside its exact bytes. */
const constrFieldsOffset = (bytes: Buffer): number => {
  if (bytes[0] === 0xd8 && bytes[1] === 0x66) {
    return midgardCekCborItemEnd(bytes, 3);
  }
  if (bytes[0] === 0xd8) return 2;
  if (bytes[0] === 0xd9) return 3;
  throw new Error("CEK Data CBOR is not a constructor");
};

/** `headList` of a non-empty list's bytes. */
export const spliceMidgardCekListHead = (list: Buffer): Buffer =>
  itemAt(list, 1);

/** `tailList` of a non-empty list's bytes; `remaining` items stay. */
export const spliceMidgardCekListTail = (
  list: Buffer,
  remaining: bigint,
): Buffer =>
  remaining === 0n
    ? EMPTY_LIST
    : Buffer.concat([OPEN_LIST, list.subarray(midgardCekCborItemEnd(list, 1))]);

/** `mkCons item list`. */
export const spliceMidgardCekListCons = (item: Buffer, list: Buffer): Buffer =>
  list[0] === 0x80
    ? Buffer.concat([OPEN_LIST, item, BREAK])
    : Buffer.concat([OPEN_LIST, item, list.subarray(1)]);

/** The two fields of a canonical pair constructor's bytes. */
export const spliceMidgardCekPairFields = (
  pair: Buffer,
): readonly [Buffer, Buffer] => {
  const firstEnd = midgardCekCborItemEnd(pair, 3);
  return [pair.subarray(3, firstEnd), itemAt(pair, firstEnd)];
};

/** `constrData constructor fields`, from the fields list's bytes. */
export const spliceMidgardCekConstr = (
  constructor: bigint,
  fields: Buffer,
): Buffer => Buffer.concat([constrHeader(constructor), fields]);

/** `unConstrData` of a constructor's bytes. */
export const spliceMidgardCekUnConstr = (
  constr: Buffer,
  constructor: bigint,
): Buffer =>
  Buffer.concat([
    PAIR_OPEN,
    encodeCardanoInteger(constructor),
    constr.subarray(constrFieldsOffset(constr)),
    BREAK,
  ]);

/** `mkPairData first second`. */
export const spliceMidgardCekPair = (first: Buffer, second: Buffer): Buffer =>
  Buffer.concat([PAIR_OPEN, first, second, BREAK]);

/** `mapData` of the bytes of a list of `count` canonical pairs. */
export const spliceMidgardCekMapFromPairs = (
  list: Buffer,
  count: bigint,
): Buffer => {
  if (count === 0n) return EMPTY_MAP;
  const parts: Buffer[] = [encodeSmallCborArgument(5, count)];
  let offset = 1;
  for (let index = 0n; index < count; index += 1n) {
    const keyEnd = midgardCekCborItemEnd(list, offset + 3);
    const valueEnd = midgardCekCborItemEnd(list, keyEnd);
    parts.push(list.subarray(offset + 3, valueEnd));
    offset = valueEnd + 1;
  }
  return Buffer.concat(parts);
};

/** `unMapData` of the bytes of a map with `count` entries. */
export const spliceMidgardCekPairsFromMap = (
  map: Buffer,
  count: bigint,
): Buffer => {
  if (count === 0n) return EMPTY_LIST;
  const parts: Buffer[] = [OPEN_LIST];
  let offset = encodeSmallCborArgument(5, count).length;
  for (let index = 0n; index < count; index += 1n) {
    const keyEnd = midgardCekCborItemEnd(map, offset);
    const valueEnd = midgardCekCborItemEnd(map, keyEnd);
    parts.push(PAIR_OPEN, map.subarray(offset, valueEnd), BREAK);
    offset = valueEnd;
  }
  parts.push(BREAK);
  return Buffer.concat(parts);
};

/** The installed semantic nodes a materialization reads. */
export type MidgardCekSemanticByteSource = {
  readonly node: (root: Bytes) => MidgardCekDataNode;
  readonly list: (root: Bytes) => MidgardCekDataListNode;
  readonly pair: (root: Bytes) => MidgardCekDataPairNode;
  readonly blob: (root: Bytes) => Buffer;
  /** Exact bytes already known for a Data root. */
  readonly known: (key: string) => Buffer | undefined;
};

type ByteWork =
  | { readonly kind: "node"; readonly root: Bytes }
  | { readonly kind: "bytes"; readonly bytes: Buffer };

/**
 * Exact Data CBOR of an installed node, read from its semantic tree. Any
 * subtree whose bytes are already known is copied rather than re-read. The
 * walk keeps its own stack, so any nesting depth is encoded.
 */
export const materializeMidgardCekDataBytes = (
  source: MidgardCekSemanticByteSource,
  root: Bytes,
): Buffer => {
  const out: Buffer[] = [];
  const work: ByteWork[] = [{ kind: "node", root }];
  const pushItems = (itemsRoot: Bytes, count: bigint): void => {
    if (count === 0n) {
      out.push(EMPTY_LIST);
      return;
    }
    out.push(OPEN_LIST);
    const heads: Bytes[] = [];
    let cursor = itemsRoot;
    for (let index = 0n; index < count; index += 1n) {
      const link = source.list(cursor);
      heads.push(link.head);
      cursor = link.tail;
    }
    work.push({ kind: "bytes", bytes: BREAK });
    for (let index = heads.length - 1; index >= 0; index -= 1) {
      work.push({ kind: "node", root: heads[index]! });
    }
  };
  while (work.length > 0) {
    const next = work.pop()!;
    if (next.kind === "bytes") {
      out.push(next.bytes);
      continue;
    }
    const known = source.known(rootHex(next.root));
    if (known !== undefined) {
      out.push(known);
      continue;
    }
    const node = source.node(next.root);
    switch (node.kind) {
      case "integer":
        out.push(source.blob(node.cborRoot));
        break;
      case "bytes":
        out.push(encodeCardanoBytes(source.blob(node.bytesRoot)));
        break;
      case "constrSmall":
        out.push(constrHeader(node.constructor));
        pushItems(node.fieldsRoot, node.fieldsCount);
        break;
      case "constrLarge":
        out.push(LARGE_CONSTR_OPEN, source.blob(node.constructorCborRoot));
        pushItems(node.fieldsRoot, node.fieldsCount);
        break;
      case "list":
        pushItems(node.itemsRoot, node.itemsCount);
        break;
      case "map": {
        out.push(encodeSmallCborArgument(5, node.entriesCount));
        const entries: Bytes[] = [];
        let cursor = node.entriesRoot;
        for (let index = 0n; index < node.entriesCount; index += 1n) {
          const link = source.pair(cursor);
          entries.push(link.key, link.value);
          cursor = link.tail;
        }
        for (let index = entries.length - 1; index >= 0; index -= 1) {
          work.push({ kind: "node", root: entries[index]! });
        }
        break;
      }
    }
  }
  return Buffer.concat(out);
};
