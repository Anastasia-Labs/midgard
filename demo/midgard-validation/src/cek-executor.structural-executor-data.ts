import {
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
  type MidgardCekDecodedProgramBlob,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  type DataPair,
} from "@harmoniclabs/plutus-data";

import {
  commitMidgardCekDataTree,
  type MidgardCekDataSummaryMemo,
} from "./cek-data-tree.js";
import {
  type Bytes,
  rootHex,
  sameBytes,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { midgardDataPair } from "./plutus-data-iterative.pair.js";

/** The authenticated semantic material a structural executor has installed. */
export type MidgardCekSemanticDataMaterial = {
  readonly dataNodes: ReadonlyMap<string, MidgardCekDataNode>;
  readonly dataLists: ReadonlyMap<string, MidgardCekDataListNode>;
  readonly dataPairs: ReadonlyMap<string, MidgardCekDataPairNode>;
  readonly blobs: ReadonlyMap<string, MidgardCekDecodedProgramBlob>;
};

type BlobFrame = {
  readonly key: string;
  readonly byteLength: bigint;
  readonly right: Bytes;
  readonly parts: Buffer[];
};

/**
 * Reassembles an authenticated blob tree, left subtree before right. A node
 * that is its own ancestor is refused as cyclic; a subtree reached twice is
 * read once. The walk keeps its own stack, so any tree depth is read.
 */
export const readMidgardCekSemanticBlob = (
  blobs: MidgardCekSemanticDataMaterial["blobs"],
  root: Bytes,
): Buffer => {
  const done = new Map<string, Buffer>();
  const active = new Set<string>();
  const frames: BlobFrame[] = [];
  let request: Bytes | undefined = root;
  let result: Buffer | undefined;
  for (;;) {
    if (request !== undefined) {
      const key = rootHex(request);
      request = undefined;
      if (active.has(key)) {
        throw new Error("cyclic CEK semantic blob commitment");
      }
      const node = blobs.get(key);
      if (node === undefined) {
        throw new Error(`missing authenticated CEK blob ${key}`);
      }
      const read = done.get(key);
      if (read !== undefined) {
        result = read;
      } else if (node.kind === "chunk") {
        result = Buffer.from(node.bytes);
        done.set(key, result);
      } else {
        active.add(key);
        frames.push({
          key,
          byteLength: node.byteLength,
          right: node.right,
          parts: [],
        });
        request = node.left;
        continue;
      }
    }
    const frame = frames.at(-1);
    if (frame === undefined) return result!;
    frame.parts.push(result!);
    result = undefined;
    if (frame.parts.length === 1) {
      request = frame.right;
      continue;
    }
    const bytes = Buffer.concat(frame.parts);
    if (BigInt(bytes.length) !== frame.byteLength) {
      throw new Error("CEK semantic blob length does not match its root");
    }
    frames.pop();
    active.delete(frame.key);
    done.set(frame.key, bytes);
    result = bytes;
  }
};

type NodeFrame = {
  readonly key: string;
  readonly root: Bytes;
  readonly node: MidgardCekDataNode;
  /** Constructor of a constructor node; undefined for lists and maps. */
  readonly constructor: bigint | undefined;
  /** Whether the children are map entries (pair nodes) or list items. */
  readonly pairs: boolean;
  cursor: Buffer;
  remaining: bigint;
  /** Tail of the list or pair node whose children are being read. */
  tail: Buffer;
  /** Value root of the pair node whose key is being read. */
  pendingValue: Bytes | undefined;
  pendingKey: Data | undefined;
  readonly items: Data[];
  readonly entries: DataPair<Data, Data>[];
};

const childSequence = (
  node: MidgardCekDataNode,
): {
  readonly root: Bytes;
  readonly count: bigint;
  readonly pairs: boolean;
} => {
  switch (node.kind) {
    case "constrSmall":
    case "constrLarge":
      return { root: node.fieldsRoot, count: node.fieldsCount, pairs: false };
    case "list":
      return { root: node.itemsRoot, count: node.itemsCount, pairs: false };
    case "map":
      return { root: node.entriesRoot, count: node.entriesCount, pairs: true };
    case "integer":
    case "bytes":
      throw new Error("CEK Data leaf has no children");
  }
};

/**
 * Rebuilds the `Data` value an authenticated Data-node root commits to.
 *
 * Every node is checked in the order a depth-first reading meets it: its
 * lookup, its own leaf or constructor payload, then its children left to
 * right (each list or pair node looked up only after the previous child is
 * complete), the empty tail, and finally that the rebuilt value commits back
 * to the node's root, CBOR length and memory. The per-node commitment shares
 * one summary memo, so each node is summarised once. The walk keeps its own
 * stack, so any nesting depth is read.
 */
export const readMidgardCekSemanticData = (
  material: MidgardCekSemanticDataMaterial,
  root: Bytes,
): Data => {
  const built = new Map<string, Data>();
  const summaries: MidgardCekDataSummaryMemo = new Map();
  const frames: NodeFrame[] = [];
  const active = new Set<string>();
  let request: Bytes | undefined = root;
  let result: Data | undefined;

  const finish = (
    key: string,
    nodeRoot: Bytes,
    node: MidgardCekDataNode,
    value: Data,
  ): Data => {
    const canonical = commitMidgardCekDataTree(value, summaries);
    if (
      !sameBytes(canonical.root, nodeRoot) ||
      canonical.cborLength !== node.cborLength ||
      canonical.memory !== node.memory
    ) {
      throw new Error("CEK semantic Data material is not self-consistent");
    }
    built.set(key, value);
    return value;
  };

  const blob = (blobRoot: Bytes): Buffer =>
    readMidgardCekSemanticBlob(material.blobs, blobRoot);

  const open = (key: string, nodeRoot: Bytes): Data | undefined => {
    const node = material.dataNodes.get(key);
    if (node === undefined) {
      throw new Error(`missing authenticated CEK Data node ${key}`);
    }
    let constructor: bigint | undefined;
    switch (node.kind) {
      case "integer": {
        const integer = plutusDataFromCborIterative(blob(node.cborRoot));
        if (!(integer instanceof DataI)) {
          throw new Error("CEK integer Data leaf has a non-integer payload");
        }
        return finish(key, nodeRoot, node, integer);
      }
      case "bytes": {
        const bytes = blob(node.bytesRoot);
        if (BigInt(bytes.length) !== node.bytesLength) {
          throw new Error("CEK bytes Data leaf has the wrong length");
        }
        return finish(key, nodeRoot, node, new DataB(bytes));
      }
      case "constrSmall":
        constructor = node.constructor;
        break;
      case "constrLarge": {
        const decoded = plutusDataFromCborIterative(
          blob(node.constructorCborRoot),
        );
        if (!(decoded instanceof DataI) || decoded.int <= 127n) {
          throw new Error("CEK large Data constructor is not canonical");
        }
        constructor = decoded.int;
        break;
      }
      case "list":
      case "map":
        break;
    }
    const children = childSequence(node);
    active.add(key);
    frames.push({
      key,
      root: nodeRoot,
      node,
      constructor,
      pairs: children.pairs,
      cursor: Buffer.from(children.root),
      remaining: children.count,
      tail: Buffer.alloc(0),
      pendingValue: undefined,
      pendingKey: undefined,
      items: [],
      entries: [],
    });
    return undefined;
  };

  for (;;) {
    if (request !== undefined) {
      const key = rootHex(request);
      const nodeRoot = request;
      request = undefined;
      const known = built.get(key);
      if (known !== undefined) {
        result = known;
      } else {
        if (active.has(key)) {
          throw new Error("cyclic CEK semantic Data commitment");
        }
        result = open(key, nodeRoot);
      }
    }
    const frame = frames.at(-1);
    if (frame === undefined) return result!;
    if (result !== undefined) {
      if (frame.pairs && frame.pendingValue !== undefined) {
        frame.pendingKey = result;
        request = frame.pendingValue;
        frame.pendingValue = undefined;
        result = undefined;
        continue;
      }
      if (frame.pairs) {
        frame.entries.push(midgardDataPair(frame.pendingKey!, result));
        frame.pendingKey = undefined;
      } else {
        frame.items.push(result);
      }
      result = undefined;
      frame.cursor = frame.tail;
      frame.remaining -= 1n;
    }
    if (frame.remaining > 0n) {
      const key = rootHex(frame.cursor);
      if (frame.pairs) {
        const pair = material.dataPairs.get(key);
        if (pair === undefined || pair.length !== frame.remaining) {
          throw new Error(`missing authenticated CEK Data-pair node ${key}`);
        }
        frame.tail = Buffer.from(pair.tail);
        frame.pendingValue = pair.value;
        request = pair.key;
      } else {
        const item = material.dataLists.get(key);
        if (item === undefined || item.length !== frame.remaining) {
          throw new Error(`missing authenticated CEK Data-list node ${key}`);
        }
        frame.tail = Buffer.from(item.tail);
        request = item.head;
      }
      continue;
    }
    frames.pop();
    active.delete(frame.key);
    let value: Data;
    if (frame.pairs) {
      if (!sameBytes(frame.cursor, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)) {
        throw new Error("CEK Data-map commitment has a non-empty tail");
      }
      value = new DataMap(frame.entries);
    } else {
      if (!sameBytes(frame.cursor, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT)) {
        throw new Error("CEK Data-list commitment has a non-empty tail");
      }
      value =
        frame.constructor === undefined
          ? new DataList(frame.items)
          : new DataConstr(frame.constructor, frame.items);
    }
    result = finish(frame.key, frame.root, frame.node, value);
  }
};
