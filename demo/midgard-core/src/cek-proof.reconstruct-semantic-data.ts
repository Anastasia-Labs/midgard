import { Data as LucidData } from "@lucid-evolution/lucid";

import { MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES } from "./cek-proof.encode-midgard-cek-term-node.js";
import { type SemanticDataValue } from "./cek-proof.program-material-task.js";
import {
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "./cek-semantic.js";

export type SemanticDataReconstructionSource = {
  readonly dataNodes: ReadonlyMap<string, MidgardCekDataNode>;
  readonly dataLists: ReadonlyMap<string, MidgardCekDataListNode>;
  readonly dataPairs: ReadonlyMap<string, MidgardCekDataPairNode>;
  readonly materializeBlob: (
    root: Uint8Array,
    maximumByteLength: bigint,
    fieldName: string,
  ) => Buffer;
};

const rootKey = (root: Uint8Array): string => Buffer.from(root).toString("hex");

const EMPTY_LIST_KEY = rootKey(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT);
const EMPTY_PAIR_KEY = rootKey(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT);
const MAX_LEAF_BYTES = BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES);

/**
 * Whether `bytes` starts with an integer head (major 0 or 1) or a bignum tag
 * (2 or 3, in any head width). Lucid decodes anything else to a non-integer
 * or fails, so refusing it first keeps the accept set while keeping nested
 * CBOR away from Lucid's recursive decoder. (Where Lucid itself would fail,
 * the error now names the leaf instead of carrying Lucid's message.)
 */
const startsWithIntegerHead = (bytes: Uint8Array): boolean => {
  const initial = bytes[0];
  if (initial === undefined) return false;
  const major = initial >> 5;
  if (major === 0 || major === 1) return true;
  if (major !== 6) return false;
  const info = initial & 0x1f;
  if (info < 24) return info === 2 || info === 3;
  const width = info === 24 ? 1 : info === 25 ? 2 : info === 26 ? 4 : 8;
  if (info > 27 || bytes.length < 1 + width) return false;
  let tag = 0n;
  for (let index = 1; index <= width; index += 1) {
    tag = (tag << 8n) | BigInt(bytes[index]!);
  }
  return tag === 2n || tag === 3n;
};

const decodeIntegerLeaf = (bytes: Buffer, invalid: string): bigint => {
  if (!startsWithIntegerHead(bytes)) {
    throw new Error(invalid);
  }
  const decoded = LucidData.from(bytes.toString("hex"));
  if (typeof decoded !== "bigint") {
    throw new Error(invalid);
  }
  return decoded;
};

type ChainFrame = {
  readonly key: string;
  readonly kind: "list" | "map" | "constr";
  readonly constructor: bigint;
  cursor: string;
  remaining: bigint;
  readonly items: SemanticDataValue[];
  readonly entries: Map<SemanticDataValue, SemanticDataValue>;
  /** The map link whose key has been rebuilt and whose value is next. */
  pendingPair: MidgardCekDataPairNode | undefined;
  pendingKey: SemanticDataValue | undefined;
};

/**
 * Rebuilds semantic Data values from committed Data nodes, list links and
 * pair links, caching each rebuilt value by its root.
 *
 * The walk uses an explicit stack. It visits nodes and links in the order of
 * the recursive walk it replaced (each link checked, then its head, or its key
 * then value, rebuilt in full), so it fails with the same first error. Rebuilt
 * values are shared by root: a map whose keys share a root collapses in the
 * JS `Map`, and the caller's root check then refuses it, as before.
 */
export const makeSemanticDataReconstructor = (
  source: SemanticDataReconstructionSource,
): ((root: Uint8Array) => SemanticDataValue) => {
  const reconstructed = new Map<string, SemanticDataValue>();
  const inProgress = new Set<string>();

  const openFrame = (
    stack: ChainFrame[],
    key: string,
    kind: ChainFrame["kind"],
    chainRoot: Uint8Array,
    count: bigint,
    constructor = 0n,
  ): void => {
    inProgress.add(key);
    stack.push({
      key,
      kind,
      constructor,
      cursor: rootKey(chainRoot),
      remaining: count,
      items: [],
      entries: new Map(),
      pendingPair: undefined,
      pendingKey: undefined,
    });
  };

  /** A leaf or cached value, or `undefined` after opening a frame. */
  const start = (
    stack: ChainFrame[],
    root: Uint8Array,
  ): SemanticDataValue | undefined => {
    const key = rootKey(root);
    const cached = reconstructed.get(key);
    if (cached !== undefined) return cached;
    const node = source.dataNodes.get(key);
    if (node === undefined) {
      throw new Error("CEK semantic Data node is missing");
    }
    if (inProgress.has(key)) {
      throw new Error("CEK semantic Data node contains itself");
    }
    if (node.kind === "integer") {
      const bytes = source.materializeBlob(
        node.cborRoot,
        MAX_LEAF_BYTES,
        "CEK semantic integer",
      );
      const value = decodeIntegerLeaf(
        bytes,
        "CEK semantic integer leaf is invalid",
      );
      reconstructed.set(key, value);
      return value;
    }
    if (node.kind === "bytes") {
      const value = source
        .materializeBlob(node.bytesRoot, MAX_LEAF_BYTES, "CEK semantic bytes")
        .toString("hex");
      reconstructed.set(key, value);
      return value;
    }
    if (node.kind === "list") {
      openFrame(stack, key, "list", node.itemsRoot, node.itemsCount);
      return undefined;
    }
    if (node.kind === "map") {
      openFrame(stack, key, "map", node.entriesRoot, node.entriesCount);
      return undefined;
    }
    const constructor =
      node.kind === "constrLarge"
        ? decodeIntegerLeaf(
            source.materializeBlob(
              node.constructorCborRoot,
              MAX_LEAF_BYTES,
              "CEK semantic constructor",
            ),
            "CEK semantic constructor index is invalid",
          )
        : node.constructor;
    if (constructor < 0n) {
      throw new Error("CEK semantic constructor index must be non-negative");
    }
    openFrame(
      stack,
      key,
      "constr",
      node.fieldsRoot,
      node.fieldsCount,
      constructor,
    );
    return undefined;
  };

  const deliver = (frame: ChainFrame, value: SemanticDataValue): void => {
    if (frame.kind !== "map") {
      frame.items.push(value);
    } else if (frame.pendingKey === undefined) {
      frame.pendingKey = value;
    } else {
      frame.entries.set(frame.pendingKey, value);
      frame.pendingKey = undefined;
      frame.pendingPair = undefined;
    }
  };

  /** The next root to rebuild for `frame`, or `undefined` when it is done. */
  const nextChild = (frame: ChainFrame): Uint8Array | undefined => {
    if (frame.pendingPair !== undefined) {
      return frame.pendingPair.value;
    }
    if (frame.remaining <= 0n) {
      const emptyKey = frame.kind === "map" ? EMPTY_PAIR_KEY : EMPTY_LIST_KEY;
      if (frame.cursor !== emptyKey) {
        throw new Error(
          `CEK semantic Data ${frame.kind === "map" ? "map" : "list"} has a non-empty tail`,
        );
      }
      return undefined;
    }
    if (frame.kind === "map") {
      const link = source.dataPairs.get(frame.cursor);
      if (link === undefined || link.length !== frame.remaining) {
        throw new Error("CEK semantic Data map cannot be reconstructed");
      }
      frame.pendingPair = link;
      frame.cursor = rootKey(link.tail);
      frame.remaining -= 1n;
      return link.key;
    }
    const link = source.dataLists.get(frame.cursor);
    if (link === undefined || link.length !== frame.remaining) {
      throw new Error("CEK semantic Data list cannot be reconstructed");
    }
    frame.cursor = rootKey(link.tail);
    frame.remaining -= 1n;
    return link.head;
  };

  const finish = (frame: ChainFrame): SemanticDataValue => {
    if (frame.kind === "list") return frame.items;
    if (frame.kind === "map") return frame.entries;
    return {
      kind: "constr",
      constructor: frame.constructor,
      fields: frame.items,
    };
  };

  return (root) => {
    // A walk that threw leaves its open nodes behind; they are not open now.
    inProgress.clear();
    const stack: ChainFrame[] = [];
    let result = start(stack, root);
    for (;;) {
      const top = stack[stack.length - 1];
      if (top === undefined) {
        return result!;
      }
      if (result !== undefined) {
        deliver(top, result);
      }
      const child = nextChild(top);
      if (child !== undefined) {
        result = start(stack, child);
        continue;
      }
      stack.pop();
      inProgress.delete(top.key);
      result = finish(top);
      reconstructed.set(top.key, result);
    }
  };
};
