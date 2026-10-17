import {
  commitMidgardCekBlob,
  encodeMidgardCekDataListNode,
  encodeMidgardCekDataNode,
  encodeMidgardCekDataPairNode,
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  hashMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataBytesMemory,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import {
  encodeMidgardCekPlutusData,
  midgardCekIntegerMemorySize,
} from "./cek-constant.js";
import { isPlutusDataMap } from "./plutus-data-narrowing.js";

type HashedMaterial<Node> = {
  readonly node: Node;
  readonly preimage: Buffer;
};

export type MidgardCekDataTreeCommitment = {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
  readonly dataNodes: ReadonlyMap<string, HashedMaterial<MidgardCekDataNode>>;
  readonly listNodes: ReadonlyMap<
    string,
    HashedMaterial<MidgardCekDataListNode>
  >;
  readonly pairNodes: ReadonlyMap<
    string,
    HashedMaterial<MidgardCekDataPairNode>
  >;
  readonly blobNodes: ReadonlyMap<
    string,
    {
      readonly kind: "chunk" | "branch";
      readonly preimage: Buffer;
    }
  >;
};

type DataSummary = {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

/**
 * Per-value summaries keyed by `Data` object identity. Passing the same memo
 * to repeated commitments of values that share subtrees makes each later
 * commitment cost only its new nodes.
 */
export type MidgardCekDataSummaryMemo = Map<Data, DataSummary>;

type ListSummary = {
  readonly root: Uint8Array;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

type ListWork = {
  summary: ListSummary;
};

type DataWork =
  | { readonly kind: "visit"; readonly data: Data }
  | {
      readonly kind: "finishData";
      readonly data: Data;
      readonly children: ListWork;
    }
  | {
      readonly kind: "finishListItem";
      readonly item: Data;
      readonly list: ListWork;
    }
  | {
      readonly kind: "finishPairItem";
      readonly key: Data;
      readonly value: Data;
      readonly list: ListWork;
    };

/** Cardano CBOR of an integer leaf or large constructor index. */
export const encodeMidgardCekDataTreeInteger = (value: bigint): Buffer =>
  encodeMidgardCekPlutusData(new DataI(value));

/** Commits leaf or constructor-index bytes as a blob and returns its root. */
export type MidgardCekDataBlobSink = (bytes: Uint8Array) => Uint8Array;

/** The summary a parent node records for one child Data node. */
export type MidgardCekDataNodeSummary = DataSummary;

/** The summary a parent node records for its list or map entry sequence. */
export type MidgardCekDataLinkSummary = ListSummary;

/** List link `head :: tail`. */
export const buildMidgardCekDataListLink = (
  head: DataSummary,
  tail: ListSummary,
): MidgardCekDataListNode => ({
  head: head.root,
  headCborLength: head.cborLength,
  headMemory: head.memory,
  tail: tail.root,
  length: tail.length + 1n,
  payloadCborLength: head.cborLength + tail.payloadCborLength,
  memory: head.memory + tail.memory,
});

/** Map entry link `(key, value) :: tail`. */
export const buildMidgardCekDataPairLink = (
  key: DataSummary,
  value: DataSummary,
  tail: ListSummary,
): MidgardCekDataPairNode => ({
  key: key.root,
  keyCborLength: key.cborLength,
  keyMemory: key.memory,
  value: value.root,
  valueCborLength: value.cborLength,
  valueMemory: value.memory,
  tail: tail.root,
  length: tail.length + 1n,
  payloadCborLength: key.cborLength + value.cborLength + tail.payloadCborLength,
  memory: key.memory + value.memory + tail.memory,
});

/** The sequence summary a list or pair link stands for. */
export const midgardCekDataSequenceSummary = (
  root: Uint8Array,
  link: MidgardCekDataListNode | MidgardCekDataPairNode,
): ListSummary => ({
  root,
  length: link.length,
  payloadCborLength: link.payloadCborLength,
  memory: link.memory,
});

/** Constructor node over an already committed fields sequence. */
export const buildMidgardCekDataConstrNode = (
  constructor: bigint,
  fields: ListSummary,
  addBlob: MidgardCekDataBlobSink,
): MidgardCekDataNode => {
  const cborLength = midgardCekDataConstrCborLength(
    constructor,
    fields.length,
    fields.payloadCborLength,
  );
  const memory = 4n + fields.memory;
  if (constructor <= 127n) {
    return {
      kind: "constrSmall",
      constructor,
      fieldsCount: fields.length,
      fieldsRoot: fields.root,
      cborLength,
      memory,
    };
  }
  const constructorCbor = encodeMidgardCekDataTreeInteger(constructor);
  return {
    kind: "constrLarge",
    constructorCborRoot: addBlob(constructorCbor),
    constructorCborLength: BigInt(constructorCbor.length),
    constructorMemory: 4n + midgardCekIntegerMemorySize(constructor),
    fieldsCount: fields.length,
    fieldsRoot: fields.root,
    cborLength,
    memory,
  };
};

/** Map node over an already committed entry sequence. */
export const buildMidgardCekDataMapNode = (
  entries: ListSummary,
): MidgardCekDataNode => ({
  kind: "map",
  entriesCount: entries.length,
  entriesRoot: entries.root,
  cborLength: midgardCekDataMapCborLength(
    entries.length,
    entries.payloadCborLength,
  ),
  memory: 4n + entries.memory,
});

/** List node over an already committed item sequence. */
export const buildMidgardCekDataListNode = (
  items: ListSummary,
): MidgardCekDataNode => ({
  kind: "list",
  itemsCount: items.length,
  itemsRoot: items.root,
  cborLength: midgardCekDataListCborLength(
    items.length,
    items.payloadCborLength,
  ),
  memory: 4n + items.memory,
});

export const buildMidgardCekDataIntegerNode = (
  value: bigint,
  addBlob: MidgardCekDataBlobSink,
): MidgardCekDataNode => {
  const cbor = encodeMidgardCekDataTreeInteger(value);
  return {
    kind: "integer",
    cborRoot: addBlob(cbor),
    cborLength: BigInt(cbor.length),
    memory: 4n + midgardCekIntegerMemorySize(value),
  };
};

export const buildMidgardCekDataBytesNode = (
  bytes: Uint8Array,
  addBlob: MidgardCekDataBlobSink,
): MidgardCekDataNode => ({
  kind: "bytes",
  bytesRoot: addBlob(bytes),
  bytesLength: BigInt(bytes.length),
  cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
  memory: midgardCekDataBytesMemory(BigInt(bytes.length)),
});

const rootKey = (root: Uint8Array): string => Buffer.from(root).toString("hex");

const addExact = <Node>(
  entries: Map<string, HashedMaterial<Node>>,
  root: Uint8Array,
  material: HashedMaterial<Node>,
): void => {
  const key = rootKey(root);
  const existing = entries.get(key);
  if (existing !== undefined && !existing.preimage.equals(material.preimage)) {
    throw new Error("CEK semantic material hash collision");
  }
  entries.set(key, material);
};

/**
 * Builds the semantic commitment consumed by the incremental script-context
 * and CEK builtin proofs. No complete-value proof bound is imposed: leaf
 * bytes and large integer encodings use the canonical 4,095-byte CEK blob
 * chunks, while every structural node remains a small fixed preimage.
 */
export const commitMidgardCekDataTree = (
  value: Data,
  summaries: MidgardCekDataSummaryMemo = new Map(),
): MidgardCekDataTreeCommitment => {
  const dataNodes = new Map<string, HashedMaterial<MidgardCekDataNode>>();
  const listNodes = new Map<string, HashedMaterial<MidgardCekDataListNode>>();
  const pairNodes = new Map<string, HashedMaterial<MidgardCekDataPairNode>>();
  const blobNodes = new Map<
    string,
    {
      readonly kind: "chunk" | "branch";
      readonly preimage: Buffer;
    }
  >();

  const addBlob = (bytes: Uint8Array): Uint8Array => {
    const committed = commitMidgardCekBlob(bytes);
    for (const [key, node] of committed.nodes) {
      const existing = blobNodes.get(key);
      if (
        existing !== undefined &&
        (existing.kind !== node.kind ||
          !existing.preimage.equals(node.preimage))
      ) {
        throw new Error("CEK semantic blob material hash collision");
      }
      blobNodes.set(key, node);
    }
    return committed.root;
  };

  const operations: DataWork[] = [{ kind: "visit", data: value }];

  const emptyListWork = (root: Uint8Array): ListWork => ({
    summary: {
      root,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    },
  });

  const finishListItem = (item: Data, list: ListWork): void => {
    const head = summaries.get(item);
    if (head === undefined) {
      throw new Error("V1 semantic tree lost a list child summary");
    }
    const node = buildMidgardCekDataListLink(head, list.summary);
    const preimage = encodeMidgardCekDataListNode(node);
    const root = hashMidgardCekDataListNode(node);
    addExact(listNodes, root, { node, preimage });
    list.summary = midgardCekDataSequenceSummary(root, node);
  };

  const finishPairItem = (
    keyData: Data,
    valueData: Data,
    list: ListWork,
  ): void => {
    const key = summaries.get(keyData);
    const mapped = summaries.get(valueData);
    if (key === undefined || mapped === undefined) {
      throw new Error("V1 semantic tree lost a pair child summary");
    }
    const node = buildMidgardCekDataPairLink(key, mapped, list.summary);
    const preimage = encodeMidgardCekDataPairNode(node);
    const root = hashMidgardCekDataPairNode(node);
    addExact(pairNodes, root, { node, preimage });
    list.summary = midgardCekDataSequenceSummary(root, node);
  };

  const finishData = (data: Data, children: ListWork): void => {
    const sequence = children.summary;
    let node: MidgardCekDataNode;
    if (data instanceof DataConstr) {
      node = buildMidgardCekDataConstrNode(data.constr, sequence, addBlob);
    } else if (isPlutusDataMap(data)) {
      node = buildMidgardCekDataMapNode(sequence);
    } else if (data instanceof DataList) {
      node = buildMidgardCekDataListNode(sequence);
    } else {
      throw new Error("V1 semantic tree contains unknown Plutus Data");
    }

    const preimage = encodeMidgardCekDataNode(node);
    const root = hashMidgardCekDataNode(node);
    addExact(dataNodes, root, { node, preimage });
    summaries.set(data, {
      root,
      cborLength: node.cborLength,
      memory: node.memory,
    });
  };

  while (operations.length > 0) {
    const operation = operations.pop()!;
    if (operation.kind === "visit") {
      if (summaries.has(operation.data)) {
        continue;
      }
      if (operation.data instanceof DataConstr) {
        if (operation.data.constr < 0n) {
          throw new Error("Plutus Data constructor must be non-negative");
        }
        const children = emptyListWork(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT);
        operations.push({
          kind: "finishData",
          data: operation.data,
          children,
        });
        // Push in reverse execution order to preserve the recursive fold:
        // each right-to-left child is finalized before its list node.
        for (let index = 0; index < operation.data.fields.length; index += 1) {
          const item = operation.data.fields[index]!;
          operations.push({
            kind: "finishListItem",
            item,
            list: children,
          });
          operations.push({ kind: "visit", data: item });
        }
        continue;
      }
      if (isPlutusDataMap(operation.data)) {
        const children = emptyListWork(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT);
        operations.push({
          kind: "finishData",
          data: operation.data,
          children,
        });
        // Keys, values, and pair nodes retain the original right-to-left order.
        for (let index = 0; index < operation.data.map.length; index += 1) {
          const item = operation.data.map[index]!;
          operations.push({
            kind: "finishPairItem",
            key: item.fst,
            value: item.snd,
            list: children,
          });
          operations.push({ kind: "visit", data: item.snd });
          operations.push({ kind: "visit", data: item.fst });
        }
        continue;
      }
      if (operation.data instanceof DataList) {
        const children = emptyListWork(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT);
        operations.push({
          kind: "finishData",
          data: operation.data,
          children,
        });
        for (let index = 0; index < operation.data.list.length; index += 1) {
          const item = operation.data.list[index]!;
          operations.push({
            kind: "finishListItem",
            item,
            list: children,
          });
          operations.push({ kind: "visit", data: item });
        }
        continue;
      }

      let node: MidgardCekDataNode;
      if (operation.data instanceof DataI) {
        node = buildMidgardCekDataIntegerNode(operation.data.int, addBlob);
      } else if (operation.data instanceof DataB) {
        node = buildMidgardCekDataBytesNode(operation.data.bytes, addBlob);
      } else {
        throw new Error("V1 semantic tree contains unknown Plutus Data");
      }

      const preimage = encodeMidgardCekDataNode(node);
      const root = hashMidgardCekDataNode(node);
      addExact(dataNodes, root, { node, preimage });
      summaries.set(operation.data, {
        root,
        cborLength: node.cborLength,
        memory: node.memory,
      });
      continue;
    }
    if (operation.kind === "finishListItem") {
      finishListItem(operation.item, operation.list);
      continue;
    }
    if (operation.kind === "finishPairItem") {
      finishPairItem(operation.key, operation.value, operation.list);
      continue;
    }
    finishData(operation.data, operation.children);
  }

  const committed = summaries.get(value);
  if (committed === undefined) {
    throw new Error("V1 semantic tree lost its root summary");
  }
  return Object.freeze({
    ...committed,
    dataNodes,
    listNodes,
    pairNodes,
    blobNodes,
  });
};
