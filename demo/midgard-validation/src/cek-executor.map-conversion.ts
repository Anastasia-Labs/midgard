import {
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
} from "@al-ft/midgard-core";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.js";
import { type MidgardCekConstantType } from "./cek-constant.js";
import { type MidgardCekBuiltinBudget } from "./cek-cost.js";
import {
  buildMidgardCekDataConstrNode,
  buildMidgardCekDataListLink,
  buildMidgardCekDataListNode,
  buildMidgardCekDataMapNode,
  buildMidgardCekDataPairLink,
  type MidgardCekDataLinkSummary,
  type MidgardCekDataNodeSummary,
  midgardCekDataSequenceSummary,
} from "./cek-data-tree.js";
import {
  type Bytes,
  rootHex,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import {
  spliceMidgardCekMapFromPairs,
  spliceMidgardCekPairsFromMap,
} from "./cek-executor.semantic-bytes.js";
import { type MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";
import { type MidgardCekMapConversionControl } from "./cek-machine.js";

const DATA: MidgardCekConstantType = Object.freeze({ kind: "data" });
const LIST_PAIR_DATA: MidgardCekConstantType = Object.freeze({
  kind: "list",
  element: Object.freeze({ kind: "pair", first: DATA, second: DATA }),
});

const EMPTY_SEQUENCE_ROOT = new Uint8Array(0);

type Entry = {
  readonly key: MidgardCekDataNodeSummary;
  readonly value: MidgardCekDataNodeSummary;
};

/** The two fields of a canonical pair node, as their summaries. */
const pairEntry = (store: MidgardCekSemanticStore, pairRoot: Bytes): Entry => {
  const pair = store.node(pairRoot);
  if (
    pair.kind !== "constrSmall" ||
    pair.constructor !== 0n ||
    pair.fieldsCount !== 2n
  ) {
    throw new Error("mapData requires canonical Data pairs");
  }
  const first = store.link(pair.fieldsRoot);
  const second = store.link(first.tail);
  return {
    key: {
      root: first.head,
      cborLength: first.headCborLength,
      memory: first.headMemory,
    },
    value: {
      root: second.head,
      cborLength: second.headCborLength,
      memory: second.headMemory,
    },
  };
};

/** Builds the canonical pair node `(key, value)` and returns its summary. */
const addPairNode = (
  store: MidgardCekSemanticStore,
  entry: Entry,
): MidgardCekDataNodeSummary => {
  const second = buildMidgardCekDataListLink(
    entry.value,
    store.items(EMPTY_SEQUENCE_ROOT, 0n),
  );
  const secondRoot = store.addListLink(second);
  const first = buildMidgardCekDataListLink(
    entry.key,
    midgardCekDataSequenceSummary(secondRoot, second),
  );
  const firstRoot = store.addListLink(first);
  const node = buildMidgardCekDataConstrNode(
    0n,
    midgardCekDataSequenceSummary(firstRoot, first),
    store.addBlob,
  );
  return {
    root: store.addDataNode(node),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

/**
 * The result of `mapData` (tag 38) or `unMapData` (tag 43) over the source
 * payload at `sourceRoot`. The conversion re-links the source's entries;
 * every key and value node is shared with the source.
 */
export const buildMidgardCekMapConversionResult = (
  store: MidgardCekSemanticStore,
  tag: 38n | 43n,
  sourceRoot: Bytes,
): {
  readonly result: MidgardCekDirectValueWitness;
  readonly root: Bytes;
  readonly sourceNode: MidgardCekDataNode;
  readonly resultNode: MidgardCekDataNode;
} => {
  const sourceNode = store.node(sourceRoot);
  const entries: Entry[] = [];
  let dataRoot: Bytes;
  let type: MidgardCekConstantType;
  let splice: (bytes: Buffer) => Buffer;
  if (tag === 38n) {
    if (sourceNode.kind !== "list") {
      throw new Error("mapData requires a list payload");
    }
    let cursor = sourceNode.itemsRoot;
    for (let index = 0n; index < sourceNode.itemsCount; index += 1n) {
      const link = store.link(cursor);
      entries.push(pairEntry(store, link.head));
      cursor = link.tail;
    }
    let tail: MidgardCekDataLinkSummary = store.entries(
      EMPTY_SEQUENCE_ROOT,
      0n,
    );
    for (let index = entries.length - 1; index >= 0; index -= 1) {
      const entry = entries[index]!;
      const link = buildMidgardCekDataPairLink(entry.key, entry.value, tail);
      tail = midgardCekDataSequenceSummary(store.addPairLink(link), link);
    }
    dataRoot = store.addDataNode(buildMidgardCekDataMapNode(tail));
    type = DATA;
    const count = sourceNode.itemsCount;
    splice = (bytes) => spliceMidgardCekMapFromPairs(bytes, count);
  } else {
    if (sourceNode.kind !== "map") {
      throw new Error("unMapData requires a map Data payload");
    }
    let cursor = sourceNode.entriesRoot;
    for (let index = 0n; index < sourceNode.entriesCount; index += 1n) {
      const link = store.pairLink(cursor);
      entries.push({
        key: {
          root: link.key,
          cborLength: link.keyCborLength,
          memory: link.keyMemory,
        },
        value: {
          root: link.value,
          cborLength: link.valueCborLength,
          memory: link.valueMemory,
        },
      });
      cursor = link.tail;
    }
    let tail: MidgardCekDataLinkSummary = store.items(EMPTY_SEQUENCE_ROOT, 0n);
    for (let index = entries.length - 1; index >= 0; index -= 1) {
      const link = buildMidgardCekDataListLink(
        addPairNode(store, entries[index]!),
        tail,
      );
      tail = midgardCekDataSequenceSummary(store.addListLink(link), link);
    }
    dataRoot = store.addDataNode(buildMidgardCekDataListNode(tail));
    type = LIST_PAIR_DATA;
    const count = sourceNode.entriesCount;
    splice = (bytes) => spliceMidgardCekPairsFromMap(bytes, count);
  }
  const added = store.addConstant(type, dataRoot, () => {
    const bytes = store.cachedBytes(sourceRoot);
    return bytes === undefined ? undefined : splice(bytes);
  });
  return {
    result: added.result,
    root: added.root,
    sourceNode,
    resultNode: store.node(dataRoot),
  };
};

const mapHeaderLength = (count: bigint): bigint =>
  count < 24n ? 1n : count <= 0xffn ? 2n : count <= 0xffffn ? 3n : 5n;

/** The first control of a map conversion between two top nodes. */
export const midgardCekMapConversionControl = (
  tag: 38n | 43n,
  resultRoot: Bytes,
  sourceNode: MidgardCekDataNode,
  resultNode: MidgardCekDataNode,
  budget: MidgardCekBuiltinBudget,
): MidgardCekMapConversionControl => {
  const listSide = (node: MidgardCekDataNode) =>
    node.kind === "list"
      ? {
          root: node.itemsRoot,
          length: node.itemsCount,
          payload: node.cborLength - (node.itemsCount === 0n ? 1n : 2n),
          memory: node.memory - 4n,
        }
      : null;
  const mapSide = (node: MidgardCekDataNode) =>
    node.kind === "map"
      ? {
          root: node.entriesRoot,
          length: node.entriesCount,
          payload: node.cborLength - mapHeaderLength(node.entriesCount),
          memory: node.memory - 4n,
        }
      : null;
  const source = tag === 38n ? listSide(sourceNode) : mapSide(sourceNode);
  const destination = tag === 38n ? mapSide(resultNode) : listSide(resultNode);
  if (
    source === null ||
    destination === null ||
    source.length !== destination.length
  ) {
    throw new Error("CEK map conversion has incompatible semantic roots");
  }
  return Object.freeze({
    tag,
    resultRoot: Buffer.from(resultRoot),
    sourceRoot: Buffer.from(source.root),
    sourceRemaining: source.length,
    sourcePayloadCborLength: source.payload,
    sourceMemory: source.memory,
    destinationRoot: Buffer.from(destination.root),
    destinationRemaining: destination.length,
    destinationPayloadCborLength: destination.payload,
    destinationMemory: destination.memory,
    budgetCpu: budget.cpu,
    budgetMemory: budget.memory,
  });
};

/** The control after one entry of a map conversion has moved. */
export const advanceMidgardCekMapConversionControl = (
  control: MidgardCekMapConversionControl,
  sourcePayload: bigint,
  sourceMemory: bigint,
  sourceTail: Bytes,
  destinationPayload: bigint,
  destinationMemory: bigint,
  destinationTail: Bytes,
): MidgardCekMapConversionControl =>
  Object.freeze({
    ...control,
    sourceRoot: Buffer.from(sourceTail),
    sourceRemaining: control.sourceRemaining - 1n,
    sourcePayloadCborLength: control.sourcePayloadCborLength - sourcePayload,
    sourceMemory: control.sourceMemory - sourceMemory,
    destinationRoot: Buffer.from(destinationTail),
    destinationRemaining: control.destinationRemaining - 1n,
    destinationPayloadCborLength:
      control.destinationPayloadCborLength - destinationPayload,
    destinationMemory: control.destinationMemory - destinationMemory,
  });

/** The pair node one map-conversion micro-step moves, with its fields. */
export const midgardCekMapPairMaterial = (
  store: MidgardCekSemanticStore,
  pairRoot: Bytes,
): {
  readonly pair: MidgardCekDataNode;
  readonly first: MidgardCekDataListNode;
  readonly second: MidgardCekDataListNode;
  readonly key: MidgardCekDataNode;
  readonly value: MidgardCekDataNode;
} => {
  const pair = store.dataNodes.get(rootHex(pairRoot));
  if (
    pair?.kind !== "constrSmall" ||
    pair.constructor !== 0n ||
    pair.fieldsCount !== 2n
  ) {
    throw new Error("CEK map conversion pair wrapper is malformed");
  }
  const first = store.dataLists.get(rootHex(pair.fieldsRoot));
  const second =
    first === undefined ? undefined : store.dataLists.get(rootHex(first.tail));
  const key =
    first === undefined ? undefined : store.dataNodes.get(rootHex(first.head));
  const value =
    second === undefined
      ? undefined
      : store.dataNodes.get(rootHex(second.head));
  if (
    first === undefined ||
    second === undefined ||
    key === undefined ||
    value === undefined
  ) {
    throw new Error("CEK map conversion pair material is incomplete");
  }
  return { pair, first, second, key, value };
};
