/**
 * Test-only builder of the committed Data material (Data nodes, list links,
 * pair links and blobs) that the semantic-Data reconstructor reads. It mirrors
 * the node shapes `commitSemanticData` hashes, records every node by its root,
 * and lets a test write maps as raw entry lists (duplicate keys allowed) and
 * override an integer leaf's CBOR.
 */
import { Data as LucidData } from "@lucid-evolution/lucid";

import { semanticIntegerMemory } from "../src/cek-proof.commit-semantic-data.js";
import { commitMidgardCekBlob } from "../src/cek-proof.encode-midgard-cek-continuation-frame.js";
import { type SemanticDataReconstructionSource } from "../src/cek-proof.reconstruct-semantic-data.js";
import {
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  hashMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "../src/cek-semantic.js";
import { type Hash32 } from "../src/codec/hash.js";

export type RawData =
  | bigint
  | string
  | { readonly kind: "intCbor"; readonly cborHex: string }
  | readonly RawData[]
  | { readonly kind: "map"; readonly entries: readonly [RawData, RawData][] }
  | {
      readonly kind: "constr";
      readonly constructor: bigint;
      readonly fields: readonly RawData[];
    };

export type SemanticMaterial = {
  readonly dataNodes: Map<string, MidgardCekDataNode>;
  readonly dataLists: Map<string, MidgardCekDataListNode>;
  readonly dataPairs: Map<string, MidgardCekDataPairNode>;
  readonly blobs: Map<string, Buffer>;
};

type Summary = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

type ChainSummary = Summary & { readonly length: bigint };

export const hexRoot = (root: Uint8Array): string =>
  Buffer.from(root).toString("hex");

export const emptySemanticMaterial = (): SemanticMaterial => ({
  dataNodes: new Map(),
  dataLists: new Map(),
  dataPairs: new Map(),
  blobs: new Map(),
});

export const materialSource = (
  material: SemanticMaterial,
): SemanticDataReconstructionSource => ({
  dataNodes: material.dataNodes,
  dataLists: material.dataLists,
  dataPairs: material.dataPairs,
  materializeBlob: (root, maximumByteLength, fieldName) => {
    const bytes = material.blobs.get(hexRoot(root));
    if (bytes === undefined) throw new Error(`${fieldName} blob is missing`);
    if (BigInt(bytes.length) > maximumByteLength) {
      throw new Error(`${fieldName} blob is too long`);
    }
    return Buffer.from(bytes);
  },
});

const addBlob = (material: SemanticMaterial, bytes: Buffer): Hash32 => {
  const root = commitMidgardCekBlob(bytes).root;
  material.blobs.set(hexRoot(root), Buffer.from(bytes));
  return root;
};

export const addDataNode = (
  material: SemanticMaterial,
  node: MidgardCekDataNode,
): Summary => {
  const root = hashMidgardCekDataNode(node);
  material.dataNodes.set(hexRoot(root), node);
  return { root, cborLength: node.cborLength, memory: node.memory };
};

export const addListLink = (
  material: SemanticMaterial,
  head: Summary,
  tail: ChainSummary,
): ChainSummary => {
  const node: MidgardCekDataListNode = {
    head: head.root,
    headCborLength: head.cborLength,
    headMemory: head.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength: head.cborLength + tail.cborLength,
    memory: head.memory + tail.memory,
  };
  const root = hashMidgardCekDataListNode(node);
  material.dataLists.set(hexRoot(root), node);
  return {
    root,
    length: node.length,
    cborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

export const addPairLink = (
  material: SemanticMaterial,
  key: Summary,
  value: Summary,
  tail: ChainSummary,
): ChainSummary => {
  const node: MidgardCekDataPairNode = {
    key: key.root,
    keyCborLength: key.cborLength,
    keyMemory: key.memory,
    value: value.root,
    valueCborLength: value.cborLength,
    valueMemory: value.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength: key.cborLength + value.cborLength + tail.cborLength,
    memory: key.memory + value.memory + tail.memory,
  };
  const root = hashMidgardCekDataPairNode(node);
  material.dataPairs.set(hexRoot(root), node);
  return {
    root,
    length: node.length,
    cborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

export const EMPTY_LIST: ChainSummary = {
  root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  length: 0n,
  cborLength: 0n,
  memory: 0n,
};

export const EMPTY_PAIRS: ChainSummary = {
  root: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  length: 0n,
  cborLength: 0n,
  memory: 0n,
};

export const addIntegerNode = (
  material: SemanticMaterial,
  value: bigint,
  cbor = Buffer.from(LucidData.to(value), "hex"),
): Summary =>
  addDataNode(material, {
    kind: "integer",
    cborRoot: addBlob(material, cbor),
    cborLength: BigInt(cbor.length),
    memory: 4n + semanticIntegerMemory(value),
  });

export const addListNode = (
  material: SemanticMaterial,
  items: ChainSummary,
): Summary =>
  addDataNode(material, {
    kind: "list",
    itemsCount: items.length,
    itemsRoot: items.root,
    cborLength: midgardCekDataListCborLength(items.length, items.cborLength),
    memory: 4n + items.memory,
  });

export const addMapNode = (
  material: SemanticMaterial,
  entries: ChainSummary,
): Summary =>
  addDataNode(material, {
    kind: "map",
    entriesCount: entries.length,
    entriesRoot: entries.root,
    cborLength: midgardCekDataMapCborLength(entries.length, entries.cborLength),
    memory: 4n + entries.memory,
  });

export const addConstrNode = (
  material: SemanticMaterial,
  constructor: bigint,
  fields: ChainSummary,
): Summary => {
  const cborLength = midgardCekDataConstrCborLength(
    constructor,
    fields.length,
    fields.cborLength,
  );
  if (constructor <= 127n) {
    return addDataNode(material, {
      kind: "constrSmall",
      constructor,
      fieldsCount: fields.length,
      fieldsRoot: fields.root,
      cborLength,
      memory: 4n + fields.memory,
    });
  }
  const constructorCbor = Buffer.from(LucidData.to(constructor), "hex");
  return addDataNode(material, {
    kind: "constrLarge",
    constructorCborRoot: addBlob(material, constructorCbor),
    constructorCborLength: BigInt(constructorCbor.length),
    constructorMemory: 4n + semanticIntegerMemory(constructor),
    fieldsCount: fields.length,
    fieldsRoot: fields.root,
    cborLength,
    memory: 4n + fields.memory,
  });
};

/** Commits `value` into `material` (recursively; for shallow test trees). */
export const addRawData = (
  material: SemanticMaterial,
  value: RawData,
): Summary => {
  if (typeof value === "bigint") return addIntegerNode(material, value);
  if (typeof value === "string") {
    const bytes = Buffer.from(value, "hex");
    return addDataNode(material, {
      kind: "bytes",
      bytesRoot: addBlob(material, bytes),
      bytesLength: BigInt(bytes.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
      memory: 4n + BigInt(Math.max(1, bytes.length)),
    });
  }
  const addList = (items: readonly RawData[]): ChainSummary => {
    let chain = EMPTY_LIST;
    for (let index = items.length - 1; index >= 0; index -= 1) {
      chain = addListLink(material, addRawData(material, items[index]!), chain);
    }
    return chain;
  };
  if (Array.isArray(value)) return addListNode(material, addList(value));
  const node = value as Exclude<RawData, bigint | string | readonly RawData[]>;
  if (node.kind === "intCbor") {
    const cbor = Buffer.from(node.cborHex, "hex");
    const decoded = LucidData.from(node.cborHex);
    return addIntegerNode(
      material,
      typeof decoded === "bigint" ? decoded : 0n,
      cbor,
    );
  }
  if (node.kind === "map") {
    let chain = EMPTY_PAIRS;
    for (let index = node.entries.length - 1; index >= 0; index -= 1) {
      const [key, mapped] = node.entries[index]!;
      const keySummary = addRawData(material, key);
      chain = addPairLink(
        material,
        keySummary,
        addRawData(material, mapped),
        chain,
      );
    }
    return addMapNode(material, chain);
  }
  return addConstrNode(material, node.constructor, addList(node.fields));
};
