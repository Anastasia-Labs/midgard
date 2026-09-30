/**
 * Deep one-path Data values and CEK program material for the depth tests,
 * built with loops so that the fixtures themselves never recurse.
 */
import {
  commitMidgardCekBlob,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
} from "../src/cek-proof.js";
import {
  encodeMidgardCekDataListNode,
  encodeMidgardCekDataNode,
  encodeMidgardCekDataPairNode,
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  hashMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
} from "../src/cek-semantic.js";
import { type Hash32 } from "../src/codec/hash.js";

export type SemanticChainShape = "list" | "map" | "constr";

/** A semantic Data value one container per level around the integer 0. */
export const deepSemanticValue = (
  shape: SemanticChainShape,
  depth: number,
): unknown => {
  let value: unknown = 0n;
  for (let level = 0; level < depth; level += 1) {
    value =
      shape === "list"
        ? [value]
        : shape === "map"
          ? { kind: "map", entries: [[0n, value]] }
          : { kind: "constr", constructor: 0n, fields: [value] };
  }
  return value;
};

/**
 * The canonical semantic encoding of {@link deepSemanticValue}: indefinite
 * lists, definite maps, compact constructor tags.
 */
export const deepSemanticCbor = (
  shape: SemanticChainShape,
  depth: number,
): Buffer => {
  const open =
    shape === "list"
      ? [0x9f]
      : shape === "map"
        ? [0xa1, 0x00]
        : [0xd8, 0x79, 0x9f];
  const close = shape === "map" ? [] : [0xff];
  const out: number[] = [];
  for (let level = 0; level < depth; level += 1) {
    for (const byte of open) out.push(byte);
  }
  out.push(0x00);
  for (let level = 0; level < depth; level += 1) {
    for (const byte of close) out.push(byte);
  }
  return Buffer.from(out);
};

type Summary = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

/**
 * Program material for one `data` constant holding
 * {@link deepSemanticValue}(shape, depth): the constant term, its value node,
 * the type and integer blobs, and one Data node plus one list or pair link
 * per level.
 */
export const deepDataConstantProgramMaterial = (
  shape: SemanticChainShape,
  depth: number,
): {
  readonly envelope: MidgardCekProgramEnvelope;
  readonly material: readonly MidgardCekProgramMaterialEntry[];
  readonly payloadCbor: Buffer;
} => {
  const material: MidgardCekProgramMaterialEntry[] = [];
  const addBlob = (bytes: Buffer): Hash32 => {
    const blob = commitMidgardCekBlob(bytes);
    for (const [rootHex, node] of blob.nodes.entries()) {
      material.push({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(rootHex, "hex") as Hash32,
        preimage: node.preimage,
      });
    }
    return blob.root;
  };
  const addNode = (node: MidgardCekDataNode): Summary => {
    const root = hashMidgardCekDataNode(node);
    material.push({
      kind: "dataNode",
      root,
      preimage: encodeMidgardCekDataNode(node),
    });
    return { root, cborLength: node.cborLength, memory: node.memory };
  };
  const addSingleLink = (head: Summary): Summary => {
    const link: MidgardCekDataListNode = {
      head: head.root,
      headCborLength: head.cborLength,
      headMemory: head.memory,
      tail: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
      length: 1n,
      payloadCborLength: head.cborLength,
      memory: head.memory,
    };
    const root = hashMidgardCekDataListNode(link);
    material.push({
      kind: "dataList",
      root,
      preimage: encodeMidgardCekDataListNode(link),
    });
    return { root, cborLength: link.payloadCborLength, memory: link.memory };
  };

  const typeRoot = addBlob(Buffer.from("9f08ff", "hex"));
  const zero = addNode({
    kind: "integer",
    cborRoot: addBlob(Buffer.from([0x00])),
    cborLength: 1n,
    memory: 5n,
  });
  let current = zero;
  for (let level = 0; level < depth; level += 1) {
    if (shape === "map") {
      const pair = {
        key: zero.root,
        keyCborLength: zero.cborLength,
        keyMemory: zero.memory,
        value: current.root,
        valueCborLength: current.cborLength,
        valueMemory: current.memory,
        tail: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
        length: 1n,
        payloadCborLength: zero.cborLength + current.cborLength,
        memory: zero.memory + current.memory,
      };
      const entriesRoot = hashMidgardCekDataPairNode(pair);
      material.push({
        kind: "dataPair",
        root: entriesRoot,
        preimage: encodeMidgardCekDataPairNode(pair),
      });
      current = addNode({
        kind: "map",
        entriesCount: 1n,
        entriesRoot,
        cborLength: midgardCekDataMapCborLength(1n, pair.payloadCborLength),
        memory: 4n + pair.memory,
      });
      continue;
    }
    const link = addSingleLink(current);
    current = addNode(
      shape === "list"
        ? {
            kind: "list",
            itemsCount: 1n,
            itemsRoot: link.root,
            cborLength: midgardCekDataListCborLength(1n, link.cborLength),
            memory: 4n + link.memory,
          }
        : {
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 1n,
            fieldsRoot: link.root,
            cborLength: midgardCekDataConstrCborLength(0n, 1n, link.cborLength),
            memory: 4n + link.memory,
          },
    );
  }

  const valueNode = {
    kind: "constant",
    typeRoot,
    payloadRoot: current.root,
    payloadLength: current.cborLength,
    semanticRoot: current.root,
    memory: current.memory,
  } as const;
  const valueRoot = hashMidgardCekValueNode(valueNode);
  material.push({
    kind: "value",
    root: valueRoot,
    preimage: encodeMidgardCekValueNode(valueNode),
  });
  const termNode = { kind: "constant", value: valueRoot } as const;
  const termRoot = hashMidgardCekTermNode(termNode);
  material.push({
    kind: "term",
    root: termRoot,
    preimage: encodeMidgardCekTermNode(termNode),
  });
  return {
    envelope: {
      uplcVersion: [1n, 1n, 0n],
      termRoot,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    },
    material,
    payloadCbor: deepSemanticCbor(shape, depth),
  };
};
