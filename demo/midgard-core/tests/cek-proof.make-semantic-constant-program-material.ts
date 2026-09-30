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
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataNode,
} from "../src/cek-semantic.js";
import type { Hash32 } from "../src/codec/hash.js";
import {
  encodeCanonicalSemanticBytes,
  encodeFixtureHeader,
  type FixtureData,
  type FixtureListSummary,
  type FixtureSummary,
  hex,
  isFixtureConstr,
} from "./cek-proof.make-nested-list-constant-program-material.js";

const encodeFixtureList = (values: readonly FixtureData[]): Buffer =>
  values.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([
        Buffer.from([0x9f]),
        ...values.map(encodeFixtureData),
        Buffer.from([0xff]),
      ]);

const encodeFixtureData = (value: FixtureData): Buffer => {
  if (typeof value === "bigint") {
    if (value >= 0n && value < 24n) return Buffer.from([Number(value)]);
    throw new Error("fixture integers are limited to small values");
  }
  if (typeof value === "string") {
    return encodeCanonicalSemanticBytes(Buffer.from(value, "hex"));
  }
  if (Array.isArray(value)) return encodeFixtureList(value);
  if (!isFixtureConstr(value)) throw new Error("unknown fixture Data");
  if (value.constructor > 6n) {
    throw new Error("fixture constructors are limited to small values");
  }
  return Buffer.concat([
    encodeFixtureHeader(6, 121n + value.constructor),
    encodeFixtureList(value.fields),
  ]);
};

export const makeSemanticConstantProgramMaterial = (
  typeTags: readonly number[],
  payload: FixtureData,
  memory: bigint,
): {
  readonly envelope: MidgardCekProgramEnvelope;
  readonly material: readonly MidgardCekProgramMaterialEntry[];
  readonly typeCbor: Buffer;
  readonly payloadCbor: Buffer;
} => {
  const byRoot = new Map<string, MidgardCekProgramMaterialEntry>();
  const addEntry = (entry: MidgardCekProgramMaterialEntry): void => {
    if (!byRoot.has(hex(entry.root))) byRoot.set(hex(entry.root), entry);
  };
  const addBlob = (bytes: Buffer): Hash32 => {
    const blob = commitMidgardCekBlob(bytes);
    for (const [rootHex, node] of blob.nodes.entries()) {
      addEntry({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(rootHex, "hex") as Hash32,
        preimage: node.preimage,
      });
    }
    return blob.root;
  };
  const commitList = (items: readonly FixtureData[]): FixtureListSummary => {
    let summary: FixtureListSummary = {
      root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    };
    for (let index = items.length - 1; index >= 0; index -= 1) {
      const head = commitData(items[index]!);
      const node = {
        head: head.root,
        headCborLength: head.cborLength,
        headMemory: head.memory,
        tail: summary.root,
        length: summary.length + 1n,
        payloadCborLength: head.cborLength + summary.payloadCborLength,
        memory: head.memory + summary.memory,
      };
      const root = hashMidgardCekDataListNode(node);
      addEntry({
        kind: "dataList",
        root,
        preimage: encodeMidgardCekDataListNode(node),
      });
      summary = {
        root,
        length: node.length,
        payloadCborLength: node.payloadCborLength,
        memory: node.memory,
      };
    }
    return summary;
  };
  function commitData(value: FixtureData): FixtureSummary {
    const cbor = encodeFixtureData(value);
    let node: MidgardCekDataNode;
    if (typeof value === "bigint") {
      node = {
        kind: "integer",
        cborRoot: addBlob(cbor),
        cborLength: BigInt(cbor.length),
        memory: 5n,
      };
    } else if (typeof value === "string") {
      const bytes = Buffer.from(value, "hex");
      node = {
        kind: "bytes",
        bytesRoot: addBlob(bytes),
        bytesLength: BigInt(bytes.length),
        cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
        memory: 4n + BigInt(Math.max(1, bytes.length)),
      };
    } else if (Array.isArray(value)) {
      const items = commitList(value);
      node = {
        kind: "list",
        itemsCount: items.length,
        itemsRoot: items.root,
        cborLength: midgardCekDataListCborLength(
          items.length,
          items.payloadCborLength,
        ),
        memory: 4n + items.memory,
      };
    } else {
      if (!isFixtureConstr(value)) throw new Error("unknown fixture Data");
      const fields = commitList(value.fields);
      node = {
        kind: "constrSmall",
        constructor: value.constructor,
        fieldsCount: fields.length,
        fieldsRoot: fields.root,
        cborLength: midgardCekDataConstrCborLength(
          value.constructor,
          fields.length,
          fields.payloadCborLength,
        ),
        memory: 4n + fields.memory,
      };
    }
    if (node.cborLength !== BigInt(cbor.length)) {
      throw new Error("fixture semantic Data CBOR summary is not exact");
    }
    const root = hashMidgardCekDataNode(node);
    addEntry({
      kind: "dataNode",
      root,
      preimage: encodeMidgardCekDataNode(node),
    });
    return { root, cborLength: node.cborLength, memory: node.memory };
  }
  const typeCbor = Buffer.from([0x9f, ...typeTags, 0xff]);
  const typeRoot = addBlob(typeCbor);
  const semantic = commitData(payload);
  const valueNode = {
    kind: "constant",
    typeRoot,
    payloadRoot: semantic.root,
    payloadLength: semantic.cborLength,
    semanticRoot: semantic.root,
    memory,
  } as const;
  const valueRoot = hashMidgardCekValueNode(valueNode);
  const termNode = { kind: "constant", value: valueRoot } as const;
  const termRoot = hashMidgardCekTermNode(termNode);
  addEntry({
    kind: "value",
    root: valueRoot,
    preimage: encodeMidgardCekValueNode(valueNode),
  });
  addEntry({
    kind: "term",
    root: termRoot,
    preimage: encodeMidgardCekTermNode(termNode),
  });
  const material = [...byRoot.values()];
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
    typeCbor,
    payloadCbor: encodeFixtureData(payload),
  };
};
