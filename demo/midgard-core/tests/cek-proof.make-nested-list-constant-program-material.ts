import {
  commitMidgardCekBlob,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  hashMidgardCekProgramMaterialPreimage,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  type MidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialKind,
} from "../src/cek-proof.js";
import {
  encodeMidgardCekDataListNode,
  encodeMidgardCekDataNode,
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  midgardCekDataListCborLength,
} from "../src/cek-semantic.js";
import type { Hash32 } from "../src/codec/hash.js";

export const hash = (fill: number): Buffer => Buffer.alloc(32, fill);

export const hex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const programMaterialEntry = (
  kind: MidgardCekProgramMaterialKind,
  preimage: Buffer,
): MidgardCekProgramMaterialEntry => ({
  kind,
  root: hashMidgardCekProgramMaterialPreimage(kind, preimage),
  preimage,
});

export const makeUnaryProgramMaterial = (
  nodeCount: number,
): {
  readonly envelope: {
    readonly uplcVersion: readonly [1n, 1n, 0n];
    readonly termRoot: Hash32;
    readonly nodeCount: bigint;
    readonly materialByteLength: bigint;
  };
  readonly material: readonly MidgardCekProgramMaterialEntry[];
} => {
  const material: MidgardCekProgramMaterialEntry[] = [];
  const terminal = { kind: "error" } as const;
  let preimage = encodeMidgardCekTermNode(terminal);
  let root = hashMidgardCekTermNode(terminal);
  material.push({ kind: "term", root, preimage });
  for (let index = 1; index < nodeCount; index += 1) {
    const parent = { kind: "lambda", body: root } as const;
    preimage = encodeMidgardCekTermNode(parent);
    root = hashMidgardCekTermNode(parent);
    material.push({ kind: "term", root, preimage });
  }
  return {
    envelope: {
      uplcVersion: [1n, 1n, 0n],
      termRoot: root,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    },
    material,
  };
};

export const encodeCanonicalSemanticBytes = (bytes: Buffer): Buffer => {
  if (bytes.length <= 64) {
    const header =
      bytes.length < 24
        ? Buffer.from([0x40 + bytes.length])
        : Buffer.from([0x58, bytes.length]);
    return Buffer.concat([header, bytes]);
  }
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < bytes.length; offset += 64) {
    const chunk = bytes.subarray(offset, offset + 64);
    const header =
      chunk.length < 24
        ? Buffer.from([0x40 + chunk.length])
        : Buffer.from([0x58, chunk.length]);
    chunks.push(header, chunk);
  }
  chunks.push(Buffer.from([0xff]));
  return Buffer.concat(chunks);
};

export const makeBytesConstantProgramMaterial = (
  rawByteLength: number,
): {
  readonly envelope: {
    readonly uplcVersion: readonly [1n, 1n, 0n];
    readonly termRoot: Hash32;
    readonly nodeCount: bigint;
    readonly materialByteLength: bigint;
  };
  readonly material: readonly MidgardCekProgramMaterialEntry[];
  readonly termRoot: Hash32;
  readonly valueRoot: Hash32;
  readonly payloadCbor: Buffer;
} => {
  const typeBlob = commitMidgardCekBlob(Buffer.from("9f01ff", "hex"));
  const rawBytes = Buffer.alloc(rawByteLength, 0x5a);
  const rawBlob = commitMidgardCekBlob(rawBytes);
  const payloadCbor = encodeCanonicalSemanticBytes(rawBytes);
  const semanticNode = {
    kind: "bytes",
    bytesRoot: rawBlob.root,
    bytesLength: BigInt(rawBytes.length),
    cborLength: BigInt(payloadCbor.length),
    memory: 4n + BigInt(rawBytes.length),
  } as const;
  const semanticRoot = hashMidgardCekDataNode(semanticNode);
  const valueNode = {
    kind: "constant",
    typeRoot: typeBlob.root,
    payloadRoot: semanticRoot,
    payloadLength: BigInt(payloadCbor.length),
    semanticRoot,
    memory: BigInt(Math.max(1, rawBytes.length)),
  } as const;
  const valueRoot = hashMidgardCekValueNode(valueNode);
  const termNode = { kind: "constant", value: valueRoot } as const;
  const termRoot = hashMidgardCekTermNode(termNode);
  const material: MidgardCekProgramMaterialEntry[] = [
    {
      kind: "term",
      root: termRoot,
      preimage: encodeMidgardCekTermNode(termNode),
    },
    {
      kind: "value",
      root: valueRoot,
      preimage: encodeMidgardCekValueNode(valueNode),
    },
    {
      kind: "dataNode",
      root: semanticRoot,
      preimage: encodeMidgardCekDataNode(semanticNode),
    },
    ...[...typeBlob.nodes.entries(), ...rawBlob.nodes.entries()].map(
      ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(rootHex, "hex") as Hash32,
        preimage: node.preimage,
      }),
    ),
  ];
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
    termRoot,
    valueRoot,
    payloadCbor,
  };
};

export const makeNestedListConstantProgramMaterial = (
  listDepth: number,
  typeCbor = Buffer.from([
    0x9f,
    ...Array.from({ length: listDepth }, () => 5),
    0,
    0xff,
  ]),
): ReturnType<typeof makeBytesConstantProgramMaterial> => {
  const typeBlob = commitMidgardCekBlob(typeCbor);
  const integerBlob = commitMidgardCekBlob(Buffer.from([0]));
  let semanticRoot = hashMidgardCekDataNode({
    kind: "integer",
    cborRoot: integerBlob.root,
    cborLength: 1n,
    memory: 5n,
  });
  let semanticCborLength = 1n;
  let semanticMemory = 5n;
  const semanticMaterial: MidgardCekProgramMaterialEntry[] = [
    {
      kind: "dataNode",
      root: semanticRoot,
      preimage: encodeMidgardCekDataNode({
        kind: "integer",
        cborRoot: integerBlob.root,
        cborLength: 1n,
        memory: 5n,
      }),
    },
  ];
  for (let depth = 0; depth < listDepth; depth += 1) {
    const listNode = {
      head: semanticRoot,
      headCborLength: semanticCborLength,
      headMemory: semanticMemory,
      tail: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
      length: 1n,
      payloadCborLength: semanticCborLength,
      memory: semanticMemory,
    };
    const listRoot = hashMidgardCekDataListNode(listNode);
    semanticMaterial.push({
      kind: "dataList",
      root: listRoot,
      preimage: encodeMidgardCekDataListNode(listNode),
    });
    semanticCborLength = midgardCekDataListCborLength(1n, semanticCborLength);
    semanticMemory += 4n;
    const listDataNode = {
      kind: "list",
      itemsCount: 1n,
      itemsRoot: listRoot,
      cborLength: semanticCborLength,
      memory: semanticMemory,
    } as const;
    semanticRoot = hashMidgardCekDataNode(listDataNode);
    semanticMaterial.push({
      kind: "dataNode",
      root: semanticRoot,
      preimage: encodeMidgardCekDataNode(listDataNode),
    });
  }
  const valueNode = {
    kind: "constant",
    typeRoot: typeBlob.root,
    payloadRoot: semanticRoot,
    payloadLength: semanticCborLength,
    semanticRoot,
    memory: 1n,
  } as const;
  const valueRoot = hashMidgardCekValueNode(valueNode);
  const termNode = { kind: "constant", value: valueRoot } as const;
  const termRoot = hashMidgardCekTermNode(termNode);
  const material: MidgardCekProgramMaterialEntry[] = [
    {
      kind: "term",
      root: termRoot,
      preimage: encodeMidgardCekTermNode(termNode),
    },
    {
      kind: "value",
      root: valueRoot,
      preimage: encodeMidgardCekValueNode(valueNode),
    },
    ...semanticMaterial,
    ...[...typeBlob.nodes.entries(), ...integerBlob.nodes.entries()].map(
      ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(rootHex, "hex") as Hash32,
        preimage: node.preimage,
      }),
    ),
  ];
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
    termRoot,
    valueRoot,
    payloadCbor: Buffer.concat([
      ...Array.from({ length: listDepth }, () => Buffer.from([0x9f])),
      Buffer.from([0]),
      ...Array.from({ length: listDepth }, () => Buffer.from([0xff])),
    ]),
  };
};

type FixtureConstr = {
  readonly kind: "constr";
  readonly constructor: bigint;
  readonly fields: readonly FixtureData[];
};

export type FixtureData =
  | bigint
  | string
  | readonly FixtureData[]
  | FixtureConstr;

export type FixtureSummary = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type FixtureListSummary = {
  readonly root: Hash32;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

export const isFixtureConstr = (value: FixtureData): value is FixtureConstr =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  "kind" in value &&
  value.kind === "constr";

export const encodeFixtureHeader = (major: number, value: bigint): Buffer => {
  const prefix = major << 5;
  if (value < 24n) return Buffer.from([prefix | Number(value)]);
  if (value <= 0xffn) return Buffer.from([prefix | 24, Number(value)]);
  const result = Buffer.alloc(3);
  result[0] = prefix | 25;
  result.writeUInt16BE(Number(value), 1);
  return result;
};
