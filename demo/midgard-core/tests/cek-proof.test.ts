import { describe, expect, it } from "vitest";

import {
  commitMidgardCekBlob,
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardCekProgramMaterialEntry,
  decodeMidgardCekProgramMaterialSidecar,
  decodeMidgardProofSubmission,
  encodeMidgardCekBlobBranch,
  encodeMidgardCekBlobChunk,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekSequenceNode,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  encodeMidgardProofSubmission,
  hashMidgardCekBlobChunk,
  hashMidgardCekBlsExpressionNode,
  hashMidgardCekContinuationFrame,
  hashMidgardCekEnvironmentNode,
  hashMidgardCekMachineState,
  hashMidgardCekProgramEnvelope,
  hashMidgardCekProgramMaterialPreimage,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  mergeMidgardCekProgramMaterialSidecars,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK,
  MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS,
  MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
  type MidgardCekProgramEnvelope,
  midgardCekProgramMaterialDependencies,
  type MidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialKind,
  MidgardCekProgramMaterialMissingRootError,
  verifyMidgardCekProgramMaterial,
  verifyMidgardCekProgramMaterialBundle,
} from "../src/cek-proof.js";
import {
  encodeMidgardCekDataListNode,
  encodeMidgardCekDataNode,
  encodeMidgardCekDataPairNode,
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataNode,
} from "../src/cek-semantic.js";
import type { Hash32 } from "../src/codec/hash.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);
const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");
const programMaterialEntry = (
  kind: MidgardCekProgramMaterialKind,
  preimage: Buffer,
): MidgardCekProgramMaterialEntry => ({
  kind,
  root: hashMidgardCekProgramMaterialPreimage(kind, preimage),
  preimage,
});

const makeUnaryProgramMaterial = (
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

const encodeCanonicalSemanticBytes = (bytes: Buffer): Buffer => {
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

const makeBytesConstantProgramMaterial = (
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

const makeNestedListConstantProgramMaterial = (
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

type FixtureData = bigint | string | readonly FixtureData[] | FixtureConstr;

type FixtureSummary = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

type FixtureListSummary = {
  readonly root: Hash32;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

const isFixtureConstr = (value: FixtureData): value is FixtureConstr =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  "kind" in value &&
  value.kind === "constr";

const encodeFixtureHeader = (major: number, value: bigint): Buffer => {
  const prefix = major << 5;
  if (value < 24n) return Buffer.from([prefix | Number(value)]);
  if (value <= 0xffn) return Buffer.from([prefix | 24, Number(value)]);
  const result = Buffer.alloc(3);
  result[0] = prefix | 25;
  result.writeUInt16BE(Number(value), 1);
  return result;
};

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

const makeSemanticConstantProgramMaterial = (
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

describe("V1 CEK commitments", () => {
  it("matches the Aiken node, state, and program vectors", () => {
    const term = hashMidgardCekTermNode({
      kind: "application",
      function: hash(1),
      argument: hash(2),
    });
    const value = hashMidgardCekValueNode({
      kind: "lambda",
      body: hash(3),
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
    });
    const sequence = hashMidgardCekSequenceNode({
      head: term,
      tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
      length: 1n,
    });
    const environment = hashMidgardCekEnvironmentNode({
      value,
      tail: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      length: 1n,
    });
    const continuation = hashMidgardCekContinuationFrame({
      kind: "applyArgument",
      argument: hash(4),
      environment,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
    });
    const state = hashMidgardCekMachineState({
      mode: "compute",
      executionIndex: 2n,
      focusRoot: term,
      environmentRoot: environment,
      continuationRoot: continuation,
      auxiliary: 0n,
      cpu: 16_000n,
      memory: 100n,
    });
    const program = hashMidgardCekProgramEnvelope({
      uplcVersion: [1n, 1n, 0n],
      termRoot: term,
      nodeCount: 3n,
      materialByteLength: 144n,
    });
    const applyValue = hashMidgardCekContinuationFrame({
      kind: "applyValue",
      value: hash(1),
      tail: hash(2),
    });
    const caseSelect = hashMidgardCekContinuationFrame({
      kind: "caseSelect",
      environment: hash(1),
      tail: hash(2),
      valuesCount: 3n,
    });
    const caseApply = hashMidgardCekContinuationFrame({
      kind: "caseApply",
      environment: hash(1),
      builtContinuation: hash(2),
    });
    const caseState = hashMidgardCekMachineState({
      mode: "caseApply",
      executionIndex: 0n,
      focusRoot: hash(1),
      environmentRoot: hash(2),
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      auxiliary: 3n,
      cpu: 4n,
      memory: 5n,
    });
    const semanticBuiltinState = hashMidgardCekMachineState({
      mode: "semanticBuiltin",
      executionIndex: 0n,
      focusRoot: hash(1),
      environmentRoot: hash(2),
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      auxiliary: 3n,
      cpu: 4n,
      memory: 5n,
    });
    const blsLeaf = hashMidgardCekBlsExpressionNode({
      kind: "millerLoop",
      g1Value: hash(1),
      g2Value: hash(2),
    });
    const blsMultiply = hashMidgardCekBlsExpressionNode({
      kind: "multiply",
      left: hash(1),
      right: hash(2),
    });
    const blsValue = hashMidgardCekValueNode({
      kind: "blsMillerLoop",
      expressionRoot: blsLeaf,
    });

    expect(hex(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)).toBe(
      "8ab46e13655026ca6fe253b057ff678ebf9fa088097d6fa4c62276ba140f1743",
    );
    expect(hex(MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT)).toBe(
      "0b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7",
    );
    expect(hex(MIDGARD_CEK_EMPTY_CONTINUATION_ROOT)).toBe(
      "53163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b",
    );
    expect(hex(term)).toBe(
      "2a37aa5b923cf90c6f3c8849e8fe2b28adcda97ccd736af6bf35b8312035f431",
    );
    expect(hex(value)).toBe(
      "a103975a15b084afa3e69e3f71ff66b57d3ce83f992df1325d0958245d337941",
    );
    expect(hex(sequence)).toBe(
      "854e2610e77c03a89283632923ddb99af0f276bedae48865036e85c2ed1f23cb",
    );
    expect(hex(environment)).toBe(
      "4c2bb324a912cff2fc99c9056faa7d8bb72ab5dd512362fbd6a7383fe9c71a5c",
    );
    expect(hex(continuation)).toBe(
      "7082fd0df1eb9680517dd87620f9fa94e5ea4598a56676e224f20a833bea3513",
    );
    expect(hex(state)).toBe(
      "8a475241923d49a38fa1d6376dd1c40ebb8d16adc87f51bc4a6dea7d954f88d1",
    );
    expect(hex(program)).toBe(
      "e9d2696eff22d8078ae7bd71c83f6058e0db4d938a016314a2cd35feaccfdefa",
    );
    expect(hex(applyValue)).toBe(
      "facc375c9390b9503ccb80c0500bb3b88dc62c5da62c05599e6035938535fe61",
    );
    expect(hex(caseSelect)).toBe(
      "e71825335e324be2d099381365d5bc42c769bb334df509e5adae883842b9c1ea",
    );
    expect(hex(caseApply)).toBe(
      "12aedf8df3ce9e44db4cc847cd52d0cff0b9b832ff4425954fa7c38c523e9888",
    );
    expect(hex(caseState)).toBe(
      "31884c469ef240c7bad0a586ca242f45fa1728d13ab8385a97e5e0a18b806eb6",
    );
    expect(hex(semanticBuiltinState)).toBe(
      "9d83a7edc397becce073970978dff4d6bae2c0334f0dc7c3525e35d901f47a13",
    );
    expect(hex(blsLeaf)).toBe(
      "6bd7d80222a87e5a09102534274099c501f82c5d898fe75f24fdc951c2fb3cc0",
    );
    expect(hex(blsMultiply)).toBe(
      "be5de65e45ff867ca8394e227ee30e4f8cfaee0d4cb1d2834fe1af943b806082",
    );
    expect(hex(blsValue)).toBe(
      "d93762ca25b0c585073f9e97f50873b76e13de1290182f6bb4744d29d9fb3fb7",
    );
    expect(hex(hashMidgardCekBlobChunk(Buffer.from("010203", "hex")))).toBe(
      "0bb4f6f24cd0080e59e98d57a13c6453e07991452b80f14ff1d9791c02db1fc9",
    );
  });

  it("fails closed on malformed hashes, oversized chunks, and unknown builtins", () => {
    expect(() =>
      hashMidgardCekTermNode({
        kind: "application",
        function: Buffer.alloc(31),
        argument: hash(2),
      }),
    ).toThrow(/must be 32 bytes/u);
    expect(() =>
      hashMidgardCekBlobChunk(Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES + 1)),
    ).toThrow(/at most 4095 bytes/u);
    expect(() => hashMidgardCekTermNode({ kind: "builtin", tag: 87n })).toThrow(
      /between 0 and 86/u,
    );
    expect(() =>
      hashMidgardCekSequenceNode({
        head: hash(1),
        tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
        length: 0n,
      }),
    ).toThrow(/must be positive/u);
  });

  it("decodes only the canonical bounded V1 program envelope", () => {
    const envelope = encodeMidgardCekProgramEnvelope({
      uplcVersion: [1n, 1n, 0n],
      termRoot: hash(1),
      nodeCount: MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
      materialByteLength: MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
    });
    expect(envelope).toHaveLength(MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES);
    expect(decodeMidgardCekProgramEnvelope(envelope)).toEqual({
      uplcVersion: [1n, 1n, 0n],
      termRoot: hash(1),
      nodeCount: MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
      materialByteLength: MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
    });

    const noncanonical = Buffer.concat([
      Buffer.from("8501830101005820", "hex"),
      hash(1),
      Buffer.from("18031890", "hex"),
    ]);
    expect(() => decodeMidgardCekProgramEnvelope(noncanonical)).toThrow(
      /Non-minimal CBOR/u,
    );
    const unsupportedEnvelope = Buffer.concat([
      Buffer.from("8517830101005820", "hex"),
      hash(1),
      Buffer.from("031890", "hex"),
    ]);
    expect(() => decodeMidgardCekProgramEnvelope(unsupportedEnvelope)).toThrow(
      /unsupported CEK program envelope version 23/u,
    );
    expect(() =>
      decodeMidgardCekProgramEnvelope(
        encodeMidgardCekProgramEnvelope({
          uplcVersion: [1n, 0n, 0n],
          termRoot: hash(1),
          nodeCount: 3n,
          materialByteLength: 144n,
        }),
      ),
    ).toThrow(/only UPLC 1.1.0/u);
    expect(() =>
      decodeMidgardCekProgramEnvelope(
        encodeMidgardCekProgramEnvelope({
          uplcVersion: [1n, 1n, 0n],
          termRoot: hash(1),
          nodeCount: MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT + 1n,
          materialByteLength: 144n,
        }),
      ),
    ).toThrow(/node count/u);
    expect(() =>
      decodeMidgardCekProgramEnvelope(
        encodeMidgardCekProgramEnvelope({
          uplcVersion: [1n, 1n, 0n],
          termRoot: hash(1),
          nodeCount: 3n,
          materialByteLength: MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES + 1n,
        }),
      ),
    ).toThrow(/material length/u);
  });

  it("extracts the exact ordered direct dependency set for every V1 material kind", () => {
    const one = hash(1);
    const two = hash(2);
    const three = hash(3);
    const four = hash(4);
    const five = hash(5);
    const six = hash(6);
    const dataList = (length: bigint, head: Hash32, tail: Hash32) =>
      encodeMidgardCekDataListNode({
        head,
        headCborLength: 1n,
        headMemory: 1n,
        tail,
        length,
        payloadCborLength: length,
        memory: length,
      });
    const dataPair = (
      length: bigint,
      key: Hash32,
      value: Hash32,
      tail: Hash32,
    ) =>
      encodeMidgardCekDataPairNode({
        key,
        keyCborLength: 1n,
        keyMemory: 1n,
        value,
        valueCborLength: 1n,
        valueMemory: 1n,
        tail,
        length,
        payloadCborLength: length * 2n,
        memory: length * 2n,
      });
    const cases: readonly {
      readonly name: string;
      readonly entry: MidgardCekProgramMaterialEntry;
      readonly expected: readonly Hash32[];
    }[] = [
      {
        name: "term variable",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "variable", index: 0n }),
        ),
        expected: [],
      },
      {
        name: "term error",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "error" }),
        ),
        expected: [],
      },
      {
        name: "term builtin",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "builtin", tag: 0n }),
        ),
        expected: [],
      },
      {
        name: "term delay",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "delay", body: one }),
        ),
        expected: [one],
      },
      {
        name: "term lambda",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "lambda", body: one }),
        ),
        expected: [one],
      },
      {
        name: "term force",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "force", term: one }),
        ),
        expected: [one],
      },
      {
        name: "term application",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "application",
            function: one,
            argument: two,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "term application deduplicates",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "application",
            function: one,
            argument: one,
          }),
        ),
        expected: [one],
      },
      {
        name: "term constant",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "constant", value: three }),
        ),
        expected: [three],
      },
      {
        name: "empty term constr",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 0n,
            termsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty term constr",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 2n,
            termsRoot: four,
          }),
        ),
        expected: [four],
      },
      {
        name: "empty term case",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 0n,
            branchesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        expected: [one],
      },
      {
        name: "non-empty term case",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 2n,
            branchesRoot: four,
          }),
        ),
        expected: [one, four],
      },
      {
        name: "value",
        entry: programMaterialEntry(
          "value",
          encodeMidgardCekValueNode({
            kind: "constant",
            typeRoot: one,
            payloadRoot: two,
            payloadLength: 1n,
            semanticRoot: two,
            memory: 1n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "value deduplicates roots",
        entry: programMaterialEntry(
          "value",
          encodeMidgardCekValueNode({
            kind: "constant",
            typeRoot: one,
            payloadRoot: one,
            payloadLength: 1n,
            semanticRoot: one,
            memory: 1n,
          }),
        ),
        expected: [one],
      },
      {
        name: "one-item sequence",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            length: 1n,
          }),
        ),
        expected: [one],
      },
      {
        name: "multi-item sequence",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: two,
            length: 2n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "blob chunk",
        entry: programMaterialEntry(
          "blobChunk",
          encodeMidgardCekBlobChunk(Buffer.from("0102", "hex")),
        ),
        expected: [],
      },
      {
        name: "blob branch",
        entry: programMaterialEntry(
          "blobBranch",
          encodeMidgardCekBlobBranch({
            left: one,
            right: two,
            byteLength: 2n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "blob branch deduplicates",
        entry: programMaterialEntry(
          "blobBranch",
          encodeMidgardCekBlobBranch({
            left: one,
            right: one,
            byteLength: 2n,
          }),
        ),
        expected: [one],
      },
      {
        name: "empty small constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 0n,
            fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty small constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 1n,
            fieldsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [one],
      },
      {
        name: "empty large constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrLarge",
            constructorCborRoot: two,
            constructorCborLength: 1n,
            constructorMemory: 1n,
            fieldsCount: 0n,
            fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [two],
      },
      {
        name: "non-empty large constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrLarge",
            constructorCborRoot: two,
            constructorCborLength: 1n,
            constructorMemory: 1n,
            fieldsCount: 1n,
            fieldsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "empty map Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 0n,
            entriesRoot: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty map Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 1n,
            entriesRoot: three,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [three],
      },
      {
        name: "empty list Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 0n,
            itemsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty list Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 1n,
            itemsRoot: four,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [four],
      },
      {
        name: "integer Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "integer",
            cborRoot: five,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [five],
      },
      {
        name: "bytes Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "bytes",
            bytesRoot: six,
            bytesLength: 1n,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [six],
      },
      {
        name: "one-item Data list",
        entry: programMaterialEntry(
          "dataList",
          dataList(1n, one, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
        ),
        expected: [one],
      },
      {
        name: "multi-item Data list",
        entry: programMaterialEntry("dataList", dataList(2n, one, two)),
        expected: [one, two],
      },
      {
        name: "one-item Data pair",
        entry: programMaterialEntry(
          "dataPair",
          dataPair(1n, one, two, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT),
        ),
        expected: [one, two],
      },
      {
        name: "multi-item Data pair deduplicates",
        entry: programMaterialEntry("dataPair", dataPair(2n, one, one, two)),
        expected: [one, two],
      },
    ];

    expect(
      cases.map(({ name, entry }) => ({
        name,
        dependencies: midgardCekProgramMaterialDependencies(entry).map(hex),
      })),
    ).toEqual(
      cases.map(({ name, expected }) => ({
        name,
        dependencies: expected.map(hex),
      })),
    );
  });

  it("fails closed while extracting hostile material dependencies", () => {
    const one = hash(1);
    const two = hash(2);
    const dataList = (length: bigint, tail: Hash32) =>
      programMaterialEntry(
        "dataList",
        encodeMidgardCekDataListNode({
          head: one,
          headCborLength: 1n,
          headMemory: 1n,
          tail,
          length,
          payloadCborLength: 1n,
          memory: 1n,
        }),
      );
    const dataPair = (length: bigint, tail: Hash32) =>
      programMaterialEntry(
        "dataPair",
        encodeMidgardCekDataPairNode({
          key: one,
          keyCborLength: 1n,
          keyMemory: 1n,
          value: two,
          valueCborLength: 1n,
          valueMemory: 1n,
          tail,
          length,
          payloadCborLength: 2n,
          memory: 2n,
        }),
      );
    const hostile: readonly {
      readonly name: string;
      readonly entry: MidgardCekProgramMaterialEntry;
      readonly error: RegExp;
    }[] = [
      {
        name: "unauthenticated root",
        entry: {
          ...programMaterialEntry(
            "term",
            encodeMidgardCekTermNode({ kind: "error" }),
          ),
          root: hash(99),
        },
        error: /root does not match its preimage/u,
      },
      {
        name: "non-canonical preimage",
        entry: programMaterialEntry("term", Buffer.from("811806", "hex")),
        error: /Non-minimal CBOR/u,
      },
      {
        name: "runtime-only context constant",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "contextConstant",
            value: one,
          }),
        ),
        error: /runtime-only context constant/u,
      },
      {
        name: "split value roots",
        entry: programMaterialEntry(
          "value",
          encodeMidgardCekValueNode({
            kind: "constant",
            typeRoot: one,
            payloadRoot: one,
            payloadLength: 1n,
            semanticRoot: two,
            memory: 1n,
          }),
        ),
        error: /payload root must equal its canonical semantic root/u,
      },
      {
        name: "empty constr with non-canonical root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 0n,
            termsRoot: one,
          }),
        ),
        error: /empty CEK constr sequence/u,
      },
      {
        name: "non-empty constr with empty root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 1n,
            termsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        error: /non-empty CEK constr sequence/u,
      },
      {
        name: "empty case with non-canonical root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 0n,
            branchesRoot: two,
          }),
        ),
        error: /empty CEK case sequence/u,
      },
      {
        name: "non-empty case with empty root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 1n,
            branchesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        error: /non-empty CEK case sequence/u,
      },
      {
        name: "one-item sequence with non-canonical tail",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: two,
            length: 1n,
          }),
        ),
        error: /one-item CEK sequence/u,
      },
      {
        name: "multi-item sequence with empty tail",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            length: 2n,
          }),
        ),
        error: /multi-item CEK sequence/u,
      },
      {
        name: "empty Data constructor with non-canonical root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 0n,
            fieldsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /empty CEK Data constructor/u,
      },
      {
        name: "non-empty Data constructor with empty root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 1n,
            fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /non-empty CEK Data constructor/u,
      },
      {
        name: "empty Data map with non-canonical root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 0n,
            entriesRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /empty CEK Data map/u,
      },
      {
        name: "non-empty Data map with empty root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 1n,
            entriesRoot: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /non-empty CEK Data map/u,
      },
      {
        name: "empty Data list node with non-canonical root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 0n,
            itemsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /empty CEK Data list/u,
      },
      {
        name: "non-empty Data list node with empty root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 1n,
            itemsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /non-empty CEK Data list/u,
      },
      {
        name: "zero-length Data list material",
        entry: dataList(0n, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
        error: /Data list material length must be positive/u,
      },
      {
        name: "one-item Data list with non-canonical tail",
        entry: dataList(1n, two),
        error: /one-item CEK Data list/u,
      },
      {
        name: "multi-item Data list with empty tail",
        entry: dataList(2n, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
        error: /multi-item CEK Data list/u,
      },
      {
        name: "zero-length Data pair material",
        entry: dataPair(0n, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT),
        error: /Data pair material length must be positive/u,
      },
      {
        name: "one-item Data pair with non-canonical tail",
        entry: dataPair(1n, hash(3)),
        error: /one-item CEK Data pair list/u,
      },
      {
        name: "multi-item Data pair with empty tail",
        entry: dataPair(2n, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT),
        error: /multi-item CEK Data pair list/u,
      },
    ];

    for (const { entry, error } of hostile) {
      expect(() => midgardCekProgramMaterialDependencies(entry)).toThrow(error);
    }
  });

  it("matches semantic constant payloads at the authenticated material boundary", () => {
    const validCases: readonly {
      readonly name: string;
      readonly typeTags: readonly number[];
      readonly payload: FixtureData;
      readonly memory: bigint;
    }[] = [
      { name: "integer", typeTags: [0], payload: 0n, memory: 1n },
      { name: "bytes", typeTags: [1], payload: "00ff", memory: 2n },
      {
        name: "UTF-8 string",
        typeTags: [2],
        payload: "c3a9",
        memory: 2n,
      },
      {
        name: "Unit",
        typeTags: [3],
        payload: { kind: "constr", constructor: 0n, fields: [] },
        memory: 1n,
      },
      {
        name: "Bool false",
        typeTags: [4],
        payload: { kind: "constr", constructor: 0n, fields: [] },
        memory: 1n,
      },
      {
        name: "Bool true",
        typeTags: [4],
        payload: { kind: "constr", constructor: 1n, fields: [] },
        memory: 1n,
      },
      {
        name: "list",
        typeTags: [5, 3],
        payload: [
          { kind: "constr", constructor: 0n, fields: [] },
          { kind: "constr", constructor: 0n, fields: [] },
        ],
        memory: 2n,
      },
      {
        name: "pair",
        typeTags: [6, 1, 0],
        payload: {
          kind: "constr",
          constructor: 0n,
          fields: ["aa", 0n],
        },
        memory: 2n,
      },
      {
        name: "Data",
        typeTags: [8],
        payload: { kind: "constr", constructor: 2n, fields: [0n] },
        memory: 9n,
      },
      {
        name: "G1",
        typeTags: [9],
        payload: "00".repeat(48),
        memory: 48n,
      },
      {
        name: "G2",
        typeTags: [10],
        payload: "00".repeat(96),
        memory: 96n,
      },
    ];
    for (const testCase of validCases) {
      const fixture = makeSemanticConstantProgramMaterial(
        testCase.typeTags,
        testCase.payload,
        testCase.memory,
      );
      const verified = verifyMidgardCekProgramMaterial(
        fixture.envelope,
        fixture.material,
      );
      expect(verified.constants, testCase.name).toHaveLength(1);
      expect(verified.constants[0]!.typeCbor, testCase.name).toEqual(
        fixture.typeCbor,
      );
      expect(verified.constants[0]!.payloadCbor, testCase.name).toEqual(
        fixture.payloadCbor,
      );
    }

    const invalidCases: readonly {
      readonly name: string;
      readonly typeTags: readonly number[];
      readonly payload: FixtureData;
      readonly memory: bigint;
    }[] = [
      {
        name: "integer rejects bytes",
        typeTags: [0],
        payload: "00",
        memory: 1n,
      },
      {
        name: "bytes rejects integer",
        typeTags: [1],
        payload: 0n,
        memory: 1n,
      },
      {
        name: "string rejects invalid UTF-8",
        typeTags: [2],
        payload: "ff",
        memory: 1n,
      },
      {
        name: "Unit rejects another constructor",
        typeTags: [3],
        payload: { kind: "constr", constructor: 1n, fields: [] },
        memory: 1n,
      },
      {
        name: "Bool rejects an unknown constructor",
        typeTags: [4],
        payload: { kind: "constr", constructor: 2n, fields: [] },
        memory: 1n,
      },
      {
        name: "list rejects a wrongly typed element",
        typeTags: [5, 3],
        payload: [0n],
        memory: 1n,
      },
      {
        name: "pair rejects a non-zero constructor",
        typeTags: [6, 1, 0],
        payload: {
          kind: "constr",
          constructor: 1n,
          fields: ["aa", 0n],
        },
        memory: 1n,
      },
      {
        name: "G1 rejects the wrong payload length",
        typeTags: [9],
        payload: "00".repeat(47),
        memory: 48n,
      },
      {
        name: "G2 rejects the wrong payload length",
        typeTags: [10],
        payload: "00".repeat(95),
        memory: 96n,
      },
      {
        name: "Miller-loop type rejects Data",
        typeTags: [11],
        payload: { kind: "constr", constructor: 0n, fields: [] },
        memory: 192n,
      },
    ];
    for (const testCase of invalidCases) {
      const fixture = makeSemanticConstantProgramMaterial(
        testCase.typeTags,
        testCase.payload,
        testCase.memory,
      );
      expect(
        () =>
          verifyMidgardCekProgramMaterial(fixture.envelope, fixture.material),
        testCase.name,
      ).toThrow(/payload does not match its semantic type/u);
    }
  });

  it("authenticates and traverses exact content-addressed program material", () => {
    const typeBlob = commitMidgardCekBlob(Buffer.from("9f01ff", "hex"));
    const rawValue = Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES + 1, 0x5a);
    const payloadBytes = Buffer.concat([
      Buffer.from([0x5f]),
      ...Array.from({ length: Math.ceil(rawValue.length / 64) }, (_, index) => {
        const chunk = rawValue.subarray(index * 64, (index + 1) * 64);
        return Buffer.concat([Buffer.from([0x58, chunk.length]), chunk]);
      }),
      Buffer.from([0xff]),
    ]);
    const rawBlob = commitMidgardCekBlob(rawValue);
    const semanticNode = {
      kind: "bytes",
      bytesRoot: rawBlob.root,
      bytesLength: BigInt(rawValue.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(rawValue.length)),
      memory: 4n + BigInt(rawValue.length),
    } as const;
    const semanticRoot = hashMidgardCekDataNode(semanticNode);
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: semanticRoot,
      payloadLength: BigInt(payloadBytes.length),
      semanticRoot,
      memory: BigInt(rawValue.length),
    } as const;
    const valuePreimage = encodeMidgardCekValueNode(valueNode);
    const valueRoot = hashMidgardCekValueNode(valueNode);
    const termNode = { kind: "constant", value: valueRoot } as const;
    const termPreimage = encodeMidgardCekTermNode(termNode);
    const termRoot = hashMidgardCekTermNode(termNode);

    const material: MidgardCekProgramMaterialEntry[] = [
      {
        kind: "term",
        root: termRoot,
        preimage: termPreimage,
      },
      {
        kind: "value",
        root: valueRoot,
        preimage: valuePreimage,
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
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    };

    const verified = verifyMidgardCekProgramMaterial(envelope, material);
    expect(verified.nodeCount).toBe(BigInt(material.length));
    expect(verified.constants).toHaveLength(1);
    expect(verified.constants[0]!.typeCbor).toEqual(
      Buffer.from("9f01ff", "hex"),
    );
    expect(verified.constants[0]!.payloadCbor).toEqual(payloadBytes);
    expect(
      verifyMidgardCekProgramMaterialBundle([envelope], material),
    ).toHaveLength(1);

    const unrelated = programMaterialEntry(
      "term",
      encodeMidgardCekTermNode({ kind: "error" }),
    );
    const availableByRoot = new Map(
      [...material, unrelated].map((entry) => [hex(entry.root), entry]),
    );
    const fetchedRoots: string[] = [];
    const gathered = new Set<string>();
    const pending: Hash32[] = [termRoot];
    for (let cursor = 0; cursor < pending.length; cursor += 1) {
      const root = pending[cursor]!;
      const key = hex(root);
      if (gathered.has(key)) continue;
      fetchedRoots.push(key);
      const entry = availableByRoot.get(key);
      if (entry === undefined) {
        throw new Error(`test material is missing ${key}`);
      }
      gathered.add(key);
      pending.push(...midgardCekProgramMaterialDependencies(entry));
    }
    expect(gathered).toEqual(verified.reachableRoots);
    expect(fetchedRoots).toHaveLength(gathered.size);
    expect(fetchedRoots).not.toContain(hex(unrelated.root));

    const maximumChunkEntry = {
      kind: "blobChunk",
      root: hashMidgardCekBlobChunk(Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES)),
      preimage: encodeMidgardCekBlobChunk(
        Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES),
      ),
    } as const;
    expect(maximumChunkEntry.preimage).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES,
    );
    const encodedEntry =
      encodeMidgardCekProgramMaterialEntry(maximumChunkEntry);
    expect(encodedEntry).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES,
    );
    expect(decodeMidgardCekProgramMaterialEntry(encodedEntry)).toEqual(
      maximumChunkEntry,
    );
    const daValue = encodeMidgardCekProgramMaterialDaValue(maximumChunkEntry);
    expect(daValue).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES,
    );
    expect(
      decodeMidgardCekProgramMaterialDaEntry(maximumChunkEntry.root, daValue),
    ).toEqual(maximumChunkEntry);
    const unsupportedDaValue = Buffer.from(daValue);
    unsupportedDaValue[1] = 23;
    expect(() =>
      decodeMidgardCekProgramMaterialDaEntry(
        maximumChunkEntry.root,
        unsupportedDaValue,
      ),
    ).toThrow(/unsupported CEK program material DA value version 23/u);
  });

  it("authenticates constructor indices beyond JavaScript's safe-integer range", () => {
    const constructor = 1n << 80n;
    const constructorCbor = Buffer.from("c24b0100000000000000000000", "hex");
    const payloadCbor = Buffer.concat([
      Buffer.from("d86682", "hex"),
      constructorCbor,
      Buffer.from([0x80]),
    ]);
    const typeBlob = commitMidgardCekBlob(Buffer.from("9f08ff", "hex"));
    const constructorBlob = commitMidgardCekBlob(constructorCbor);
    const semanticNode = {
      kind: "constrLarge",
      constructorCborRoot: constructorBlob.root,
      constructorCborLength: BigInt(constructorCbor.length),
      constructorMemory: 15n,
      fieldsCount: 0n,
      fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
      cborLength: midgardCekDataConstrCborLength(constructor, 0n, 0n),
      memory: 4n,
    } as const;
    const semanticRoot = hashMidgardCekDataNode(semanticNode);
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: semanticRoot,
      payloadLength: BigInt(payloadCbor.length),
      semanticRoot,
      memory: 4n,
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
      ...[...typeBlob.nodes.entries(), ...constructorBlob.nodes.entries()].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    };

    const verified = verifyMidgardCekProgramMaterial(envelope, material);
    expect(verified.constants[0]!.payloadCbor).toEqual(payloadCbor);
    expect(verified.constants[0]!.memory).toBe(4n);
  });

  it("rejects the retired split payload/semantic-root representation", () => {
    const typeBlob = commitMidgardCekBlob(Buffer.from("9f01ff", "hex"));
    const payloadValue = Buffer.alloc(65, 0x5a);
    const payloadBytes = Buffer.concat([
      Buffer.from("5f5840", "hex"),
      payloadValue.subarray(0, 64),
      Buffer.from("415aff", "hex"),
    ]);
    const payloadBlob = commitMidgardCekBlob(payloadBytes);

    // Canonical V1 has one semantic payload root. A value that retains the
    // retired split whole-payload blob root must fail before graph traversal.
    const conflictingValue = Buffer.alloc(65, 0x5b);
    const conflictingBlob = commitMidgardCekBlob(conflictingValue);
    const conflictingSemanticNode = {
      kind: "bytes",
      bytesRoot: conflictingBlob.root,
      bytesLength: 65n,
      cborLength: midgardCekDataBytesCborLength(65n),
      memory: 69n,
    } as const;
    const conflictingSemanticRoot = hashMidgardCekDataNode(
      conflictingSemanticNode,
    );
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: payloadBlob.root,
      payloadLength: BigInt(payloadBytes.length),
      semanticRoot: conflictingSemanticRoot,
      memory: 65n,
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
        root: conflictingSemanticRoot,
        preimage: encodeMidgardCekDataNode(conflictingSemanticNode),
      },
      ...[
        ...typeBlob.nodes.entries(),
        ...payloadBlob.nodes.entries(),
        ...conflictingBlob.nodes.entries(),
      ].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    };

    expect(() => verifyMidgardCekProgramMaterial(envelope, material)).toThrow(
      /payload root must equal its canonical semantic root/u,
    );
  });

  it("classifies only an absent root and does not mask later bundle mismatches", () => {
    const missingNode = { kind: "error" } as const;
    const missingRoot = hashMidgardCekTermNode(missingNode);
    const missingEnvelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: missingRoot,
      nodeCount: 1n,
      materialByteLength: BigInt(encodeMidgardCekTermNode(missingNode).length),
    };
    let missingError: unknown;
    try {
      verifyMidgardCekProgramMaterial(missingEnvelope, []);
    } catch (cause) {
      missingError = cause;
    }
    expect(missingError).toBeInstanceOf(
      MidgardCekProgramMaterialMissingRootError,
    );
    expect(
      (missingError as MidgardCekProgramMaterialMissingRootError).rootHex,
    ).toBe(Buffer.from(missingRoot).toString("hex"));

    const presentNode = { kind: "builtin", tag: 0n } as const;
    const present = {
      kind: "term" as const,
      root: hashMidgardCekTermNode(presentNode),
      preimage: encodeMidgardCekTermNode(presentNode),
    };
    const laterMismatchEnvelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: present.root,
      nodeCount: 2n,
      materialByteLength: BigInt(present.preimage.length),
    };
    expect(() =>
      verifyMidgardCekProgramMaterialBundle(
        [missingEnvelope, laterMismatchEnvelope],
        [present],
      ),
    ).toThrow(/envelope declares 2/u);
  });

  it("fails closed on incomplete, duplicate, unreachable, and malformed material", () => {
    const termNode = { kind: "error" } as const;
    const term = {
      kind: "term",
      root: hashMidgardCekTermNode(termNode),
      preimage: encodeMidgardCekTermNode(termNode),
    } as const;
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: term.root,
      nodeCount: 1n,
      materialByteLength: BigInt(term.preimage.length),
    };
    const submission = encodeMidgardProofSubmission({
      transactionCbor: Buffer.from("820102", "hex"),
      programMaterial: [term],
    });
    expect(decodeMidgardProofSubmission(submission)).toEqual({
      transactionCbor: Buffer.from("820102", "hex"),
      programMaterial: [term],
    });
    const unsupportedSubmission = Buffer.from(submission);
    unsupportedSubmission[1] = 23;
    expect(() => decodeMidgardProofSubmission(unsupportedSubmission)).toThrow(
      /unsupported V1 submission version 23/u,
    );
    const sidecar = encodeMidgardCekProgramMaterialSidecar([term]);
    expect(decodeMidgardCekProgramMaterialSidecar(sidecar)).toEqual([term]);
    expect(encodeMidgardCekProgramMaterialSidecar([]).toString("hex")).toBe(
      "820180",
    );
    expect(() =>
      decodeMidgardCekProgramMaterialSidecar(Buffer.from("821780", "hex")),
    ).toThrow(/unsupported V1 program material sidecar version 23/u);
    expect(() =>
      decodeMidgardCekProgramMaterialSidecar(
        Buffer.concat([sidecar, Buffer.from([0])]),
      ),
    ).toThrow(/trailing bytes/u);
    expect(mergeMidgardCekProgramMaterialSidecars([sidecar, sidecar])).toEqual([
      term,
    ]);
    expect(() => verifyMidgardCekProgramMaterial(envelope, [])).toThrow(
      /missing root/u,
    );
    expect(() =>
      verifyMidgardCekProgramMaterial(envelope, [term, term]),
    ).toThrow(/duplicate/u);
    expect(() =>
      encodeMidgardProofSubmission({
        transactionCbor: Buffer.from("820102", "hex"),
        programMaterial: [term, term],
      }),
    ).toThrow(/duplicate/u);

    const extraNode = { kind: "builtin", tag: 0n } as const;
    const extra = {
      kind: "term",
      root: hashMidgardCekTermNode(extraNode),
      preimage: encodeMidgardCekTermNode(extraNode),
    } as const;
    expect(() =>
      verifyMidgardCekProgramMaterial(envelope, [term, extra]),
    ).toThrow(/unreachable/u);
    expect(() => verifyMidgardCekProgramMaterialBundle([], [term])).toThrow(
      /without a program envelope/u,
    );
    expect(() =>
      verifyMidgardCekProgramMaterial({ ...envelope, nodeCount: 2n }, [term]),
    ).toThrow(/envelope declares 2/u);

    const encoded = encodeMidgardCekProgramMaterialEntry(term);
    const tampered = Buffer.from(encoded);
    tampered[tampered.length - 1] ^= 1;
    expect(() => decodeMidgardCekProgramMaterialEntry(tampered)).toThrow(
      /root does not match/u,
    );

    const nonCanonicalBlobPreimage = Buffer.from("5800", "hex");
    const malformedBlob = {
      kind: "blobChunk",
      root: hashMidgardCekBlobChunk(Buffer.alloc(0)),
      preimage: nonCanonicalBlobPreimage,
    } as const;
    expect(() =>
      encodeMidgardCekProgramMaterialEntry(malformedBlob),
    ).not.toThrow();
    expect(() =>
      decodeMidgardCekProgramMaterialEntry(
        encodeMidgardCekProgramMaterialEntry(malformedBlob),
      ),
    ).toThrow(/root does not match/u);
  });

  it("keeps the authenticated runtime context term out of source programs", () => {
    const preimage = encodeMidgardCekTermNode({
      kind: "contextConstant",
      value: hash(1),
    });
    const root = hashMidgardCekTermNode({
      kind: "contextConstant",
      value: hash(1),
    });
    expect(() =>
      verifyMidgardCekProgramMaterial(
        {
          uplcVersion: [1n, 1n, 0n],
          termRoot: root,
          nodeCount: 1n,
          materialByteLength: BigInt(preimage.length),
        },
        [{ kind: "term", root, preimage }],
      ),
    ).toThrow(/runtime-only context constant/u);
  });

  it("rejects an authenticated oversized semantic payload before materialization", () => {
    const oversized = makeBytesConstantProgramMaterial(9_000);
    expect(oversized.payloadCbor.length).toBeGreaterThan(9_215);
    let materializedRoots = 0;

    expect(() =>
      verifyMidgardCekProgramMaterial(oversized.envelope, oversized.material, {
        onBlobMaterialized: () => {
          materializedRoots += 1;
        },
      }),
    ).toThrow(/source constant payload exceeds the 9215-byte/u);
    expect(materializedRoots).toBe(0);
  });

  it("requires canonical indefinite semantic constant type lists", () => {
    const canonical = makeNestedListConstantProgramMaterial(0);
    const verified = verifyMidgardCekProgramMaterial(
      canonical.envelope,
      canonical.material,
    );
    expect(verified.constants[0]?.typeCbor).toEqual(
      Buffer.from("9f00ff", "hex"),
    );

    const nonCanonical = makeNestedListConstantProgramMaterial(
      0,
      Buffer.from("8100", "hex"),
    );
    expect(() =>
      verifyMidgardCekProgramMaterial(
        nonCanonical.envelope,
        nonCanonical.material,
      ),
    ).toThrow(/CEK constant type.*canonical/u);
  });

  it("parses the exact bounded semantic type iteratively and rejects one byte over deterministically", () => {
    const boundaryDepth = MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES - 3;
    const boundary = makeNestedListConstantProgramMaterial(boundaryDepth);
    const verified = verifyMidgardCekProgramMaterial(
      boundary.envelope,
      boundary.material,
    );
    expect(verified.constants[0]?.typeCbor).toHaveLength(
      MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES,
    );
    expect(verified.constants[0]?.payloadCbor).toEqual(boundary.payloadCbor);

    const over = makeNestedListConstantProgramMaterial(boundaryDepth + 1);
    let rejection: unknown;
    try {
      verifyMidgardCekProgramMaterial(over.envelope, over.material);
    } catch (cause) {
      rejection = cause;
    }
    expect(rejection).toBeInstanceOf(Error);
    expect(rejection).not.toBeInstanceOf(RangeError);
    expect((rejection as Error).message).toMatch(
      /declares 65 bytes, exceeding 64/u,
    );
  });

  it("bounds distinct-envelope byte work before traversal and materializes shared final roots once", () => {
    const shared = makeBytesConstantProgramMaterial(8_900);
    expect(shared.payloadCbor.length).toBeLessThanOrEqual(9_215);
    const wrappers: MidgardCekProgramMaterialEntry[] = [];
    const underCapEnvelopes: MidgardCekProgramEnvelope[] = [];
    let root = shared.termRoot;
    let reachableBytes = shared.envelope.materialByteLength;
    for (let depth = 1; depth <= 3; depth += 1) {
      const wrapper = {
        kind: "application",
        function: root,
        argument: shared.termRoot,
      } as const;
      const preimage = encodeMidgardCekTermNode(wrapper);
      root = hashMidgardCekTermNode(wrapper);
      wrappers.push({ kind: "term", root, preimage });
      reachableBytes += BigInt(preimage.length);
      underCapEnvelopes.push({
        uplcVersion: [1n, 1n, 0n] as const,
        termRoot: root,
        nodeCount: shared.envelope.nodeCount + BigInt(depth),
        materialByteLength: reachableBytes,
      });
    }

    const materializedRoots: string[] = [];
    const materializedConstants: string[] = [];
    const verified = verifyMidgardCekProgramMaterialBundle(
      underCapEnvelopes,
      [...shared.material, ...wrappers],
      {
        onBlobMaterialized: (rootHex) => {
          materializedRoots.push(rootHex);
        },
        onConstantMaterialized: (valueRootHex) => {
          materializedConstants.push(valueRootHex);
        },
      },
    );
    expect(verified).toHaveLength(underCapEnvelopes.length);
    expect(new Set(materializedRoots).size).toBe(materializedRoots.length);
    expect(materializedRoots).toHaveLength(2);
    expect(materializedConstants).toEqual([
      Buffer.from(shared.valueRoot).toString("hex"),
    ]);
    expect(
      verified.every((result) =>
        result.constants[0]?.payloadCbor.equals(shared.payloadCbor),
      ),
    ).toBe(true);
    expect(verified[0]!.constants[0]!.payloadCbor).not.toBe(
      verified[1]!.constants[0]!.payloadCbor,
    );

    const overCapEnvelopes: MidgardCekProgramEnvelope[] = [];
    let aggregateNodeVisits = 0n;
    let aggregateByteWork = 0n;
    while (aggregateByteWork <= MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK) {
      const wrapper = {
        kind: "application",
        function: root,
        argument: shared.termRoot,
      } as const;
      const preimage = encodeMidgardCekTermNode(wrapper);
      root = hashMidgardCekTermNode(wrapper);
      reachableBytes += BigInt(preimage.length);
      const envelope: MidgardCekProgramEnvelope = {
        uplcVersion: [1n, 1n, 0n] as const,
        termRoot: root,
        nodeCount:
          shared.envelope.nodeCount +
          BigInt(wrappers.length + overCapEnvelopes.length + 1),
        materialByteLength: reachableBytes,
      };
      aggregateNodeVisits += envelope.nodeCount;
      aggregateByteWork += envelope.materialByteLength;
      overCapEnvelopes.push(envelope);
    }
    expect(aggregateNodeVisits).toBeLessThanOrEqual(
      MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS,
    );
    const traversalTrap = {
      *[Symbol.iterator](): IterableIterator<MidgardCekProgramMaterialEntry> {
        throw new Error("material traversal must not start");
      },
    };
    expect(() =>
      verifyMidgardCekProgramMaterialBundle(overCapEnvelopes, traversalTrap),
    ).toThrow(/aggregate unique-envelope byte work\/result/u);
  }, 10_000);

  it("verifies canonical duplicate envelopes once while preserving positional results", () => {
    const { envelope, material } = makeUnaryProgramMaterial(20_000);
    const envelopes = Array.from({ length: 100 }, () => ({
      ...envelope,
      termRoot: Buffer.from(envelope.termRoot),
    }));
    const startedAt = performance.now();
    const verified = verifyMidgardCekProgramMaterialBundle(envelopes, material);
    const elapsedMs = performance.now() - startedAt;

    expect(verified).toHaveLength(envelopes.length);
    for (const result of verified.slice(1)) {
      expect(result).toBe(verified[0]);
    }
    expect(verified[0]?.nodeCount).toBe(20_000n);
    expect(elapsedMs).toBeLessThan(2_000);
  }, 10_000);

  it("rejects aggregate unique-envelope work before material traversal", () => {
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: hash(0xa1),
      nodeCount: MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS,
      materialByteLength: 1n,
    };
    expect(() =>
      verifyMidgardCekProgramMaterialBundle(
        [
          envelope,
          {
            ...envelope,
            termRoot: hash(0xa2),
            nodeCount: 1n,
          },
        ],
        [],
      ),
    ).toThrow(/aggregate unique-envelope node visits/u);
  });
});
