import { blake2b } from "@noble/hashes/blake2.js";

import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

const DATA_NODE_DOMAIN = Buffer.from("MidgardCekDataNodeV1", "ascii");
const DATA_LIST_NODE_DOMAIN = Buffer.from("MidgardCekDataListNodeV1", "ascii");
const DATA_PAIR_NODE_DOMAIN = Buffer.from("MidgardCekDataPairNodeV1", "ascii");

const UINT32_MAX = 0xffff_ffffn;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

type Bytes = Uint8Array;

const hash32 = (domain: Uint8Array, preimage: Uint8Array): Hash32 =>
  ensureHash32(
    blake2b(Buffer.concat([Buffer.from(domain), Buffer.from(preimage)]), {
      dkLen: 32,
    }),
    "cek_semantic_hash",
  );

const exactHash = (value: Bytes, fieldName: string): Buffer =>
  Buffer.from(ensureHash32(value, fieldName));

const asArray = (value: unknown, fieldName: string): readonly unknown[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${fieldName} must be a CBOR array`);
  }
  return value;
};

const asBigInt = (value: unknown, fieldName: string): bigint => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number" && Number.isSafeInteger(value)) {
    return BigInt(value);
  }
  throw new Error(`${fieldName} must be a CBOR integer`);
};

const asBytes = (value: unknown, fieldName: string): Buffer => {
  if (!(value instanceof Uint8Array)) {
    throw new Error(`${fieldName} must be CBOR bytes`);
  }
  return Buffer.from(value);
};

const bounded = (value: bigint, maximum: bigint, fieldName: string): bigint => {
  if (value < 0n || value > maximum) {
    throw new RangeError(
      `${fieldName} must be between 0 and ${maximum.toString(10)}`,
    );
  }
  return value;
};

const uint32 = (value: bigint, fieldName: string): bigint =>
  bounded(value, UINT32_MAX, fieldName);

const uint64 = (value: bigint, fieldName: string): bigint =>
  bounded(value, UINT64_MAX, fieldName);

export const MidgardCekDataNodeTags = Object.freeze({
  ConstrSmall: 0n,
  ConstrLarge: 1n,
  Map: 2n,
  List: 3n,
  Integer: 4n,
  Bytes: 5n,
} as const);

/**
 * A semantic Plutus Data node. Every preimage is fixed-size except for the
 * canonical CBOR integer and raw byte payloads, which are referenced by the
 * existing chunked CEK blob commitment.
 *
 * `cborLength` is the exact cardano-node `serialiseData` byte length and
 * `memory` is the exact CEK ExMemory size of the complete Data subtree.
 */
export type MidgardCekDataNode =
  | {
      readonly kind: "constrSmall";
      /** Constructor alternatives 0..127 fit directly in every proof. */
      readonly constructor: bigint;
      readonly fieldsCount: bigint;
      readonly fieldsRoot: Bytes;
      readonly cborLength: bigint;
      readonly memory: bigint;
    }
  | {
      readonly kind: "constrLarge";
      /**
       * Canonical CBOR integer encoding of an alternative above 127. It is
       * chunked so an otherwise valid large constructor is not capped by one
       * fault-proof transaction.
       */
      readonly constructorCborRoot: Bytes;
      readonly constructorCborLength: bigint;
      readonly constructorMemory: bigint;
      readonly fieldsCount: bigint;
      readonly fieldsRoot: Bytes;
      readonly cborLength: bigint;
      readonly memory: bigint;
    }
  | {
      readonly kind: "map";
      readonly entriesCount: bigint;
      readonly entriesRoot: Bytes;
      readonly cborLength: bigint;
      readonly memory: bigint;
    }
  | {
      readonly kind: "list";
      readonly itemsCount: bigint;
      readonly itemsRoot: Bytes;
      readonly cborLength: bigint;
      readonly memory: bigint;
    }
  | {
      readonly kind: "integer";
      /** Canonical CBOR encoding of the integer Data leaf. */
      readonly cborRoot: Bytes;
      readonly cborLength: bigint;
      readonly memory: bigint;
    }
  | {
      readonly kind: "bytes";
      /** Raw byte payload, without its Cardano CBOR bytestring framing. */
      readonly bytesRoot: Bytes;
      readonly bytesLength: bigint;
      readonly cborLength: bigint;
      readonly memory: bigint;
    };

export const encodeMidgardCekDataNode = (node: MidgardCekDataNode): Buffer => {
  switch (node.kind) {
    case "constrSmall":
      if (node.constructor < 0n || node.constructor > 127n) {
        throw new RangeError(
          "cek_data.constr_small.constructor must be between 0 and 127",
        );
      }
      return encodeCbor([
        MidgardCekDataNodeTags.ConstrSmall,
        node.constructor,
        uint32(node.fieldsCount, "cek_data.constr.fields_count"),
        exactHash(node.fieldsRoot, "cek_data.constr.fields_root"),
        uint64(node.cborLength, "cek_data.constr.cbor_length"),
        uint64(node.memory, "cek_data.constr.memory"),
      ]);
    case "constrLarge":
      return encodeCbor([
        MidgardCekDataNodeTags.ConstrLarge,
        exactHash(
          node.constructorCborRoot,
          "cek_data.constr_large.constructor_cbor_root",
        ),
        uint32(
          node.constructorCborLength,
          "cek_data.constr_large.constructor_cbor_length",
        ),
        uint64(
          node.constructorMemory,
          "cek_data.constr_large.constructor_memory",
        ),
        uint32(node.fieldsCount, "cek_data.constr.fields_count"),
        exactHash(node.fieldsRoot, "cek_data.constr.fields_root"),
        uint64(node.cborLength, "cek_data.constr.cbor_length"),
        uint64(node.memory, "cek_data.constr.memory"),
      ]);
    case "map":
      return encodeCbor([
        MidgardCekDataNodeTags.Map,
        uint32(node.entriesCount, "cek_data.map.entries_count"),
        exactHash(node.entriesRoot, "cek_data.map.entries_root"),
        uint64(node.cborLength, "cek_data.map.cbor_length"),
        uint64(node.memory, "cek_data.map.memory"),
      ]);
    case "list":
      return encodeCbor([
        MidgardCekDataNodeTags.List,
        uint32(node.itemsCount, "cek_data.list.items_count"),
        exactHash(node.itemsRoot, "cek_data.list.items_root"),
        uint64(node.cborLength, "cek_data.list.cbor_length"),
        uint64(node.memory, "cek_data.list.memory"),
      ]);
    case "integer":
      return encodeCbor([
        MidgardCekDataNodeTags.Integer,
        exactHash(node.cborRoot, "cek_data.integer.cbor_root"),
        uint32(node.cborLength, "cek_data.integer.cbor_length"),
        uint64(node.memory, "cek_data.integer.memory"),
      ]);
    case "bytes":
      return encodeCbor([
        MidgardCekDataNodeTags.Bytes,
        exactHash(node.bytesRoot, "cek_data.bytes.bytes_root"),
        uint32(node.bytesLength, "cek_data.bytes.bytes_length"),
        uint32(node.cborLength, "cek_data.bytes.cbor_length"),
        uint64(node.memory, "cek_data.bytes.memory"),
      ]);
  }
};

export const hashMidgardCekDataNode = (node: MidgardCekDataNode): Hash32 =>
  hash32(DATA_NODE_DOMAIN, encodeMidgardCekDataNode(node));

export const hashMidgardCekDataNodePreimage = (preimage: Bytes): Hash32 =>
  hash32(DATA_NODE_DOMAIN, preimage);

export const decodeMidgardCekDataNode = (
  preimage: Bytes,
): MidgardCekDataNode => {
  const source = Buffer.from(preimage);
  const fields = asArray(decodeSingleCbor(source), "cek_data_node");
  const tag = asBigInt(fields[0], "cek_data_node.tag");
  let node: MidgardCekDataNode;
  if (tag === MidgardCekDataNodeTags.ConstrSmall) {
    if (fields.length !== 6) {
      throw new Error("CEK small constructor node must have six fields");
    }
    node = {
      kind: "constrSmall",
      constructor: asBigInt(fields[1], "cek_data_node.constructor"),
      fieldsCount: asBigInt(fields[2], "cek_data_node.fields_count"),
      fieldsRoot: asBytes(fields[3], "cek_data_node.fields_root"),
      cborLength: asBigInt(fields[4], "cek_data_node.cbor_length"),
      memory: asBigInt(fields[5], "cek_data_node.memory"),
    };
  } else if (tag === MidgardCekDataNodeTags.ConstrLarge) {
    if (fields.length !== 8) {
      throw new Error("CEK large constructor node must have eight fields");
    }
    node = {
      kind: "constrLarge",
      constructorCborRoot: asBytes(
        fields[1],
        "cek_data_node.constructor_cbor_root",
      ),
      constructorCborLength: asBigInt(
        fields[2],
        "cek_data_node.constructor_cbor_length",
      ),
      constructorMemory: asBigInt(
        fields[3],
        "cek_data_node.constructor_memory",
      ),
      fieldsCount: asBigInt(fields[4], "cek_data_node.fields_count"),
      fieldsRoot: asBytes(fields[5], "cek_data_node.fields_root"),
      cborLength: asBigInt(fields[6], "cek_data_node.cbor_length"),
      memory: asBigInt(fields[7], "cek_data_node.memory"),
    };
  } else if (tag === MidgardCekDataNodeTags.Map) {
    if (fields.length !== 5) {
      throw new Error("CEK map Data node must have five fields");
    }
    node = {
      kind: "map",
      entriesCount: asBigInt(fields[1], "cek_data_node.entries_count"),
      entriesRoot: asBytes(fields[2], "cek_data_node.entries_root"),
      cborLength: asBigInt(fields[3], "cek_data_node.cbor_length"),
      memory: asBigInt(fields[4], "cek_data_node.memory"),
    };
  } else if (tag === MidgardCekDataNodeTags.List) {
    if (fields.length !== 5) {
      throw new Error("CEK list Data node must have five fields");
    }
    node = {
      kind: "list",
      itemsCount: asBigInt(fields[1], "cek_data_node.items_count"),
      itemsRoot: asBytes(fields[2], "cek_data_node.items_root"),
      cborLength: asBigInt(fields[3], "cek_data_node.cbor_length"),
      memory: asBigInt(fields[4], "cek_data_node.memory"),
    };
  } else if (tag === MidgardCekDataNodeTags.Integer) {
    if (fields.length !== 4) {
      throw new Error("CEK integer Data node must have four fields");
    }
    node = {
      kind: "integer",
      cborRoot: asBytes(fields[1], "cek_data_node.cbor_root"),
      cborLength: asBigInt(fields[2], "cek_data_node.cbor_length"),
      memory: asBigInt(fields[3], "cek_data_node.memory"),
    };
  } else if (tag === MidgardCekDataNodeTags.Bytes) {
    if (fields.length !== 5) {
      throw new Error("CEK bytes Data node must have five fields");
    }
    node = {
      kind: "bytes",
      bytesRoot: asBytes(fields[1], "cek_data_node.bytes_root"),
      bytesLength: asBigInt(fields[2], "cek_data_node.bytes_length"),
      cborLength: asBigInt(fields[3], "cek_data_node.cbor_length"),
      memory: asBigInt(fields[4], "cek_data_node.memory"),
    };
  } else {
    throw new Error(`unsupported CEK Data node tag ${tag.toString()}`);
  }
  if (!encodeMidgardCekDataNode(node).equals(source)) {
    throw new Error("CEK Data node CBOR is not canonical");
  }
  return Object.freeze(node);
};

/**
 * Authenticated list summary used for constructor fields and Data list
 * elements. The cumulative summaries make parent-node length and memory
 * checks local: one head Data node plus one tail summary is enough.
 */
export type MidgardCekDataListNode = {
  readonly head: Bytes;
  readonly headCborLength: bigint;
  readonly headMemory: bigint;
  readonly tail: Bytes;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

export const encodeMidgardCekDataListNode = (
  node: MidgardCekDataListNode,
): Buffer =>
  encodeCbor([
    exactHash(node.head, "cek_data_list.head"),
    uint32(node.headCborLength, "cek_data_list.head_cbor_length"),
    uint64(node.headMemory, "cek_data_list.head_memory"),
    exactHash(node.tail, "cek_data_list.tail"),
    uint32(node.length, "cek_data_list.length"),
    uint64(node.payloadCborLength, "cek_data_list.payload_cbor_length"),
    uint64(node.memory, "cek_data_list.memory"),
  ]);

export const hashMidgardCekDataListNode = (
  node: MidgardCekDataListNode,
): Hash32 => hash32(DATA_LIST_NODE_DOMAIN, encodeMidgardCekDataListNode(node));

export const hashMidgardCekDataListNodePreimage = (preimage: Bytes): Hash32 =>
  hash32(DATA_LIST_NODE_DOMAIN, preimage);

export const decodeMidgardCekDataListNode = (
  preimage: Bytes,
): MidgardCekDataListNode => {
  const source = Buffer.from(preimage);
  const fields = asArray(decodeSingleCbor(source), "cek_data_list_node");
  if (fields.length !== 7) {
    throw new Error("CEK Data list node must have seven fields");
  }
  const node = {
    head: asBytes(fields[0], "cek_data_list.head"),
    headCborLength: asBigInt(fields[1], "cek_data_list.head_cbor_length"),
    headMemory: asBigInt(fields[2], "cek_data_list.head_memory"),
    tail: asBytes(fields[3], "cek_data_list.tail"),
    length: asBigInt(fields[4], "cek_data_list.length"),
    payloadCborLength: asBigInt(fields[5], "cek_data_list.payload_cbor_length"),
    memory: asBigInt(fields[6], "cek_data_list.memory"),
  } satisfies MidgardCekDataListNode;
  if (!encodeMidgardCekDataListNode(node).equals(source)) {
    throw new Error("CEK Data list node CBOR is not canonical");
  }
  return Object.freeze(node);
};

/**
 * Authenticated map-entry summary. Map ordering remains the original Plutus
 * Data order; no host-language map sorting is introduced.
 */
export type MidgardCekDataPairNode = {
  readonly key: Bytes;
  readonly keyCborLength: bigint;
  readonly keyMemory: bigint;
  readonly value: Bytes;
  readonly valueCborLength: bigint;
  readonly valueMemory: bigint;
  readonly tail: Bytes;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

export const encodeMidgardCekDataPairNode = (
  node: MidgardCekDataPairNode,
): Buffer =>
  encodeCbor([
    exactHash(node.key, "cek_data_pair.key"),
    uint32(node.keyCborLength, "cek_data_pair.key_cbor_length"),
    uint64(node.keyMemory, "cek_data_pair.key_memory"),
    exactHash(node.value, "cek_data_pair.value"),
    uint32(node.valueCborLength, "cek_data_pair.value_cbor_length"),
    uint64(node.valueMemory, "cek_data_pair.value_memory"),
    exactHash(node.tail, "cek_data_pair.tail"),
    uint32(node.length, "cek_data_pair.length"),
    uint64(node.payloadCborLength, "cek_data_pair.payload_cbor_length"),
    uint64(node.memory, "cek_data_pair.memory"),
  ]);

export const hashMidgardCekDataPairNode = (
  node: MidgardCekDataPairNode,
): Hash32 => hash32(DATA_PAIR_NODE_DOMAIN, encodeMidgardCekDataPairNode(node));

export const hashMidgardCekDataPairNodePreimage = (preimage: Bytes): Hash32 =>
  hash32(DATA_PAIR_NODE_DOMAIN, preimage);

export const decodeMidgardCekDataPairNode = (
  preimage: Bytes,
): MidgardCekDataPairNode => {
  const source = Buffer.from(preimage);
  const fields = asArray(decodeSingleCbor(source), "cek_data_pair_node");
  if (fields.length !== 10) {
    throw new Error("CEK Data pair node must have ten fields");
  }
  const node = {
    key: asBytes(fields[0], "cek_data_pair.key"),
    keyCborLength: asBigInt(fields[1], "cek_data_pair.key_cbor_length"),
    keyMemory: asBigInt(fields[2], "cek_data_pair.key_memory"),
    value: asBytes(fields[3], "cek_data_pair.value"),
    valueCborLength: asBigInt(fields[4], "cek_data_pair.value_cbor_length"),
    valueMemory: asBigInt(fields[5], "cek_data_pair.value_memory"),
    tail: asBytes(fields[6], "cek_data_pair.tail"),
    length: asBigInt(fields[7], "cek_data_pair.length"),
    payloadCborLength: asBigInt(fields[8], "cek_data_pair.payload_cbor_length"),
    memory: asBigInt(fields[9], "cek_data_pair.memory"),
  } satisfies MidgardCekDataPairNode;
  if (!encodeMidgardCekDataPairNode(node).equals(source)) {
    throw new Error("CEK Data pair node CBOR is not canonical");
  }
  return Object.freeze(node);
};

export const MIDGARD_CEK_EMPTY_DATA_LIST_ROOT = hash32(
  DATA_LIST_NODE_DOMAIN,
  encodeCbor([]),
);

export const MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT = hash32(
  DATA_PAIR_NODE_DOMAIN,
  encodeCbor([]),
);

export const midgardCekDataListCborLength = (
  length: bigint,
  payloadCborLength: bigint,
): bigint => {
  uint32(length, "cek_data_list.length");
  uint64(payloadCborLength, "cek_data_list.payload_cbor_length");
  return length === 0n ? 1n : 2n + payloadCborLength;
};

export const midgardCekDataMapCborLength = (
  length: bigint,
  payloadCborLength: bigint,
): bigint => {
  uint32(length, "cek_data_map.length");
  uint64(payloadCborLength, "cek_data_map.payload_cbor_length");
  const headerLength =
    length < 24n ? 1n : length <= 0xffn ? 2n : length <= 0xffffn ? 3n : 5n;
  return headerLength + payloadCborLength;
};

const unsignedCborLength = (value: bigint): bigint => {
  if (value < 0n) {
    throw new RangeError("cbor.unsigned must be non-negative");
  }
  if (value < 24n) return 1n;
  if (value <= 0xffn) return 2n;
  if (value <= 0xffffn) return 3n;
  if (value <= 0xffff_ffffn) return 5n;
  if (value <= UINT64_MAX) return 9n;
  const magnitudeBytes = BigInt(Math.ceil(value.toString(2).length / 8));
  // Positive-bignum tag 2 followed by its shortest magnitude bytestring.
  return 1n + definiteBytesHeaderLength(magnitudeBytes) + magnitudeBytes;
};

export const midgardCekDataConstrCborLength = (
  constructor: bigint,
  fieldsLength: bigint,
  fieldsPayloadCborLength: bigint,
): bigint => {
  if (constructor < 0n) {
    throw new RangeError("cek_data.constr.constructor must be non-negative");
  }
  const listLength = midgardCekDataListCborLength(
    fieldsLength,
    fieldsPayloadCborLength,
  );
  if (constructor <= 6n) return 2n + listLength;
  if (constructor <= 127n) return 3n + listLength;
  // Tag 102, a definite pair, the constructor integer, then fields.
  return 3n + unsignedCborLength(constructor) + listLength;
};

const definiteBytesHeaderLength = (length: bigint): bigint => {
  uint32(length, "cek_data.bytes.bytes_length");
  if (length < 24n) return 1n;
  if (length <= 0xffn) return 2n;
  if (length <= 0xffffn) return 3n;
  return 5n;
};

/**
 * Exact Cardano Plutus-Data bytestring length. Values above 64 bytes use the
 * ledger's indefinite bytestring form with canonical 64-byte chunks.
 */
export const midgardCekDataBytesCborLength = (bytesLength: bigint): bigint => {
  uint32(bytesLength, "cek_data.bytes.bytes_length");
  if (bytesLength <= 64n) {
    return definiteBytesHeaderLength(bytesLength) + bytesLength;
  }
  const fullChunks = bytesLength / 64n;
  const remainder = bytesLength % 64n;
  const fullChunkBytes = fullChunks * (2n + 64n);
  const remainderBytes =
    remainder === 0n ? 0n : definiteBytesHeaderLength(remainder) + remainder;
  // Indefinite bytestring start and break.
  return 2n + fullChunkBytes + remainderBytes;
};

export const midgardCekDataBytesMemory = (bytesLength: bigint): bigint => {
  uint32(bytesLength, "cek_data.bytes.bytes_length");
  return 4n + (bytesLength === 0n ? 1n : bytesLength);
};

export type MidgardCekDataSummary = {
  readonly root: Bytes;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardCekDataSequenceSummary = {
  readonly root: Bytes;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

export const emptyMidgardCekDataListSummary =
  (): MidgardCekDataSequenceSummary => ({
    root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
    length: 0n,
    payloadCborLength: 0n,
    memory: 0n,
  });

export const prependMidgardCekDataListSummary = (
  head: MidgardCekDataSummary,
  tail: MidgardCekDataSequenceSummary,
): MidgardCekDataSequenceSummary => {
  const node: MidgardCekDataListNode = {
    head: head.root,
    headCborLength: head.cborLength,
    headMemory: head.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength: head.cborLength + tail.payloadCborLength,
    memory: head.memory + tail.memory,
  };
  return {
    root: hashMidgardCekDataListNode(node),
    length: node.length,
    payloadCborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

export const emptyMidgardCekDataPairSummary =
  (): MidgardCekDataSequenceSummary => ({
    root: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
    length: 0n,
    payloadCborLength: 0n,
    memory: 0n,
  });

export const prependMidgardCekDataPairSummary = (
  key: MidgardCekDataSummary,
  value: MidgardCekDataSummary,
  tail: MidgardCekDataSequenceSummary,
): MidgardCekDataSequenceSummary => {
  const node: MidgardCekDataPairNode = {
    key: key.root,
    keyCborLength: key.cborLength,
    keyMemory: key.memory,
    value: value.root,
    valueCborLength: value.cborLength,
    valueMemory: value.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength:
      key.cborLength + value.cborLength + tail.payloadCborLength,
    memory: key.memory + value.memory + tail.memory,
  };
  return {
    root: hashMidgardCekDataPairNode(node),
    length: node.length,
    payloadCborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

const summarizeMidgardCekDataNode = (
  node: MidgardCekDataNode,
): MidgardCekDataSummary => ({
  root: hashMidgardCekDataNode(node),
  cborLength: node.cborLength,
  memory: node.memory,
});

export const summarizeMidgardCekSmallConstrData = (
  constructor: bigint,
  fields: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary =>
  summarizeMidgardCekDataNode({
    kind: "constrSmall",
    constructor,
    fieldsCount: fields.length,
    fieldsRoot: fields.root,
    cborLength: midgardCekDataConstrCborLength(
      constructor,
      fields.length,
      fields.payloadCborLength,
    ),
    memory: 4n + fields.memory,
  });

/**
 * Builds the semantic node for a constructor above 127 without materializing
 * its arbitrary-size alternative on L1. The authenticated integer submachine
 * supplies the canonical CBOR root, exact length, and exact integer memory.
 */
export const summarizeMidgardCekLargeConstrData = ({
  constructorCborRoot,
  constructorCborLength,
  constructorMemory,
  fields,
}: {
  readonly constructorCborRoot: Bytes;
  readonly constructorCborLength: bigint;
  readonly constructorMemory: bigint;
  readonly fields: MidgardCekDataSequenceSummary;
}): MidgardCekDataSummary => {
  exactHash(constructorCborRoot, "cek_data.constr_large.constructor_cbor_root");
  uint32(
    constructorCborLength,
    "cek_data.constr_large.constructor_cbor_length",
  );
  uint64(constructorMemory, "cek_data.constr_large.constructor_memory");
  if (constructorCborLength === 0n) {
    throw new RangeError(
      "cek_data.constr_large.constructor_cbor_length must be positive",
    );
  }
  if (constructorMemory < 5n) {
    throw new RangeError(
      "cek_data.constr_large.constructor_memory must be at least 5",
    );
  }
  const fieldsCborLength = midgardCekDataListCborLength(
    fields.length,
    fields.payloadCborLength,
  );
  return summarizeMidgardCekDataNode({
    kind: "constrLarge",
    constructorCborRoot,
    constructorCborLength,
    constructorMemory,
    fieldsCount: fields.length,
    fieldsRoot: fields.root,
    cborLength: 3n + constructorCborLength + fieldsCborLength,
    memory: 4n + fields.memory,
  });
};

export const summarizeMidgardCekListData = (
  items: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary =>
  summarizeMidgardCekDataNode({
    kind: "list",
    itemsCount: items.length,
    itemsRoot: items.root,
    cborLength: midgardCekDataListCborLength(
      items.length,
      items.payloadCborLength,
    ),
    memory: 4n + items.memory,
  });

export const summarizeMidgardCekMapData = (
  entries: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary =>
  summarizeMidgardCekDataNode({
    kind: "map",
    entriesCount: entries.length,
    entriesRoot: entries.root,
    cborLength: midgardCekDataMapCborLength(
      entries.length,
      entries.payloadCborLength,
    ),
    memory: 4n + entries.memory,
  });
