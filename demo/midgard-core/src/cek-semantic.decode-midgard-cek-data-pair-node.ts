import {
  asArray,
  asBigInt,
  asBytes,
  type Bytes,
  DATA_LIST_NODE_DOMAIN,
  DATA_PAIR_NODE_DOMAIN,
  exactHash,
  hash32,
  uint32,
  uint64,
  UINT64_MAX,
} from "./cek-semantic.decode-midgard-cek-data-node.js";
import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";
import { type Hash32 } from "./codec/hash.js";

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
