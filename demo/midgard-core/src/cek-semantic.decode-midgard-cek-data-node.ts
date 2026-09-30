import { blake2b } from "@noble/hashes/blake2.js";

import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

const DATA_NODE_DOMAIN = Buffer.from("MidgardCekDataNodeV1", "ascii");

export const DATA_LIST_NODE_DOMAIN = Buffer.from(
  "MidgardCekDataListNodeV1",
  "ascii",
);

export const DATA_PAIR_NODE_DOMAIN = Buffer.from(
  "MidgardCekDataPairNodeV1",
  "ascii",
);

const UINT32_MAX = 0xffff_ffffn;

export const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export type Bytes = Uint8Array;

export const hash32 = (domain: Uint8Array, preimage: Uint8Array): Hash32 =>
  ensureHash32(
    blake2b(Buffer.concat([Buffer.from(domain), Buffer.from(preimage)]), {
      dkLen: 32,
    }),
    "cek_semantic_hash",
  );

export const exactHash = (value: Bytes, fieldName: string): Buffer =>
  Buffer.from(ensureHash32(value, fieldName));

export const asArray = (
  value: unknown,
  fieldName: string,
): readonly unknown[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${fieldName} must be a CBOR array`);
  }
  return value;
};

export const asBigInt = (value: unknown, fieldName: string): bigint => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number" && Number.isSafeInteger(value)) {
    return BigInt(value);
  }
  throw new Error(`${fieldName} must be a CBOR integer`);
};

export const asBytes = (value: unknown, fieldName: string): Buffer => {
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

export const uint32 = (value: bigint, fieldName: string): bigint =>
  bounded(value, UINT32_MAX, fieldName);

export const uint64 = (value: bigint, fieldName: string): bigint =>
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
