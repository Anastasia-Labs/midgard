/**
 * Root markers, LevelDB batch application, and overlay digests shared by the stores.
 */

import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";

import { type ParkedMpfOverlay } from "./engine-config.js";
import { type MpfReadableValue, type MpfStoredValue } from "./types.js";

export const consumeMpfMutationProof =
  Trie.prototype.consumeMidgardMutationProof;

export const ROOT_KEY = "__root__";

export const JSON_LEVEL_ENCODING_OPTS = { valueEncoding: "json" as const };

export const MPF_EMPTY_ROOT_HEX = SDK.EMPTY_MERKLE_TREE_ROOT;

export const MPF_EMPTY_ROOT = Buffer.from(MPF_EMPTY_ROOT_HEX, "hex");

export const MPF_INTERNAL_NULL_ROOT_HEX = "00".repeat(32);

export const MPF_INTERNAL_NULL_ROOT = Buffer.alloc(32);

export const exactArrayBuffer = (bytes: Uint8Array): ArrayBuffer =>
  bytes.buffer.slice(
    bytes.byteOffset,
    bytes.byteOffset + bytes.byteLength,
  ) as ArrayBuffer;

export const parkedOverlayDigest = ({
  trieName,
  baseRoot,
  candidateRoot,
  nodeCount,
  nodeHashes,
  nodeValues,
  nodeValueOffsets,
}: Omit<
  ParkedMpfOverlay,
  "schemaVersion" | "closureDigest" | "encodedBytes"
>): Buffer => {
  const trieNameBytes = Buffer.from(trieName);
  const header = Buffer.alloc(8);
  header.writeUInt32BE(trieNameBytes.length, 0);
  header.writeUInt32BE(nodeCount, 4);
  const hash = blake2b.create({ dkLen: 32 });
  hash.update(header);
  hash.update(trieNameBytes);
  hash.update(new Uint8Array(baseRoot));
  hash.update(new Uint8Array(candidateRoot));
  hash.update(new Uint8Array(nodeHashes));
  hash.update(new Uint8Array(nodeValueOffsets));
  hash.update(new Uint8Array(nodeValues));
  return Buffer.from(hash.digest());
};

export type LevelBatchOp =
  | {
      readonly type: "put";
      readonly key: string;
      readonly value: MpfStoredValue;
      readonly encodedBytes?: number;
    }
  | { readonly type: "del"; readonly key: string };

const normalizeRootMarkerHex = (rootHex: string, fieldName: string): string =>
  normalizeHex(rootHex, { fieldName, byteLength: 32 });

export const normalizeStoredRootHex = (rootHex: string): string => {
  const normalized = normalizeRootMarkerHex(rootHex, "MPF root marker");
  return normalized === MPF_INTERNAL_NULL_ROOT_HEX
    ? MPF_EMPTY_ROOT_HEX
    : normalized;
};

export const parseStoredRootHex = (rootHex: unknown): Buffer => {
  if (rootHex === undefined) {
    return MPF_EMPTY_ROOT;
  }
  if (typeof rootHex !== "string") {
    throw new Error("Persisted MPF root marker is not a string");
  }
  const normalized = normalizeRootMarkerHex(
    rootHex,
    "Persisted MPF root marker",
  );
  if (normalized === MPF_INTERNAL_NULL_ROOT_HEX) {
    throw new Error(
      "Persisted MPF root marker uses the library internal null root instead of the canonical Midgard empty root",
    );
  }
  return Buffer.from(normalized, "hex");
};

export const applyPendingBatch = (
  key: string,
  value: MpfReadableValue | undefined,
  ops: readonly LevelBatchOp[] | undefined,
): MpfReadableValue | undefined =>
  (ops ?? []).reduce<MpfReadableValue | undefined>((current, op) => {
    if (op.key !== key) {
      return current;
    }
    return op.type === "put" ? op.value : undefined;
  }, value);
