import {
  BLOB_BRANCH_DOMAIN,
  BLOB_CHUNK_DOMAIN,
  exactHash,
  hash32,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  SEQUENCE_NODE_DOMAIN,
  TERM_NODE_DOMAIN,
  VALUE_NODE_DOMAIN,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import {
  hashMidgardCekDataListNodePreimage,
  hashMidgardCekDataNodePreimage,
  hashMidgardCekDataPairNodePreimage,
} from "./cek-semantic.js";
import { encodeCbor } from "./codec/cbor.js";
import { type Hash32 } from "./codec/hash.js";

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES =
  MIDGARD_CEK_BLOB_CHUNK_BYTES + 3;

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES =
  1 + 1 + 34 + 3 + MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES;

export const MidgardCekProgramMaterialKindTags = Object.freeze({
  Term: 0n,
  Value: 1n,
  Sequence: 2n,
  BlobChunk: 3n,
  BlobBranch: 4n,
  DataNode: 5n,
  DataList: 6n,
  DataPair: 7n,
} as const);

export type MidgardCekProgramMaterialKind =
  | "term"
  | "value"
  | "sequence"
  | "blobChunk"
  | "blobBranch"
  | "dataNode"
  | "dataList"
  | "dataPair";

export type MidgardCekProgramMaterialEntry = {
  readonly kind: MidgardCekProgramMaterialKind;
  readonly root: Hash32;
  readonly preimage: Buffer;
};

export const midgardCekProgramMaterialKindTag = (
  kind: MidgardCekProgramMaterialKind,
): bigint => {
  switch (kind) {
    case "term":
      return MidgardCekProgramMaterialKindTags.Term;
    case "value":
      return MidgardCekProgramMaterialKindTags.Value;
    case "sequence":
      return MidgardCekProgramMaterialKindTags.Sequence;
    case "blobChunk":
      return MidgardCekProgramMaterialKindTags.BlobChunk;
    case "blobBranch":
      return MidgardCekProgramMaterialKindTags.BlobBranch;
    case "dataNode":
      return MidgardCekProgramMaterialKindTags.DataNode;
    case "dataList":
      return MidgardCekProgramMaterialKindTags.DataList;
    case "dataPair":
      return MidgardCekProgramMaterialKindTags.DataPair;
  }
};

export const midgardCekProgramMaterialKindFromTag = (
  tag: bigint,
): MidgardCekProgramMaterialKind => {
  switch (tag) {
    case MidgardCekProgramMaterialKindTags.Term:
      return "term";
    case MidgardCekProgramMaterialKindTags.Value:
      return "value";
    case MidgardCekProgramMaterialKindTags.Sequence:
      return "sequence";
    case MidgardCekProgramMaterialKindTags.BlobChunk:
      return "blobChunk";
    case MidgardCekProgramMaterialKindTags.BlobBranch:
      return "blobBranch";
    case MidgardCekProgramMaterialKindTags.DataNode:
      return "dataNode";
    case MidgardCekProgramMaterialKindTags.DataList:
      return "dataList";
    case MidgardCekProgramMaterialKindTags.DataPair:
      return "dataPair";
    default:
      throw new Error(
        `unsupported CEK program material kind ${tag.toString()}`,
      );
  }
};

const materialDomain = (kind: MidgardCekProgramMaterialKind): Buffer => {
  switch (kind) {
    case "term":
      return TERM_NODE_DOMAIN;
    case "value":
      return VALUE_NODE_DOMAIN;
    case "sequence":
      return SEQUENCE_NODE_DOMAIN;
    case "blobChunk":
      return BLOB_CHUNK_DOMAIN;
    case "blobBranch":
      return BLOB_BRANCH_DOMAIN;
    case "dataNode":
    case "dataList":
    case "dataPair":
      throw new Error("CEK semantic material uses its dedicated domain");
  }
};

export const exactMaterialPreimage = (preimage: Uint8Array): Buffer => {
  const exact = Buffer.from(preimage);
  if (exact.length === 0) {
    throw new Error("CEK program material preimage must not be empty");
  }
  if (exact.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES) {
    throw new Error(
      `CEK program material preimage exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES.toString()} bytes`,
    );
  }
  return exact;
};

export const hashMidgardCekProgramMaterialPreimage = (
  kind: MidgardCekProgramMaterialKind,
  preimage: Uint8Array,
): Hash32 => {
  const exact = exactMaterialPreimage(preimage);
  // Data nodes use specialized hashes; all remaining material kinds use the
  // generic domain-separated hash in the default arm.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (kind) {
    case "dataNode":
      return hashMidgardCekDataNodePreimage(exact);
    case "dataList":
      return hashMidgardCekDataListNodePreimage(exact);
    case "dataPair":
      return hashMidgardCekDataPairNodePreimage(exact);
    default:
      return hash32(materialDomain(kind), exact);
  }
};

/**
 * Canonical one-node proof witness. The root is repeated deliberately so an
 * independently revealed entry is self-authenticating before graph traversal.
 */
export const encodeMidgardCekProgramMaterialEntry = (
  entry: MidgardCekProgramMaterialEntry,
): Buffer => {
  const encoded = encodeCbor([
    midgardCekProgramMaterialKindTag(entry.kind),
    exactHash(entry.root, "cek_program_material.root"),
    exactMaterialPreimage(entry.preimage),
  ]);
  if (encoded.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES) {
    throw new Error(
      `CEK program material entry exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES.toString()} bytes`,
    );
  }
  return encoded;
};
