import "./cek-proof.encode-midgard-cek-value-node.js";

import {
  encodeMidgardCekProgramEnvelope,
  type MidgardCekProgramEnvelope,
} from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import {
  BLOB_BRANCH_DOMAIN,
  BLOB_CHUNK_DOMAIN,
  exactHash,
  hash32,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
  MIDGARD_CEK_PROGRAM_ENVELOPE_VERSION,
  MIDGARD_CEK_PROGRAM_UPLC_VERSION,
  PROGRAM_ENVELOPE_DOMAIN,
  SEQUENCE_NODE_DOMAIN,
  TERM_NODE_DOMAIN,
  VALUE_NODE_DOMAIN,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import {
  hashMidgardCekDataListNodePreimage,
  hashMidgardCekDataNodePreimage,
  hashMidgardCekDataPairNodePreimage,
} from "./cek-semantic.js";
import {
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";
import { type Hash32 } from "./codec/hash.js";

/**
 * Decodes the exact V1 consensus payload carried by PlutusV3 and
 * MidgardV1 script witnesses/reference scripts. Raw Flat programs are SDK
 * inputs only and must be canonicalized before transaction construction.
 */
export const decodeMidgardCekProgramEnvelope = (
  bytes: Uint8Array,
): MidgardCekProgramEnvelope => {
  const source = Buffer.from(bytes);
  if (source.length > MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES) {
    throw new Error(
      `CEK program envelope exceeds ${MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES.toString()} bytes`,
    );
  }

  const envelopeHeader = readCborArrayHeader(source, 0, "cek_program_envelope");
  if (envelopeHeader.length !== 5) {
    throw new Error("CEK program envelope must contain exactly five fields");
  }
  const envelopeVersion = readCborUnsigned(
    source,
    envelopeHeader.nextOffset,
    "cek_program_envelope.version",
  );
  if (envelopeVersion.value !== MIDGARD_CEK_PROGRAM_ENVELOPE_VERSION) {
    throw new Error(
      `unsupported CEK program envelope version ${envelopeVersion.value.toString()}`,
    );
  }

  const uplcHeader = readCborArrayHeader(
    source,
    envelopeVersion.nextOffset,
    "cek_program_envelope.uplc_version",
  );
  if (uplcHeader.length !== 3) {
    throw new Error("CEK UPLC version must contain exactly three components");
  }
  const major = readCborUnsigned(
    source,
    uplcHeader.nextOffset,
    "cek_program_envelope.uplc_version.major",
  );
  const minor = readCborUnsigned(
    source,
    major.nextOffset,
    "cek_program_envelope.uplc_version.minor",
  );
  const patch = readCborUnsigned(
    source,
    minor.nextOffset,
    "cek_program_envelope.uplc_version.patch",
  );
  if (
    major.value !== MIDGARD_CEK_PROGRAM_UPLC_VERSION[0] ||
    minor.value !== MIDGARD_CEK_PROGRAM_UPLC_VERSION[1] ||
    patch.value !== MIDGARD_CEK_PROGRAM_UPLC_VERSION[2]
  ) {
    throw new Error(
      `V1 supports only UPLC ${MIDGARD_CEK_PROGRAM_UPLC_VERSION.join(".")}`,
    );
  }

  const termRoot = readCborBytes(
    source,
    patch.nextOffset,
    "cek_program_envelope.term_root",
  );
  const exactTermRoot = exactHash(
    termRoot.value,
    "cek_program_envelope.term_root",
  );
  const nodeCount = readCborUnsigned(
    source,
    termRoot.nextOffset,
    "cek_program_envelope.node_count",
  );
  const materialByteLength = readCborUnsigned(
    source,
    nodeCount.nextOffset,
    "cek_program_envelope.material_byte_length",
  );
  if (materialByteLength.nextOffset !== source.length) {
    throw new Error("CEK program envelope has trailing bytes");
  }
  if (
    nodeCount.value === 0n ||
    nodeCount.value > MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT
  ) {
    throw new Error(
      `CEK program node count must be between 1 and ${MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT.toString()}`,
    );
  }
  if (
    materialByteLength.value === 0n ||
    materialByteLength.value > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES
  ) {
    throw new Error(
      `CEK program material length must be between 1 and ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES.toString()}`,
    );
  }

  const decoded: MidgardCekProgramEnvelope = Object.freeze({
    uplcVersion: MIDGARD_CEK_PROGRAM_UPLC_VERSION,
    termRoot: exactTermRoot,
    nodeCount: nodeCount.value,
    materialByteLength: materialByteLength.value,
  });
  if (!encodeMidgardCekProgramEnvelope(decoded).equals(source)) {
    throw new Error("CEK program envelope CBOR is not canonical");
  }
  return decoded;
};

export const hashMidgardCekProgramEnvelope = (
  envelope: MidgardCekProgramEnvelope,
): Hash32 =>
  hash32(PROGRAM_ENVELOPE_DOMAIN, encodeMidgardCekProgramEnvelope(envelope));

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES =
  MIDGARD_CEK_BLOB_CHUNK_BYTES + 3;

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES =
  1 + 1 + 34 + 3 + MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES;

export const MIDGARD_CEK_PROGRAM_MATERIAL_VERSION = 1n;

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES =
  1 + 1 + 1 + 3 + MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES;

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

/**
 * Exact decoded body of the versioned K09 DA/publication value
 * `[1, kind, preimage]`. The version is implicit in the V1 type and is
 * emitted and checked by the encoder/decoder.
 */
export type MidgardCekProgramMaterialValue = Pick<
  MidgardCekProgramMaterialEntry,
  "kind" | "preimage"
>;

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
