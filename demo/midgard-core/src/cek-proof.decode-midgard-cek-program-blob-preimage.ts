import {
  assertPreimageConsumed,
  type MidgardCekDecodedProgramBlob,
  readExactHashAt,
} from "./cek-proof.decode-midgard-cek-program-material-da-entry.js";
import { encodeMidgardCekBlobChunk } from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import {
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  uint64,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";
import { type Hash32 } from "./codec/hash.js";

export const decodeMidgardCekProgramBlobPreimage = (
  kind: "blobChunk" | "blobBranch",
  preimage: Buffer,
): MidgardCekDecodedProgramBlob => {
  if (kind === "blobChunk") {
    const chunk = readCborBytes(preimage, 0, "cek_program_blob.chunk");
    assertPreimageConsumed(preimage, chunk.nextOffset, "CEK blob chunk");
    if (chunk.value.length > MIDGARD_CEK_BLOB_CHUNK_BYTES) {
      throw new Error(
        `CEK blob chunk exceeds ${MIDGARD_CEK_BLOB_CHUNK_BYTES.toString()} bytes`,
      );
    }
    if (!encodeMidgardCekBlobChunk(chunk.value).equals(preimage)) {
      throw new Error("CEK blob chunk CBOR is not canonical");
    }
    return { kind: "chunk", bytes: chunk.value };
  }

  const header = readCborArrayHeader(preimage, 0, "cek_program_blob.branch");
  if (header.length !== 3) {
    throw new Error("CEK blob branch must contain three fields");
  }
  const left = readExactHashAt(
    preimage,
    header.nextOffset,
    "cek_program_blob.branch.left",
  );
  const right = readExactHashAt(
    preimage,
    left.nextOffset,
    "cek_program_blob.branch.right",
  );
  const byteLength = readCborUnsigned(
    preimage,
    right.nextOffset,
    "cek_program_blob.branch.byte_length",
  );
  uint64(byteLength.value, "cek_program_blob.branch.byte_length");
  assertPreimageConsumed(preimage, byteLength.nextOffset, "CEK blob branch");
  return {
    kind: "branch",
    left: left.value,
    right: right.value,
    byteLength: byteLength.value,
  };
};

export const isProgramMaterialRoot = (
  actual: Uint8Array,
  expected: Uint8Array,
): boolean => Buffer.from(actual).equals(expected);

export const uniqueProgramMaterialRoots = (
  roots: readonly Hash32[],
): readonly Hash32[] => {
  const seen = new Set<string>();
  const unique: Hash32[] = [];
  for (const root of roots) {
    const key = Buffer.from(root).toString("hex");
    if (seen.has(key)) continue;
    seen.add(key);
    unique.push(root);
  }
  return Object.freeze(unique);
};
