import {
  type Hash32,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";

import type { MidgardCekConstantValueWitness } from "./cek-builtin.js";

export const MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT =
  MIDGARD_CONSENSUS_LIMITS.maxCekProgramNodeCount;

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxCekProgramMaterialBytes;

export type MidgardCekProgramMaterialKind =
  MidgardCekProgramMaterialEntry["kind"];

export type MidgardCekProgramMaterialNode = MidgardCekProgramMaterialEntry;

export type MidgardCanonicalCekProgram = {
  readonly envelope: MidgardCekProgramEnvelope;
  readonly envelopeCbor: Buffer;
  readonly envelopeHash: Hash32;
  readonly material: ReadonlyMap<string, MidgardCekProgramMaterialNode>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitness
  >;
};

export type MidgardCanonicalScriptArtifactLanguage = "PlutusV3" | "MidgardV1";

export type MidgardCanonicalScriptArtifactInput = {
  readonly language: MidgardCanonicalScriptArtifactLanguage;
  readonly sourceRawFlatProgramBytes: Uint8Array;
};

/**
 * A canonical script authoring result with deliberately distinct source and
 * consensus identities. The source hash is audit/remapping metadata only;
 * credentials must use canonicalMidgardCredentialScriptHash.
 *
 * Byte-bearing accessors return defensive values so callers cannot mutate the
 * artifact or create aliases between its script, program, material, or sidecar
 * representations.
 */
export type MidgardCanonicalScriptArtifact = {
  readonly canonicalMidgardCredentialScript: MidgardVersionedScript;
  readonly canonicalMidgardCredentialScriptHash: string;
  readonly sourceRawScriptAuditHash: string;
  readonly canonicalProgram: MidgardCanonicalCekProgram;
  readonly canonicalMaterialEntries: readonly MidgardCekProgramMaterialEntry[];
  readonly canonicalMaterialSidecarCbor: Buffer;
};

export const rootHex = (root: Uint8Array): string =>
  Buffer.from(root).toString("hex");

export const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const unwrapCanonicalCborByteString = (bytes: Buffer): Buffer | null => {
  if (bytes.length === 0 || bytes[0]! >> 5 !== 2) return null;
  const additional = bytes[0]! & 0x1f;
  let headerLength = 1;
  let payloadLength: bigint;
  if (additional < 24) {
    payloadLength = BigInt(additional);
  } else if (additional === 24) {
    if (bytes.length < 2 || bytes[1]! < 24) return null;
    headerLength = 2;
    payloadLength = BigInt(bytes[1]!);
  } else if (additional === 25) {
    if (bytes.length < 3) return null;
    const length = bytes.readUInt16BE(1);
    if (length <= 0xff) return null;
    headerLength = 3;
    payloadLength = BigInt(length);
  } else if (additional === 26) {
    if (bytes.length < 5) return null;
    const length = bytes.readUInt32BE(1);
    if (length <= 0xffff) return null;
    headerLength = 5;
    payloadLength = BigInt(length);
  } else if (additional === 27) {
    if (bytes.length < 9) return null;
    const length = bytes.readBigUInt64BE(1);
    if (length <= 0xffff_ffffn) return null;
    headerLength = 9;
    payloadLength = length;
  } else {
    return null;
  }
  if (
    payloadLength > BigInt(Number.MAX_SAFE_INTEGER) ||
    BigInt(headerLength) + payloadLength !== BigInt(bytes.length)
  ) {
    return null;
  }
  return bytes.subarray(headerLength, headerLength + Number(payloadLength));
};

export const canonicalFlatProgramBytes = (scriptBytes: Buffer): Buffer => {
  let flat = scriptBytes;
  for (;;) {
    const unwrapped = unwrapCanonicalCborByteString(flat);
    if (unwrapped === null) break;
    flat = unwrapped;
  }
  return flat;
};
