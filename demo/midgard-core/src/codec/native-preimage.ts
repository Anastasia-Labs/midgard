/**
 * Binary preimage codecs for the proof-critical fields of a Midgard native tx.
 *
 * Each preimage is a `u64` length-prefixed list of fixed-shape or
 * variable-shape entries (fuel-vm static/dynamic split). The list head sits in
 * the static section; element bytes — and any trailing alignment padding —
 * live in the dynamic section.
 *
 * Layouts (mirroring midgard-ts/staging):
 *
 *   OutputReference:    tx_id(32) + index(u16 padded to 8) = 40 bytes
 *   Hash28:             28 bytes + 4 zero pad = 32 bytes
 *   VKeyWitness:        vkey(32) + signature(64) = 96 bytes
 *   Mint:               outer_len + per-policy (hash28 + inner_len +
 *                       per-asset (name_len + i64 amount));
 *                       asset name bytes + pad in the dynamic section.
 *   VersionedScript:    per entry: language_tag(u64) + payload_len(u64) +
 *                       payload_bytes + pad. Inner payload is opaque
 *                       (NativeCardano / MidgardV1: CBOR; PlutusV3: raw UPLC).
 *   Redeemers:          a single opaque blob (Cardano CBOR) — stored verbatim
 *                       inside the canonical preimage slot, no inner wrapping.
 *
 * The "Cbor" suffix is intentionally absent — none of these wrap CBOR.
 */

import {
  BinaryReader,
  BinaryWriter,
  ensureNoTrailingBytes,
  readBigI64,
  readBigU64,
  readHash28,
  readHash32,
  readU16,
  readU64,
  readVarBytesDynamic,
  readVarBytesLen,
  writeBigI64,
  writeBigU64,
  writeHash28,
  writeHash32,
  writeU16,
  writeU64,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHash32, type Hash32 } from "./hash.js";

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const ensureLen28 = (bytes: Uint8Array, fieldName: string): Buffer => {
  if (bytes.length !== 28) {
    fail(`${fieldName} must be 28 bytes`, `length=${bytes.length}`);
  }
  return Buffer.from(bytes);
};

const ensureLen32 = (bytes: Uint8Array, fieldName: string): Buffer => {
  if (bytes.length !== 32) {
    fail(`${fieldName} must be 32 bytes`, `length=${bytes.length}`);
  }
  return Buffer.from(bytes);
};

const ensureLen64 = (bytes: Uint8Array, fieldName: string): Buffer => {
  if (bytes.length !== 64) {
    fail(`${fieldName} must be 64 bytes`, `length=${bytes.length}`);
  }
  return Buffer.from(bytes);
};

// ---------------------------------------------------------------------------
// OutputReference list — used for spend_inputs and reference_inputs.
// ---------------------------------------------------------------------------

export type MidgardOutputReference = {
  readonly txId: Buffer; // 32 bytes
  readonly index: number; // u16
};

export const encodeMidgardOutputReferenceListPreimage = (
  refs: readonly MidgardOutputReference[],
): Buffer => {
  const w = new BinaryWriter();
  writeU64(w, refs.length);
  for (let i = 0; i < refs.length; i++) {
    const ref = refs[i];
    writeHash32(w, ensureLen32(ref.txId, `output_reference[${i}].tx_id`));
    writeU16(w, ref.index);
  }
  return w.toBytes();
};

export const decodeMidgardOutputReferenceListPreimage = (
  bytes: Uint8Array,
  fieldName = "output_reference_list",
): MidgardOutputReference[] => {
  const r = new BinaryReader(bytes);
  const len = readU64(r);
  const refs: MidgardOutputReference[] = [];
  for (let i = 0; i < len; i++) {
    const txId = readHash32(r);
    const index = readU16(r);
    refs.push({ txId, index });
  }
  ensureNoTrailingBytes(r, fieldName);
  return refs;
};

// ---------------------------------------------------------------------------
// Hash28 list — used for required_signers and required_observers.
//
// Observers are restricted to 28-byte script hashes in this branch; the legacy
// CBOR encoding also accepted full Credential CBOR for them, but no producer
// ever emitted that path.
// ---------------------------------------------------------------------------

export const encodeMidgardHash28ListPreimage = (
  hashes: readonly Uint8Array[],
  fieldName = "hash28_list",
): Buffer => {
  const w = new BinaryWriter();
  writeU64(w, hashes.length);
  for (let i = 0; i < hashes.length; i++) {
    writeHash28(w, ensureLen28(hashes[i], `${fieldName}[${i}]`));
  }
  return w.toBytes();
};

export const decodeMidgardHash28ListPreimage = (
  bytes: Uint8Array,
  fieldName = "hash28_list",
): Buffer[] => {
  const r = new BinaryReader(bytes);
  const len = readU64(r);
  const out: Buffer[] = [];
  for (let i = 0; i < len; i++) {
    out.push(readHash28(r));
  }
  ensureNoTrailingBytes(r, fieldName);
  return out;
};

// ---------------------------------------------------------------------------
// VKeyWitness list — used for addr_tx_wits.
// ---------------------------------------------------------------------------

export type MidgardVKeyWitness = {
  readonly vkey: Buffer; // 32 bytes
  readonly signature: Buffer; // 64 bytes
};

export const encodeMidgardVKeyWitnessListPreimage = (
  witnesses: readonly MidgardVKeyWitness[],
): Buffer => {
  const w = new BinaryWriter();
  writeU64(w, witnesses.length);
  for (let i = 0; i < witnesses.length; i++) {
    const witness = witnesses[i];
    w.write(ensureLen32(witness.vkey, `vkey_witness[${i}].vkey`));
    w.write(ensureLen64(witness.signature, `vkey_witness[${i}].signature`));
  }
  return w.toBytes();
};

export const decodeMidgardVKeyWitnessListPreimage = (
  bytes: Uint8Array,
  fieldName = "vkey_witness_list",
): MidgardVKeyWitness[] => {
  const r = new BinaryReader(bytes);
  const len = readU64(r);
  const out: MidgardVKeyWitness[] = [];
  for (let i = 0; i < len; i++) {
    const vkey = r.read(32);
    const signature = r.read(64);
    out.push({ vkey, signature });
  }
  ensureNoTrailingBytes(r, fieldName);
  return out;
};

// ---------------------------------------------------------------------------
// Variable-payload list: used for outputs (per-entry payload stays CBOR for
// now — `encodeMidgardTxOutput` bytes) and for versioned scripts (per-entry
// payload is one CBOR `[lang_tag, script_bytes]`, kept as today).
//
// Static:  list_len(u64) + per-entry payload_len(u64)
// Dynamic: per-entry payload_bytes + alignment padding
// ---------------------------------------------------------------------------

export const encodeMidgardBytesListPreimage = (
  entries: readonly Uint8Array[],
): Buffer => {
  const sw = new BinaryWriter();
  writeU64(sw, entries.length);
  for (const e of entries) writeVarBytesStatic(sw, e);
  const dw = new BinaryWriter();
  for (const e of entries) writeVarBytesDynamic(dw, e);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardBytesListPreimage = (
  bytes: Uint8Array,
  fieldName = "bytes_list",
): Buffer[] => {
  const r = new BinaryReader(bytes);
  const len = readU64(r);
  const lens: number[] = [];
  for (let i = 0; i < len; i++) lens.push(readVarBytesLen(r));
  const out: Buffer[] = [];
  for (let i = 0; i < len; i++) out.push(readVarBytesDynamic(r, lens[i]));
  ensureNoTrailingBytes(r, fieldName);
  return out;
};

// ---------------------------------------------------------------------------
// Mint — multiasset map with signed i64 amounts.
//
// Static:  outer_len(u64) + for each policy:
//            policy_id(Hash28 padded to 32) + inner_len(u64) +
//            for each asset: name_len(u64) + amount(i64)
// Dynamic: for each policy: for each asset: asset_name bytes + pad
// ---------------------------------------------------------------------------

export type MidgardMintPolicy = {
  readonly policyId: Buffer; // 28 bytes
  readonly assets: readonly MidgardMintAsset[];
};

export type MidgardMintAsset = {
  readonly name: Buffer;
  readonly amount: bigint; // i64; nonzero
};

export type MidgardMint = readonly MidgardMintPolicy[];

const writeMintPreimageStatic = (w: BinaryWriter, mint: MidgardMint): void => {
  writeU64(w, mint.length);
  for (let i = 0; i < mint.length; i++) {
    const policy = mint[i];
    writeHash28(w, ensureLen28(policy.policyId, `mint[${i}].policy_id`));
    writeU64(w, policy.assets.length);
    for (let j = 0; j < policy.assets.length; j++) {
      const asset = policy.assets[j];
      writeVarBytesStatic(w, asset.name);
      writeBigI64(w, asset.amount);
    }
  }
};

const writeMintPreimageDynamic = (w: BinaryWriter, mint: MidgardMint): void => {
  for (const policy of mint) {
    for (const asset of policy.assets) {
      writeVarBytesDynamic(w, asset.name);
    }
  }
};

export const encodeMidgardMintPreimage = (mint: MidgardMint): Buffer => {
  const sw = new BinaryWriter();
  writeMintPreimageStatic(sw, mint);
  const dw = new BinaryWriter();
  writeMintPreimageDynamic(dw, mint);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardMintPreimage = (
  bytes: Uint8Array,
  fieldName = "mint",
): MidgardMint => {
  const r = new BinaryReader(bytes);
  const outerLen = readU64(r);
  type Partial = {
    policyId: Buffer;
    assets: { nameLen: number; amount: bigint }[];
  };
  const partial: Partial[] = [];
  for (let i = 0; i < outerLen; i++) {
    const policyId = readHash28(r);
    const innerLen = readU64(r);
    const assets: { nameLen: number; amount: bigint }[] = [];
    for (let j = 0; j < innerLen; j++) {
      const nameLen = readVarBytesLen(r);
      const amount = readBigI64(r);
      if (amount === 0n) {
        fail(`${fieldName}.policy[${i}].asset[${j}] amount must be non-zero`);
      }
      assets.push({ nameLen, amount });
    }
    partial.push({ policyId, assets });
  }
  const out: MidgardMintPolicy[] = [];
  for (const p of partial) {
    const assets: MidgardMintAsset[] = [];
    for (const a of p.assets) {
      const name = readVarBytesDynamic(r, a.nameLen);
      assets.push({ name, amount: a.amount });
    }
    out.push({ policyId: p.policyId, assets });
  }
  ensureNoTrailingBytes(r, fieldName);
  return out;
};

// ---------------------------------------------------------------------------
// Empty preimage sentinels: encoded empty list = u64 zero (8 zero bytes).
// ---------------------------------------------------------------------------

export const EMPTY_PREIMAGE_LIST: Buffer = (() => {
  const w = new BinaryWriter();
  writeU64(w, 0);
  return w.toBytes();
})();

// Re-export for callers that previously used `ensureHash32` on these blobs.
export { ensureHash32, type Hash32 };
