/**
 * Binary codec for the Midgard native transaction body
 * (canonical/full and compact representations).
 *
 * Replaces the previous CBOR encoding with the fuel-vm-style static/dynamic
 * binary layout (see ./binary.ts). The six variable proof-critical fields stay
 * opaque CBOR preimage blobs; only the surrounding body structure is binary.
 */

import {
  BinaryReader,
  BinaryWriter,
  ensureNoTrailingBytes,
  readBigI64,
  readBigU64,
  readHash32,
  readVarBytesDynamic,
  readVarBytesLen,
  writeBigI64,
  writeBigU64,
  writeHash32,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import { asSigned, asUnsigned } from "./native-validation.js";
import type {
  MidgardNativeTxBodyCanonical,
  MidgardNativeTxBodyCompact,
} from "./native.js";

// ===========================================================================
// Canonical (full) body
//
// Static:  spend/reference/outputs blob lengths (u64), fee (u64),
//          validity interval start/end (i64), required-observers/signers/mint
//          blob lengths (u64), script-integrity & auxiliary-data hashes (32),
//          network id (u64)
// Dynamic: spend, reference, outputs, required-observers, required-signers,
//          mint blobs (bytes + alignment padding)
// ===========================================================================

/** Static-phase decode state: blob lengths plus the decoded fixed fields. */
export interface NativeTxBodyCanonicalPartial {
  spendInputsLen: number;
  referenceInputsLen: number;
  outputsLen: number;
  fee: bigint;
  validityIntervalStart: bigint;
  validityIntervalEnd: bigint;
  requiredObserversLen: number;
  requiredSignersLen: number;
  mintLen: number;
  scriptIntegrityHash: Hash32;
  auxiliaryDataHash: Hash32;
  networkId: bigint;
}

export const writeNativeTxBodyCanonicalStatic = (
  w: BinaryWriter,
  body: MidgardNativeTxBodyCanonical,
): void => {
  writeVarBytesStatic(w, body.spendInputsPreimage);
  writeVarBytesStatic(w, body.referenceInputsPreimage);
  writeVarBytesStatic(w, body.outputsPreimage);
  writeBigU64(w, asUnsigned(body.fee, "transaction_body.fee"));
  writeBigI64(
    w,
    asSigned(
      body.validityIntervalStart,
      "transaction_body.validity_interval_start",
    ),
  );
  writeBigI64(
    w,
    asSigned(
      body.validityIntervalEnd,
      "transaction_body.validity_interval_end",
    ),
  );
  writeVarBytesStatic(w, body.requiredObserversPreimage);
  writeVarBytesStatic(w, body.requiredSignersPreimage);
  writeVarBytesStatic(w, body.mintPreimage);
  writeHash32(
    w,
    ensureHash32(
      body.scriptIntegrityHash,
      "transaction_body.script_integrity_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(
      body.auxiliaryDataHash,
      "transaction_body.auxiliary_data_hash",
    ),
  );
  writeBigU64(w, asUnsigned(body.networkId, "transaction_body.network_id"));
};

export const writeNativeTxBodyCanonicalDynamic = (
  w: BinaryWriter,
  body: MidgardNativeTxBodyCanonical,
): void => {
  writeVarBytesDynamic(w, body.spendInputsPreimage);
  writeVarBytesDynamic(w, body.referenceInputsPreimage);
  writeVarBytesDynamic(w, body.outputsPreimage);
  writeVarBytesDynamic(w, body.requiredObserversPreimage);
  writeVarBytesDynamic(w, body.requiredSignersPreimage);
  writeVarBytesDynamic(w, body.mintPreimage);
};

export const readNativeTxBodyCanonicalStatic = (
  r: BinaryReader,
): NativeTxBodyCanonicalPartial => {
  const spendInputsLen = readVarBytesLen(r);
  const referenceInputsLen = readVarBytesLen(r);
  const outputsLen = readVarBytesLen(r);
  const fee = readBigU64(r);
  const validityIntervalStart = readBigI64(r);
  const validityIntervalEnd = readBigI64(r);
  const requiredObserversLen = readVarBytesLen(r);
  const requiredSignersLen = readVarBytesLen(r);
  const mintLen = readVarBytesLen(r);
  const scriptIntegrityHash = ensureHash32(
    readHash32(r),
    "transaction_body.script_integrity_hash",
  );
  const auxiliaryDataHash = ensureHash32(
    readHash32(r),
    "transaction_body.auxiliary_data_hash",
  );
  const networkId = readBigU64(r);
  return {
    spendInputsLen,
    referenceInputsLen,
    outputsLen,
    fee,
    validityIntervalStart,
    validityIntervalEnd,
    requiredObserversLen,
    requiredSignersLen,
    mintLen,
    scriptIntegrityHash,
    auxiliaryDataHash,
    networkId,
  };
};

export const readNativeTxBodyCanonicalDynamic = (
  r: BinaryReader,
  p: NativeTxBodyCanonicalPartial,
): MidgardNativeTxBodyCanonical => {
  const spendInputsPreimage = readVarBytesDynamic(r, p.spendInputsLen);
  const referenceInputsPreimage = readVarBytesDynamic(
    r,
    p.referenceInputsLen,
  );
  const outputsPreimage = readVarBytesDynamic(r, p.outputsLen);
  const requiredObserversPreimage = readVarBytesDynamic(
    r,
    p.requiredObserversLen,
  );
  const requiredSignersPreimage = readVarBytesDynamic(
    r,
    p.requiredSignersLen,
  );
  const mintPreimage = readVarBytesDynamic(r, p.mintLen);
  return {
    spendInputsPreimage,
    referenceInputsPreimage,
    outputsPreimage,
    fee: p.fee,
    validityIntervalStart: p.validityIntervalStart,
    validityIntervalEnd: p.validityIntervalEnd,
    requiredObserversPreimage,
    requiredSignersPreimage,
    mintPreimage,
    scriptIntegrityHash: p.scriptIntegrityHash,
    auxiliaryDataHash: p.auxiliaryDataHash,
    networkId: p.networkId,
  };
};

export const encodeNativeTxBodyCanonical = (
  body: MidgardNativeTxBodyCanonical,
): Buffer => {
  const w = new BinaryWriter();
  writeNativeTxBodyCanonicalStatic(w, body);
  writeNativeTxBodyCanonicalDynamic(w, body);
  return w.toBytes();
};

export const decodeNativeTxBodyCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonical => {
  const r = new BinaryReader(bytes);
  const partial = readNativeTxBodyCanonicalStatic(r);
  const body = readNativeTxBodyCanonicalDynamic(r, partial);
  ensureNoTrailingBytes(r, "transaction_body");
  return body;
};

// ===========================================================================
// Compact body — every field fixed-size, fully static (no dynamic section).
// Each variable preimage blob is replaced by its 32-byte blake2b hash.
// ===========================================================================

export const writeNativeTxBodyCompact = (
  w: BinaryWriter,
  body: MidgardNativeTxBodyCompact,
): void => {
  writeHash32(
    w,
    ensureHash32(
      body.spendInputsHash,
      "transaction_body_compact.spend_inputs_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(
      body.referenceInputsHash,
      "transaction_body_compact.reference_inputs_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(body.outputsHash, "transaction_body_compact.outputs_hash"),
  );
  writeBigU64(w, asUnsigned(body.fee, "transaction_body_compact.fee"));
  writeBigI64(
    w,
    asSigned(
      body.validityIntervalStart,
      "transaction_body_compact.validity_interval_start",
    ),
  );
  writeBigI64(
    w,
    asSigned(
      body.validityIntervalEnd,
      "transaction_body_compact.validity_interval_end",
    ),
  );
  writeHash32(
    w,
    ensureHash32(
      body.requiredObserversHash,
      "transaction_body_compact.required_observers_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(
      body.requiredSignersHash,
      "transaction_body_compact.required_signers_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(body.mintHash, "transaction_body_compact.mint_hash"),
  );
  writeHash32(
    w,
    ensureHash32(
      body.scriptIntegrityHash,
      "transaction_body_compact.script_integrity_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(
      body.auxiliaryDataHash,
      "transaction_body_compact.auxiliary_data_hash",
    ),
  );
  writeBigU64(
    w,
    asUnsigned(body.networkId, "transaction_body_compact.network_id"),
  );
};

export const readNativeTxBodyCompact = (
  r: BinaryReader,
): MidgardNativeTxBodyCompact => {
  const spendInputsHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.spend_inputs_hash",
  );
  const referenceInputsHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.reference_inputs_hash",
  );
  const outputsHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.outputs_hash",
  );
  const fee = readBigU64(r);
  const validityIntervalStart = readBigI64(r);
  const validityIntervalEnd = readBigI64(r);
  const requiredObserversHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.required_observers_hash",
  );
  const requiredSignersHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.required_signers_hash",
  );
  const mintHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.mint_hash",
  );
  const scriptIntegrityHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.script_integrity_hash",
  );
  const auxiliaryDataHash = ensureHash32(
    readHash32(r),
    "transaction_body_compact.auxiliary_data_hash",
  );
  const networkId = readBigU64(r);
  return {
    spendInputsHash,
    referenceInputsHash,
    outputsHash,
    fee,
    validityIntervalStart,
    validityIntervalEnd,
    requiredObserversHash,
    requiredSignersHash,
    mintHash,
    scriptIntegrityHash,
    auxiliaryDataHash,
    networkId,
  };
};

export const encodeNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCompact,
): Buffer => {
  const w = new BinaryWriter();
  writeNativeTxBodyCompact(w, body);
  return w.toBytes();
};

export const decodeNativeTxBodyCompact = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact => {
  const r = new BinaryReader(bytes);
  const body = readNativeTxBodyCompact(r);
  ensureNoTrailingBytes(r, "transaction_body_compact");
  return body;
};

// ===========================================================================
// Derivation: compact body = canonical body with each preimage blob replaced
// by its blake2b hash. Independent of the envelope encoding.
// ===========================================================================

export const deriveNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCanonical,
): MidgardNativeTxBodyCompact => ({
  spendInputsHash: computeHash32(body.spendInputsPreimage),
  referenceInputsHash: computeHash32(body.referenceInputsPreimage),
  outputsHash: computeHash32(body.outputsPreimage),
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversHash: computeHash32(body.requiredObserversPreimage),
  requiredSignersHash: computeHash32(body.requiredSignersPreimage),
  mintHash: computeHash32(body.mintPreimage),
  scriptIntegrityHash: body.scriptIntegrityHash,
  auxiliaryDataHash: body.auxiliaryDataHash,
  networkId: body.networkId,
});
