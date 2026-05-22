/**
 * Midgard native transaction types (Canonical/Full and Compact Representations).
 *
 * This mirrors the canonical Midgard-native tx codec implemented in
 * midgard-core/src/codec/ (native.ts, native-body.ts, native-witness.ts,
 * native-validation.ts), which is the current source of truth and supersedes
 * the older Conway-style transaction layout that cddl-files/codec.cddl was
 * originally based on. Type and field names match midgard-core exactly
 * (camelCase).
 *
 * The native tx format does NOT embed Cardano tx body/witness structures.
 * Variable-size, proof-critical body and witness fields are carried as opaque
 * CBOR preimage byte blobs in the canonical (full) representation, and as
 * 32-byte blake2b hashes of those preimages in the compact representation.
 *
 * Encoding uses the fuel-vm-style static/dynamic split (see ../codec):
 *   - Fixed-size fields and the u64 lengths of variable fields go in the
 *     static section.
 *   - Variable-length byte blobs go in the dynamic section (bytes + padding).
 *
 * Unlike the old Conway-style body, every field is mandatory — there is no
 * optional-field bitmask. validityIntervalStart / validityIntervalEnd use
 * -1 ("unbounded") and networkId uses 255 ("none") as sentinel values.
 */

import {
  Writer,
  Reader,
  writeU64,
  readU64,
  writeBigU64,
  readBigU64,
  writeBigI64,
  readBigI64,
  writeVarBytesStatic,
  writeVarBytesDynamic,
  readVarBytesLen,
  readVarBytesDynamic,
} from "../codec";

import { Hash32, writeHash32Static, readHash32Static } from "./primitives";

// ===========================================================================
// Version
// codec: midgard-core/src/codec/native-constants.ts:4 — MIDGARD_NATIVE_TX_VERSION
// cddl:  codec.cddl:114 — version_v1 = 1
// ===========================================================================

/** Only supported Midgard native tx wire-format version. */
export const MIDGARD_NATIVE_TX_VERSION = 1n;

// ===========================================================================
// MidgardTxValidity
// codec: midgard-core/src/codec/native-validation.ts:5 — MidgardTxValidityCodes
// cddl:  codec.cddl:47 — midgard_tx_validity_code = 0 / 1 / 2 / 3 / 4 / 5
//
// A transaction's L2 validity verdict, encoded on the wire as a u64 code.
// ===========================================================================

/** Maps each validity variant to its canonical u64 wire code. */
export const MidgardTxValidityCodes = {
  TxIsValid: 0,
  NonExistentInputUtxo: 1,
  InvalidSignature: 2,
  FailedScript: 3,
  FeeTooLow: 4,
  UnbalancedTx: 5,
} as const;

export type MidgardTxValidity = keyof typeof MidgardTxValidityCodes;

// Reverse lookup: wire code -> variant name (index = code).
const VALIDITY_BY_CODE: readonly MidgardTxValidity[] = [
  "TxIsValid",
  "NonExistentInputUtxo",
  "InvalidSignature",
  "FailedScript",
  "FeeTooLow",
  "UnbalancedTx",
];

// fuel: fuel-types/src/canonical.rs:223 — primitive u64 encode_static
function writeValidity(w: Writer, v: MidgardTxValidity): void {
  writeU64(w, MidgardTxValidityCodes[v]);
}

// fuel: fuel-types/src/canonical.rs:249 — primitive u64 decode_static
function readValidity(r: Reader): MidgardTxValidity {
  const code = readU64(r);
  const v = VALIDITY_BY_CODE[code];
  if (v === undefined) {
    throw new Error(`UnknownDiscriminant for MidgardTxValidity: ${code}`);
  }
  return v;
}

// ===========================================================================
// MidgardNativeTxBodyCanonical   (Canonical / Full Representation)
// codec: midgard-core/src/codec/native.ts:95 — MidgardNativeTxBodyCanonical
// codec: midgard-core/src/codec/native-body.ts:127 — encodeNativeTxBodyCanonicalValue
// cddl:  codec.cddl:77 — midgard_tx_body_full_v1
// fuel:  fuel-types/src/canonical.rs:71 — trait Serialize (static/dynamic split)
//
// All twelve fields are mandatory. The six variable proof-critical fields are
// opaque CBOR preimage byte blobs; scriptIntegrityHash and auxiliaryDataHash
// are fixed 32-byte hashes carried directly.
//
// Static:
//   spendInputsPreimageCbor:       len u64
//   referenceInputsPreimageCbor:   len u64
//   outputsPreimageCbor:           len u64
//   fee:                          u64
//   validityIntervalStart:        i64
//   validityIntervalEnd:          i64
//   requiredObserversPreimageCbor: len u64
//   requiredSignersPreimageCbor:   len u64
//   mintPreimageCbor:              len u64
//   scriptIntegrityHash:          Hash32 (32)
//   auxiliaryDataHash:            Hash32 (32)
//   networkId:                    u64
// Dynamic (bytes + alignment padding, in field order):
//   spendInputs, referenceInputs, outputs,
//   requiredObservers, requiredSigners, mint
// ===========================================================================

export interface MidgardNativeTxBodyCanonical {
  spendInputsPreimageCbor: Uint8Array;
  referenceInputsPreimageCbor: Uint8Array;
  outputsPreimageCbor: Uint8Array;
  fee: bigint;
  /** POSIX time; -1 means "unbounded". */
  validityIntervalStart: bigint;
  /** POSIX time; -1 means "unbounded". */
  validityIntervalEnd: bigint;
  requiredObserversPreimageCbor: Uint8Array;
  requiredSignersPreimageCbor: Uint8Array;
  mintPreimageCbor: Uint8Array;
  scriptIntegrityHash: Hash32;
  auxiliaryDataHash: Hash32;
  /** Cardano network id; 255 means "none". */
  networkId: bigint;
}

// Partial state captured by the static decode phase: the six blob lengths plus
// every fully-decoded fixed-size field.
interface MidgardNativeTxBodyCanonicalPartial {
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

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static
// fuel: fuel-types/src/canonical.rs:301 — Vec<u8>::encode_static (len u64 per blob)
function writeBodyCanonicalStatic(
  w: Writer,
  b: MidgardNativeTxBodyCanonical,
): void {
  writeVarBytesStatic(w, b.spendInputsPreimageCbor);
  writeVarBytesStatic(w, b.referenceInputsPreimageCbor);
  writeVarBytesStatic(w, b.outputsPreimageCbor);
  writeBigU64(w, b.fee);
  writeBigI64(w, b.validityIntervalStart);
  writeBigI64(w, b.validityIntervalEnd);
  writeVarBytesStatic(w, b.requiredObserversPreimageCbor);
  writeVarBytesStatic(w, b.requiredSignersPreimageCbor);
  writeVarBytesStatic(w, b.mintPreimageCbor);
  writeHash32Static(w, b.scriptIntegrityHash);
  writeHash32Static(w, b.auxiliaryDataHash);
  writeBigU64(w, b.networkId);
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic
// fuel: fuel-types/src/canonical.rs:309 — Vec<u8>::encode_dynamic (bytes + tail pad)
function writeBodyCanonicalDynamic(
  w: Writer,
  b: MidgardNativeTxBodyCanonical,
): void {
  writeVarBytesDynamic(w, b.spendInputsPreimageCbor);
  writeVarBytesDynamic(w, b.referenceInputsPreimageCbor);
  writeVarBytesDynamic(w, b.outputsPreimageCbor);
  writeVarBytesDynamic(w, b.requiredObserversPreimageCbor);
  writeVarBytesDynamic(w, b.requiredSignersPreimageCbor);
  writeVarBytesDynamic(w, b.mintPreimageCbor);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
// fuel: fuel-types/src/canonical.rs:332 — Vec<u8>::decode_static (reads lens)
function readBodyCanonicalStatic(
  r: Reader,
): MidgardNativeTxBodyCanonicalPartial {
  const spendInputsLen = readVarBytesLen(r);
  const referenceInputsLen = readVarBytesLen(r);
  const outputsLen = readVarBytesLen(r);
  const fee = readBigU64(r);
  const validityIntervalStart = readBigI64(r);
  const validityIntervalEnd = readBigI64(r);
  const requiredObserversLen = readVarBytesLen(r);
  const requiredSignersLen = readVarBytesLen(r);
  const mintLen = readVarBytesLen(r);
  const scriptIntegrityHash = readHash32Static(r);
  const auxiliaryDataHash = readHash32Static(r);
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
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic
// fuel: fuel-types/src/canonical.rs:352 — Vec<u8>::decode_dynamic (bytes + skip pad)
function readBodyCanonicalDynamic(
  r: Reader,
  p: MidgardNativeTxBodyCanonicalPartial,
): MidgardNativeTxBodyCanonical {
  const spendInputsPreimageCbor = readVarBytesDynamic(r, p.spendInputsLen);
  const referenceInputsPreimageCbor = readVarBytesDynamic(
    r,
    p.referenceInputsLen,
  );
  const outputsPreimageCbor = readVarBytesDynamic(r, p.outputsLen);
  const requiredObserversPreimageCbor = readVarBytesDynamic(
    r,
    p.requiredObserversLen,
  );
  const requiredSignersPreimageCbor = readVarBytesDynamic(
    r,
    p.requiredSignersLen,
  );
  const mintPreimageCbor = readVarBytesDynamic(r, p.mintLen);
  return {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: p.fee,
    validityIntervalStart: p.validityIntervalStart,
    validityIntervalEnd: p.validityIntervalEnd,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash: p.scriptIntegrityHash,
    auxiliaryDataHash: p.auxiliaryDataHash,
    networkId: p.networkId,
  };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeMidgardNativeTxBodyCanonical(
  b: MidgardNativeTxBodyCanonical,
): Uint8Array {
  const sw = new Writer();
  writeBodyCanonicalStatic(sw, b);
  const dw = new Writer();
  writeBodyCanonicalDynamic(dw, b);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeMidgardNativeTxBodyCanonical(
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonical {
  const r = new Reader(bytes);
  const p = readBodyCanonicalStatic(r);
  return readBodyCanonicalDynamic(r, p);
}

// ===========================================================================
// MidgardNativeTxBodyCompact   (Compact Representation)
// codec: midgard-core/src/codec/native.ts:74 — MidgardNativeTxBodyCompact
// codec: midgard-core/src/codec/native-body.ts:65 — encodeNativeTxBodyCompactValue
// cddl:  codec.cddl:56 — midgard_tx_body_compact_v1
// fuel:  fuel-types/src/canonical.rs:71 — trait Serialize
//
// Identical field layout to MidgardNativeTxBodyCanonical, but each variable
// preimage blob is replaced by its 32-byte blake2b hash. Every field is
// fixed-size, so the whole struct lives in the static section — no dynamic.
// ===========================================================================

export interface MidgardNativeTxBodyCompact {
  spendInputsHash: Hash32;
  referenceInputsHash: Hash32;
  outputsHash: Hash32;
  fee: bigint;
  /** POSIX time; -1 means "unbounded". */
  validityIntervalStart: bigint;
  /** POSIX time; -1 means "unbounded". */
  validityIntervalEnd: bigint;
  requiredObserversHash: Hash32;
  requiredSignersHash: Hash32;
  mintHash: Hash32;
  scriptIntegrityHash: Hash32;
  auxiliaryDataHash: Hash32;
  /** Cardano network id; 255 means "none". */
  networkId: bigint;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (all fixed-size)
function writeBodyCompactStatic(
  w: Writer,
  b: MidgardNativeTxBodyCompact,
): void {
  writeHash32Static(w, b.spendInputsHash);
  writeHash32Static(w, b.referenceInputsHash);
  writeHash32Static(w, b.outputsHash);
  writeBigU64(w, b.fee);
  writeBigI64(w, b.validityIntervalStart);
  writeBigI64(w, b.validityIntervalEnd);
  writeHash32Static(w, b.requiredObserversHash);
  writeHash32Static(w, b.requiredSignersHash);
  writeHash32Static(w, b.mintHash);
  writeHash32Static(w, b.scriptIntegrityHash);
  writeHash32Static(w, b.auxiliaryDataHash);
  writeBigU64(w, b.networkId);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
function readBodyCompactStatic(r: Reader): MidgardNativeTxBodyCompact {
  const spendInputsHash = readHash32Static(r);
  const referenceInputsHash = readHash32Static(r);
  const outputsHash = readHash32Static(r);
  const fee = readBigU64(r);
  const validityIntervalStart = readBigI64(r);
  const validityIntervalEnd = readBigI64(r);
  const requiredObserversHash = readHash32Static(r);
  const requiredSignersHash = readHash32Static(r);
  const mintHash = readHash32Static(r);
  const scriptIntegrityHash = readHash32Static(r);
  const auxiliaryDataHash = readHash32Static(r);
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
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes (static-only)
export function encodeMidgardNativeTxBodyCompact(
  b: MidgardNativeTxBodyCompact,
): Uint8Array {
  const w = new Writer();
  writeBodyCompactStatic(w, b);
  return w.toBytes();
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeMidgardNativeTxBodyCompact(
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact {
  return readBodyCompactStatic(new Reader(bytes));
}

// ===========================================================================
// MidgardNativeTxWitnessSetCanonical   (Canonical / Full Representation)
// codec: midgard-core/src/codec/native.ts:110 — MidgardNativeTxWitnessSetCanonical
// codec: midgard-core/src/codec/native-witness.ts:65 — encodeNativeTxWitnessSetCanonicalValue
// cddl:  codec.cddl:98 — midgard_tx_witness_set_full_v1
// fuel:  fuel-types/src/canonical.rs:71 — trait Serialize
//
// Three mandatory opaque CBOR preimage byte blobs.
//
// Static:  addr / script / redeemer preimage lengths (u64 each)
// Dynamic: addr / script / redeemer bytes (+ alignment padding)
// ===========================================================================

export interface MidgardNativeTxWitnessSetCanonical {
  addrTxWitsPreimageCbor: Uint8Array;
  scriptTxWitsPreimageCbor: Uint8Array;
  redeemerTxWitsPreimageCbor: Uint8Array;
}

// Partial state captured by the static decode phase: the three blob lengths.
interface MidgardNativeTxWitnessSetCanonicalPartial {
  addrLen: number;
  scriptLen: number;
  redeemerLen: number;
}

// fuel: fuel-types/src/canonical.rs:301 — Vec<u8>::encode_static (len u64 per blob)
function writeWitnessSetCanonicalStatic(
  w: Writer,
  ws: MidgardNativeTxWitnessSetCanonical,
): void {
  writeVarBytesStatic(w, ws.addrTxWitsPreimageCbor);
  writeVarBytesStatic(w, ws.scriptTxWitsPreimageCbor);
  writeVarBytesStatic(w, ws.redeemerTxWitsPreimageCbor);
}

// fuel: fuel-types/src/canonical.rs:309 — Vec<u8>::encode_dynamic (bytes + tail pad)
function writeWitnessSetCanonicalDynamic(
  w: Writer,
  ws: MidgardNativeTxWitnessSetCanonical,
): void {
  writeVarBytesDynamic(w, ws.addrTxWitsPreimageCbor);
  writeVarBytesDynamic(w, ws.scriptTxWitsPreimageCbor);
  writeVarBytesDynamic(w, ws.redeemerTxWitsPreimageCbor);
}

// fuel: fuel-types/src/canonical.rs:332 — Vec<u8>::decode_static (reads lens)
function readWitnessSetCanonicalStatic(
  r: Reader,
): MidgardNativeTxWitnessSetCanonicalPartial {
  const addrLen = readVarBytesLen(r);
  const scriptLen = readVarBytesLen(r);
  const redeemerLen = readVarBytesLen(r);
  return { addrLen, scriptLen, redeemerLen };
}

// fuel: fuel-types/src/canonical.rs:352 — Vec<u8>::decode_dynamic (bytes + skip pad)
function readWitnessSetCanonicalDynamic(
  r: Reader,
  p: MidgardNativeTxWitnessSetCanonicalPartial,
): MidgardNativeTxWitnessSetCanonical {
  const addrTxWitsPreimageCbor = readVarBytesDynamic(r, p.addrLen);
  const scriptTxWitsPreimageCbor = readVarBytesDynamic(r, p.scriptLen);
  const redeemerTxWitsPreimageCbor = readVarBytesDynamic(r, p.redeemerLen);
  return {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeMidgardNativeTxWitnessSetCanonical(
  ws: MidgardNativeTxWitnessSetCanonical,
): Uint8Array {
  const sw = new Writer();
  writeWitnessSetCanonicalStatic(sw, ws);
  const dw = new Writer();
  writeWitnessSetCanonicalDynamic(dw, ws);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeMidgardNativeTxWitnessSetCanonical(
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonical {
  const r = new Reader(bytes);
  const p = readWitnessSetCanonicalStatic(r);
  return readWitnessSetCanonicalDynamic(r, p);
}

// ===========================================================================
// MidgardNativeTxWitnessSetCompact   (Compact Representation)
// codec: midgard-core/src/codec/native.ts:89 — MidgardNativeTxWitnessSetCompact
// codec: midgard-core/src/codec/native-witness.ts:35 — encodeNativeTxWitnessSetCompactValue
// cddl:  codec.cddl:71 — midgard_tx_witness_set_compact_v1
// fuel:  fuel-types/src/canonical.rs:71 — trait Serialize
//
// Three mandatory 32-byte hashes of the witness preimage blobs. Fully static.
// ===========================================================================

export interface MidgardNativeTxWitnessSetCompact {
  addrTxWitsHash: Hash32;
  scriptTxWitsHash: Hash32;
  redeemerTxWitsHash: Hash32;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (three Hash32)
function writeWitnessSetCompactStatic(
  w: Writer,
  ws: MidgardNativeTxWitnessSetCompact,
): void {
  writeHash32Static(w, ws.addrTxWitsHash);
  writeHash32Static(w, ws.scriptTxWitsHash);
  writeHash32Static(w, ws.redeemerTxWitsHash);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
function readWitnessSetCompactStatic(
  r: Reader,
): MidgardNativeTxWitnessSetCompact {
  const addrTxWitsHash = readHash32Static(r);
  const scriptTxWitsHash = readHash32Static(r);
  const redeemerTxWitsHash = readHash32Static(r);
  return { addrTxWitsHash, scriptTxWitsHash, redeemerTxWitsHash };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes (static-only)
export function encodeMidgardNativeTxWitnessSetCompact(
  ws: MidgardNativeTxWitnessSetCompact,
): Uint8Array {
  const w = new Writer();
  writeWitnessSetCompactStatic(w, ws);
  return w.toBytes();
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeMidgardNativeTxWitnessSetCompact(
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact {
  return readWitnessSetCompactStatic(new Reader(bytes));
}

// ===========================================================================
// MidgardNativeTxCanonical   (Canonical / Full Representation)
// codec: midgard-core/src/codec/native.ts:116 — MidgardNativeTxCanonical
// codec: midgard-core/src/codec/native.ts:292 — encodeMidgardNativeTxFull
// cddl:  codec.cddl:107 — midgard_tx_full_v1
// fuel:  fuel-tx/src/transaction/types/chargeable_transaction.rs:86 — ChargeableTransaction
// fuel:  fuel-types/src/canonical.rs:71 — trait Serialize
//
// A native tx carries a version, an L2 validity verdict, a canonical body and
// a canonical witness set. The compact representation (MidgardNativeTxCompact,
// and the tx id derived from it) is obtained by hashing the body/witness
// preimages — that requires a blake2b hasher and is therefore left to
// midgard-core, not this pure codec.
//
// Static:  version(u64) + validity(u64) + body.static + witnessSet.static
// Dynamic: body.dynamic + witnessSet.dynamic
// ===========================================================================

export interface MidgardNativeTxCanonical {
  version: bigint;
  validity: MidgardTxValidity;
  body: MidgardNativeTxBodyCanonical;
  witnessSet: MidgardNativeTxWitnessSetCanonical;
}

export interface MidgardNativeTxCanonicalPartial {
  version: bigint;
  validity: MidgardTxValidity;
  bodyPartial: MidgardNativeTxBodyCanonicalPartial;
  wsPartial: MidgardNativeTxWitnessSetCanonicalPartial;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static
export function writeMidgardNativeTxCanonicalStatic(
  w: Writer,
  tx: MidgardNativeTxCanonical,
): void {
  writeBigU64(w, tx.version);
  writeValidity(w, tx.validity);
  writeBodyCanonicalStatic(w, tx.body);
  writeWitnessSetCanonicalStatic(w, tx.witnessSet);
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic
export function writeMidgardNativeTxCanonicalDynamic(
  w: Writer,
  tx: MidgardNativeTxCanonical,
): void {
  writeBodyCanonicalDynamic(w, tx.body);
  writeWitnessSetCanonicalDynamic(w, tx.witnessSet);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
export function readMidgardNativeTxCanonicalStatic(
  r: Reader,
): MidgardNativeTxCanonicalPartial {
  const version = readBigU64(r);
  const validity = readValidity(r);
  const bodyPartial = readBodyCanonicalStatic(r);
  const wsPartial = readWitnessSetCanonicalStatic(r);
  return { version, validity, bodyPartial, wsPartial };
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic
export function readMidgardNativeTxCanonicalDynamic(
  r: Reader,
  p: MidgardNativeTxCanonicalPartial,
): MidgardNativeTxCanonical {
  const body = readBodyCanonicalDynamic(r, p.bodyPartial);
  const witnessSet = readWitnessSetCanonicalDynamic(r, p.wsPartial);
  return { version: p.version, validity: p.validity, body, witnessSet };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeMidgardNativeTxCanonical(
  tx: MidgardNativeTxCanonical,
): Uint8Array {
  const sw = new Writer();
  writeMidgardNativeTxCanonicalStatic(sw, tx);
  const dw = new Writer();
  writeMidgardNativeTxCanonicalDynamic(dw, tx);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeMidgardNativeTxCanonical(
  bytes: Uint8Array,
): MidgardNativeTxCanonical {
  const r = new Reader(bytes);
  const p = readMidgardNativeTxCanonicalStatic(r);
  return readMidgardNativeTxCanonicalDynamic(r, p);
}

// ===========================================================================
// MidgardNativeTxCompact   (Compact Representation)
// codec: midgard-core/src/codec/native.ts:67 — MidgardNativeTxCompact
// codec: midgard-core/src/codec/native.ts:131 — encodeNativeTxCompactValue
// cddl:  codec.cddl:49 — midgard_tx_compact_v1
// fuel:  fuel-types/src/canonical.rs:71 — trait Serialize
//
// version + compact body + witness-set hash + validity. Fully static: the
// compact body is itself fixed-size, so there is no dynamic section.
// ===========================================================================

export interface MidgardNativeTxCompact {
  version: bigint;
  transactionBody: MidgardNativeTxBodyCompact;
  transactionWitnessSetHash: Hash32;
  validity: MidgardTxValidity;
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes (static-only)
export function encodeMidgardNativeTxCompact(
  tc: MidgardNativeTxCompact,
): Uint8Array {
  const w = new Writer();
  writeBigU64(w, tc.version);
  writeBodyCompactStatic(w, tc.transactionBody);
  writeHash32Static(w, tc.transactionWitnessSetHash);
  writeValidity(w, tc.validity);
  return w.toBytes();
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeMidgardNativeTxCompact(
  bytes: Uint8Array,
): MidgardNativeTxCompact {
  const r = new Reader(bytes);
  const version = readBigU64(r);
  const transactionBody = readBodyCompactStatic(r);
  const transactionWitnessSetHash = readHash32Static(r);
  const validity = readValidity(r);
  return {
    version,
    transactionBody,
    transactionWitnessSetHash,
    validity,
  };
}
