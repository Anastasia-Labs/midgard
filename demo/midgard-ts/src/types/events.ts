/**
 * Deposit and withdrawal event types (Full and Compact Representations).
 */

import {
  Writer,
  Reader,
  writeU64,
  readU64,
  writeVarBytesStatic,
  writeVarBytesDynamic,
  readVarBytesLen,
  readVarBytesDynamic,
} from "../codec";
import {
  Hash32,
  OutputReference,
  writeHash32Static,
  readHash32Static,
  writeAddressStatic,
  writeAddressDynamic,
  readAddressLen,
  readAddressDynamic,
  writeOutputReferenceStatic,
  readOutputReferenceStatic,
} from "./primitives";

// ===========================================================================
// DepositInfo   (Full Representation)
// cddl: codec.cddl:73 — deposit_info = { l2_address : address, ? l2_datum : data }
// cddl: codec.cddl:71 — deposit_id = output_reference  (map key, not in this struct)
// fuel: fuel-types/src/canonical.rs:71  — trait Serialize (encode_static/encode_dynamic pattern)
// fuel: fuel-types/src/canonical.rs:150 — trait Deserialize (decode_static/decode_dynamic pattern)
//
// Fields: l2_address, l2_datum?
//
// Static:  l2_address.static (len u64)
//          l2_datum.static   (presence u64 [+ len u64 if Some])
// Dynamic: l2_address.dynamic (bytes+pad)
//          l2_datum.dynamic   (bytes+pad if Some)
// ===========================================================================

export interface DepositInfo {
  l2_address: Uint8Array;
  l2_datum: Uint8Array | undefined;
}

interface DepositInfoPartial {
  addrLen: number;
  datumPresent: boolean;
  datumLen: number;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static
// fuel: fuel-types/src/canonical.rs:301 — Vec<u8>::encode_static (l2_address len + datum len as u64)
function writeDepositInfoStatic(w: Writer, d: DepositInfo): void {
  writeAddressStatic(w, d.l2_address);
  if (d.l2_datum !== undefined) {
    writeU64(w, 1);
    writeVarBytesStatic(w, d.l2_datum);
  } else {
    writeU64(w, 0);
  }
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic
// fuel: fuel-types/src/canonical.rs:309 — Vec<u8>::encode_dynamic (bytes + tail alignment pad)
function writeDepositInfoDynamic(w: Writer, d: DepositInfo): void {
  writeAddressDynamic(w, d.l2_address);
  if (d.l2_datum !== undefined) writeVarBytesDynamic(w, d.l2_datum);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
// fuel: fuel-types/src/canonical.rs:332 — Vec<u8>::decode_static (reads len, stores as capacity)
function readDepositInfoStatic(r: Reader): DepositInfoPartial {
  const addrLen = readAddressLen(r);
  const datumPresent = readU64(r) !== 0;
  const datumLen = datumPresent ? readVarBytesLen(r) : 0;
  return { addrLen, datumPresent, datumLen };
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic
// fuel: fuel-types/src/canonical.rs:352 — Vec<u8>::decode_dynamic (reads bytes + skips tail pad)
function readDepositInfoDynamic(r: Reader, p: DepositInfoPartial): DepositInfo {
  const l2_address = readAddressDynamic(r, p.addrLen);
  const l2_datum = p.datumPresent
    ? readVarBytesDynamic(r, p.datumLen)
    : undefined;
  return { l2_address, l2_datum };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeDepositInfo(d: DepositInfo): Uint8Array {
  const sw = new Writer();
  writeDepositInfoStatic(sw, d);
  const dw = new Writer();
  writeDepositInfoDynamic(dw, d);
  const s = sw.toBytes();
  const dy = dw.toBytes();
  const out = new Uint8Array(s.length + dy.length);
  out.set(s);
  out.set(dy, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeDepositInfo(bytes: Uint8Array): DepositInfo {
  const r = new Reader(bytes);
  const p = readDepositInfoStatic(r);
  return readDepositInfoDynamic(r, p);
}

// ===========================================================================
// DepositInfoCompact   (Compact Representation)
// cddl: codec.cddl:82 — deposit_info_compact = { l2_address : address, ? l2_datum : $hash32 }
// fuel: fuel-types/src/canonical.rs:71  — trait Serialize
// fuel: fuel-types/src/canonical.rs:150 — trait Deserialize
//
// Fields: l2_address, l2_datum?  (datum is Hash32, not raw bytes)
//
// Static:  l2_address.static (len u64)
//          l2_datum.static   (presence u64 [+ 32 bytes if Some])
// Dynamic: l2_address.dynamic (bytes+pad)
// ===========================================================================

export interface DepositInfoCompact {
  l2_address: Uint8Array;
  l2_datum: Hash32 | undefined;
}

interface DepositInfoCompactPartial {
  addrLen: number;
  datum: Hash32 | undefined;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static
function writeDepositInfoCompactStatic(w: Writer, d: DepositInfoCompact): void {
  writeAddressStatic(w, d.l2_address);
  if (d.l2_datum !== undefined) {
    writeU64(w, 1);
    writeHash32Static(w, d.l2_datum);
  } else {
    writeU64(w, 0);
  }
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic (address bytes only; datum is Hash32 = static)
function writeDepositInfoCompactDynamic(
  w: Writer,
  d: DepositInfoCompact,
): void {
  writeAddressDynamic(w, d.l2_address);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
function readDepositInfoCompactStatic(r: Reader): DepositInfoCompactPartial {
  const addrLen = readAddressLen(r);
  const present = readU64(r) !== 0;
  const datum = present ? readHash32Static(r) : undefined;
  return { addrLen, datum };
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic (address bytes only)
function readDepositInfoCompactDynamic(
  r: Reader,
  p: DepositInfoCompactPartial,
): DepositInfoCompact {
  const l2_address = readAddressDynamic(r, p.addrLen);
  return { l2_address, l2_datum: p.datum };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeDepositInfoCompact(d: DepositInfoCompact): Uint8Array {
  const sw = new Writer();
  writeDepositInfoCompactStatic(sw, d);
  const dw = new Writer();
  writeDepositInfoCompactDynamic(dw, d);
  const s = sw.toBytes();
  const dy = dw.toBytes();
  const out = new Uint8Array(s.length + dy.length);
  out.set(s);
  out.set(dy, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeDepositInfoCompact(
  bytes: Uint8Array,
): DepositInfoCompact {
  const r = new Reader(bytes);
  const p = readDepositInfoCompactStatic(r);
  return readDepositInfoCompactDynamic(r, p);
}

// ===========================================================================
// WithdrawalInfo   (Full Representation)
// cddl: codec.cddl:93 — withdrawal_info = { l2_outref, l1_address, ? l1_datum }
// cddl: codec.cddl:91 — withdrawal_id = output_reference  (map key, not in this struct)
// fuel: fuel-types/src/canonical.rs:71  — trait Serialize
// fuel: fuel-types/src/canonical.rs:150 — trait Deserialize
//
// Fields: l2_outref (OutputReference), l1_address, l1_datum?
//
// Static:  l2_outref.static (40 bytes)
//          l1_address.static (len u64)
//          l1_datum.static   (presence u64 [+ len u64 if Some])
// Dynamic: l1_address.dynamic (bytes+pad)
//          l1_datum.dynamic   (bytes+pad if Some)
// ===========================================================================

export interface WithdrawalInfo {
  l2_outref: OutputReference;
  l1_address: Uint8Array;
  l1_datum: Uint8Array | undefined;
}

interface WithdrawalInfoPartial {
  l2_outref: OutputReference;
  addrLen: number;
  datumPresent: boolean;
  datumLen: number;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static
// fuel: fuel-types/src/canonical.rs:301 — Vec<u8>::encode_static (address len + datum len as u64)
function writeWithdrawalInfoStatic(w: Writer, wi: WithdrawalInfo): void {
  writeOutputReferenceStatic(w, wi.l2_outref);
  writeAddressStatic(w, wi.l1_address);
  if (wi.l1_datum !== undefined) {
    writeU64(w, 1);
    writeVarBytesStatic(w, wi.l1_datum);
  } else {
    writeU64(w, 0);
  }
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic
// fuel: fuel-types/src/canonical.rs:309 — Vec<u8>::encode_dynamic (bytes + tail alignment pad)
function writeWithdrawalInfoDynamic(w: Writer, wi: WithdrawalInfo): void {
  writeAddressDynamic(w, wi.l1_address);
  if (wi.l1_datum !== undefined) writeVarBytesDynamic(w, wi.l1_datum);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
// fuel: fuel-types/src/canonical.rs:332 — Vec<u8>::decode_static (reads len, stores as capacity)
function readWithdrawalInfoStatic(r: Reader): WithdrawalInfoPartial {
  const l2_outref = readOutputReferenceStatic(r);
  const addrLen = readAddressLen(r);
  const datumPresent = readU64(r) !== 0;
  const datumLen = datumPresent ? readVarBytesLen(r) : 0;
  return { l2_outref, addrLen, datumPresent, datumLen };
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic
// fuel: fuel-types/src/canonical.rs:352 — Vec<u8>::decode_dynamic (reads bytes + skips tail pad)
function readWithdrawalInfoDynamic(
  r: Reader,
  p: WithdrawalInfoPartial,
): WithdrawalInfo {
  const l1_address = readAddressDynamic(r, p.addrLen);
  const l1_datum = p.datumPresent
    ? readVarBytesDynamic(r, p.datumLen)
    : undefined;
  return { l2_outref: p.l2_outref, l1_address, l1_datum };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeWithdrawalInfo(wi: WithdrawalInfo): Uint8Array {
  const sw = new Writer();
  writeWithdrawalInfoStatic(sw, wi);
  const dw = new Writer();
  writeWithdrawalInfoDynamic(dw, wi);
  const s = sw.toBytes();
  const dy = dw.toBytes();
  const out = new Uint8Array(s.length + dy.length);
  out.set(s);
  out.set(dy, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeWithdrawalInfo(bytes: Uint8Array): WithdrawalInfo {
  const r = new Reader(bytes);
  const p = readWithdrawalInfoStatic(r);
  return readWithdrawalInfoDynamic(r, p);
}

// ===========================================================================
// WithdrawalInfoCompact   (Compact Representation)
// cddl: codec.cddl:103 — withdrawal_info_compact = { l2_outref, l1_address, ? l1_datum : $hash32 }
// fuel: fuel-types/src/canonical.rs:71  — trait Serialize
// fuel: fuel-types/src/canonical.rs:150 — trait Deserialize
//
// Fields: l2_outref, l1_address, l1_datum?  (datum is Hash32)
// ===========================================================================

export interface WithdrawalInfoCompact {
  l2_outref: OutputReference;
  l1_address: Uint8Array;
  l1_datum: Hash32 | undefined;
}

interface WithdrawalInfoCompactPartial {
  l2_outref: OutputReference;
  addrLen: number;
  datum: Hash32 | undefined;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (l2_outref fixed + addr len + opt hash32)
function writeWithdrawalInfoCompactStatic(
  w: Writer,
  wi: WithdrawalInfoCompact,
): void {
  writeOutputReferenceStatic(w, wi.l2_outref);
  writeAddressStatic(w, wi.l1_address);
  if (wi.l1_datum !== undefined) {
    writeU64(w, 1);
    writeHash32Static(w, wi.l1_datum);
  } else {
    writeU64(w, 0);
  }
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic (address bytes only; datum is Hash32 = static)
function writeWithdrawalInfoCompactDynamic(
  w: Writer,
  wi: WithdrawalInfoCompact,
): void {
  writeAddressDynamic(w, wi.l1_address);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
function readWithdrawalInfoCompactStatic(
  r: Reader,
): WithdrawalInfoCompactPartial {
  const l2_outref = readOutputReferenceStatic(r);
  const addrLen = readAddressLen(r);
  const present = readU64(r) !== 0;
  const datum = present ? readHash32Static(r) : undefined;
  return { l2_outref, addrLen, datum };
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic (address bytes only)
function readWithdrawalInfoCompactDynamic(
  r: Reader,
  p: WithdrawalInfoCompactPartial,
): WithdrawalInfoCompact {
  const l1_address = readAddressDynamic(r, p.addrLen);
  return { l2_outref: p.l2_outref, l1_address, l1_datum: p.datum };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeWithdrawalInfoCompact(
  wi: WithdrawalInfoCompact,
): Uint8Array {
  const sw = new Writer();
  writeWithdrawalInfoCompactStatic(sw, wi);
  const dw = new Writer();
  writeWithdrawalInfoCompactDynamic(dw, wi);
  const s = sw.toBytes();
  const dy = dw.toBytes();
  const out = new Uint8Array(s.length + dy.length);
  out.set(s);
  out.set(dy, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeWithdrawalInfoCompact(
  bytes: Uint8Array,
): WithdrawalInfoCompact {
  const r = new Reader(bytes);
  const p = readWithdrawalInfoCompactStatic(r);
  return readWithdrawalInfoCompactDynamic(r, p);
}
