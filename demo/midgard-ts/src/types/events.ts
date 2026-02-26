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

function writeDepositInfoStatic(w: Writer, d: DepositInfo): void {
  writeAddressStatic(w, d.l2_address);
  if (d.l2_datum !== undefined) {
    writeU64(w, 1);
    writeVarBytesStatic(w, d.l2_datum);
  } else {
    writeU64(w, 0);
  }
}

function writeDepositInfoDynamic(w: Writer, d: DepositInfo): void {
  writeAddressDynamic(w, d.l2_address);
  if (d.l2_datum !== undefined) writeVarBytesDynamic(w, d.l2_datum);
}

function readDepositInfoStatic(r: Reader): DepositInfoPartial {
  const addrLen = readAddressLen(r);
  const datumPresent = readU64(r) !== 0;
  const datumLen = datumPresent ? readVarBytesLen(r) : 0;
  return { addrLen, datumPresent, datumLen };
}

function readDepositInfoDynamic(r: Reader, p: DepositInfoPartial): DepositInfo {
  const l2_address = readAddressDynamic(r, p.addrLen);
  const l2_datum = p.datumPresent
    ? readVarBytesDynamic(r, p.datumLen)
    : undefined;
  return { l2_address, l2_datum };
}

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

export function decodeDepositInfo(bytes: Uint8Array): DepositInfo {
  const r = new Reader(bytes);
  const p = readDepositInfoStatic(r);
  return readDepositInfoDynamic(r, p);
}

// ===========================================================================
// DepositInfoCompact   (Compact Representation)
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

function writeDepositInfoCompactStatic(w: Writer, d: DepositInfoCompact): void {
  writeAddressStatic(w, d.l2_address);
  if (d.l2_datum !== undefined) {
    writeU64(w, 1);
    writeHash32Static(w, d.l2_datum);
  } else {
    writeU64(w, 0);
  }
}

function writeDepositInfoCompactDynamic(
  w: Writer,
  d: DepositInfoCompact,
): void {
  writeAddressDynamic(w, d.l2_address);
}

function readDepositInfoCompactStatic(r: Reader): DepositInfoCompactPartial {
  const addrLen = readAddressLen(r);
  const present = readU64(r) !== 0;
  const datum = present ? readHash32Static(r) : undefined;
  return { addrLen, datum };
}

function readDepositInfoCompactDynamic(
  r: Reader,
  p: DepositInfoCompactPartial,
): DepositInfoCompact {
  const l2_address = readAddressDynamic(r, p.addrLen);
  return { l2_address, l2_datum: p.datum };
}

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

export function decodeDepositInfoCompact(
  bytes: Uint8Array,
): DepositInfoCompact {
  const r = new Reader(bytes);
  const p = readDepositInfoCompactStatic(r);
  return readDepositInfoCompactDynamic(r, p);
}

// ===========================================================================
// WithdrawalInfo   (Full Representation)
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

function writeWithdrawalInfoDynamic(w: Writer, wi: WithdrawalInfo): void {
  writeAddressDynamic(w, wi.l1_address);
  if (wi.l1_datum !== undefined) writeVarBytesDynamic(w, wi.l1_datum);
}

function readWithdrawalInfoStatic(r: Reader): WithdrawalInfoPartial {
  const l2_outref = readOutputReferenceStatic(r);
  const addrLen = readAddressLen(r);
  const datumPresent = readU64(r) !== 0;
  const datumLen = datumPresent ? readVarBytesLen(r) : 0;
  return { l2_outref, addrLen, datumPresent, datumLen };
}

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

export function decodeWithdrawalInfo(bytes: Uint8Array): WithdrawalInfo {
  const r = new Reader(bytes);
  const p = readWithdrawalInfoStatic(r);
  return readWithdrawalInfoDynamic(r, p);
}

// ===========================================================================
// WithdrawalInfoCompact   (Compact Representation)
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

function writeWithdrawalInfoCompactDynamic(
  w: Writer,
  wi: WithdrawalInfoCompact,
): void {
  writeAddressDynamic(w, wi.l1_address);
}

function readWithdrawalInfoCompactStatic(
  r: Reader,
): WithdrawalInfoCompactPartial {
  const l2_outref = readOutputReferenceStatic(r);
  const addrLen = readAddressLen(r);
  const present = readU64(r) !== 0;
  const datum = present ? readHash32Static(r) : undefined;
  return { l2_outref, addrLen, datum };
}

function readWithdrawalInfoCompactDynamic(
  r: Reader,
  p: WithdrawalInfoCompactPartial,
): WithdrawalInfoCompact {
  const l1_address = readAddressDynamic(r, p.addrLen);
  return { l2_outref: p.l2_outref, l1_address, l1_datum: p.datum };
}

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

export function decodeWithdrawalInfoCompact(
  bytes: Uint8Array,
): WithdrawalInfoCompact {
  const r = new Reader(bytes);
  const p = readWithdrawalInfoCompactStatic(r);
  return readWithdrawalInfoCompactDynamic(r, p);
}
