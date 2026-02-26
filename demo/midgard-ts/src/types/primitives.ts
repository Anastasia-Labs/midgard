/**
 * Primitive types from the Midgard CDDL codec specification.
 *
 * Fixed-size byte arrays (Hash28, Hash32, VKey, Signature) are encoded
 * entirely in the static section.  Variable-length types (Address, AssetName)
 * store only their length in the static section and write the bytes in the
 * dynamic section.
 */

import {
  Writer,
  Reader,
  writeU16,
  readU16,
  writeU64,
  readU64,
  writeFixedBytes,
  readFixedBytes,
  writeVarBytesStatic,
  writeVarBytesDynamic,
  readVarBytesLen,
  readVarBytesDynamic,
  alignmentBytes,
} from "../codec";

// ---------------------------------------------------------------------------
// Type aliases
// ---------------------------------------------------------------------------

/** 28-byte (224-bit) hash. */
export type Hash28 = Uint8Array;
/** 32-byte (256-bit) hash / Merkle root. */
export type Hash32 = Uint8Array;
/** 32-byte verification key. */
export type VKey = Uint8Array;
/** 64-byte signature. */
export type Signature = Uint8Array;

export type Coin = number;
export type PosixTime = number;

// Semantic aliases
export type HeaderHash = Hash28;
export type TransactionId = Hash32;
export type Mptr = Hash32;
export type PolicyId = Hash28;
export type AddrKeyHash = Hash28;
export type ScriptHash = Hash28;

// ---------------------------------------------------------------------------
// OutputReference  =  [transaction_id, index : uint .size 2]
// ---------------------------------------------------------------------------

export interface OutputReference {
  tx_id: TransactionId; // 32 bytes
  index: number; // u16
}

// ---------------------------------------------------------------------------
// Credential  =  [0, addr_keyhash] / [1, scripthash]
// ---------------------------------------------------------------------------

export type Credential =
  | { type: "PubKey"; hash: Hash28 }
  | { type: "Script"; hash: Hash28 };

// ===========================================================================
// Hash28  (28 bytes, padded to 32 = alignedSize(28))
// ===========================================================================

export function writeHash28Static(w: Writer, h: Hash28): void {
  writeFixedBytes(w, h); // 28 bytes + 4 zeros = 32
}

export function readHash28Static(r: Reader): Hash28 {
  return readFixedBytes(r, 28); // reads 28 + skips 4
}

// ===========================================================================
// Hash32  (32 bytes, already aligned)
// ===========================================================================

export function writeHash32Static(w: Writer, h: Hash32): void {
  w.write(h);
}

export function readHash32Static(r: Reader): Hash32 {
  return r.read(32);
}

// ===========================================================================
// VKey  (32 bytes)
// ===========================================================================

export function writeVKeyStatic(w: Writer, v: VKey): void {
  w.write(v);
}

export function readVKeyStatic(r: Reader): VKey {
  return r.read(32);
}

// ===========================================================================
// Signature  (64 bytes)
// ===========================================================================

export function writeSignatureStatic(w: Writer, s: Signature): void {
  w.write(s);
}

export function readSignatureStatic(r: Reader): Signature {
  return r.read(64);
}

// ===========================================================================
// Address  (variable Vec<u8>)
// Static  = length as u64 (8 bytes)
// Dynamic = bytes + alignment padding
// ===========================================================================

export function writeAddressStatic(w: Writer, addr: Uint8Array): void {
  writeVarBytesStatic(w, addr);
}

export function writeAddressDynamic(w: Writer, addr: Uint8Array): void {
  writeVarBytesDynamic(w, addr);
}

export function readAddressLen(r: Reader): number {
  return readVarBytesLen(r);
}

export function readAddressDynamic(r: Reader, len: number): Uint8Array {
  return readVarBytesDynamic(r, len);
}

// AssetName has identical encoding to Address.
export const writeAssetNameStatic = writeAddressStatic;
export const writeAssetNameDynamic = writeAddressDynamic;
export const readAssetNameLen = readAddressLen;
export const readAssetNameDynamic = readAddressDynamic;

// ===========================================================================
// OutputReference
// Static  = Hash32 (32) + u16 (8) = 40 bytes, no dynamic
// ===========================================================================

export function writeOutputReferenceStatic(
  w: Writer,
  ref: OutputReference,
): void {
  writeHash32Static(w, ref.tx_id);
  writeU16(w, ref.index);
}

export function readOutputReferenceStatic(r: Reader): OutputReference {
  const tx_id = readHash32Static(r);
  const index = readU16(r);
  return { tx_id, index };
}

/** Full encode (= static only, no dynamic). */
export function encodeOutputReference(ref: OutputReference): Uint8Array {
  const w = new Writer();
  writeOutputReferenceStatic(w, ref);
  return w.toBytes();
}

export function decodeOutputReference(bytes: Uint8Array): OutputReference {
  const r = new Reader(bytes);
  return readOutputReferenceStatic(r);
}

// ===========================================================================
// Credential
// Static  = u64 discriminant (8) + Hash28 (32) = 40 bytes, no dynamic
// ===========================================================================

export function writeCredentialStatic(w: Writer, c: Credential): void {
  writeU64(w, c.type === "PubKey" ? 0 : 1);
  writeHash28Static(w, c.hash);
}

export function readCredentialStatic(r: Reader): Credential {
  const disc = readU64(r);
  const hash = readHash28Static(r);
  if (disc === 0) return { type: "PubKey", hash };
  if (disc === 1) return { type: "Script", hash };
  throw new Error("UnknownDiscriminant for Credential");
}

export function encodeCredential(c: Credential): Uint8Array {
  const w = new Writer();
  writeCredentialStatic(w, c);
  return w.toBytes();
}

export function decodeCredential(bytes: Uint8Array): Credential {
  const r = new Reader(bytes);
  return readCredentialStatic(r);
}
