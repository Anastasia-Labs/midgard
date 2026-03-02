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

/** 28-byte (224-bit) hash.
 * cddl: codec.cddl:334 — hash28 = bytes .size 28
 * fuel: fuel-types/src/array_types.rs:354 — Bytes32 (closest; no 28-byte type in Fuel)
 */
export type Hash28 = Uint8Array;
/** 32-byte (256-bit) hash / Merkle root.
 * cddl: codec.cddl:336 — hash32 = bytes .size 32
 * fuel: fuel-types/src/array_types.rs:354 — key!(Bytes32, 32)
 */
export type Hash32 = Uint8Array;
/** 32-byte verification key.
 * cddl: codec.cddl:342 — vkey = bytes .size 32
 * fuel: fuel-types/src/array_types.rs:345 — key!(Address, 32) (same 32-byte fixed array)
 */
export type VKey = Uint8Array;
/** 64-byte signature.
 * cddl: codec.cddl:340 — signature = bytes .size 64
 * fuel: fuel-types/src/array_types.rs:359 — key_with_big_array!(Bytes64, 64)
 */
export type Signature = Uint8Array;

export type Coin = number;
export type PosixTime = number;

// Semantic aliases
// cddl: codec.cddl:350 — header_hash = $hash28
export type HeaderHash = Hash28;
// cddl: codec.cddl:217 — transaction_id = $hash32
export type TransactionId = Hash32;
// cddl: codec.cddl:348 — mptr = $hash32  (Merkle Patricia Tree Root)
export type Mptr = Hash32;
// cddl: codec.cddl:256 — policy_id (key in multiasset map) = $hash28
export type PolicyId = Hash28;
// cddl: codec.cddl:344 — addr_keyhash = $hash28
export type AddrKeyHash = Hash28;
// cddl: codec.cddl:346 — scripthash = $hash28
export type ScriptHash = Hash28;

// ---------------------------------------------------------------------------
// OutputReference  =  [transaction_id, index : uint .size 2]
// cddl: codec.cddl:215 — output_reference = [transaction_id : transaction_id, index : uint .size 2]
// cddl: codec.cddl:213 — transaction_input = output_reference  (same type, different semantic role)
// fuel: fuel-tx/src/transaction/types/utxo_id.rs:28 — UtxoId { tx_id: TxId, output_index: u16 }
//   Encodes as: Hash32 (32) + u16 padded to 8 = 40 bytes total, fully static
// ---------------------------------------------------------------------------

export interface OutputReference {
  tx_id: TransactionId; // 32 bytes
  index: number; // u16
}

// ---------------------------------------------------------------------------
// Credential  =  [0, addr_keyhash] / [1, scripthash]
// cddl: codec.cddl:271 — credential = [0, addr_keyhash // 1, scripthash]
// fuel: fuel-types/src/canonical.rs:71 — trait Serialize (discriminant + variant static fields pattern)
//   Fuel uses Address/ContractId directly with no tagged credential union.
//   Encodes as: u64 discriminant (8) + Hash28 (32) = 40 bytes, fully static
// ---------------------------------------------------------------------------

export type Credential =
  | { type: "PubKey"; hash: Hash28 }
  | { type: "Script"; hash: Hash28 };

// ===========================================================================
// Hash28  (28 bytes, padded to 32 = alignedSize(28))
// cddl: codec.cddl:334 — hash28 = bytes .size 28
// fuel: fuel-types/src/canonical.rs:374 — impl Serialize for [T; N], UNALIGNED_BYTES path (lines 406-413)
//   writes raw bytes then pads to next 8-byte boundary
// ===========================================================================

// fuel: fuel-types/src/canonical.rs:401 — [T; N]::encode_static (raw bytes + tail pad)
export function writeHash28Static(w: Writer, h: Hash28): void {
  writeFixedBytes(w, h); // 28 bytes + 4 zeros = 32
}

// fuel: fuel-types/src/canonical.rs:420 — [T; N]::decode_static (reads N bytes + skips pad)
export function readHash28Static(r: Reader): Hash28 {
  return readFixedBytes(r, 28); // reads 28 + skips 4
}

// ===========================================================================
// Hash32  (32 bytes, already aligned — no padding needed)
// cddl: codec.cddl:336 — hash32 = bytes .size 32
// fuel: fuel-types/src/canonical.rs:374 — impl Serialize for [T; N], UNALIGNED_BYTES path
// fuel: fuel-types/src/array_types.rs:354 — key!(Bytes32, 32) (exact Fuel equivalent)
// ===========================================================================

// fuel: fuel-types/src/canonical.rs:401 — [T; N]::encode_static (32 bytes, no pad)
export function writeHash32Static(w: Writer, h: Hash32): void {
  w.write(h);
}

// fuel: fuel-types/src/canonical.rs:420 — [T; N]::decode_static (reads 32 bytes, no skip)
export function readHash32Static(r: Reader): Hash32 {
  return r.read(32);
}

// ===========================================================================
// VKey  (32 bytes, already aligned — no padding needed)
// cddl: codec.cddl:342 — vkey = bytes .size 32
// fuel: fuel-types/src/canonical.rs:374 — impl Serialize for [T; N], UNALIGNED_BYTES path
// fuel: fuel-types/src/array_types.rs:345 — key!(Address, 32) (same 32-byte fixed array shape)
// ===========================================================================

// fuel: fuel-types/src/canonical.rs:401 — [T; N]::encode_static (32 bytes, no pad)
export function writeVKeyStatic(w: Writer, v: VKey): void {
  w.write(v);
}

// fuel: fuel-types/src/canonical.rs:420 — [T; N]::decode_static (reads 32 bytes, no skip)
export function readVKeyStatic(r: Reader): VKey {
  return r.read(32);
}

// ===========================================================================
// Signature  (64 bytes, already aligned — no padding needed)
// cddl: codec.cddl:340 — signature = bytes .size 64
// fuel: fuel-types/src/canonical.rs:374 — impl Serialize for [T; N], UNALIGNED_BYTES path
// fuel: fuel-types/src/array_types.rs:359 — key_with_big_array!(Bytes64, 64) (exact 64-byte equivalent)
// ===========================================================================

// fuel: fuel-types/src/canonical.rs:401 — [T; N]::encode_static (64 bytes, no pad)
export function writeSignatureStatic(w: Writer, s: Signature): void {
  w.write(s);
}

// fuel: fuel-types/src/canonical.rs:420 — [T; N]::decode_static (reads 64 bytes, no skip)
export function readSignatureStatic(r: Reader): Signature {
  return r.read(64);
}

// ===========================================================================
// Address  (variable Vec<u8>)
// Cardano address: variable-length byte blob, NOT a fixed hash.
// Static  = length as u64 (8 bytes)
// Dynamic = bytes + alignment padding
// fuel: fuel-types/src/canonical.rs:279 — impl Serialize for Vec<T>
//   encode_static (line 301): writes length as u64
//   encode_dynamic (line 309): writes bytes + tail alignment padding
//   decode_static (line 332): reads length
//   decode_dynamic (line 352): reads bytes + skips tail pad
// Note: Fuel's Address (array_types.rs:345) is fixed 32 bytes; Cardano address is variable.
// ===========================================================================

// fuel: fuel-types/src/canonical.rs:301 — Vec<T>::encode_static (writes length u64)
export function writeAddressStatic(w: Writer, addr: Uint8Array): void {
  writeVarBytesStatic(w, addr);
}

// fuel: fuel-types/src/canonical.rs:309 — Vec<T>::encode_dynamic (writes bytes + tail pad)
export function writeAddressDynamic(w: Writer, addr: Uint8Array): void {
  writeVarBytesDynamic(w, addr);
}

// fuel: fuel-types/src/canonical.rs:332 — Vec<T>::decode_static (reads length u64)
export function readAddressLen(r: Reader): number {
  return readVarBytesLen(r);
}

// fuel: fuel-types/src/canonical.rs:352 — Vec<T>::decode_dynamic (reads bytes + skips tail pad)
export function readAddressDynamic(r: Reader, len: number): Uint8Array {
  return readVarBytesDynamic(r, len);
}

// AssetName has identical encoding to Address (variable-length Vec<u8>).
// cddl: codec.cddl:258 — asset_name in multiasset<a0> = bytes (variable)
export const writeAssetNameStatic = writeAddressStatic;
export const writeAssetNameDynamic = writeAddressDynamic;
export const readAssetNameLen = readAddressLen;
export const readAssetNameDynamic = readAddressDynamic;

// ===========================================================================
// OutputReference
// cddl: codec.cddl:215 — output_reference = [transaction_id : transaction_id, index : uint .size 2]
// fuel: fuel-tx/src/transaction/types/utxo_id.rs:28 — UtxoId { tx_id: TxId, output_index: u16 }
// fuel: fuel-types/src/canonical.rs:71 — trait Serialize (#[derive(canonical::Serialize)])
// Static  = Hash32 (32) + u16 padded to 8 = 40 bytes, no dynamic
// ===========================================================================

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (all fields fixed-size)
export function writeOutputReferenceStatic(
  w: Writer,
  ref: OutputReference,
): void {
  writeHash32Static(w, ref.tx_id);
  writeU16(w, ref.index);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
export function readOutputReferenceStatic(r: Reader): OutputReference {
  const tx_id = readHash32Static(r);
  const index = readU16(r);
  return { tx_id, index };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes (static-only, no dynamic section)
export function encodeOutputReference(ref: OutputReference): Uint8Array {
  const w = new Writer();
  writeOutputReferenceStatic(w, ref);
  return w.toBytes();
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeOutputReference(bytes: Uint8Array): OutputReference {
  const r = new Reader(bytes);
  return readOutputReferenceStatic(r);
}

// ===========================================================================
// Credential
// cddl: codec.cddl:271 — credential = [0, addr_keyhash // 1, scripthash]
// fuel: fuel-types/src/canonical.rs:71 — trait Serialize (enum discriminant + variant static fields)
//   Fuel uses Address/ContractId directly; no tagged credential union equivalent.
// Static  = u64 discriminant (8) + Hash28 (32) = 40 bytes, no dynamic
// ===========================================================================

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (discriminant u64 + Hash28)
export function writeCredentialStatic(w: Writer, c: Credential): void {
  writeU64(w, c.type === "PubKey" ? 0 : 1);
  writeHash28Static(w, c.hash);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static (reads discriminant, branches)
export function readCredentialStatic(r: Reader): Credential {
  const disc = readU64(r);
  const hash = readHash28Static(r);
  if (disc === 0) return { type: "PubKey", hash };
  if (disc === 1) return { type: "Script", hash };
  throw new Error("UnknownDiscriminant for Credential");
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes (static-only, no dynamic section)
export function encodeCredential(c: Credential): Uint8Array {
  const w = new Writer();
  writeCredentialStatic(w, c);
  return w.toBytes();
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeCredential(bytes: Uint8Array): Credential {
  const r = new Reader(bytes);
  return readCredentialStatic(r);
}
