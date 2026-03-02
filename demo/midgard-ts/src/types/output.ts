/**
 * Transaction output, value and multi-asset types
 * (Full and Compact Representations).
 */

import {
  Writer,
  Reader,
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

import {
  Hash28,
  Hash32,
  PolicyId,
  Coin,
  VKey,
  Signature,
  writeHash28Static,
  readHash28Static,
  writeHash32Static,
  readHash32Static,
  writeVKeyStatic,
  readVKeyStatic,
  writeSignatureStatic,
  readSignatureStatic,
  writeAddressStatic,
  writeAddressDynamic,
  readAddressLen,
  readAddressDynamic,
  writeAssetNameStatic,
  writeAssetNameDynamic,
  readAssetNameLen,
  readAssetNameDynamic,
} from "./primitives";

// ---------------------------------------------------------------------------
// VKeyWitness  =  [vkey, signature]   (fully static, no dynamic)
// cddl: codec.cddl:196 — referenced as nonempty_set<vkeywitness> in transaction_witness_set
// cddl: codec.cddl:342 — vkey = bytes .size 32
// cddl: codec.cddl:340 — signature = bytes .size 64
// fuel: fuel-tx/src/transaction/types/witness.rs:32 — Witness struct (Fuel's opaque witness payload)
// fuel: fuel-types/src/canonical.rs:374 — impl Serialize for [T; N], UNALIGNED_BYTES path (fixed byte arrays)
// ---------------------------------------------------------------------------

export interface VKeyWitness {
  vkey: VKey; // 32 bytes
  signature: Signature; // 64 bytes
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (both fields fixed-size, no dynamic)
export function writeVKeyWitness(w: Writer, ww: VKeyWitness): void {
  writeVKeyStatic(w, ww.vkey);
  writeSignatureStatic(w, ww.signature);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
export function readVKeyWitness(r: Reader): VKeyWitness {
  const vkey = readVKeyStatic(r);
  const signature = readSignatureStatic(r);
  return { vkey, signature };
}

// ===========================================================================
// Multiasset<u64>   (Full Representation)
// cddl: codec.cddl:256 — multiasset<a0> = { + policy_id => { + asset_name => a0 } }
// cddl: codec.cddl:334 — hash28 = bytes .size 28  (policy_id)
// fuel: fuel-types/src/canonical.rs:279 — impl Serialize for Vec<T> (nested: outer Vec of inner Vec)
//   encode_static (line 301): outer_len u64 + per-policy static
//   encode_dynamic (line 309): asset name bytes + tail pad
//
// Flat encoding:
//   Static: outer_len(u64) + for each policy:
//             policyId(32) + inner_len(u64) + for each asset:
//               assetName_len(u64) + amount(u64)
//   Dynamic: for each policy: for each asset:
//               assetName_bytes+pad
// ===========================================================================

export type MultiassetEntry = [PolicyId, Array<[Uint8Array, number]>]; // [policyId, [[assetName, amount]]]
export type Multiasset = MultiassetEntry[];

// Intermediate type used between the two decode phases.
interface MultiassetPartialEntry {
  pid: Hash28;
  assets: Array<{ nameLen: number; amount: number }>;
}

// fuel: fuel-types/src/canonical.rs:301 — Vec<T>::encode_static (len u64 + nested element statics)
function writeMultiassetStatic(w: Writer, ma: Multiasset): void {
  writeU64(w, ma.length);
  for (const [pid, assets] of ma) {
    writeHash28Static(w, pid);
    writeU64(w, assets.length);
    for (const [name, amount] of assets) {
      writeAssetNameStatic(w, name); // length u64
      writeU64(w, amount);
    }
  }
}

// fuel: fuel-types/src/canonical.rs:309 — Vec<u8>::encode_dynamic (asset name bytes + tail pad)
function writeMultiassetDynamic(w: Writer, ma: Multiasset): void {
  for (const [, assets] of ma) {
    for (const [name] of assets) {
      writeAssetNameDynamic(w, name); // actual bytes + pad
    }
  }
}

// fuel: fuel-types/src/canonical.rs:332 — Vec<T>::decode_static (reads len, allocates capacity)
function readMultiassetStatic(r: Reader): MultiassetPartialEntry[] {
  const outerLen = readU64(r);
  const partial: MultiassetPartialEntry[] = [];
  for (let i = 0; i < outerLen; i++) {
    const pid = readHash28Static(r);
    const innerLen = readU64(r);
    const assets: Array<{ nameLen: number; amount: number }> = [];
    for (let j = 0; j < innerLen; j++) {
      const nameLen = readAssetNameLen(r);
      const amount = readU64(r);
      assets.push({ nameLen, amount });
    }
    partial.push({ pid, assets });
  }
  return partial;
}

// fuel: fuel-types/src/canonical.rs:352 — Vec<T>::decode_dynamic (reads bytes + skips tail pad)
function readMultiassetDynamic(
  r: Reader,
  partial: MultiassetPartialEntry[],
): Multiasset {
  const result: Multiasset = [];
  for (const { pid, assets } of partial) {
    const assetEntries: Array<[Uint8Array, number]> = [];
    for (const { nameLen, amount } of assets) {
      const name = readAssetNameDynamic(r, nameLen);
      assetEntries.push([name, amount]);
    }
    result.push([pid, assetEntries]);
  }
  return result;
}

// ===========================================================================
// MultiassetCompact   { + policy_id => $hash32 }
// cddl: codec.cddl:263 — multiasset_compact<a0> = { + policy_id => $hash32 }
// fuel: fuel-types/src/canonical.rs:279 — impl Serialize for Vec<T> (static only, no dynamic)
//   encode_static (line 301): len u64 + per-policy (hash28 + hash32), all fixed-size
// Fully static, no dynamic.
// ===========================================================================

export type MultiassetCompact = Array<[PolicyId, Hash32]>;

// fuel: fuel-types/src/canonical.rs:301 — Vec<T>::encode_static
function writeMultiassetCompactStatic(w: Writer, mac: MultiassetCompact): void {
  writeU64(w, mac.length);
  for (const [pid, hash] of mac) {
    writeHash28Static(w, pid);
    writeHash32Static(w, hash);
  }
}

// fuel: fuel-types/src/canonical.rs:332 — Vec<T>::decode_static
function readMultiassetCompactStatic(r: Reader): MultiassetCompact {
  const len = readU64(r);
  const result: MultiassetCompact = [];
  for (let i = 0; i < len; i++) {
    const pid = readHash28Static(r);
    const hash = readHash32Static(r);
    result.push([pid, hash]);
  }
  return result;
}

// ===========================================================================
// Value   (Full Representation)
// cddl: codec.cddl:240 — value = coin / [coin, multiasset<positive_coin>]
// fuel: fuel-tx/src/transaction/types/output.rs:38 — Output enum (Fuel's enum with discriminant + fields)
// fuel: fuel-types/src/canonical.rs:71 — trait Serialize (encode_static writes discriminant + variant statics)
//   Coin variant:       discriminant(u64=0) + coin(u64)            static only
//   MultiAsset variant: discriminant(u64=1) + coin(u64) + ma.static + ma.dynamic
// ===========================================================================

export type Value =
  | { type: "Coin"; coin: Coin }
  | { type: "MultiAsset"; coin: Coin; assets: Multiasset };

type ValuePartial =
  | { type: "Coin"; coin: Coin }
  | { type: "MultiAsset"; coin: Coin; maPartial: MultiassetPartialEntry[] };

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (discriminant u64 + variant static fields)
function writeValueStatic(w: Writer, v: Value): void {
  if (v.type === "Coin") {
    writeU64(w, 0);
    writeU64(w, v.coin);
  } else {
    writeU64(w, 1);
    writeU64(w, v.coin);
    writeMultiassetStatic(w, v.assets);
  }
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic (MultiAsset variant only)
function writeValueDynamic(w: Writer, v: Value): void {
  if (v.type === "MultiAsset") {
    writeMultiassetDynamic(w, v.assets);
  }
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static (reads discriminant, branches on variant)
function readValueStatic(r: Reader): ValuePartial {
  const disc = readU64(r);
  if (disc === 0) {
    return { type: "Coin", coin: readU64(r) };
  } else if (disc === 1) {
    const coin = readU64(r);
    const maPartial = readMultiassetStatic(r);
    return { type: "MultiAsset", coin, maPartial };
  }
  throw new Error("UnknownDiscriminant for Value");
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic (MultiAsset variant only)
function readValueDynamic(r: Reader, partial: ValuePartial): Value {
  if (partial.type === "Coin") return partial;
  const assets = readMultiassetDynamic(r, partial.maPartial);
  return { type: "MultiAsset", coin: partial.coin, assets };
}

// ===========================================================================
// ValueCompact   (Compact Representation)
// cddl: codec.cddl:248 — value_compact = coin / [coin, $hash32]
// fuel: fuel-tx/src/transaction/types/output.rs:38 — Output enum (same discriminant pattern)
// fuel: fuel-types/src/canonical.rs:71 — trait Serialize
//   Coin variant:       discriminant(u64=0) + coin(u64)               static
//   MultiAsset variant: discriminant(u64=1) + coin(u64) + hash32(32)  static
//   No dynamic in either case.
// ===========================================================================

export type ValueCompact =
  | { type: "Coin"; coin: Coin }
  | { type: "MultiAsset"; coin: Coin; hash: Hash32 };

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (discriminant + variant static fields only)
function writeValueCompactStatic(w: Writer, v: ValueCompact): void {
  if (v.type === "Coin") {
    writeU64(w, 0);
    writeU64(w, v.coin);
  } else {
    writeU64(w, 1);
    writeU64(w, v.coin);
    writeHash32Static(w, v.hash);
  }
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static (reads discriminant + variant fields)
function readValueCompactStatic(r: Reader): ValueCompact {
  const disc = readU64(r);
  if (disc === 0) return { type: "Coin", coin: readU64(r) };
  if (disc === 1) {
    const coin = readU64(r);
    const hash = readHash32Static(r);
    return { type: "MultiAsset", coin, hash };
  }
  throw new Error("UnknownDiscriminant for ValueCompact");
}

// ===========================================================================
// TransactionOutput   (Full Representation)
// cddl: codec.cddl:221 — transaction_output = { 0: address, 1: value, ? 2: data, ? 3: script_ref }
// fuel: fuel-tx/src/transaction/types/output.rs:38 — Output enum (#[derive(canonical::Serialize)])
// fuel: fuel-types/src/canonical.rs:71  — trait Serialize
// fuel: fuel-types/src/canonical.rs:150 — trait Deserialize
//
// Fields: address, value, datum?, script_ref?
//
// Static:
//   address.static (= len u64)
//   value.static
//   datum.static   (= presence u64 [+ len u64 if Some])
//   script_ref.static (= presence u64 [+ len u64 if Some])
//
// Dynamic:
//   address.dynamic (bytes + pad)
//   value.dynamic   (multiasset bytes if MultiAsset)
//   datum.dynamic   (bytes + pad if Some)
//   script_ref.dynamic (bytes + pad if Some)
// ===========================================================================

export interface TransactionOutput {
  address: Uint8Array;
  value: Value;
  datum: Uint8Array | undefined;
  script_ref: Uint8Array | undefined;
}

interface TransactionOutputPartial {
  addrLen: number;
  value: ValuePartial;
  datumPresent: boolean;
  datumLen: number;
  scriptRefPresent: boolean;
  scriptRefLen: number;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static
// fuel: fuel-types/src/canonical.rs:301 — Vec<u8>::encode_static (address len + datum/script_ref lens)
function writeTransactionOutputStatic(w: Writer, o: TransactionOutput): void {
  writeAddressStatic(w, o.address);
  writeValueStatic(w, o.value);
  // datum: Option<Vec<u8>>
  if (o.datum !== undefined) {
    writeU64(w, 1);
    writeVarBytesStatic(w, o.datum);
  } else {
    writeU64(w, 0);
  }
  // script_ref: Option<Vec<u8>>
  if (o.script_ref !== undefined) {
    writeU64(w, 1);
    writeVarBytesStatic(w, o.script_ref);
  } else {
    writeU64(w, 0);
  }
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic
// fuel: fuel-types/src/canonical.rs:309 — Vec<u8>::encode_dynamic (bytes + tail alignment pad)
function writeTransactionOutputDynamic(w: Writer, o: TransactionOutput): void {
  writeAddressDynamic(w, o.address);
  writeValueDynamic(w, o.value);
  if (o.datum !== undefined) writeVarBytesDynamic(w, o.datum);
  if (o.script_ref !== undefined) writeVarBytesDynamic(w, o.script_ref);
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
// fuel: fuel-types/src/canonical.rs:332 — Vec<u8>::decode_static (reads lens, stores as capacity)
function readTransactionOutputStatic(r: Reader): TransactionOutputPartial {
  const addrLen = readAddressLen(r);
  const value = readValueStatic(r);
  const datumPresent = readU64(r) !== 0;
  const datumLen = datumPresent ? readVarBytesLen(r) : 0;
  const scriptRefPresent = readU64(r) !== 0;
  const scriptRefLen = scriptRefPresent ? readVarBytesLen(r) : 0;
  return {
    addrLen,
    value,
    datumPresent,
    datumLen,
    scriptRefPresent,
    scriptRefLen,
  };
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic
// fuel: fuel-types/src/canonical.rs:352 — Vec<u8>::decode_dynamic (reads bytes + skips tail pad)
function readTransactionOutputDynamic(
  r: Reader,
  p: TransactionOutputPartial,
): TransactionOutput {
  const address = readAddressDynamic(r, p.addrLen);
  const value = readValueDynamic(r, p.value);
  const datum = p.datumPresent ? readVarBytesDynamic(r, p.datumLen) : undefined;
  const script_ref = p.scriptRefPresent
    ? readVarBytesDynamic(r, p.scriptRefLen)
    : undefined;
  return { address, value, datum, script_ref };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeTransactionOutput(o: TransactionOutput): Uint8Array {
  const sw = new Writer();
  writeTransactionOutputStatic(sw, o);
  const dw = new Writer();
  writeTransactionOutputDynamic(dw, o);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeTransactionOutput(bytes: Uint8Array): TransactionOutput {
  const r = new Reader(bytes);
  const partial = readTransactionOutputStatic(r);
  return readTransactionOutputDynamic(r, partial);
}

// ===========================================================================
// TransactionOutputCompact   (Compact Representation)
// cddl: codec.cddl:231 — transaction_output_compact = { 0: address, 1: value_compact, ? 2: $hash32, ? 3: $hash32 }
// fuel: fuel-tx/src/transaction/types/output.rs:38 — Output enum
// fuel: fuel-types/src/canonical.rs:71  — trait Serialize
// fuel: fuel-types/src/canonical.rs:150 — trait Deserialize
//
// Fields: address, value (ValueCompact), datum_hash?, script_ref_hash?
//
// Static:
//   address.static (len u64)
//   value_compact.static (disc + coin [+ hash32 if MultiAsset])
//   datum_hash.static    (presence u64 [+ 32 bytes if Some])
//   script_ref_hash.static (presence u64 [+ 32 bytes if Some])
//
// Dynamic:
//   address.dynamic (bytes + pad)
//   (everything else is static)
// ===========================================================================

export interface TransactionOutputCompact {
  address: Uint8Array;
  value: ValueCompact;
  datum_hash: Hash32 | undefined;
  script_ref_hash: Hash32 | undefined;
}

interface TransactionOutputCompactPartial {
  addrLen: number;
  value: ValueCompact;
  datumHash: Hash32 | undefined;
  scriptRefHash: Hash32 | undefined;
}

// fuel: fuel-types/src/canonical.rs:101 — Serialize::encode_static (all fields fixed-size except address len)
function writeTransactionOutputCompactStatic(
  w: Writer,
  o: TransactionOutputCompact,
): void {
  writeAddressStatic(w, o.address);
  writeValueCompactStatic(w, o.value);
  if (o.datum_hash !== undefined) {
    writeU64(w, 1);
    writeHash32Static(w, o.datum_hash);
  } else {
    writeU64(w, 0);
  }
  if (o.script_ref_hash !== undefined) {
    writeU64(w, 1);
    writeHash32Static(w, o.script_ref_hash);
  } else {
    writeU64(w, 0);
  }
}

// fuel: fuel-types/src/canonical.rs:106 — Serialize::encode_dynamic (address bytes only)
function writeTransactionOutputCompactDynamic(
  w: Writer,
  o: TransactionOutputCompact,
): void {
  writeAddressDynamic(w, o.address);
  // datum_hash, script_ref_hash are Hash32 → no dynamic
}

// fuel: fuel-types/src/canonical.rs:167 — Deserialize::decode_static
function readTransactionOutputCompactStatic(
  r: Reader,
): TransactionOutputCompactPartial {
  const addrLen = readAddressLen(r);
  const value = readValueCompactStatic(r);
  const dhPresent = readU64(r) !== 0;
  const datumHash = dhPresent ? readHash32Static(r) : undefined;
  const srPresent = readU64(r) !== 0;
  const scriptRefHash = srPresent ? readHash32Static(r) : undefined;
  return { addrLen, value, datumHash, scriptRefHash };
}

// fuel: fuel-types/src/canonical.rs:172 — Deserialize::decode_dynamic (address bytes only)
function readTransactionOutputCompactDynamic(
  r: Reader,
  p: TransactionOutputCompactPartial,
): TransactionOutputCompact {
  const address = readAddressDynamic(r, p.addrLen);
  return {
    address,
    value: p.value,
    datum_hash: p.datumHash,
    script_ref_hash: p.scriptRefHash,
  };
}

// fuel: fuel-types/src/canonical.rs:112 — Serialize::to_bytes
export function encodeTransactionOutputCompact(
  o: TransactionOutputCompact,
): Uint8Array {
  const sw = new Writer();
  writeTransactionOutputCompactStatic(sw, o);
  const dw = new Writer();
  writeTransactionOutputCompactDynamic(dw, o);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

// fuel: fuel-types/src/canonical.rs:180 — Deserialize::from_bytes
export function decodeTransactionOutputCompact(
  bytes: Uint8Array,
): TransactionOutputCompact {
  const r = new Reader(bytes);
  const partial = readTransactionOutputCompactStatic(r);
  return readTransactionOutputCompactDynamic(r, partial);
}

// Re-export internal helpers needed by other modules (block.ts, transaction.ts).
export {
  writeTransactionOutputStatic,
  writeTransactionOutputDynamic,
  readTransactionOutputStatic,
  readTransactionOutputDynamic,
  TransactionOutputPartial,
};
