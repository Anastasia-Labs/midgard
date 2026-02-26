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
// ---------------------------------------------------------------------------

export interface VKeyWitness {
  vkey: VKey; // 32 bytes
  signature: Signature; // 64 bytes
}

export function writeVKeyWitness(w: Writer, ww: VKeyWitness): void {
  writeVKeyStatic(w, ww.vkey);
  writeSignatureStatic(w, ww.signature);
}

export function readVKeyWitness(r: Reader): VKeyWitness {
  const vkey = readVKeyStatic(r);
  const signature = readSignatureStatic(r);
  return { vkey, signature };
}

// ===========================================================================
// Multiasset<u64>   (Full Representation)
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

function writeMultiassetDynamic(w: Writer, ma: Multiasset): void {
  for (const [, assets] of ma) {
    for (const [name] of assets) {
      writeAssetNameDynamic(w, name); // actual bytes + pad
    }
  }
}

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
// Fully static, no dynamic.
// ===========================================================================

export type MultiassetCompact = Array<[PolicyId, Hash32]>;

function writeMultiassetCompactStatic(w: Writer, mac: MultiassetCompact): void {
  writeU64(w, mac.length);
  for (const [pid, hash] of mac) {
    writeHash28Static(w, pid);
    writeHash32Static(w, hash);
  }
}

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
//   Coin variant:       discriminant(u64=0) + coin(u64)            static only
//   MultiAsset variant: discriminant(u64=1) + coin(u64) + ma.static + ma.dynamic
// ===========================================================================

export type Value =
  | { type: "Coin"; coin: Coin }
  | { type: "MultiAsset"; coin: Coin; assets: Multiasset };

type ValuePartial =
  | { type: "Coin"; coin: Coin }
  | { type: "MultiAsset"; coin: Coin; maPartial: MultiassetPartialEntry[] };

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

function writeValueDynamic(w: Writer, v: Value): void {
  if (v.type === "MultiAsset") {
    writeMultiassetDynamic(w, v.assets);
  }
}

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

function readValueDynamic(r: Reader, partial: ValuePartial): Value {
  if (partial.type === "Coin") return partial;
  const assets = readMultiassetDynamic(r, partial.maPartial);
  return { type: "MultiAsset", coin: partial.coin, assets };
}

// ===========================================================================
// ValueCompact   (Compact Representation)
//   Coin variant:       discriminant(u64=0) + coin(u64)               static
//   MultiAsset variant: discriminant(u64=1) + coin(u64) + hash32(32)  static
//   No dynamic in either case.
// ===========================================================================

export type ValueCompact =
  | { type: "Coin"; coin: Coin }
  | { type: "MultiAsset"; coin: Coin; hash: Hash32 };

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

function writeTransactionOutputDynamic(w: Writer, o: TransactionOutput): void {
  writeAddressDynamic(w, o.address);
  writeValueDynamic(w, o.value);
  if (o.datum !== undefined) writeVarBytesDynamic(w, o.datum);
  if (o.script_ref !== undefined) writeVarBytesDynamic(w, o.script_ref);
}

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

export function decodeTransactionOutput(bytes: Uint8Array): TransactionOutput {
  const r = new Reader(bytes);
  const partial = readTransactionOutputStatic(r);
  return readTransactionOutputDynamic(r, partial);
}

// ===========================================================================
// TransactionOutputCompact   (Compact Representation)
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

function writeTransactionOutputCompactDynamic(
  w: Writer,
  o: TransactionOutputCompact,
): void {
  writeAddressDynamic(w, o.address);
  // datum_hash, script_ref_hash are Hash32 → no dynamic
}

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
