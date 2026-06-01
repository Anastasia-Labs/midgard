/**
 * Binary encoders/decoders for primitive Midgard types used by the native
 * transaction codec. All encodings follow the 8-byte-aligned static/dynamic
 * layout defined in `./binary.ts` (staging:midgard-ts/src/codec.ts).
 */

import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  BinaryReader,
  BinaryWriter,
  readBigU64,
  readBigI64,
  readFixedBytes,
  readU16,
  readU64,
  readVarBytesDynamic,
  readVarBytesLen,
  writeBigU64,
  writeBigI64,
  writeFixedBytes,
  writeU16,
  writeU64,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import {
  HASH32_LENGTH,
  ensureHash32,
  type Hash32,
} from "./hash.js";

// ---------------------------------------------------------------------------
// Hash28 / Hash32 / VKey / Signature
// ---------------------------------------------------------------------------

export const HASH28_LENGTH = 28;
export const VKEY_LENGTH = 32;
export const SIGNATURE_LENGTH = 64;

export type Hash28 = Buffer;
export type VKey = Buffer;
export type Signature = Buffer;
export type AddrKeyHash = Hash28;
export type ScriptHash = Hash28;
export type TransactionId = Hash32;

const ensureLen = (
  value: Uint8Array,
  expected: number,
  fieldName: string,
): Buffer => {
  if (value.length !== expected) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be ${expected} bytes`,
      `length=${value.length}`,
    );
  }
  return Buffer.from(value);
};

export const ensureHash28 = (value: Uint8Array, fieldName: string): Hash28 =>
  ensureLen(value, HASH28_LENGTH, fieldName);

export const ensureVKey = (value: Uint8Array, fieldName: string): VKey =>
  ensureLen(value, VKEY_LENGTH, fieldName);

export const ensureSignature = (value: Uint8Array, fieldName: string): Signature =>
  ensureLen(value, SIGNATURE_LENGTH, fieldName);

export const writeHash28 = (w: BinaryWriter, h: Hash28): void =>
  writeFixedBytes(w, ensureHash28(h, "hash28"));

export const readHash28 = (r: BinaryReader): Hash28 =>
  readFixedBytes(r, HASH28_LENGTH);

export const writeHash32 = (w: BinaryWriter, h: Hash32): void =>
  writeFixedBytes(w, ensureHash32(h, "hash32"));

export const readHash32 = (r: BinaryReader): Hash32 =>
  readFixedBytes(r, HASH32_LENGTH);

export const writeVKey = (w: BinaryWriter, v: VKey): void =>
  writeFixedBytes(w, ensureVKey(v, "vkey"));

export const readVKey = (r: BinaryReader): VKey =>
  readFixedBytes(r, VKEY_LENGTH);

export const writeSignature = (w: BinaryWriter, s: Signature): void =>
  writeFixedBytes(w, ensureSignature(s, "signature"));

export const readSignature = (r: BinaryReader): Signature =>
  readFixedBytes(r, SIGNATURE_LENGTH);

// ---------------------------------------------------------------------------
// OutputReference  =  [tx_id : hash32, index : u16]
// Fully static: 32 + 8 = 40 bytes.
// ---------------------------------------------------------------------------

export type OutputReference = {
  readonly txId: TransactionId;
  readonly index: number;
};

export const writeOutputReferenceStatic = (
  w: BinaryWriter,
  outref: OutputReference,
): void => {
  writeHash32(w, outref.txId);
  writeU16(w, outref.index);
};

export const readOutputReferenceStatic = (r: BinaryReader): OutputReference => {
  const txId = readHash32(r);
  const index = readU16(r);
  return { txId, index };
};

/** Encode a single OutputReference to its 40-byte binary form. */
export const encodeOutputReference = (outref: OutputReference): Buffer => {
  const w = new BinaryWriter();
  writeOutputReferenceStatic(w, outref);
  return w.toBytes();
};

export const decodeOutputReference = (bytes: Uint8Array): OutputReference => {
  const r = new BinaryReader(bytes);
  const value = readOutputReferenceStatic(r);
  r.expectEnd("output_reference");
  return value;
};

// ---------------------------------------------------------------------------
// Vec<OutputReference> — len (u64) + n × outref_static (40)
// ---------------------------------------------------------------------------

export const writeOutputReferenceListStatic = (
  w: BinaryWriter,
  list: readonly OutputReference[],
): void => {
  writeU64(w, list.length);
  for (const o of list) writeOutputReferenceStatic(w, o);
};

export const readOutputReferenceListStatic = (
  r: BinaryReader,
): OutputReference[] => {
  const len = readU64(r);
  const list: OutputReference[] = [];
  for (let i = 0; i < len; i += 1) list.push(readOutputReferenceStatic(r));
  return list;
};

/** Encode a list of OutputReferences in their own self-contained byte blob. */
export const encodeOutputReferenceList = (
  list: readonly OutputReference[],
): Buffer => {
  const w = new BinaryWriter();
  writeOutputReferenceListStatic(w, list);
  return w.toBytes();
};

export const decodeOutputReferenceList = (
  bytes: Uint8Array,
): OutputReference[] => {
  const r = new BinaryReader(bytes);
  const list = readOutputReferenceListStatic(r);
  r.expectEnd("output_reference_list");
  return list;
};

// ---------------------------------------------------------------------------
// Vec<Hash28> — len (u64) + n × hash28 (32)
// ---------------------------------------------------------------------------

export const writeHash28ListStatic = (
  w: BinaryWriter,
  list: readonly Hash28[],
): void => {
  writeU64(w, list.length);
  for (const h of list) writeHash28(w, h);
};

export const readHash28ListStatic = (r: BinaryReader): Hash28[] => {
  const len = readU64(r);
  const list: Hash28[] = [];
  for (let i = 0; i < len; i += 1) list.push(readHash28(r));
  return list;
};

export const encodeHash28List = (list: readonly Hash28[]): Buffer => {
  const w = new BinaryWriter();
  writeHash28ListStatic(w, list);
  return w.toBytes();
};

export const decodeHash28List = (bytes: Uint8Array): Hash28[] => {
  const r = new BinaryReader(bytes);
  const list = readHash28ListStatic(r);
  r.expectEnd("hash28_list");
  return list;
};

// ---------------------------------------------------------------------------
// Vec<Buffer> — len (u64) + n × var-bytes (u64 len + bytes + pad)
// Used for required_observers (mix of 28-byte hashes and CBOR credential
// envelopes).
// ---------------------------------------------------------------------------

export const writeBytesListStatic = (
  w: BinaryWriter,
  list: readonly Uint8Array[],
): void => {
  writeU64(w, list.length);
  for (const b of list) writeVarBytesStatic(w, b);
};

export const writeBytesListDynamic = (
  w: BinaryWriter,
  list: readonly Uint8Array[],
): void => {
  for (const b of list) writeVarBytesDynamic(w, b);
};

export const readBytesListStatic = (
  r: BinaryReader,
): { readonly count: number; readonly lengths: readonly number[] } => {
  const count = readU64(r);
  const lengths: number[] = [];
  for (let i = 0; i < count; i += 1) lengths.push(readVarBytesLen(r));
  return { count, lengths };
};

export const readBytesListDynamic = (
  r: BinaryReader,
  lengths: readonly number[],
): Buffer[] => {
  const list: Buffer[] = [];
  for (const len of lengths) list.push(readVarBytesDynamic(r, len));
  return list;
};

export const encodeBytesList = (list: readonly Uint8Array[]): Buffer => {
  const sw = new BinaryWriter();
  writeBytesListStatic(sw, list);
  const dw = new BinaryWriter();
  writeBytesListDynamic(dw, list);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeBytesList = (bytes: Uint8Array): Buffer[] => {
  const r = new BinaryReader(bytes);
  const { lengths } = readBytesListStatic(r);
  const list = readBytesListDynamic(r, lengths);
  r.expectEnd("bytes_list");
  return list;
};

// ---------------------------------------------------------------------------
// VKeyWitness  =  [vkey (32), signature (64)]  (fully static, 96 bytes total)
// ---------------------------------------------------------------------------

export type VKeyWitness = {
  readonly vkey: VKey;
  readonly signature: Signature;
};

export const writeVKeyWitnessStatic = (
  w: BinaryWriter,
  ww: VKeyWitness,
): void => {
  writeVKey(w, ww.vkey);
  writeSignature(w, ww.signature);
};

export const readVKeyWitnessStatic = (r: BinaryReader): VKeyWitness => {
  const vkey = readVKey(r);
  const signature = readSignature(r);
  return { vkey, signature };
};

export const encodeVKeyWitness = (ww: VKeyWitness): Buffer => {
  const w = new BinaryWriter();
  writeVKeyWitnessStatic(w, ww);
  return w.toBytes();
};

export const decodeVKeyWitness = (bytes: Uint8Array): VKeyWitness => {
  const r = new BinaryReader(bytes);
  const ww = readVKeyWitnessStatic(r);
  r.expectEnd("vkey_witness");
  return ww;
};

// ---------------------------------------------------------------------------
// Vec<VKeyWitness> — len (u64) + n × 96 bytes
// ---------------------------------------------------------------------------

export const writeVKeyWitnessListStatic = (
  w: BinaryWriter,
  list: readonly VKeyWitness[],
): void => {
  writeU64(w, list.length);
  for (const ww of list) writeVKeyWitnessStatic(w, ww);
};

export const readVKeyWitnessListStatic = (r: BinaryReader): VKeyWitness[] => {
  const len = readU64(r);
  const list: VKeyWitness[] = [];
  for (let i = 0; i < len; i += 1) list.push(readVKeyWitnessStatic(r));
  return list;
};

export const encodeVKeyWitnessList = (list: readonly VKeyWitness[]): Buffer => {
  const w = new BinaryWriter();
  writeVKeyWitnessListStatic(w, list);
  return w.toBytes();
};

export const decodeVKeyWitnessList = (bytes: Uint8Array): VKeyWitness[] => {
  const r = new BinaryReader(bytes);
  const list = readVKeyWitnessListStatic(r);
  r.expectEnd("vkey_witness_list");
  return list;
};

// ---------------------------------------------------------------------------
// PolicyId / AssetName ordering helpers (deterministic by raw bytes).
// ---------------------------------------------------------------------------

export const compareBytes = (a: Uint8Array, b: Uint8Array): number => {
  const len = Math.min(a.length, b.length);
  for (let i = 0; i < len; i += 1) {
    if (a[i] !== b[i]) return a[i] - b[i];
  }
  return a.length - b.length;
};

export { writeBigU64, writeBigI64, readBigU64, readBigI64, writeU64, readU64 };
