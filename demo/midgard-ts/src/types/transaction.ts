/**
 * Transaction types (Full and Compact Representations).
 *
 * TransactionBody uses a u64 bitmask (opts_mask) in the static section to
 * indicate which of the nine optional fields are present.  Those fields are
 * then encoded sequentially in the dynamic section.
 *
 * TransactionWitnessSet uses a u64 presence bitmask in the static section;
 * all field data lives in the dynamic section.
 */

import {
  Writer,
  Reader,
  writeU64,
  readU64,
  writeBool,
  readBool,
  writeVarBytesStatic,
  writeVarBytesDynamic,
  readVarBytesLen,
  readVarBytesDynamic,
  writeFixedBytes,
  readFixedBytes,
  alignmentBytes,
} from "../codec";

import {
  Hash32,
  AddrKeyHash,
  ScriptHash,
  OutputReference,
  writeHash28Static,
  readHash28Static,
  writeHash32Static,
  readHash32Static,
  writeOutputReferenceStatic,
  readOutputReferenceStatic,
} from "./primitives";

import {
  VKeyWitness,
  TransactionOutput,
  TransactionOutputPartial,
  writeVKeyWitness,
  readVKeyWitness,
  writeTransactionOutputStatic,
  writeTransactionOutputDynamic,
  readTransactionOutputStatic,
  readTransactionOutputDynamic,
} from "./output";

// ===========================================================================
// TransactionWitnessSet   (Full Representation)
//
// Static:  presence_mask (u64)
// Dynamic (for each present field):
//   vkey_witnesses:   len(u64) + for each: vkey(32)+sig(64)
//   native_scripts:   len(u64) + for each: blen(u64)+bytes+pad
//   redeemers:        blen(u64) + bytes+pad
//   plutus_v3_scripts: len(u64) + for each: blen(u64)+bytes+pad
// ===========================================================================

export interface TransactionWitnessSet {
  vkey_witnesses: VKeyWitness[] | undefined;
  native_scripts: Uint8Array[] | undefined;
  redeemers: Uint8Array | undefined;
  plutus_v3_scripts: Uint8Array[] | undefined;
}

function witnessMask(ws: TransactionWitnessSet): number {
  let m = 0;
  if (ws.vkey_witnesses !== undefined) m |= 1 << 0;
  if (ws.native_scripts !== undefined) m |= 1 << 1;
  if (ws.redeemers !== undefined) m |= 1 << 2;
  if (ws.plutus_v3_scripts !== undefined) m |= 1 << 3;
  return m;
}

function writeTransactionWitnessSetStatic(
  w: Writer,
  ws: TransactionWitnessSet,
): void {
  writeU64(w, witnessMask(ws));
}

function writeTransactionWitnessSetDynamic(
  w: Writer,
  ws: TransactionWitnessSet,
): void {
  if (ws.vkey_witnesses !== undefined) {
    writeU64(w, ws.vkey_witnesses.length);
    for (const ww of ws.vkey_witnesses) writeVKeyWitness(w, ww);
  }
  if (ws.native_scripts !== undefined) {
    writeU64(w, ws.native_scripts.length);
    for (const s of ws.native_scripts) {
      writeU64(w, s.length);
      writeVarBytesDynamic(w, s);
    }
  }
  if (ws.redeemers !== undefined) {
    writeU64(w, ws.redeemers.length);
    writeVarBytesDynamic(w, ws.redeemers);
  }
  if (ws.plutus_v3_scripts !== undefined) {
    writeU64(w, ws.plutus_v3_scripts.length);
    for (const s of ws.plutus_v3_scripts) {
      writeU64(w, s.length);
      writeVarBytesDynamic(w, s);
    }
  }
}

function readTransactionWitnessSetStatic(r: Reader): number {
  return readU64(r); // just the mask
}

function readTransactionWitnessSetDynamic(
  r: Reader,
  mask: number,
): TransactionWitnessSet {
  let vkey_witnesses: VKeyWitness[] | undefined;
  let native_scripts: Uint8Array[] | undefined;
  let redeemers: Uint8Array | undefined;
  let plutus_v3_scripts: Uint8Array[] | undefined;

  if (mask & (1 << 0)) {
    const len = readU64(r);
    vkey_witnesses = [];
    for (let i = 0; i < len; i++) vkey_witnesses.push(readVKeyWitness(r));
  }
  if (mask & (1 << 1)) {
    const len = readU64(r);
    native_scripts = [];
    for (let i = 0; i < len; i++) {
      const blen = readU64(r);
      native_scripts.push(readVarBytesDynamic(r, blen));
    }
  }
  if (mask & (1 << 2)) {
    const blen = readU64(r);
    redeemers = readVarBytesDynamic(r, blen);
  }
  if (mask & (1 << 3)) {
    const len = readU64(r);
    plutus_v3_scripts = [];
    for (let i = 0; i < len; i++) {
      const blen = readU64(r);
      plutus_v3_scripts.push(readVarBytesDynamic(r, blen));
    }
  }
  return { vkey_witnesses, native_scripts, redeemers, plutus_v3_scripts };
}

// ===========================================================================
// TransactionWitnessSetCompact   (Compact Representation)
//
// Fields: four optional Hash32 hashes — all encoded as Option<Hash32> (static).
// No dynamic section.
// ===========================================================================

export interface TransactionWitnessSetCompact {
  vkey_witnesses_hash: Hash32 | undefined;
  native_scripts_hash: Hash32 | undefined;
  redeemers_hash: Hash32 | undefined;
  plutus_v3_scripts_hash: Hash32 | undefined;
}

function writeTransactionWitnessSetCompactStatic(
  w: Writer,
  ws: TransactionWitnessSetCompact,
): void {
  const writeOpt = (h: Hash32 | undefined) => {
    if (h !== undefined) {
      writeU64(w, 1);
      writeHash32Static(w, h);
    } else writeU64(w, 0);
  };
  writeOpt(ws.vkey_witnesses_hash);
  writeOpt(ws.native_scripts_hash);
  writeOpt(ws.redeemers_hash);
  writeOpt(ws.plutus_v3_scripts_hash);
}

function readTransactionWitnessSetCompactStatic(
  r: Reader,
): TransactionWitnessSetCompact {
  const readOpt = (): Hash32 | undefined => {
    return readU64(r) !== 0 ? readHash32Static(r) : undefined;
  };
  return {
    vkey_witnesses_hash: readOpt(),
    native_scripts_hash: readOpt(),
    redeemers_hash: readOpt(),
    plutus_v3_scripts_hash: readOpt(),
  };
}

export function encodeTransactionWitnessSetCompact(
  ws: TransactionWitnessSetCompact,
): Uint8Array {
  const w = new Writer();
  writeTransactionWitnessSetCompactStatic(w, ws);
  return w.toBytes();
}

export function decodeTransactionWitnessSetCompact(
  bytes: Uint8Array,
): TransactionWitnessSetCompact {
  const r = new Reader(bytes);
  return readTransactionWitnessSetCompactStatic(r);
}

// ===========================================================================
// TransactionBody   (Full Representation)
//
// Mandatory: inputs (Vec<OutputReference>), outputs (Vec<TransactionOutput>), fee
// Optional (bitmask):
//   bit 0 → ttl (u64)
//   bit 1 → auxiliary_data_hash (Hash32)
//   bit 2 → validity_interval_start (u64)
//   bit 3 → mint (Vec<u8>)
//   bit 4 → script_data_hash (Hash32)
//   bit 5 → required_signers (Vec<Hash28>)
//   bit 6 → network_id (u64)
//   bit 7 → reference_inputs (Vec<OutputReference>)
//   bit 8 → required_observers (Vec<Hash28>)
//
// Static section:
//   inputs_len(u64) + for each input: outref_static(40)
//   outputs_len(u64) + for each output: txout_static
//   fee(u64)
//   opts_mask(u64)
//
// Dynamic section:
//   for each input:  outref_dynamic = nothing
//   for each output: txout_dynamic
//   optional fields in bit order (full encode per field):
//     ttl: u64 (8)
//     auxiliary_data_hash: Hash32 (32)
//     validity_interval_start: u64 (8)
//     mint: len(u64) + bytes+pad
//     script_data_hash: Hash32 (32)
//     required_signers: len(u64) + n×Hash28(32)
//     network_id: u64 (8)
//     reference_inputs: len(u64) + n×outref(40)
//     required_observers: len(u64) + n×Hash28(32)
// ===========================================================================

export interface TransactionBody {
  inputs: OutputReference[];
  outputs: TransactionOutput[];
  fee: number;
  ttl: number | undefined;
  auxiliary_data_hash: Hash32 | undefined;
  validity_interval_start: number | undefined;
  mint: Uint8Array | undefined;
  script_data_hash: Hash32 | undefined;
  required_signers: Uint8Array[] | undefined; // Vec<Hash28>
  network_id: number | undefined;
  reference_inputs: OutputReference[] | undefined;
  required_observers: Uint8Array[] | undefined; // Vec<Hash28>
}

function optsMask(b: TransactionBody): number {
  let m = 0;
  if (b.ttl !== undefined) m |= 1 << 0;
  if (b.auxiliary_data_hash !== undefined) m |= 1 << 1;
  if (b.validity_interval_start !== undefined) m |= 1 << 2;
  if (b.mint !== undefined) m |= 1 << 3;
  if (b.script_data_hash !== undefined) m |= 1 << 4;
  if (b.required_signers !== undefined) m |= 1 << 5;
  if (b.network_id !== undefined) m |= 1 << 6;
  if (b.reference_inputs !== undefined) m |= 1 << 7;
  if (b.required_observers !== undefined) m |= 1 << 8;
  return m;
}

// Partial state for TransactionBody: stores output partials so dynamic phase
// can complete them.
interface TransactionBodyPartial {
  inputs: OutputReference[];
  outPartials: TransactionOutputPartial[];
  fee: number;
  mask: number;
}

function writeTransactionBodyStatic(w: Writer, b: TransactionBody): void {
  writeU64(w, b.inputs.length);
  for (const inp of b.inputs) writeOutputReferenceStatic(w, inp);
  writeU64(w, b.outputs.length);
  for (const out of b.outputs) writeTransactionOutputStatic(w, out);
  writeU64(w, b.fee);
  writeU64(w, optsMask(b));
}

function writeTransactionBodyDynamic(w: Writer, b: TransactionBody): void {
  // inputs have no dynamic part (OutputReference is fully static)
  for (const out of b.outputs) writeTransactionOutputDynamic(w, out);

  // Optional fields — each written as full encode (static+dynamic together,
  // but for these types static=full since they have no nested dynamic).
  if (b.ttl !== undefined) writeU64(w, b.ttl);
  if (b.auxiliary_data_hash !== undefined)
    writeHash32Static(w, b.auxiliary_data_hash);
  if (b.validity_interval_start !== undefined)
    writeU64(w, b.validity_interval_start);
  if (b.mint !== undefined) {
    writeU64(w, b.mint.length);
    writeVarBytesDynamic(w, b.mint);
  }
  if (b.script_data_hash !== undefined)
    writeHash32Static(w, b.script_data_hash);
  if (b.required_signers !== undefined) {
    writeU64(w, b.required_signers.length);
    for (const h of b.required_signers) writeHash28Static(w, h);
  }
  if (b.network_id !== undefined) writeU64(w, b.network_id);
  if (b.reference_inputs !== undefined) {
    writeU64(w, b.reference_inputs.length);
    for (const inp of b.reference_inputs) writeOutputReferenceStatic(w, inp);
  }
  if (b.required_observers !== undefined) {
    writeU64(w, b.required_observers.length);
    for (const h of b.required_observers) writeHash28Static(w, h);
  }
}

function readTransactionBodyStatic(r: Reader): TransactionBodyPartial {
  const inputsLen = readU64(r);
  const inputs: OutputReference[] = [];
  for (let i = 0; i < inputsLen; i++) inputs.push(readOutputReferenceStatic(r));

  const outputsLen = readU64(r);
  const outPartials: TransactionOutputPartial[] = [];
  for (let i = 0; i < outputsLen; i++)
    outPartials.push(readTransactionOutputStatic(r));

  const fee = readU64(r);
  const mask = readU64(r);
  return { inputs, outPartials, fee, mask };
}

function readTransactionBodyDynamic(
  r: Reader,
  p: TransactionBodyPartial,
): TransactionBody {
  // Complete outputs
  const outputs: TransactionOutput[] = [];
  for (const op of p.outPartials)
    outputs.push(readTransactionOutputDynamic(r, op));

  const mask = p.mask;
  const ttl = mask & (1 << 0) ? readU64(r) : undefined;
  const auxiliary_data_hash = mask & (1 << 1) ? readHash32Static(r) : undefined;
  const validity_interval_start = mask & (1 << 2) ? readU64(r) : undefined;
  let mint: Uint8Array | undefined;
  if (mask & (1 << 3)) {
    const blen = readU64(r);
    mint = readVarBytesDynamic(r, blen);
  }
  const script_data_hash = mask & (1 << 4) ? readHash32Static(r) : undefined;
  let required_signers: Uint8Array[] | undefined;
  if (mask & (1 << 5)) {
    const len = readU64(r);
    required_signers = [];
    for (let i = 0; i < len; i++) required_signers.push(readHash28Static(r));
  }
  const network_id = mask & (1 << 6) ? readU64(r) : undefined;
  let reference_inputs: OutputReference[] | undefined;
  if (mask & (1 << 7)) {
    const len = readU64(r);
    reference_inputs = [];
    for (let i = 0; i < len; i++)
      reference_inputs.push(readOutputReferenceStatic(r));
  }
  let required_observers: Uint8Array[] | undefined;
  if (mask & (1 << 8)) {
    const len = readU64(r);
    required_observers = [];
    for (let i = 0; i < len; i++) required_observers.push(readHash28Static(r));
  }

  return {
    inputs: p.inputs,
    outputs,
    fee: p.fee,
    ttl,
    auxiliary_data_hash,
    validity_interval_start,
    mint,
    script_data_hash,
    required_signers,
    network_id,
    reference_inputs,
    required_observers,
  };
}

export function encodeTransactionBody(b: TransactionBody): Uint8Array {
  const sw = new Writer();
  writeTransactionBodyStatic(sw, b);
  const dw = new Writer();
  writeTransactionBodyDynamic(dw, b);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

export function decodeTransactionBody(bytes: Uint8Array): TransactionBody {
  const r = new Reader(bytes);
  const p = readTransactionBodyStatic(r);
  return readTransactionBodyDynamic(r, p);
}

// ===========================================================================
// TransactionBodyCompact   (Compact Representation)
//
// Mandatory: inputs_hash(Hash32), outputs_hash(Hash32), fee(u64), opts_mask(u64)
// Optional (bitmask — same bit layout):
//   bit 0 → ttl (u64)
//   bit 1 → auxiliary_data_hash (Hash32)
//   bit 2 → validity_interval_start (u64)
//   bit 3 → mint_hash (Hash32)
//   bit 4 → script_data_hash (Hash32)
//   bit 5 → required_signers_hash (Hash32)
//   bit 6 → network_id (u64)
//   bit 7 → reference_inputs_hash (Hash32)
//   bit 8 → required_observers_hash (Hash32)
//
// Static: inputs_hash(32) + outputs_hash(32) + fee(8) + opts_mask(8)
// Dynamic: optional fields in bit order
// ===========================================================================

export interface TransactionBodyCompact {
  inputs_hash: Hash32;
  outputs_hash: Hash32;
  fee: number;
  ttl: number | undefined;
  auxiliary_data_hash: Hash32 | undefined;
  validity_interval_start: number | undefined;
  mint_hash: Hash32 | undefined;
  script_data_hash: Hash32 | undefined;
  required_signers_hash: Hash32 | undefined;
  network_id: number | undefined;
  reference_inputs_hash: Hash32 | undefined;
  required_observers_hash: Hash32 | undefined;
}

function optsMaskCompact(b: TransactionBodyCompact): number {
  let m = 0;
  if (b.ttl !== undefined) m |= 1 << 0;
  if (b.auxiliary_data_hash !== undefined) m |= 1 << 1;
  if (b.validity_interval_start !== undefined) m |= 1 << 2;
  if (b.mint_hash !== undefined) m |= 1 << 3;
  if (b.script_data_hash !== undefined) m |= 1 << 4;
  if (b.required_signers_hash !== undefined) m |= 1 << 5;
  if (b.network_id !== undefined) m |= 1 << 6;
  if (b.reference_inputs_hash !== undefined) m |= 1 << 7;
  if (b.required_observers_hash !== undefined) m |= 1 << 8;
  return m;
}

function writeTransactionBodyCompactStatic(
  w: Writer,
  b: TransactionBodyCompact,
): void {
  writeHash32Static(w, b.inputs_hash);
  writeHash32Static(w, b.outputs_hash);
  writeU64(w, b.fee);
  writeU64(w, optsMaskCompact(b));
}

function writeTransactionBodyCompactDynamic(
  w: Writer,
  b: TransactionBodyCompact,
): void {
  if (b.ttl !== undefined) writeU64(w, b.ttl);
  if (b.auxiliary_data_hash !== undefined)
    writeHash32Static(w, b.auxiliary_data_hash);
  if (b.validity_interval_start !== undefined)
    writeU64(w, b.validity_interval_start);
  if (b.mint_hash !== undefined) writeHash32Static(w, b.mint_hash);
  if (b.script_data_hash !== undefined)
    writeHash32Static(w, b.script_data_hash);
  if (b.required_signers_hash !== undefined)
    writeHash32Static(w, b.required_signers_hash);
  if (b.network_id !== undefined) writeU64(w, b.network_id);
  if (b.reference_inputs_hash !== undefined)
    writeHash32Static(w, b.reference_inputs_hash);
  if (b.required_observers_hash !== undefined)
    writeHash32Static(w, b.required_observers_hash);
}

function readTransactionBodyCompact(r: Reader): TransactionBodyCompact {
  const inputs_hash = readHash32Static(r);
  const outputs_hash = readHash32Static(r);
  const fee = readU64(r);
  const mask = readU64(r);

  const ttl = mask & (1 << 0) ? readU64(r) : undefined;
  const auxiliary_data_hash = mask & (1 << 1) ? readHash32Static(r) : undefined;
  const validity_interval_start = mask & (1 << 2) ? readU64(r) : undefined;
  const mint_hash = mask & (1 << 3) ? readHash32Static(r) : undefined;
  const script_data_hash = mask & (1 << 4) ? readHash32Static(r) : undefined;
  const required_signers_hash =
    mask & (1 << 5) ? readHash32Static(r) : undefined;
  const network_id = mask & (1 << 6) ? readU64(r) : undefined;
  const reference_inputs_hash =
    mask & (1 << 7) ? readHash32Static(r) : undefined;
  const required_observers_hash =
    mask & (1 << 8) ? readHash32Static(r) : undefined;

  return {
    inputs_hash,
    outputs_hash,
    fee,
    ttl,
    auxiliary_data_hash,
    validity_interval_start,
    mint_hash,
    script_data_hash,
    required_signers_hash,
    network_id,
    reference_inputs_hash,
    required_observers_hash,
  };
}

export function encodeTransactionBodyCompact(
  b: TransactionBodyCompact,
): Uint8Array {
  const sw = new Writer();
  writeTransactionBodyCompactStatic(sw, b);
  const dw = new Writer();
  writeTransactionBodyCompactDynamic(dw, b);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

export function decodeTransactionBodyCompact(
  bytes: Uint8Array,
): TransactionBodyCompact {
  const r = new Reader(bytes);
  return readTransactionBodyCompact(r);
}

// ===========================================================================
// Transaction   (Full Representation)
//
// Fields: body, witness_set, is_valid
//
// Static: body.static + witness_set.static(=mask u64) + is_valid(u8 padded=8)
// Dynamic: body.dynamic + witness_set.dynamic
// ===========================================================================

export interface Transaction {
  body: TransactionBody;
  witness_set: TransactionWitnessSet;
  is_valid: boolean;
}

interface TransactionPartial {
  bodyPartial: TransactionBodyPartial;
  wsMask: number;
  is_valid: boolean;
}

function writeTransactionStatic(w: Writer, tx: Transaction): void {
  writeTransactionBodyStatic(w, tx.body);
  writeTransactionWitnessSetStatic(w, tx.witness_set);
  writeBool(w, tx.is_valid);
}

function writeTransactionDynamic(w: Writer, tx: Transaction): void {
  writeTransactionBodyDynamic(w, tx.body);
  writeTransactionWitnessSetDynamic(w, tx.witness_set);
}

function readTransactionStatic(r: Reader): TransactionPartial {
  const bodyPartial = readTransactionBodyStatic(r);
  const wsMask = readTransactionWitnessSetStatic(r);
  const is_valid = readBool(r);
  return { bodyPartial, wsMask, is_valid };
}

function readTransactionDynamic(r: Reader, p: TransactionPartial): Transaction {
  const body = readTransactionBodyDynamic(r, p.bodyPartial);
  const witness_set = readTransactionWitnessSetDynamic(r, p.wsMask);
  return { body, witness_set, is_valid: p.is_valid };
}

export function encodeTransaction(tx: Transaction): Uint8Array {
  const sw = new Writer();
  writeTransactionStatic(sw, tx);
  const dw = new Writer();
  writeTransactionDynamic(dw, tx);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

export function decodeTransaction(bytes: Uint8Array): Transaction {
  const r = new Reader(bytes);
  const p = readTransactionStatic(r);
  return readTransactionDynamic(r, p);
}

// ===========================================================================
// TransactionCompact   (Compact Representation)
//
// All fields static, no dynamic.
// Fields: transaction_body_hash(Hash32), transaction_witness_set_hash(Hash32), is_valid(bool)
// ===========================================================================

export interface TransactionCompact {
  transaction_body_hash: Hash32;
  transaction_witness_set_hash: Hash32;
  is_valid: boolean;
}

export function encodeTransactionCompact(tc: TransactionCompact): Uint8Array {
  const w = new Writer();
  writeHash32Static(w, tc.transaction_body_hash);
  writeHash32Static(w, tc.transaction_witness_set_hash);
  writeBool(w, tc.is_valid);
  return w.toBytes();
}

export function decodeTransactionCompact(
  bytes: Uint8Array,
): TransactionCompact {
  const r = new Reader(bytes);
  const transaction_body_hash = readHash32Static(r);
  const transaction_witness_set_hash = readHash32Static(r);
  const is_valid = readBool(r);
  return { transaction_body_hash, transaction_witness_set_hash, is_valid };
}

// Re-export internal helpers needed by block.ts
export {
  writeTransactionStatic,
  writeTransactionDynamic,
  readTransactionStatic,
  readTransactionDynamic,
  TransactionPartial,
};
