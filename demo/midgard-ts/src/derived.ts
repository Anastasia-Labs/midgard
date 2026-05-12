/**
 * Derivations of the *compact* representations from *full* transaction
 * structures, plus transaction-id computation.
 *
 * A compact field hash is blake2b-256 over the canonical binary encoding of
 * that field exactly as it appears in the full structure's dynamic section
 * (or static section, for `inputs`).
 */

import { Writer, writeU64, writeVarBytesDynamic } from "./codec";
import { computeHash32 } from "./hash";
import type { Hash32, OutputReference } from "./types/primitives";
import {
  writeOutputReferenceStatic,
  writeHash28Static,
} from "./types/primitives";
import type { Mint, VKeyWitness, TransactionOutput } from "./types/output";
import {
  writeVKeyWitness,
  writeMintStatic,
  writeMintDynamic,
  writeTransactionOutputStatic,
  writeTransactionOutputDynamic,
} from "./types/output";
import { encodeVersionedScriptVec } from "./types/script";
import type {
  Transaction,
  TransactionBody,
  TransactionWitnessSet,
  TransactionBodyCompact,
  TransactionWitnessSetCompact,
  TransactionCompact,
} from "./types/transaction";
import {
  encodeTransactionBodyCompact,
  encodeTransactionWitnessSetCompact,
} from "./types/transaction";

function concat(a: Uint8Array, b: Uint8Array): Uint8Array {
  const out = new Uint8Array(a.length + b.length);
  out.set(a);
  out.set(b, a.length);
  return out;
}

function hashOutputReferenceVec(refs: OutputReference[]): Hash32 {
  const w = new Writer();
  writeU64(w, refs.length);
  for (const r of refs) writeOutputReferenceStatic(w, r);
  return computeHash32(w.toBytes());
}

function hashHash28Vec(hs: Uint8Array[]): Hash32 {
  const w = new Writer();
  writeU64(w, hs.length);
  for (const h of hs) writeHash28Static(w, h);
  return computeHash32(w.toBytes());
}

function hashTransactionOutputVec(outs: TransactionOutput[]): Hash32 {
  const sw = new Writer();
  writeU64(sw, outs.length);
  for (const o of outs) writeTransactionOutputStatic(sw, o);
  const dw = new Writer();
  for (const o of outs) writeTransactionOutputDynamic(dw, o);
  return computeHash32(concat(sw.toBytes(), dw.toBytes()));
}

function hashMint(mint: Mint): Hash32 {
  const sw = new Writer();
  writeMintStatic(sw, mint);
  const dw = new Writer();
  writeMintDynamic(dw, mint);
  return computeHash32(concat(sw.toBytes(), dw.toBytes()));
}

function hashVKeyWitnessVec(ws: VKeyWitness[]): Hash32 {
  const w = new Writer();
  writeU64(w, ws.length);
  for (const v of ws) writeVKeyWitness(w, v);
  return computeHash32(w.toBytes());
}

// Vec<u8> encoded inline as: blen(u64) + bytes + pad
function hashBytesInline(bytes: Uint8Array): Hash32 {
  const w = new Writer();
  writeU64(w, bytes.length);
  writeVarBytesDynamic(w, bytes);
  return computeHash32(w.toBytes());
}

export function deriveTransactionBodyCompact(
  b: TransactionBody,
): TransactionBodyCompact {
  return {
    inputs_hash: hashOutputReferenceVec(b.inputs),
    outputs_hash: hashTransactionOutputVec(b.outputs),
    fee: b.fee,
    ttl: b.ttl,
    auxiliary_data_hash: b.auxiliary_data_hash,
    validity_interval_start: b.validity_interval_start,
    mint_hash: b.mint !== undefined ? hashMint(b.mint) : undefined,
    script_data_hash: b.script_data_hash,
    required_signers_hash:
      b.required_signers !== undefined
        ? hashHash28Vec(b.required_signers)
        : undefined,
    network_id: b.network_id,
    reference_inputs_hash:
      b.reference_inputs !== undefined
        ? hashOutputReferenceVec(b.reference_inputs)
        : undefined,
    required_observers_hash:
      b.required_observers !== undefined
        ? hashHash28Vec(b.required_observers)
        : undefined,
  };
}

export function deriveTransactionWitnessSetCompact(
  ws: TransactionWitnessSet,
): TransactionWitnessSetCompact {
  return {
    vkey_witnesses_hash:
      ws.vkey_witnesses !== undefined
        ? hashVKeyWitnessVec(ws.vkey_witnesses)
        : undefined,
    scripts_hash:
      ws.scripts !== undefined
        ? computeHash32(encodeVersionedScriptVec(ws.scripts))
        : undefined,
    redeemers_hash:
      ws.redeemers !== undefined ? hashBytesInline(ws.redeemers) : undefined,
  };
}

/** blake2b-256 of the encoded compact body — the canonical transaction id. */
export function transactionBodyHash(b: TransactionBody): Hash32 {
  return computeHash32(
    encodeTransactionBodyCompact(deriveTransactionBodyCompact(b)),
  );
}

export function transactionWitnessSetHash(ws: TransactionWitnessSet): Hash32 {
  return computeHash32(
    encodeTransactionWitnessSetCompact(deriveTransactionWitnessSetCompact(ws)),
  );
}

export function transactionId(tx: Transaction): Hash32 {
  return transactionBodyHash(tx.body);
}

export function deriveTransactionCompact(tx: Transaction): TransactionCompact {
  return {
    transaction_body_hash: transactionBodyHash(tx.body),
    transaction_witness_set_hash: transactionWitnessSetHash(tx.witness_set),
    is_valid: tx.is_valid,
  };
}
