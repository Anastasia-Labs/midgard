/**
 * Block, block header and block body types.
 */

import {
  Writer,
  Reader,
  writeU64,
  readU64,
  readVarBytesDynamic,
} from "../codec";

import {
  HeaderHash,
  TransactionId,
  OutputReference,
  Hash32,
  VKey,
  PosixTime,
  writeHash28Static,
  readHash28Static,
  writeHash32Static,
  readHash32Static,
  writeVKeyStatic,
  readVKeyStatic,
  writeAddressStatic,
  writeAddressDynamic,
  readAddressLen,
  readAddressDynamic,
  writeOutputReferenceStatic,
  readOutputReferenceStatic,
} from "./primitives";

import {
  TransactionOutput,
  TransactionOutputPartial,
  writeTransactionOutputStatic,
  writeTransactionOutputDynamic,
  readTransactionOutputStatic,
  readTransactionOutputDynamic,
} from "./output";

import { DepositInfo, WithdrawalInfo } from "./events";

import {
  Transaction,
  TransactionPartial,
  writeTransactionStatic,
  writeTransactionDynamic,
  readTransactionStatic,
  readTransactionDynamic,
} from "./transaction";

// ===========================================================================
// Header   (fully static — no dynamic part)
// ===========================================================================

export interface Header {
  prev_utxos_root: Hash32;
  utxos_root: Hash32;
  transactions_root: Hash32;
  deposits_root: Hash32;
  withdrawals_root: Hash32;
  start_time: PosixTime;
  event_start_time: PosixTime;
  end_time: PosixTime;
  prev_header_hash: HeaderHash | undefined;
  operator_vkey: VKey;
  protocol_version: number;
}

/** Write all header fields into w (header has no dynamic section). */
function writeHeaderFields(w: Writer, h: Header): void {
  writeHash32Static(w, h.prev_utxos_root);
  writeHash32Static(w, h.utxos_root);
  writeHash32Static(w, h.transactions_root);
  writeHash32Static(w, h.deposits_root);
  writeHash32Static(w, h.withdrawals_root);
  writeU64(w, h.start_time);
  writeU64(w, h.event_start_time);
  writeU64(w, h.end_time);
  if (h.prev_header_hash !== undefined) {
    writeU64(w, 1);
    writeHash28Static(w, h.prev_header_hash);
  } else {
    writeU64(w, 0);
  }
  writeVKeyStatic(w, h.operator_vkey);
  writeU64(w, h.protocol_version);
}

/** Read all header fields from r (sequential, no split needed). */
function readHeaderFields(r: Reader): Header {
  const prev_utxos_root = readHash32Static(r);
  const utxos_root = readHash32Static(r);
  const transactions_root = readHash32Static(r);
  const deposits_root = readHash32Static(r);
  const withdrawals_root = readHash32Static(r);
  const start_time = readU64(r);
  const event_start_time = readU64(r);
  const end_time = readU64(r);
  const phhPresent = readU64(r) !== 0;
  const prev_header_hash = phhPresent ? readHash28Static(r) : undefined;
  const operator_vkey = readVKeyStatic(r);
  const protocol_version = readU64(r);
  return {
    prev_utxos_root,
    utxos_root,
    transactions_root,
    deposits_root,
    withdrawals_root,
    start_time,
    event_start_time,
    end_time,
    prev_header_hash,
    operator_vkey,
    protocol_version,
  };
}

export function encodeHeader(h: Header): Uint8Array {
  const w = new Writer();
  writeHeaderFields(w, h);
  return w.toBytes();
}

export function decodeHeader(bytes: Uint8Array): Header {
  return readHeaderFields(new Reader(bytes));
}

// ===========================================================================
// BlockBody
//
// Four maps encoded with static/dynamic split:
//   utxos:        Vec<(OutputReference, TransactionOutput)>
//   transactions: Vec<(TransactionId,   Transaction)>
//   deposits:     Vec<(OutputReference, DepositInfo)>
//   withdrawals:  Vec<(OutputReference, WithdrawalInfo)>
//
// Static:  for each map: len(u64) + for each (k,v): k.static + v.static
// Dynamic: for each map: for each (k,v): k.dynamic + v.dynamic
// ===========================================================================

export type UtxoMap = Array<[OutputReference, TransactionOutput]>;
export type TransactionMap = Array<[TransactionId, Transaction]>;
export type DepositMap = Array<[OutputReference, DepositInfo]>;
export type WithdrawalMap = Array<[OutputReference, WithdrawalInfo]>;

export interface BlockBody {
  utxos: UtxoMap;
  transactions: TransactionMap;
  deposits: DepositMap;
  withdrawals: WithdrawalMap;
}

// Partial types capturing lengths for the dynamic decode phase.
interface UtxoPartial {
  k: OutputReference;
  v: TransactionOutputPartial;
}
interface TxPartial {
  k: TransactionId;
  v: TransactionPartial;
}
interface DepositPartial {
  outerKey: OutputReference;
  addrLen: number;
  datumPresent: boolean;
  datumLen: number;
}
interface WithdrawalPartial {
  outerKey: OutputReference;
  l2_outref: OutputReference;
  addrLen: number;
  datumPresent: boolean;
  datumLen: number;
}

// ---- utxos ----------------------------------------------------------------

function writeUtxosStatic(w: Writer, map: UtxoMap): void {
  writeU64(w, map.length);
  for (const [k, v] of map) {
    writeOutputReferenceStatic(w, k);
    writeTransactionOutputStatic(w, v);
  }
}
function writeUtxosDynamic(w: Writer, map: UtxoMap): void {
  for (const [, v] of map) writeTransactionOutputDynamic(w, v);
}
function readUtxosStatic(r: Reader): UtxoPartial[] {
  const len = readU64(r);
  const ps: UtxoPartial[] = [];
  for (let i = 0; i < len; i++) {
    const k = readOutputReferenceStatic(r);
    const v = readTransactionOutputStatic(r);
    ps.push({ k, v });
  }
  return ps;
}
function readUtxosDynamic(r: Reader, ps: UtxoPartial[]): UtxoMap {
  return ps.map(({ k, v }) => [k, readTransactionOutputDynamic(r, v)]);
}

// ---- transactions ---------------------------------------------------------

function writeTransactionsStatic(w: Writer, map: TransactionMap): void {
  writeU64(w, map.length);
  for (const [k, v] of map) {
    writeHash32Static(w, k);
    writeTransactionStatic(w, v);
  }
}
function writeTransactionsDynamic(w: Writer, map: TransactionMap): void {
  for (const [, v] of map) writeTransactionDynamic(w, v);
}
function readTransactionsStatic(r: Reader): TxPartial[] {
  const len = readU64(r);
  const ps: TxPartial[] = [];
  for (let i = 0; i < len; i++) {
    const k = readHash32Static(r);
    const v = readTransactionStatic(r);
    ps.push({ k, v });
  }
  return ps;
}
function readTransactionsDynamic(r: Reader, ps: TxPartial[]): TransactionMap {
  return ps.map(({ k, v }) => [k, readTransactionDynamic(r, v)]);
}

// ---- deposits -------------------------------------------------------------

function writeDepositsStatic(w: Writer, map: DepositMap): void {
  writeU64(w, map.length);
  for (const [k, v] of map) {
    writeOutputReferenceStatic(w, k);
    writeAddressStatic(w, v.l2_address);
    if (v.l2_datum !== undefined) {
      writeU64(w, 1);
      writeU64(w, v.l2_datum.length);
    } else writeU64(w, 0);
  }
}
function writeDepositsDynamic(w: Writer, map: DepositMap): void {
  for (const [, v] of map) {
    writeAddressDynamic(w, v.l2_address);
    if (v.l2_datum !== undefined) {
      w.write(v.l2_datum);
      w.writeZeros((8 - (v.l2_datum.length % 8)) % 8);
    }
  }
}
function readDepositsStatic(r: Reader): DepositPartial[] {
  const len = readU64(r);
  const ps: DepositPartial[] = [];
  for (let i = 0; i < len; i++) {
    const outerKey = readOutputReferenceStatic(r);
    const addrLen = readU64(r);
    const datumPresent = readU64(r) !== 0;
    const datumLen = datumPresent ? readU64(r) : 0;
    ps.push({ outerKey, addrLen, datumPresent, datumLen });
  }
  return ps;
}
function readDepositsDynamic(r: Reader, ps: DepositPartial[]): DepositMap {
  return ps.map(({ outerKey, addrLen, datumPresent, datumLen }) => {
    const l2_address = readAddressDynamic(r, addrLen);
    const l2_datum = datumPresent
      ? readVarBytesDynamic(r, datumLen)
      : undefined;
    return [outerKey, { l2_address, l2_datum }] as [
      OutputReference,
      DepositInfo,
    ];
  });
}

// ---- withdrawals ----------------------------------------------------------

function writeWithdrawalsStatic(w: Writer, map: WithdrawalMap): void {
  writeU64(w, map.length);
  for (const [k, v] of map) {
    writeOutputReferenceStatic(w, k);
    writeOutputReferenceStatic(w, v.l2_outref);
    writeAddressStatic(w, v.l1_address);
    if (v.l1_datum !== undefined) {
      writeU64(w, 1);
      writeU64(w, v.l1_datum.length);
    } else writeU64(w, 0);
  }
}
function writeWithdrawalsDynamic(w: Writer, map: WithdrawalMap): void {
  for (const [, v] of map) {
    writeAddressDynamic(w, v.l1_address);
    if (v.l1_datum !== undefined) {
      w.write(v.l1_datum);
      w.writeZeros((8 - (v.l1_datum.length % 8)) % 8);
    }
  }
}
function readWithdrawalsStatic(r: Reader): WithdrawalPartial[] {
  const len = readU64(r);
  const ps: WithdrawalPartial[] = [];
  for (let i = 0; i < len; i++) {
    const outerKey = readOutputReferenceStatic(r);
    const l2_outref = readOutputReferenceStatic(r);
    const addrLen = readU64(r);
    const datumPresent = readU64(r) !== 0;
    const datumLen = datumPresent ? readU64(r) : 0;
    ps.push({ outerKey, l2_outref, addrLen, datumPresent, datumLen });
  }
  return ps;
}
function readWithdrawalsDynamic(
  r: Reader,
  ps: WithdrawalPartial[],
): WithdrawalMap {
  return ps.map(({ outerKey, l2_outref, addrLen, datumPresent, datumLen }) => {
    const l1_address = readAddressDynamic(r, addrLen);
    const l1_datum = datumPresent
      ? readVarBytesDynamic(r, datumLen)
      : undefined;
    return [outerKey, { l2_outref, l1_address, l1_datum }] as [
      OutputReference,
      WithdrawalInfo,
    ];
  });
}

// ---- BlockBody encode/decode ----------------------------------------------

interface BlockBodyPartial {
  utxoPs: UtxoPartial[];
  txPs: TxPartial[];
  depPs: DepositPartial[];
  wdPs: WithdrawalPartial[];
}

function writeBlockBodyStatic(w: Writer, bb: BlockBody): void {
  writeUtxosStatic(w, bb.utxos);
  writeTransactionsStatic(w, bb.transactions);
  writeDepositsStatic(w, bb.deposits);
  writeWithdrawalsStatic(w, bb.withdrawals);
}
function writeBlockBodyDynamic(w: Writer, bb: BlockBody): void {
  writeUtxosDynamic(w, bb.utxos);
  writeTransactionsDynamic(w, bb.transactions);
  writeDepositsDynamic(w, bb.deposits);
  writeWithdrawalsDynamic(w, bb.withdrawals);
}
function readBlockBodyStatic(r: Reader): BlockBodyPartial {
  return {
    utxoPs: readUtxosStatic(r),
    txPs: readTransactionsStatic(r),
    depPs: readDepositsStatic(r),
    wdPs: readWithdrawalsStatic(r),
  };
}
function readBlockBodyDynamic(r: Reader, p: BlockBodyPartial): BlockBody {
  return {
    utxos: readUtxosDynamic(r, p.utxoPs),
    transactions: readTransactionsDynamic(r, p.txPs),
    deposits: readDepositsDynamic(r, p.depPs),
    withdrawals: readWithdrawalsDynamic(r, p.wdPs),
  };
}

export function encodeBlockBody(bb: BlockBody): Uint8Array {
  const sw = new Writer();
  writeBlockBodyStatic(sw, bb);
  const dw = new Writer();
  writeBlockBodyDynamic(dw, bb);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

export function decodeBlockBody(bytes: Uint8Array): BlockBody {
  const r = new Reader(bytes);
  const p = readBlockBodyStatic(r);
  return readBlockBodyDynamic(r, p);
}

// ===========================================================================
// Block
//
// Static: header_hash.static(32) + header.fields(all static) + block_body.static
// Dynamic: block_body.dynamic
//
// Note: Block.encode() writes all static first, then all dynamic —
// so header_hash and header fields go into the static writer alongside
// block_body.static, and only block_body.dynamic goes in the dynamic writer.
// ===========================================================================

export interface Block {
  header_hash: HeaderHash;
  header: Header;
  block_body: BlockBody;
}

export function encodeBlock(b: Block): Uint8Array {
  const sw = new Writer();
  // header_hash (Hash28 → 32 bytes)
  writeHash28Static(sw, b.header_hash);
  // header fields (all static)
  writeHeaderFields(sw, b.header);
  // block_body static
  writeBlockBodyStatic(sw, b.block_body);

  const dw = new Writer();
  writeBlockBodyDynamic(dw, b.block_body);

  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}

export function decodeBlock(bytes: Uint8Array): Block {
  const r = new Reader(bytes);
  // Static phase
  const header_hash = readHash28Static(r);
  const header = readHeaderFields(r);
  const bbPartial = readBlockBodyStatic(r);
  // Dynamic phase
  const block_body = readBlockBodyDynamic(r, bbPartial);
  return { header_hash, header, block_body };
}
