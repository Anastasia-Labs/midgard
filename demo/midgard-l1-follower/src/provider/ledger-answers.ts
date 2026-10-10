/**
 * Answers and bytes the node-ledger provider reads: ledger chain point and
 * block number answers, a submitted transaction's id and output count, and
 * the outref and UTxO shapes Lucid speaks.
 */
import {
  type BlockPoint,
  chainPoint,
  decodeCbor,
  TransportRequestError,
} from "@al-ft/l1-node-transport";
import {
  CML,
  type OutRef as LucidOutRef,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  readArray,
  readMap,
  readSmallUint,
  skipItem,
  slice,
} from "../cbor/reader.js";
import { blake2b256 } from "../codec.js";
import type { OutputSummary, OutRef } from "../types.js";
import { L1ProviderRequestError } from "./errors.js";
import { toLucidUtxo } from "./utxo.js";

export const outRefKey = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index}`;

export const toOutRef = (outRef: LucidOutRef): OutRef => ({
  txHash: Buffer.from(outRef.txHash, "hex"),
  index: outRef.outputIndex,
});

export const lucidUtxo = (
  row: Readonly<{ outRef: OutRef; output: OutputSummary }>,
): UTxO => toLucidUtxo(row.outRef, row.output);

/** The transaction body's offset; throws on bytes that are not a tx. */
const bodyOffset = (tx: Buffer): number => {
  const body = readArray(tx, 0).items[0];
  if (body === undefined) throw new Error("the transaction has no body");
  return body;
};

/** The transaction id: blake2b-256 of the body's exact bytes. */
export const transactionId = (tx: Buffer): string => {
  try {
    const body = bodyOffset(tx);
    return blake2b256(slice(tx, body, skipItem(tx, body))).toString("hex");
  } catch (error) {
    throw new L1ProviderRequestError(
      "tx_undecodable",
      "the transaction is not a CBOR [body, witnesses, ...] array",
      { cause: error },
    );
  }
};

/** How many outputs the body (key 1) declares; undefined when unreadable. */
export const outputCount = (tx: Buffer): number | undefined => {
  try {
    const outputs = readMap(tx, bodyOffset(tx)).entries.find(
      ({ key }) => readSmallUint(tx, key) === 1,
    );
    return outputs === undefined
      ? undefined
      : readArray(tx, outputs.value).items.length;
  } catch {
    return undefined;
  }
};

/** A ledger `chain_point` answer: `[]` (origin) or `[slot, hash32]`. */
export const decodeTipPoint = (answer: Uint8Array): BlockPoint | undefined => {
  const value = decodeCbor(answer);
  if (!Array.isArray(value))
    throw new Error("the ledger's chain point is not a CBOR array");
  if (value.length === 0) return undefined;
  const [slot, hash] = value;
  if (
    value.length !== 2 ||
    !(typeof slot === "number" || typeof slot === "bigint") ||
    !(hash instanceof Uint8Array)
  )
    throw new Error("the ledger's chain point is not [slot, hash32]");
  return chainPoint(BigInt(slot), Buffer.from(hash).toString("hex"));
};

/** A ledger `chain_block_no` answer: `[0]` (origin) or `[1, n]`. */
export const decodeBlockNo = (answer: Uint8Array): number | undefined => {
  const value = decodeCbor(answer);
  const tag = Array.isArray(value) ? value[0] : undefined;
  if (
    !Array.isArray(value) ||
    (typeof tag !== "number" && typeof tag !== "bigint") ||
    (BigInt(tag) !== 0n && BigInt(tag) !== 1n)
  )
    throw new Error("the ledger's block number is not [0] or [1, n]");
  if (BigInt(tag) === 0n) return undefined;
  const blockNo = value[1];
  if (
    (typeof blockNo !== "number" && typeof blockNo !== "bigint") ||
    BigInt(blockNo) > BigInt(Number.MAX_SAFE_INTEGER)
  )
    throw new Error("the ledger's block number is not a safe natural");
  return Number(blockNo);
};

export const acquireRefusal = (
  error: unknown,
): "not_on_chain" | "immutable" | undefined => {
  if (!(error instanceof TransportRequestError)) return undefined;
  if (error.code === "acquire_point_not_on_chain") return "not_on_chain";
  if (error.code === "acquire_point_too_old") return "immutable";
  return undefined;
};

export const poolBech32 = (hashHex: string): string => {
  const hash = CML.Ed25519KeyHash.from_hex(hashHex);
  try {
    return hash.to_bech32("pool");
  } finally {
    hash.free();
  }
};
