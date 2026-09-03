import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import type { WatcherNativeChainSyncRollForward } from "./native-chain-sync-v1.js";

export const WATCHER_NATIVE_BLOCK_ADMISSION_SCHEMA_VERSION =
  "midgard-watcher-native-block-admission-v1" as const;

const BLOCK_TYPE_BY_PROTOCOL_MAJOR = Object.freeze({
  "2": "2",
  "3": "3",
  "4": "4",
  "5": "5",
  "6": "5",
  "7": "6",
  "8": "6",
  "9": "7",
  "10": "7",
  "11": "7",
  "12": "8",
  "13": "8",
} as const);

export type WatcherNativeBlockAdmission = Readonly<{
  schemaVersion: typeof WATCHER_NATIVE_BLOCK_ADMISSION_SCHEMA_VERSION;
  blockType: string;
  protocolMajor: string;
  blockHash: string;
  prevHash: string;
  slot: string;
  blockNo: string;
  rawBlockCbor: string;
  rawHeaderCbor: string;
  transactionIds: readonly string[];
  transactionCbors: readonly string[];
}>;

/**
 * Independently decodes the raw node block before durable dispatch. The Go
 * helper's metadata is treated only as a claim: CML re-derives the header,
 * protocol-era discriminator, block identity, ancestry and transaction order.
 */
export const admitWatcherNativeRollForwardBlock = (
  event: WatcherNativeChainSyncRollForward,
): WatcherNativeBlockAdmission => {
  let block: CML.Block | undefined;
  let header: CML.Header | undefined;
  let headerBody: CML.HeaderBody | undefined;
  let bodies: CML.TransactionBodyList | undefined;
  let witnesses: CML.TransactionWitnessSetList | undefined;
  let auxiliaryData: CML.MapTransactionIndexToAuxiliaryData | undefined;
  try {
    block = CML.Block.from_cbor_hex(event.rawBlockCbor);
    if (block.to_cbor_hex() !== event.rawBlockCbor) {
      throw new Error("native block CBOR is not the exact decoded encoding");
    }
    header = block.header();
    headerBody = header.header_body();
    const rawHeaderCbor = header.to_cbor_hex();
    const blockHash = computeHash32(Buffer.from(rawHeaderCbor, "hex")).toString(
      "hex",
    );
    const prevHash = headerBody.prev_hash()?.to_hex() ?? "";
    const slot = headerBody.slot().toString();
    const blockNo = headerBody.block_number().toString();
    const protocolMajor = headerBody.protocol_version().major().toString();
    const blockType =
      BLOCK_TYPE_BY_PROTOCOL_MAJOR[
        protocolMajor as keyof typeof BLOCK_TYPE_BY_PROTOCOL_MAJOR
      ];
    if (blockType === undefined) {
      throw new Error("native block protocol version has no admitted era");
    }
    if (
      blockType !== event.blockType ||
      blockHash !== event.blockHash ||
      prevHash !== event.prevHash ||
      slot !== event.slot ||
      blockNo !== event.blockNo
    ) {
      throw new Error("native block metadata differs from decoded block");
    }
    bodies = block.transaction_bodies();
    witnesses = block.transaction_witness_sets();
    auxiliaryData = block.auxiliary_data_set();
    if (witnesses.len() !== bodies.len()) {
      throw new Error("native block transaction body/witness count differs");
    }
    const invalid = new Set<number>();
    for (const index of block.invalid_transactions()) {
      if (index >= bodies.len() || invalid.has(index)) {
        throw new Error("native block invalid transaction indices are invalid");
      }
      invalid.add(index);
    }
    const transactionIds: string[] = [];
    const transactionCbors: string[] = [];
    for (let index = 0; index < bodies.len(); index += 1) {
      const body = bodies.get(index);
      const witnessSet = witnesses.get(index);
      const auxiliary = auxiliaryData.get(index);
      let transaction: CML.Transaction | undefined;
      let assembled = false;
      try {
        transactionIds.push(CML.hash_transaction(body).to_hex());
        transaction = CML.Transaction.new(
          body,
          witnessSet,
          !invalid.has(index),
          auxiliary,
        );
        assembled = true;
        transactionCbors.push(transaction.to_canonical_cbor_hex());
      } finally {
        transaction?.free();
        // CML.Transaction.new takes ownership of its child handles. Only
        // release them directly when assembly failed before that transfer.
        if (!assembled) {
          auxiliary?.free();
          witnessSet.free();
          body.free();
        }
      }
    }
    return Object.freeze({
      schemaVersion: WATCHER_NATIVE_BLOCK_ADMISSION_SCHEMA_VERSION,
      blockType,
      protocolMajor,
      blockHash,
      prevHash,
      slot,
      blockNo,
      rawBlockCbor: event.rawBlockCbor,
      rawHeaderCbor,
      transactionIds: Object.freeze(transactionIds),
      transactionCbors: Object.freeze(transactionCbors),
    });
  } catch (error) {
    throw new Error("native chain-sync block admission failed", {
      cause: error,
    });
  } finally {
    bodies?.free();
    witnesses?.free();
    auxiliaryData?.free();
    headerBody?.free();
    header?.free();
    block?.free();
  }
};
