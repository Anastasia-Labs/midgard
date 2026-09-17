import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import type { WatcherNativeChainSyncRollForward } from "./native-chain-sync.js";

export const WATCHER_NATIVE_BLOCK_ADMISSION_SCHEMA_VERSION =
  "midgard-watcher-native-block-admission-v1" as const;

// The native node-to-client codec supplies the era discriminator. Header
// protocol versions advertise what the issuer supports and can precede a
// hard fork: Cardano node 11.0.1 emits major-12 headers in the major-11 Conway
// ledger. They constrain the era, but cannot select it unambiguously.
const MIN_PROTOCOL_MAJOR_BY_BLOCK_TYPE = Object.freeze({
  "2": 2n,
  "3": 3n,
  "4": 4n,
  "5": 5n,
  "6": 7n,
  "7": 9n,
  "8": 12n,
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

const admittedNativeBlocks = new WeakSet<object>();

/** A decoded native block can wake canonical rechecks; it grants no actuation
 * or finality authority. Runtime callers must use coordinator-delivered blocks. */
export const assertWatcherNativeBlockAdmission = (
  block: WatcherNativeBlockAdmission,
): void => {
  if (!admittedNativeBlocks.has(block))
    throw new Error("watcher native block was not admitted");
};

/**
 * Independently decodes the raw node block before durable dispatch. The Go
 * helper's metadata is treated only as a claim: CML re-derives the header,
 * block identity, ancestry and transaction order, and checks that the native
 * era discriminator is compatible with the issuer's protocol advertisement.
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
    const blockType = event.blockType;
    const minimumProtocolMajor =
      MIN_PROTOCOL_MAJOR_BY_BLOCK_TYPE[
        blockType as keyof typeof MIN_PROTOCOL_MAJOR_BY_BLOCK_TYPE
      ];
    if (
      minimumProtocolMajor === undefined ||
      BigInt(protocolMajor) < minimumProtocolMajor
    ) {
      throw new Error(
        "native block era differs from its protocol advertisement",
      );
    }
    if (
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
        // Ledger transaction IDs commit to the original body encoding. Keep
        // decoded child encodings when assembling the transaction frame.
        transactionCbors.push(transaction.to_cbor_hex());
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
    const admitted = Object.freeze({
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
    admittedNativeBlocks.add(admitted);
    return admitted;
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
