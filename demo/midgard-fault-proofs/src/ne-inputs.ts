import { computeHash32 } from "@al-ft/midgard-core";
import {
  H32,
  type MidgardTxInput,
  type OutputReference,
  OutputReferenceSchema,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { aikenSerialisedPlutusDataCbor } from "./plutus-data-cbor.js";

type LucidDataSchema = Parameters<typeof Data.to>[1];

export const OutputReferenceListSchema = Data.Array(OutputReferenceSchema);
export type OutputReferenceList = Data.Static<typeof OutputReferenceListSchema>;
export const OutputReferenceList =
  OutputReferenceListSchema as unknown as OutputReferenceList;

/**
 * Canonical Aiken `serialise_data` of a PlutusData value, matching what the
 * on-chain `cbor.serialise` / `verify_hash_32` helpers operate on. Lucid's
 * `Data.to` can emit indefinite-length containers, so re-encode definitively.
 */
export const serialisePlutusDataCbor = (cbor: string): Buffer =>
  Buffer.from(aikenSerialisedPlutusDataCbor(cbor), "hex");

const aikenSerialise = serialisePlutusDataCbor;

/**
 * Midgard compact-tx id: blake2b-256 of the canonical serialisation of the
 * compact body, matching on-chain `verify_hash_32(tx.body, tx_id)`.
 */
export const computeCompactTxBodyId = (bodyCbor: string): string =>
  computeHash32(serialisePlutusDataCbor(bodyCbor)).toString("hex");

/**
 * Blake2b-256 of the canonical serialisation of the spend-inputs preimage,
 * equal to the on-chain `verify_hash_32(inputs_preimage, ...)` digest and the
 * `MidgardTxBodyCompact.spend_inputs` hash carried in the bad transaction.
 */
export const hashInputsPreimage = (
  inputs: readonly OutputReference[],
): string =>
  computeHash32(
    aikenSerialise(Data.to([...inputs], OutputReferenceList)),
  ).toString("hex");

/**
 * Native ledger-trie key for a Midgard input: the Cardano `TransactionInput`
 * CBOR (`82 5820<tx_id> <output_index>`), matching the node's `utxoToInsertBatchOp`
 * and the on-chain `encode_midgard_tx_input`.
 */
export const nativeInputKeyHex = (input: MidgardTxInput): string =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(input.tx_id),
      input.output_index,
    ).to_cbor_bytes(),
  ).toString("hex");

/** Trie key for an output reference (serialised PlutusData), as `plutarch_pexcludes` hashes it. */
export const outputReferenceTrieKey = (outRef: OutputReference): Buffer =>
  aikenSerialise(Data.to(outRef, OutputReferenceSchema as unknown as LucidDataSchema));

/** Trie key for a 32-byte transaction id (serialised PlutusData). */
export const txIdTrieKey = (txId: string): Buffer =>
  aikenSerialise(Data.to(txId, H32));

/** PlutusData CBOR (Lucid form) of an output reference, used as a pexcludes redeemer key. */
export const outputReferenceKeyCbor = (outRef: OutputReference): string =>
  Data.to(outRef, OutputReferenceSchema as unknown as LucidDataSchema);

/** PlutusData CBOR (Lucid form) of a 32-byte transaction id, used as a pexcludes redeemer key. */
export const txIdKeyCbor = (txId: string): string => Data.to(txId, H32);
