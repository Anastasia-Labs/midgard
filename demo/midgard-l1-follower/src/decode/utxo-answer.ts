import {
  readArray,
  readBytes,
  readMap,
  readSmallUint,
} from "../cbor/reader.js";
import type { OutputSummary, OutRef } from "../types.js";
import { readOutput } from "./output.js";

export type LedgerUtxo = Readonly<{ outRef: OutRef; output: OutputSummary }>;

/**
 * Decodes a ledger `utxo_by_address` or `utxo_by_txin` answer,
 * `{[txHash, index] => output}`, keeping each output's exact byte slices.
 */
export const decodeUtxoEntries = (bytes: Uint8Array): LedgerUtxo[] =>
  readMap(bytes, 0).entries.map(({ key, value }) => {
    const [hash, index] = readArray(bytes, key).items;
    if (hash === undefined || index === undefined)
      throw new Error("utxo key must be [hash, index]");
    return {
      outRef: {
        txHash: readBytes(bytes, hash),
        index: readSmallUint(bytes, index),
      },
      output: readOutput(bytes, value),
    };
  });
