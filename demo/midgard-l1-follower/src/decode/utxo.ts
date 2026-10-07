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
 * Decodes a LocalStateQuery UTxO answer (`utxo_by_address`,
 * `utxo_by_txin`): a map from `[txHash, index]` to the ledger's output, in
 * the same encoding a transaction body uses. Throws `CborReadError`.
 */
export const decodeLedgerUtxos = (bytes: Uint8Array): LedgerUtxo[] =>
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
