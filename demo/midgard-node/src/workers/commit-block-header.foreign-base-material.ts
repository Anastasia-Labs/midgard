import * as SDK from "@al-ft/midgard-sdk";

import type * as Ledger from "../database/utils/ledger.js";

export const canonicalObservation = (nodes: readonly SDK.StateQueueUTxO[]) =>
  nodes.map((node) => ({
    txHash: node.utxo.txHash,
    outputIndex: node.utxo.outputIndex,
    datumCbor: SDK.encodeLinkedListNodeView(node.datum),
  }));
export const ledgerSegmentBefore = (
  keys: Iterable<string>,
  entries: readonly Ledger.MinimalEntry[],
) => {
  const touched = new Set(keys);
  return {
    ledgerKeys: Object.freeze(
      [...touched].map((key) => Buffer.from(key, "hex")),
    ),
    ledgerBefore: Object.freeze(
      entries
        .filter((entry) => touched.has(entry.outref.toString("hex")))
        .map((entry) => ({
          outref: Buffer.from(entry.outref),
          output: Buffer.from(entry.output),
        })),
    ),
  };
};
