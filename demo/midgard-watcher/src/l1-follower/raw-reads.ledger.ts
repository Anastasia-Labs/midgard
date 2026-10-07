import { chainPoint } from "@al-ft/l1-node-transport";
import type { FraudProofRawL1Utxo } from "@al-ft/midgard-fault-proofs";
import {
  decodeLedgerUtxos,
  type WalletLedger,
} from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import type { LedgerOutputsAt } from "./raw-reads.types.js";
import { outRefLabel, rawUtxo } from "./reads.js";

/**
 * The ledger-state input resolver over the node's LocalStateQuery: the
 * `utxo_by_txin` answer at an acquired point, each output canonicalised as
 * the stored bodies are. A point the node cannot acquire (or any transport
 * failure) is null: the read then refuses with a named reason.
 */
export const ledgerOutputsFromTransport =
  (ledger: WalletLedger): LedgerOutputsAt =>
  async (point, outRefs) => {
    let answer: Uint8Array;
    try {
      answer = await ledger.withLedgerState(
        chainPoint(BigInt(point.slot), point.hash.toString("hex")),
        (session) =>
          session.query({
            query: "utxo_by_txin",
            txIns: outRefs.map((outRef) => ({
              txId: outRef.txHash.toString("hex"),
              index: outRef.index,
            })),
          }),
      );
    } catch {
      return null;
    }
    const outputs = new Map<string, FraudProofRawL1Utxo>();
    for (const utxo of decodeLedgerUtxos(answer)) {
      const output = CML.TransactionOutput.from_cbor_bytes(utxo.outputCbor);
      try {
        const label = outRefLabel(utxo.outRef);
        outputs.set(label, rawUtxo(label, output));
      } finally {
        output.free();
      }
    }
    return outputs;
  };
