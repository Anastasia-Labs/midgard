import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import type { QueuedTx } from "@al-ft/midgard-validation";
import { Effect } from "effect";

import {
  decodeNodeUtxo,
  type NodeUtxo,
} from "../../src/commands/command-utils.js";
import type { BuiltTransferTx } from "../../src/commands/transfer-build-core.js";
import * as MempoolLedgerDB from "../../src/database/mempoolLedger.js";
import type { Database } from "../../src/services/database.js";

/** Reads an address's spendable UTxOs from the test node's mempool ledger. */
export const fetchLocalUtxos = (
  address: string,
): Effect.Effect<readonly NodeUtxo[], Error, Database> =>
  MempoolLedgerDB.retrieveSpendableByAddress(address).pipe(
    Effect.map((entries) =>
      entries.map((entry) =>
        decodeNodeUtxo({
          outref: entry.outref.toString("hex"),
          outputCbor: entry.output.toString("hex"),
        }),
      ),
    ),
    Effect.mapError(
      (cause) =>
        new Error(
          `Failed to fetch local Midgard UTxOs: ${cause instanceof Error ? cause.message : String(cause)}`,
        ),
    ),
  );

/** Shapes a built transfer as the validation queue entry of a key-signed tx. */
export const toQueuedTx = (built: BuiltTransferTx): QueuedTx => ({
  txId: built.txId,
  txCbor: built.txCbor,
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
  arrivalSeq: 0n,
  createdAt: new Date(),
});
