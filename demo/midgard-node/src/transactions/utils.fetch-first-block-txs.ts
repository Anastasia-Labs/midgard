import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as BlocksDB from "../database/blocks.js";
import { ImmutableDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database } from "../services/index.js";
import { type BlockTxPayload } from "./utils.reconcile-wallet-utxos-from-signed-tx.js";

/**
 * Fetch transactions of the first block by querying BlocksDB and ImmutableDB.
 *
 * @param firstBlockUTxO - UTxO of the first block in queue.
 * @returns An Effect that resolves to an array of transactions, and block's
 *          header hash.
 */
export const fetchFirstBlockTxs = (
  firstBlockUTxO: SDK.StateQueueUTxO,
): Effect.Effect<
  {
    txs: readonly BlockTxPayload[];
    txHashes: readonly Buffer[];
    headerHash: Buffer;
  },
  SDK.HashingError | SDK.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const blockHeader = yield* SDK.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const headerHash: Buffer = yield* SDK.hashBlockHeader(blockHeader).pipe(
      Effect.map((hh) => Buffer.from(fromHex(hh))),
    );
    const txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    const txEntries = yield* ImmutableDB.retrieveTxEntriesByHashes(txHashes);
    const txById = new Map<string, Buffer>();
    for (const entry of txEntries) {
      txById.set(entry.tx_id.toString("hex"), entry.tx);
    }
    const txs: BlockTxPayload[] = [];
    for (const txHash of txHashes) {
      const txCbor = txById.get(txHash.toString("hex"));
      if (txCbor !== undefined) {
        txs.push({
          txId: txHash,
          txCbor,
        });
      }
    }
    return { txs, txHashes, headerHash };
  });
