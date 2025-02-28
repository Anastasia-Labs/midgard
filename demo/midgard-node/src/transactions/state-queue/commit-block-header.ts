// Build a tx Merkle root with all the mempool txs

import { LucidEvolution, Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Database } from "sqlite3";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "../utils.js";
import {
  LatestLedgerDB,
  MempoolDB,
  ImmutableDB,
  UtilsDB,
} from "@/database/index.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";

// Apply mempool txs to LatestLedgerDB, and find the new UTxO set

// Update LatestLedgerDB to store this updated set

// Clear MempoolDB, and inject all the processed txs into ImmutableDB

// Build a Merkle root using this updated UTxO set

// Build and submit the commitment block using these 2 roots

export const buildAndSubmitCommitmentBlock = (
  lucid: LucidEvolution,
  db: Database,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  spendScript: Script,
  endTime: number,
) =>
  Effect.gen(function* () {
    // Fetch transactions from the first block
    const txList = yield* Effect.tryPromise(() => MempoolDB.retrieve(db));
    if (txList.length > 0) {
      const txs = txList.map(([txHash, txCbor]) => ({ txHash, txCbor }));
      const txRoot = yield* SDK.Utils.mptFromList(txs.map((tx) => tx.txCbor));
      const txCbors = txList.map(([_txHash, txCbor]) => txCbor);
      const { spent: spentList, produced: producedList } =
        yield* findAllSpentAndProducedUTxOs(txCbors);
      const utxoList = yield* Effect.tryPromise(() =>
        LatestLedgerDB.retrieve(db),
      );
      // Remove spent UTxOs from utxoList
      const filteredUTxOList = utxoList.filter(
        (utxo) =>
          !spentList.some(
            (spent) =>
              spent.txHash === utxo.txHash &&
              spent.outputIndex === utxo.outputIndex,
          ),
      );

      // Merge filtered utxoList with producedList
      const newUTxOList = [...filteredUTxOList, ...producedList].map(
        (utxo) => utxo.txHash + utxo.outputIndex,
      );
      const utxoRoot = yield* SDK.Utils.mptFromList(newUTxOList);
      // Build commitment block
      const commitBlockParams: SDK.TxBuilder.StateQueue.CommitBlockParams = {
        newUTxOsRoot: utxoRoot.hash.toString(),
        transactionsRoot: txRoot.hash.toString(),
        endTime: BigInt(endTime),
        stateQueueSpendingScript: spendScript,
      };
      const aoUpdateCommitmentTimeParams = {};
      const txBuilder = yield* SDK.Endpoints.commitBlockHeaderProgram(
        lucid,
        fetchConfig,
        commitBlockParams,
        aoUpdateCommitmentTimeParams,
      );
      // Submit the transaction
      yield* handleSignSubmit(lucid, txBuilder);
      // TODO: For final product, handle tx submission failures properly.
      yield* Effect.tryPromise({
        try: () =>
          UtilsDB.modifyMultipleTables(
            db,
            [LatestLedgerDB.clearUTxOs, spentList],
            [LatestLedgerDB.insert, producedList],
            [MempoolDB.clear],
            [ImmutableDB.insertTxs, txs],
          ),
        catch: (e) => new Error(`Transaction failed: ${e}`),
      });
    }
  });
