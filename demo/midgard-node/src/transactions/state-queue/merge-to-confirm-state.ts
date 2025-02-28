/**
 * This script performs the following tasks to merge the first block into the confirmed state:
 *
 * 1. Fetch transactions of the first block by querying ImmutableDB.
 * 2. Apply those transactions to ConfirmedLedgerDB and update the table to store the updated UTxO set.
 * 3. Remove all header hashes from BlocksDB associated with the merged block.
 * 4. Build and submit the merge transaction.
 */

import { BlocksDB, ConfirmedLedgerDB, UtilsDB } from "@/database/index.js";
import { AlwaysSucceeds } from "@/services/index.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Database } from "sqlite3";
import { fetchFirstBlockTxs, handleSignSubmit } from "../utils.js";

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param db - The database instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param header_hash - The header hash of the block to be merged.
 * @returns An Effect that resolves when the merge transaction is built and submitted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  db: Database,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig
) =>
  Effect.gen(function* ($) {
    // Fetch transactions from the first block
    const { txs: firstBlockTxs, headerHash } = yield* $(
      fetchFirstBlockTxs(lucid, fetchConfig, db)
    );

    const { mintScript, spendScript } =
      yield* AlwaysSucceeds.AlwaysSucceedsContract;
    // Build the transaction
    const txBuilder = yield* SDK.Endpoints.mergeToConfirmedStateProgram(
      lucid,
      fetchConfig,
      {
        stateQueueSpendingScript: spendScript,
        stateQueueMintingScript: mintScript,
      }
    );

    // Submit the transaction
    yield* handleSignSubmit(lucid, txBuilder);

    const { spent: spentOutRefs, produced: producedUTxOs } =
      yield* findAllSpentAndProducedUTxOs(firstBlockTxs);

    // - Clear all the spent UTxOs from the confirmed ledger
    // - Add all the produced UTxOs from the confirmed ledger
    // - Remove all the tx hashes of the merged block from BlocksDB
    yield* Effect.tryPromise({
      try: () =>
        UtilsDB.modifyMultipleTables(
          //TODO: remove
          db,
          [ConfirmedLedgerDB.clearUTxOs, spentOutRefs],
          [ConfirmedLedgerDB.insert, producedUTxOs],
          [BlocksDB.clearBlock, headerHash]
        ),
      catch: (e) => new Error(`Transaction failed: ${e}`),
    });
  });
