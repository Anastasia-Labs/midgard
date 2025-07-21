/**
 * This script performs the following tasks to merge the first block into the confirmed state:
 *
 * 1. Fetch transactions of the first block by querying ImmutableDB.
 * 2. Apply those transactions to ConfirmedLedgerDB and update the table to store the updated UTxO set.
 * 3. Remove all header hashes from BlocksDB associated with the merged block.
 * 4. Build and submit the merge transaction.
 */

import { BlocksDB, ConfirmedLedgerDB } from "@/database/index.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Address,
  LucidEvolution,
  Script,
  fromHex,
} from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";
import {
  ConfirmError,
  fetchFirstBlockTxs,
  handleSignSubmit,
  SubmitError,
} from "../utils.js";
import { Database } from "@/services/database.js";

const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merge blocks",
  bigint: true,
  incremental: true,
});

// 30 minutes.
const MAX_LIFE_OF_LOCAL_SYNC: number = 1_800_000;

const MIN_QUEUE_LENGTH_FOR_MERGING: number = 8;

const getStateQueueLength = (
  lucid: LucidEvolution,
  stateQueueAddress: Address,
): Effect.Effect<number, Error> =>
  Effect.gen(function* () {
    const now_millis = Date.now();
    if (
      now_millis - global.LATEST_SYNC_OF_STATE_QUEUE_LENGTH >
      MAX_LIFE_OF_LOCAL_SYNC
    ) {
      // We consider in-memory state queue length stale.
      yield* Effect.logInfo("🔸 Fetching state queue length...");
      const stateQueueUtxos = yield* Effect.tryPromise({
        try: () => lucid.utxosAt(stateQueueAddress),
        catch: (e) => new Error(`${e}`),
      });

      global.BLOCKS_IN_QUEUE = stateQueueUtxos.length - 1;

      global.LATEST_SYNC_OF_STATE_QUEUE_LENGTH = Date.now();

      return stateQueueUtxos.length;
    } else {
      return global.BLOCKS_IN_QUEUE;
    }
  });

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param db - The database instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param spendScript - State queue's spending script.
 * @param mintScript - State queue's minting script.
 * @returns An Effect that resolves when the merge transaction is built and submitted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  spendScript: Script,
  mintScript: Script,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const currentStateQueueLength = yield* getStateQueueLength(
      lucid,
      fetchConfig.stateQueueAddress,
    );
    // Avoid a merge tx if the queue is too short (performing a merge with such
    // conditions has a chance of wasting the work done for root computaions).
    if (
      currentStateQueueLength < MIN_QUEUE_LENGTH_FOR_MERGING ||
      global.RESET_IN_PROGRESS
    ) {
      // yield* Effect.logInfo(
      //   "🔸 There are too few blocks in queue.
      // );
      return;
    }

    yield* Effect.logInfo("🔸 Merging of oldest block started.");

    yield* Effect.logInfo(
      "🔸 Fetching confirmed state and the first block in queue from L1...",
    );
    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      yield* SDK.Endpoints.fetchConfirmedStateAndItsLinkProgram(
        lucid,
        fetchConfig,
      );
    if (firstBlockUTxO) {
      yield* Effect.logInfo(
        `🔸 First block found: ${firstBlockUTxO.utxo.txHash}#${firstBlockUTxO.utxo.outputIndex}`,
      );
      // Fetch transactions from the first block
      yield* Effect.logInfo("🔸 Looking up its transactions from BlocksDB...");
      const { txs: firstBlockTxs, headerHash } = yield* fetchFirstBlockTxs(
        firstBlockUTxO,
      ).pipe(Effect.withSpan("fetchFirstBlockTxs"));
      if (firstBlockTxs.length === 0) {
        yield* Effect.logInfo(
          "🔸 ❌ Failed to find first block's transactions in BlocksDB.",
        );
        return;
      }
      yield* Effect.logInfo("🔸 Building merge transaction...");
      // Build the transaction
      const txBuilder = yield* SDK.Endpoints.mergeToConfirmedStateProgram(
        lucid,
        fetchConfig,
        {
          confirmedUTxO,
          firstBlockUTxO,
          stateQueueSpendingScript: spendScript,
          stateQueueMintingScript: mintScript,
        },
      ).pipe(Effect.withSpan("mergeToConfirmedStateProgram"));

      // Submit the transaction
      const onSubmitFailure = (err: SubmitError) =>
        Effect.gen(function* () {
          yield* Effect.logError(`Sumbit tx error: ${err}`);
          yield* Effect.fail(err.err);
        });
      const onConfirmFailure = (err: ConfirmError) =>
        Effect.logError(`Confirm tx error: ${err}`);
      yield* handleSignSubmit(
        lucid,
        txBuilder,
        onSubmitFailure,
        onConfirmFailure,
      ).pipe(Effect.withSpan("handleSignSubmit-merge-tx"));
      yield* Effect.logInfo(
        "🔸 Merge transaction submitted, updating the db...",
      );
      if (firstBlockTxs.length === 0) {
        return;
      }
      const { spent: spentOutRefs, produced: producedUTxOs } =
        yield* findAllSpentAndProducedUTxOs(firstBlockTxs).pipe(
          Effect.withSpan("findAllSpentAndProducedUTxOs"),
        );

      // - Clear all the spent UTxOs from the confirmed ledger
      // - Add all the produced UTxOs from the confirmed ledger
      // - Remove all the tx hashes of the merged block from BlocksDB
      const bs = 100;
      yield* Effect.logInfo("🔸 Clear confirmed ledger db...");
      for (let i = 0; i < spentOutRefs.length; i += bs) {
        yield* ConfirmedLedgerDB.clearUTxOs(spentOutRefs.slice(i, i + bs)).pipe(
          Effect.withSpan(`confirmed-ledger-clearUTxOs-${i}`),
        );
      }
      yield* Effect.logInfo("🔸 Insert produced UTxOs...");
      for (let i = 0; i < producedUTxOs.length; i += bs) {
        yield* ConfirmedLedgerDB.insert(producedUTxOs.slice(i, i + bs))
          // .map((u) => utxoToOutRefAndCBORArray(u)),
          .pipe(Effect.withSpan(`confirmed-ledger-insert-${i}`));
      }
      yield* Effect.logInfo("🔸 Clear block from BlocksDB...");
      yield* BlocksDB.clearBlock(fromHex(headerHash)).pipe(
        Effect.withSpan("clear-block-from-BlocksDB"),
      );
      yield* Effect.logInfo("🔸 ☑️  Merge transaction completed.");

      yield* Metric.increment(mergeBlockCounter).pipe(
        Effect.withSpan("increment-merge-block-counter"),
      );

      global.BLOCKS_IN_QUEUE -= 1;
    } else {
      global.BLOCKS_IN_QUEUE = 0;
      global.LATEST_SYNC_OF_STATE_QUEUE_LENGTH = Date.now();
      yield* Effect.logInfo("🔸 No blocks found in queue.");
      return;
    }
  });
