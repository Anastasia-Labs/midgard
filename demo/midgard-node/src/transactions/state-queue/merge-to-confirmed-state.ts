/**
 * This script performs the following tasks to merge the first block into the
 * confirmed state:
 *
 * 1. Fetches the confirmed state and the block it points to (i.e. the oldest
 *    block in the queue).
 * 2. Fetches the transactions of that block by querying BlocksDB and its
 *    associated inputs table..
 * 3. Apply those transactions to ConfirmedLedgerDB and update the table to
 *    store the updated UTxO set.
 * 4. Remove all header hashes from BlocksDB associated with the merged block.
 * 5. Build and submit the merge transaction.
 */

import { BlocksDB, ConfirmedLedgerDB } from "@/database/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Address,
  LucidEvolution,
  Script,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect, Metric, Ref } from "effect";
import {
  TxConfirmError,
  fetchFirstBlockTxs,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "../utils.js";
import { Entry as LedgerEntry } from "@/database/utils/ledger.js";
import { DatabaseError } from "@/database/utils/common.js";
import { breakDownTx } from "@/utils.js";
import { Database, Globals } from "@/services/index.js";

const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merged blocks",
  bigint: true,
  incremental: true,
});

// 30 minutes.
const MAX_LIFE_OF_LOCAL_SYNC: number = 1_800_000;

const MIN_QUEUE_LENGTH_FOR_MERGING: number = 8;

const getStateQueueLength = (
  lucid: LucidEvolution,
  stateQueueAddress: Address,
): Effect.Effect<number, SDK.LucidError, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH = yield* Ref.get(
      globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
    );
    const now_millis = Date.now();
    if (
      now_millis - LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH >
      MAX_LIFE_OF_LOCAL_SYNC
    ) {
      // We consider in-memory state queue length stale.
      yield* Effect.logInfo(
        `🔸 Fetching state queue length from ${stateQueueAddress}...`,
      );
      const stateQueueUtxos = yield* Effect.tryPromise({
        try: () => lucid.utxosAt(stateQueueAddress),
        catch: (e) =>
          new SDK.LucidError({
            message: `Failed to fetch UTxOs at state queue address: ${stateQueueAddress}`,
            cause: e,
          }),
      });

      yield* Ref.set(
        globals.BLOCKS_IN_QUEUE,
        Math.max(0, stateQueueUtxos.length - 1),
      );
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );

      return stateQueueUtxos.length;
    } else {
      return yield* Ref.get(globals.BLOCKS_IN_QUEUE);
    }
  });

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param spendScript - State queue's spending script.
 * @param mintScript - State queue's minting script.
 * @returns An Effect that resolves when the merge transaction is built and
 *          submitted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
  spendScript: Script,
  mintScript: Script,
): Effect.Effect<
  void,
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | TxSubmitError
  | TxSignError,
  Database | Globals
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const currentStateQueueLength = yield* getStateQueueLength(
      lucid,
      fetchConfig.stateQueueAddress,
    );
    // Avoid a merge tx if the queue is too short (performing a merge with such
    // conditions has a chance of wasting the work done for root computations).
    const RESET_IN_PROGRESS = Ref.get(globals.RESET_IN_PROGRESS);
    if (
      currentStateQueueLength < MIN_QUEUE_LENGTH_FOR_MERGING ||
      RESET_IN_PROGRESS
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
      yield* SDK.fetchConfirmedStateAndItsLinkProgram(
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
      const txBuilder: TxSignBuilder =
        yield* SDK.mergeToConfirmedStateProgram(
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
      const onSubmitFailure = (err: TxSubmitError) =>
        Effect.gen(function* () {
          yield* Effect.logError(`Submit tx error: ${err}`);
          yield* Effect.fail(
            new TxSubmitError({
              message: "failed to submit the merge tx",
              cause: err,
              txHash: txBuilder.toHash(),
            }),
          );
        });
      const onConfirmFailure = (err: TxConfirmError) =>
        Effect.logError(`Confirm tx error: ${err}`);
      yield* handleSignSubmit(lucid, txBuilder).pipe(
        Effect.catchTag("TxSubmitError", onSubmitFailure),
        Effect.catchTag("TxConfirmError", onConfirmFailure),
        Effect.withSpan("handleSignSubmit-merge-tx"),
      );
      yield* Effect.logInfo(
        "🔸 Merge transaction submitted, updating the db...",
      );

      const spentOutRefs: Buffer[] = [];
      const producedUTxOs: LedgerEntry[] = [];

      yield* Effect.forEach(
        firstBlockTxs,
        (txCbor) =>
          Effect.gen(function* () {
            const { spent, produced } = yield* breakDownTx(txCbor);
            spentOutRefs.push(...spent);
            producedUTxOs.push(...produced);
          }),
        { concurrency: "unbounded" },
      );

      // const { spent: spentOutRefs, produced: producedUTxOs } =
      //   yield* findAllSpentAndProducedUTxOs(firstBlockTxs).pipe(
      //     Effect.withSpan("findAllSpentAndProducedUTxOs"),
      //   );

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
        yield* ConfirmedLedgerDB.insertMultiple(producedUTxOs.slice(i, i + bs))
          // .map((u) => utxoToOutRefAndCBORArray(u)),
          .pipe(Effect.withSpan(`confirmed-ledger-insert-${i}`));
      }
      yield* Effect.logInfo("🔸 Clear block from BlocksDB...");
      yield* BlocksDB.clearBlock(headerHash).pipe(
        Effect.withSpan("clear-block-from-BlocksDB"),
      );
      yield* Effect.logInfo("🔸 ☑️  Merge transaction completed.");

      yield* Metric.increment(mergeBlockCounter).pipe(
        Effect.withSpan("increment-merge-block-counter"),
      );

      yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n - 1);
    } else {
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );
      yield* Effect.logInfo("🔸 No blocks found in queue.");
      return;
    }
  });
