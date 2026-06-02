/**
 * State-queue merge transaction for advancing committed blocks into confirmed
 * state.
 * This module owns the off-chain merge flow that replays the oldest queued
 * block into the confirmed ledger and then submits the corresponding merge tx.
 *
 * It performs the following tasks:
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

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Address,
  credentialToAddress,
  LucidEvolution,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Duration, Effect, Metric, Ref } from "effect";

import {
  BlocksDB,
  ConfirmedLedgerDB,
  MutationJobsDB,
} from "@/database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Entry as LedgerEntry } from "@/database/utils/ledger.js";
import { emitQueueStateMetrics } from "@/fibers/queue-metrics.js";
import {
  availableOperatorWalletUtxos,
  fetchOperatorWalletView,
} from "@/operator-wallet-view.js";
import { Database, Globals, NodeConfig } from "@/services/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "@/transactions/reference-scripts.js";
import {
  BlockTxPayload,
  fetchFirstBlockTxs,
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { outRefLabel } from "@/tx-context.js";
import { breakDownTx } from "@/utils.js";
import { alignedUnixTimeStrictlyAfter } from "@/workers/utils/commit-end-time.js";

const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merged blocks",
  bigint: true,
  incremental: true,
});

const mergeFailureCounter = Metric.counter("merge_failure_count", {
  description: "A counter for tracking merge failures",
  bigint: true,
  incremental: true,
});

const mergeMissingBlockTxsCounter = Metric.counter(
  "merge_missing_block_txs_count",
  {
    description: "A counter for merge attempts blocked by missing BlocksDB txs",
    bigint: true,
    incremental: true,
  },
);

const mergeBlockTxDecodeFailureCounter = Metric.counter(
  "merge_block_tx_decode_failure_count",
  {
    description:
      "A counter for merge attempts blocked by malformed block transactions",
    bigint: true,
    incremental: true,
  },
);

const mergeLocalFinalizationFailureCounter = Metric.counter(
  "merge_local_finalization_failure_count",
  {
    description:
      "A counter for failed local DB finalization after merge submit",
    bigint: true,
    incremental: true,
  },
);

const mergeDurationTimer = Metric.timer(
  "merge_duration",
  "Duration of one merge attempt in milliseconds",
);

// 30 minutes.
const MAX_LIFE_OF_LOCAL_SYNC: number = 1_800_000;

const DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING: number = 8;
const STATE_QUEUE_MATURITY_DURATION_MS = 30;
// Add buffer after maturity boundary to absorb provider slot/time drift and
// avoid invalid-before rejections right at the boundary.
const MERGE_MATURITY_DELAY_BUFFER_MS = 20_000;

type MergeErrorCode =
  | "E_MERGE_LAYOUT_DERIVATION_FAILED"
  | "E_MERGE_REDEEMER_INDEX_MISMATCH"
  | "E_MERGE_MISSING_BLOCK_TXS"
  | "E_MERGE_BLOCK_TX_DECODE_FAILED"
  | "E_MERGE_UPLC_EVAL_FAILED";

type MissingBlockTxsDiagnosis = {
  readonly reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE";
  readonly txHashesFound: number;
  readonly txsResolved: number;
};

export const diagnoseMissingBlockTxs = (
  txHashesFound: number,
  txsResolved: number,
): MissingBlockTxsDiagnosis | undefined => {
  if (txsResolved !== txHashesFound) {
    return {
      reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE",
      txHashesFound,
      txsResolved,
    };
  }
  return undefined;
};

type MergeOptions = {
  readonly bypassQueueLengthGuard?: boolean;
  readonly referenceScriptsAddress?: string;
};

const makeJsonSafe = (value: unknown): unknown => {
  try {
    return JSON.parse(
      JSON.stringify(value, (_key, nestedValue) =>
        typeof nestedValue === "bigint" ? nestedValue.toString() : nestedValue,
      ),
    ) as unknown;
  } catch {
    return formatUnknownError(value);
  }
};

const makeMergeStateQueueError = (
  errorCode: MergeErrorCode,
  message: string,
  cause: unknown,
): SDK.StateQueueError =>
  new SDK.StateQueueError({
    message: `${errorCode}: ${message}`,
    cause: {
      error_code: errorCode,
      details: makeJsonSafe(cause),
    },
  });

type MergeFailureOptions = {
  readonly missingBlockTxs?: boolean;
};

const failMergeWithCode = (
  errorCode: MergeErrorCode,
  message: string,
  cause: unknown,
  options?: MergeFailureOptions,
): Effect.Effect<never, SDK.StateQueueError> =>
  Effect.gen(function* () {
    yield* Metric.increment(mergeFailureCounter);
    if (options?.missingBlockTxs === true) {
      yield* Metric.increment(mergeMissingBlockTxsCounter);
    }
    return yield* Effect.fail(
      makeMergeStateQueueError(errorCode, message, cause),
    );
  });

type MergeDecodedBlockTx = {
  readonly txId: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly LedgerEntry[];
};

type MergeBlockTxPreflightError = {
  readonly index: number;
  readonly txIdHex: string;
  readonly reason: "DECODE_FAILED" | "TX_ID_MISMATCH";
  readonly details: string;
  readonly decodedTxIdHex?: string;
};

export const preflightDecodeBlockTxs = (
  blockTxs: readonly BlockTxPayload[],
): Effect.Effect<readonly MergeDecodedBlockTx[], MergeBlockTxPreflightError> =>
  Effect.forEach(
    blockTxs,
    (blockTx, index) =>
      Effect.gen(function* () {
        const txIdHex = blockTx.txId.toString("hex");
        const decoded = yield* breakDownTx(blockTx.txCbor).pipe(
          Effect.mapError(
            (cause): MergeBlockTxPreflightError => ({
              index,
              txIdHex,
              reason: "DECODE_FAILED",
              details: formatUnknownError(cause),
            }),
          ),
        );
        if (!decoded.txId.equals(blockTx.txId)) {
          return yield* Effect.fail<MergeBlockTxPreflightError>({
            index,
            txIdHex,
            reason: "TX_ID_MISMATCH",
            decodedTxIdHex: decoded.txId.toString("hex"),
            details:
              "Computed tx_id from payload does not match BlocksDB tx_id",
          });
        }
        return {
          txId: blockTx.txId,
          spent: decoded.spent,
          produced: decoded.produced,
        } satisfies MergeDecodedBlockTx;
      }),
    { concurrency: "unbounded" },
  );

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
      yield* emitQueueStateMetrics;

      return Math.max(0, stateQueueUtxos.length - 1);
    } else {
      return yield* Ref.get(globals.BLOCKS_IN_QUEUE);
    }
  });

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param contracts - Midgard script bundle used for state_queue and settlement.
 * @returns An Effect that resolves when the merge transaction is built and
 *          submitted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
  contracts: SDK.MidgardValidators,
  options?: MergeOptions,
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
  | TxConfirmError
  | TxSignError,
  Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const mergeStartedAt = Date.now();
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const currentStateQueueLength = yield* getStateQueueLength(
      lucid,
      fetchConfig.stateQueueAddress,
    );
    const minQueueLengthForMerging =
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING;
    // Avoid a merge tx if the queue is too short (performing a merge with such
    // conditions has a chance of wasting the work done for root computations).
    const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (
      (!options?.bypassQueueLengthGuard &&
        currentStateQueueLength < minQueueLengthForMerging) ||
      resetInProgress
    ) {
      return;
    }

    yield* Effect.logInfo("🔸 Merging of oldest block started.");

    yield* Effect.logInfo(
      "🔸 Fetching confirmed state and the first block in queue from L1...",
    );
    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      yield* SDK.fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig);
    if (firstBlockUTxO) {
      yield* Effect.logInfo(
        `🔸 First block found: ${firstBlockUTxO.utxo.txHash}#${firstBlockUTxO.utxo.outputIndex}`,
      );
      // Fetch transactions from the first block
      yield* Effect.logInfo("🔸 Looking up its transactions from BlocksDB...");
      const {
        txs: firstBlockTxs,
        txHashes: firstBlockTxHashes,
        headerHash,
      } = yield* fetchFirstBlockTxs(firstBlockUTxO).pipe(
        Effect.withSpan("fetchFirstBlockTxs"),
      );
      const missingBlockTxsDiagnosis = diagnoseMissingBlockTxs(
        firstBlockTxHashes.length,
        firstBlockTxs.length,
      );
      if (missingBlockTxsDiagnosis !== undefined) {
        return yield* failMergeWithCode(
          "E_MERGE_MISSING_BLOCK_TXS",
          "Failed to merge block into confirmed state",
          {
            headerHash: headerHash.toString("hex"),
            ...missingBlockTxsDiagnosis,
          },
          { missingBlockTxs: true },
        );
      }
      if (firstBlockTxHashes.length === 0) {
        yield* Effect.logInfo(
          `🔸 No native block tx payloads indexed for header=${headerHash.toString("hex")}; treating merge replay as a no-op for immutable txs.`,
        );
      }
      const preflightDecodedBlockTxsResult = yield* Effect.either(
        preflightDecodeBlockTxs(firstBlockTxs),
      );
      if (preflightDecodedBlockTxsResult._tag === "Left") {
        yield* Metric.increment(mergeBlockTxDecodeFailureCounter);
        return yield* failMergeWithCode(
          "E_MERGE_BLOCK_TX_DECODE_FAILED",
          "Failed preflight decode of block transactions before merge submission",
          {
            headerHash: headerHash.toString("hex"),
            failingTx: preflightDecodedBlockTxsResult.left,
            txCount: firstBlockTxs.length,
          },
        );
      }
      const preflightDecodedBlockTxs = preflightDecodedBlockTxsResult.right;
      const preflightSpentOutRefs: Buffer[] = [];
      const preflightProducedUTxOs: LedgerEntry[] = [];
      for (const decoded of preflightDecodedBlockTxs) {
        preflightSpentOutRefs.push(...decoded.spent);
        preflightProducedUTxOs.push(...decoded.produced);
      }
      yield* Effect.logInfo(
        `🔸 Preflight decoded ${preflightDecodedBlockTxs.length} block tx(s) successfully (header=${headerHash.toString("hex")}).`,
      );
      yield* Effect.logInfo("🔸 Building merge transaction...");

      const blockHeader: SDK.Header = yield* SDK.getHeaderFromStateQueueDatum(
        firstBlockUTxO.datum,
      );
      if (firstBlockUTxO.datum.key === "Empty") {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Failed to build merge transaction",
            cause: "first queued block cannot be a root node",
          }),
        );
      }
      const headerNodeKey = firstBlockUTxO.datum.key.Key.key;
      const recomputedHeaderHash = yield* SDK.hashBlockHeader(blockHeader);
      if (recomputedHeaderHash !== headerNodeKey) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to build merge transaction: queued block key/hash mismatch",
            cause: `datumKey=${headerNodeKey},computed=${recomputedHeaderHash}`,
          }),
        );
      }
      const maturityThresholdUnixTime =
        Number(blockHeader.endTime) + STATE_QUEUE_MATURITY_DURATION_MS;
      const mergeMaturityValidFromUnixTime = alignedUnixTimeStrictlyAfter(
        lucid,
        maturityThresholdUnixTime - 1,
      );
      const mergeReadyAfterUnixTime =
        mergeMaturityValidFromUnixTime + MERGE_MATURITY_DELAY_BUFFER_MS;
      if (Date.now() < mergeReadyAfterUnixTime) {
        yield* Effect.logInfo(
          `🔸 Oldest block is not mature enough for merge yet (ready_after=${mergeReadyAfterUnixTime},valid_from=${mergeMaturityValidFromUnixTime},now=${Date.now()}).`,
        );
        return;
      }

      yield* Effect.logInfo(
        `🔸 Merge policies: state_queue=${fetchConfig.stateQueuePolicyId},settlement=${contracts.settlement.policyId},state_queue_script_has_settlement_param=${contracts.stateQueue.mintingScriptCBOR.includes(contracts.settlement.policyId)}`,
      );

      const network = lucid.config().network;
      if (network === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to build merge transaction: Cardano network is undefined",
            cause: "lucid.config().network",
          }),
        );
      }
      const hubOracleAddress = credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      );
      const hubOracleUnit = toUnit(
        contracts.hubOracle.policyId,
        SDK.HUB_ORACLE_ASSET_NAME,
      );
      const hubOracleWitnessUtxos = yield* Effect.tryPromise({
        try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to fetch hub-oracle witness UTxOs for merge tx",
            cause,
          }),
      });
      if (hubOracleWitnessUtxos.length !== 1) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to resolve unique hub-oracle UTxO for merge transaction",
            cause: `expected=1,found=${hubOracleWitnessUtxos.length},address=${hubOracleAddress},unit=${hubOracleUnit}`,
          }),
        );
      }
      const hubOracleRefInput = hubOracleWitnessUtxos[0];

      const resolvedReferenceScripts =
        options?.referenceScriptsAddress === undefined
          ? []
          : yield* fetchReferenceScriptUtxosProgram(
              lucid,
              options.referenceScriptsAddress,
              [
                {
                  name: "state-queue spending",
                  script: contracts.stateQueue.spendingScript,
                },
                {
                  name: "state-queue minting",
                  script: contracts.stateQueue.mintingScript,
                },
                {
                  name: "settlement minting",
                  script: contracts.settlement.mintingScript,
                },
              ],
            );
      const referenceScripts: SDK.StateQueueMergeReferenceScripts | undefined =
        options?.referenceScriptsAddress === undefined
          ? undefined
          : {
              stateQueueSpending: referenceScriptByName(
                resolvedReferenceScripts,
                "state-queue spending",
              ),
              stateQueueMinting: referenceScriptByName(
                resolvedReferenceScripts,
                "state-queue minting",
              ),
              settlementMinting: referenceScriptByName(
                resolvedReferenceScripts,
                "settlement minting",
              ),
            };

      const operatorWalletView = yield* Effect.tryPromise({
        try: () => fetchOperatorWalletView(lucid),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to initialize merge wallet view",
            cause,
          }),
      });
      const feeInput = yield* SDK.selectPureAdaFeeInput(
        availableOperatorWalletUtxos(operatorWalletView),
      );
      yield* Effect.logInfo(
        `🔸 Using fee input ${outRefLabel(feeInput)} (lovelace=${(feeInput.assets.lovelace ?? 0n).toString()}, known_wallet_utxos=${operatorWalletView.knownUtxos.length.toString()}) for merge tx.`,
      );

      const builtMerge =
        yield* SDK.buildProductionMergeToConfirmedStateTxProgram({
          lucid,
          fetchConfig,
          contracts,
          confirmedUTxO,
          firstBlockUTxO,
          validFrom: mergeMaturityValidFromUnixTime,
          feeInput,
          hubOracleRefInput,
          referenceScripts,
        }).pipe(Effect.tapError(() => Metric.increment(mergeFailureCounter)));
      const txBuilder = builtMerge.tx;

      // Submit the transaction
      /**
       * Normalizes transaction-submission failures during confirmed-state merging.
       */
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
      /**
       * Normalizes transaction-confirmation failures during confirmed-state merging.
       */
      const onConfirmFailure = (err: TxConfirmError) =>
        Effect.gen(function* () {
          yield* Effect.logError(
            `Confirm tx error: ${err}; refusing local merge finalization until L1 confirmation is verified`,
          );
          yield* Effect.fail(
            new TxConfirmError({
              message:
                "failed to confirm the merge tx; local merge finalization blocked",
              cause: err,
              txHash: txBuilder.toHash(),
            }),
          );
        });
      yield* handleSignSubmit(lucid, txBuilder).pipe(
        Effect.catchTag("TxSubmitError", onSubmitFailure),
        Effect.catchTag("TxConfirmError", onConfirmFailure),
        Effect.withSpan("handleSignSubmit-merge-tx"),
      );
      yield* Effect.logInfo(
        "🔸 Merge transaction submitted, updating the db...",
      );

      const finalizeLocalMergeProgram = Effect.gen(function* () {
        const jobId = `confirmed_merge_finalization:${headerHash.toString(
          "hex",
        )}`;
        yield* MutationJobsDB.start({
          jobId,
          kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
          payload: {
            headerHash: headerHash.toString("hex"),
            spentOutRefCount: preflightSpentOutRefs.length,
            producedUtxoCount: preflightProducedUTxOs.length,
          },
        });
        const sql = yield* SqlClient.SqlClient;
        // - Clear all the spent UTxOs from the confirmed ledger
        // - Add all the produced UTxOs from the confirmed ledger
        // - Remove all the tx hashes of the merged block from BlocksDB
        const bs = 100;
        yield* sql
          .withTransaction(
            Effect.gen(function* () {
              yield* Effect.logInfo("🔸 Clear confirmed ledger db...");
              for (let i = 0; i < preflightSpentOutRefs.length; i += bs) {
                yield* ConfirmedLedgerDB.clearUTxOs(
                  preflightSpentOutRefs.slice(i, i + bs),
                ).pipe(Effect.withSpan(`confirmed-ledger-clearUTxOs-${i}`));
              }
              yield* Effect.logInfo("🔸 Insert produced UTxOs...");
              for (let i = 0; i < preflightProducedUTxOs.length; i += bs) {
                yield* ConfirmedLedgerDB.insertMultiple(
                  preflightProducedUTxOs.slice(i, i + bs),
                ).pipe(Effect.withSpan(`confirmed-ledger-insert-${i}`));
              }
              yield* Effect.logInfo("🔸 Clear block from BlocksDB...");
              yield* BlocksDB.clearBlock(headerHash).pipe(
                Effect.withSpan("clear-block-from-BlocksDB"),
              );
            }),
          )
          .pipe(
            sqlErrorToDatabaseError(
              "confirmed_merge_finalization",
              "Failed to finalize confirmed-state merge locally",
            ),
          );
        yield* MutationJobsDB.markCompleted(jobId);
      }).pipe(
        Effect.tapError((error) =>
          MutationJobsDB.markFailed(
            `confirmed_merge_finalization:${headerHash.toString("hex")}`,
            formatUnknownError(error),
          ).pipe(Effect.catchAll(() => Effect.void)),
        ),
      );
      yield* finalizeLocalMergeProgram.pipe(
        Effect.tapError((error) =>
          Effect.gen(function* () {
            yield* Metric.increment(mergeLocalFinalizationFailureCounter);
            yield* Effect.logError(
              `🔸 Merge local finalization failed after on-chain submit (header=${headerHash.toString(
                "hex",
              )},tx_count=${preflightDecodedBlockTxs.length},sample_tx_ids=${JSON.stringify(
                preflightDecodedBlockTxs
                  .slice(0, 10)
                  .map((decoded) => decoded.txId.toString("hex")),
              )},error=${formatUnknownError(error)})`,
            );
          }),
        ),
      );
      yield* Effect.logInfo("🔸 ☑️  Merge transaction completed.");

      yield* Metric.increment(mergeBlockCounter).pipe(
        Effect.withSpan("increment-merge-block-counter"),
      );
      yield* mergeDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - mergeStartedAt)),
      );

      yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => Math.max(0, n - 1));
      yield* emitQueueStateMetrics;
    } else {
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );
      yield* emitQueueStateMetrics;
      yield* Effect.logInfo("🔸 No blocks found in queue.");
      yield* mergeDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - mergeStartedAt)),
      );
      return;
    }
  });
