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

import {
  BlocksDB,
  ConfirmedLedgerDB,
  MutationJobsDB,
} from "@/database/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Address,
  CML,
  Data,
  LucidEvolution,
  TxSignBuilder,
  UTxO,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Duration, Effect, Metric, Ref } from "effect";
import {
  BlockTxPayload,
  TxConfirmError,
  fetchFirstBlockTxs,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "@/transactions/utils.js";
import { alignedUnixTimeStrictlyAfter } from "@/workers/utils/commit-end-time.js";
import { Entry as LedgerEntry } from "@/database/utils/ledger.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { breakDownTx } from "@/utils.js";
import { Database, Globals, NodeConfig } from "@/services/index.js";
import { SqlClient } from "@effect/sql";
import { emitQueueStateMetrics } from "@/fibers/queue-metrics.js";
import {
  findRedeemerDataCbor,
  getRedeemerPointersInContextOrder,
  getTxInfoRedeemerIndexes,
  withStubbedProviderEvaluation,
} from "@/cml-redeemers.js";
import {
  availableOperatorWalletUtxos,
  fetchOperatorWalletView,
} from "@/operator-wallet-view.js";
import {
  collectIndexedOutputs,
  collectSortedInputOutRefs,
  compareOutRefs,
  findOutRefIndex,
  outRefLabel,
  type OutRefLike,
} from "@/tx-context.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "@/transactions/reference-scripts.js";

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
const MIN_SETTLEMENT_OUTPUT_LOVELACE = 5_000_000n;
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

const formatUnknownError = (error: unknown): string => {
  if (error instanceof Error) {
    const maybeCause = (error as Error & { cause?: unknown }).cause;
    if (maybeCause === undefined) {
      return `${error.name}: ${error.message}`;
    }
    const causeMessage =
      typeof maybeCause === "string"
        ? maybeCause
        : (() => {
            try {
              return JSON.stringify(maybeCause);
            } catch {
              return `${maybeCause}`;
            }
          })();
    return `${error.name}: ${error.message}; cause=${causeMessage}`;
  }
  if (typeof error === "object" && error !== null && "message" in error) {
    const message = String((error as { readonly message: unknown }).message);
    const maybeCause = (error as { readonly cause?: unknown }).cause;
    if (maybeCause === undefined) {
      return message;
    }
    const causeMessage =
      maybeCause instanceof Error
        ? `${maybeCause.name}: ${maybeCause.message}`
        : typeof maybeCause === "string"
          ? maybeCause
          : (() => {
              try {
                return JSON.stringify(maybeCause);
              } catch {
                return `${maybeCause}`;
              }
            })();
    return `${message}; cause=${causeMessage}`;
  }
  return `${error}`;
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
      details: cause,
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

type InitialMergeRedeemerSeedIndexes = {
  readonly stateQueueMintPointerIndex: number;
  readonly settlementMintPointerIndex: number;
  readonly stateQueueRedeemerIndex: number;
  readonly settlementRedeemerIndex: number;
};

const comparePolicyIds = (a: string, b: string): number =>
  Buffer.from(a, "hex").compare(Buffer.from(b, "hex"));

// The merge validators cross-reference entries in Aiken's redeemer pair list,
// while Lucid/CML mint redeemer pointers use mint-policy ordinals. Mint
// purposes are ordered by policy id after spending purposes, matching the
// shared tx-info ordering helper used by the other node builders.
export const deriveInitialMergeRedeemerSeedIndexes = ({
  scriptSpendRedeemerCount,
  stateQueuePolicyId,
  settlementPolicyId,
}: {
  readonly scriptSpendRedeemerCount: number;
  readonly stateQueuePolicyId: string;
  readonly settlementPolicyId: string;
}): InitialMergeRedeemerSeedIndexes => {
  const mintPolicyOrder = [stateQueuePolicyId, settlementPolicyId].sort(
    comparePolicyIds,
  );
  const stateQueueMintPointerIndex =
    mintPolicyOrder.indexOf(stateQueuePolicyId);
  const settlementMintPointerIndex =
    mintPolicyOrder.indexOf(settlementPolicyId);
  if (stateQueueMintPointerIndex < 0 || settlementMintPointerIndex < 0) {
    throw new Error(
      `Failed to derive merge mint-pointer indexes (stateQueue=${stateQueueMintPointerIndex}, settlement=${settlementMintPointerIndex})`,
    );
  }
  return {
    stateQueueMintPointerIndex,
    settlementMintPointerIndex,
    stateQueueRedeemerIndex:
      scriptSpendRedeemerCount + stateQueueMintPointerIndex,
    settlementRedeemerIndex:
      scriptSpendRedeemerCount + settlementMintPointerIndex,
  };
};

const findInputIndex = (
  orderedInputs: readonly OutRefLike[],
  target: OutRefLike,
): number =>
  orderedInputs.findIndex(
    (input) =>
      input.txHash === target.txHash &&
      input.outputIndex === target.outputIndex,
  );

const selectFeeInput = (
  walletUtxos: readonly UTxO[],
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const pureAdaUtxos = walletUtxos.filter((utxo) =>
      Object.entries(utxo.assets).every(
        ([unit, amount]) => unit === "lovelace" || amount <= 0n,
      ),
    );
    if (pureAdaUtxos.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to select fee input for merge transaction",
          cause: "operator wallet has no pure-ADA UTxO",
        }),
      );
    }
    const sorted = [...pureAdaUtxos].sort((a, b) => {
      const lovelaceA = a.assets.lovelace ?? 0n;
      const lovelaceB = b.assets.lovelace ?? 0n;
      if (lovelaceA === lovelaceB) {
        return compareOutRefs(a, b);
      }
      return lovelaceA > lovelaceB ? -1 : 1;
    });
    return sorted[0];
  });

const SettlementResolutionClaimSchema = Data.Object({
  resolution_time: Data.Integer(),
  operator: Data.Bytes(),
});
type SettlementResolutionClaim = Data.Static<
  typeof SettlementResolutionClaimSchema
>;
const SettlementResolutionClaim =
  SettlementResolutionClaimSchema as unknown as SettlementResolutionClaim;

const SettlementDatumSchema = Data.Object({
  deposits_root: SDK.MerkleRoot,
  withdrawals_root: SDK.MerkleRoot,
  transactions_root: SDK.MerkleRoot,
  resolution_claim: Data.Nullable(SettlementResolutionClaimSchema),
});
type SettlementDatum = Data.Static<typeof SettlementDatumSchema>;
const SettlementDatum = SettlementDatumSchema as unknown as SettlementDatum;

const SettlementMintRedeemerSchema = Data.Enum([
  Data.Object({
    Spawn: Data.Object({
      settlement_id: Data.Bytes(),
      output_index: Data.Integer(),
      state_queue_merge_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Remove: Data.Object({
      settlement_id: Data.Bytes(),
      input_index: Data.Integer(),
      spend_redeemer_index: Data.Integer(),
    }),
  }),
]);
type SettlementMintRedeemer = Data.Static<typeof SettlementMintRedeemerSchema>;
const SettlementMintRedeemer =
  SettlementMintRedeemerSchema as unknown as SettlementMintRedeemer;

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

      const { data: confirmedStateData } =
        yield* SDK.getConfirmedStateFromStateQueueDatum(confirmedUTxO.datum);
      const updatedConfirmedState: SDK.ConfirmedState = {
        headerHash: headerNodeKey,
        prevHeaderHash: confirmedStateData.headerHash,
        utxoRoot: blockHeader.utxosRoot,
        startTime: confirmedStateData.startTime,
        endTime: blockHeader.endTime,
        protocolVersion: blockHeader.protocolVersion,
      };
      const updatedConfirmedNodeDatum: SDK.StateQueueNodeView = {
        ...confirmedUTxO.datum,
        data: SDK.castConfirmedStateToData(
          updatedConfirmedState,
        ) as SDK.LinkedListNodeView["data"],
        next: firstBlockUTxO.datum.next,
      };

      const stateQueueAssetsToBurn = {
        [toUnit(fetchConfig.stateQueuePolicyId, firstBlockUTxO.assetName)]: -1n,
      };
      const settlementUnit = toUnit(
        contracts.settlement.policyId,
        headerNodeKey,
      );
      const settlementAssetsToMint = {
        [settlementUnit]: 1n,
      };
      const settlementOutputAssets = {
        lovelace: MIN_SETTLEMENT_OUTPUT_LOVELACE,
        ...settlementAssetsToMint,
      };
      const settlementOutputDatum: SettlementDatum = {
        deposits_root: blockHeader.depositsRoot,
        withdrawals_root: blockHeader.withdrawalsRoot,
        transactions_root: blockHeader.transactionsRoot,
        resolution_claim: null,
      };
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
      const stateQueueSpendingScriptRef =
        options?.referenceScriptsAddress === undefined
          ? undefined
          : referenceScriptByName(
              resolvedReferenceScripts,
              "state-queue spending",
            );
      const stateQueueMintingScriptRef =
        options?.referenceScriptsAddress === undefined
          ? undefined
          : referenceScriptByName(
              resolvedReferenceScripts,
              "state-queue minting",
            );
      const settlementMintingScriptRef =
        options?.referenceScriptsAddress === undefined
          ? undefined
          : referenceScriptByName(
              resolvedReferenceScripts,
              "settlement minting",
            );
      const mergeReferenceInputs = [
        hubOracleRefInput,
        ...(stateQueueSpendingScriptRef === undefined
          ? []
          : [stateQueueSpendingScriptRef]),
        ...(stateQueueMintingScriptRef === undefined
          ? []
          : [stateQueueMintingScriptRef]),
        ...(settlementMintingScriptRef === undefined
          ? []
          : [settlementMintingScriptRef]),
      ];

      const operatorWalletView = yield* Effect.tryPromise({
        try: () => fetchOperatorWalletView(lucid),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to initialize merge wallet view",
            cause,
          }),
      });
      const feeInput = yield* selectFeeInput(
        availableOperatorWalletUtxos(operatorWalletView),
      );
      yield* Effect.logInfo(
        `🔸 Using fee input ${outRefLabel(feeInput)} (lovelace=${(feeInput.assets.lovelace ?? 0n).toString()}, known_wallet_utxos=${operatorWalletView.knownUtxos.length.toString()}) for merge tx.`,
      );

      const encodedConfirmedNodeDatum = yield* Effect.try({
        try: () => SDK.encodeLinkedListNodeView(updatedConfirmedNodeDatum),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to encode updated confirmed-state node datum for merge transaction",
            cause,
          }),
      });
      const encodedSettlementDatum = yield* Effect.try({
        try: () => Data.to(settlementOutputDatum, SettlementDatum),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to encode settlement output datum for merge transaction",
            cause,
          }),
      });
      type MergeRedeemerLayout = {
        readonly headerNodeInputIndex: number;
        readonly confirmedStateInputIndex: number;
        readonly confirmedStateOutputIndex: number;
        readonly settlementOutputIndex: number;
        readonly stateQueueRedeemerIndex: number;
        readonly settlementRedeemerIndex: number;
        readonly hubOracleRefInputIndex: number;
      };

      const encodeMergeRedeemers = (
        layout: MergeRedeemerLayout,
      ): Effect.Effect<
        {
          readonly encodedStateQueueMergeRedeemer: string;
          readonly encodedSettlementSpawnRedeemer: string;
        },
        SDK.StateQueueError
      > =>
        Effect.gen(function* () {
          const stateQueueMergeRedeemer: SDK.StateQueueRedeemer = {
            MergeToConfirmedState: {
              header_node_key: headerNodeKey,
              header_node_input_index: BigInt(layout.headerNodeInputIndex),
              confirmed_state_input_index: BigInt(
                layout.confirmedStateInputIndex,
              ),
              confirmed_state_output_index: BigInt(
                layout.confirmedStateOutputIndex,
              ),
              m_settlement_redeemer_index: BigInt(
                layout.settlementRedeemerIndex,
              ),
              merged_block_transactions_root: blockHeader.transactionsRoot,
              merged_block_deposits_root: blockHeader.depositsRoot,
              merged_block_withdrawals_root: blockHeader.withdrawalsRoot,
            },
          };
          const settlementSpawnRedeemer: SettlementMintRedeemer = {
            Spawn: {
              settlement_id: headerNodeKey,
              output_index: BigInt(layout.settlementOutputIndex),
              state_queue_merge_redeemer_index: BigInt(
                layout.stateQueueRedeemerIndex,
              ),
              hub_ref_input_index: BigInt(layout.hubOracleRefInputIndex),
            },
          };
          const encodedStateQueueMergeRedeemer = yield* Effect.try({
            try: () => Data.to(stateQueueMergeRedeemer, SDK.StateQueueRedeemer),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Failed to encode state_queue merge redeemer for merge transaction",
                cause,
              }),
          });
          const encodedSettlementSpawnRedeemer = yield* Effect.try({
            try: () => Data.to(settlementSpawnRedeemer, SettlementMintRedeemer),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Failed to encode settlement spawn redeemer for merge transaction",
                cause,
              }),
          });
          return {
            encodedStateQueueMergeRedeemer,
            encodedSettlementSpawnRedeemer,
          };
        });

      const makeMergeTx = (
        encodedStateQueueMergeRedeemer: string,
        encodedSettlementSpawnRedeemer: string,
      ) =>
        lucid
          .newTx()
          .validFrom(mergeMaturityValidFromUnixTime)
          .collectFrom([confirmedUTxO.utxo, firstBlockUTxO.utxo], Data.void())
          .collectFrom([feeInput])
          .readFrom(mergeReferenceInputs)
          .pay.ToContract(
            fetchConfig.stateQueueAddress,
            {
              kind: "inline",
              value: encodedConfirmedNodeDatum,
            },
            confirmedUTxO.utxo.assets,
          )
          .pay.ToContract(
            contracts.settlement.spendingScriptAddress,
            {
              kind: "inline",
              value: encodedSettlementDatum,
            },
            settlementOutputAssets,
          )
          .mintAssets(stateQueueAssetsToBurn, encodedStateQueueMergeRedeemer)
          .mintAssets(settlementAssetsToMint, encodedSettlementSpawnRedeemer);

      const makeMergeTxWithScripts = (
        encodedStateQueueMergeRedeemer: string,
        encodedSettlementSpawnRedeemer: string,
      ) => {
        const tx = makeMergeTx(
          encodedStateQueueMergeRedeemer,
          encodedSettlementSpawnRedeemer,
        );
        const withStateQueueSpendingScript =
          stateQueueSpendingScriptRef === undefined
            ? tx.attach.Script(contracts.stateQueue.spendingScript)
            : tx;
        const withStateQueueMintingScript =
          stateQueueMintingScriptRef === undefined
            ? withStateQueueSpendingScript.attach.Script(
                contracts.stateQueue.mintingScript,
              )
            : withStateQueueSpendingScript;
        return settlementMintingScriptRef === undefined
          ? withStateQueueMintingScript.attach.Script(
              contracts.settlement.mintingScript,
            )
          : withStateQueueMintingScript;
      };

      const orderedInputs = [
        confirmedUTxO.utxo,
        firstBlockUTxO.utxo,
        feeInput,
      ].sort(compareOutRefs);
      const initialRedeemerSeedIndexes = yield* Effect.try({
        try: () =>
          deriveInitialMergeRedeemerSeedIndexes({
            scriptSpendRedeemerCount: 2,
            stateQueuePolicyId: fetchConfig.stateQueuePolicyId,
            settlementPolicyId: contracts.settlement.policyId,
          }),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to derive initial merge redeemer seed indexes",
            cause,
          }),
      });
      const {
        stateQueueMintPointerIndex,
        settlementMintPointerIndex,
        stateQueueRedeemerIndex: initialStateQueueRedeemerIndex,
        settlementRedeemerIndex: initialSettlementRedeemerIndex,
      } = initialRedeemerSeedIndexes;
      const confirmedStateInputIndex = findInputIndex(
        orderedInputs,
        confirmedUTxO.utxo,
      );
      const headerNodeInputIndex = findInputIndex(
        orderedInputs,
        firstBlockUTxO.utxo,
      );
      if (confirmedStateInputIndex < 0 || headerNodeInputIndex < 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Failed to derive initial merge input indexes",
            cause: `confirmed=${confirmedStateInputIndex},header=${headerNodeInputIndex}`,
          }),
        );
      }
      const initialHubOracleRefInputIndex = findOutRefIndex(
        [...mergeReferenceInputs].sort(compareOutRefs),
        hubOracleRefInput,
      );
      if (initialHubOracleRefInputIndex === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to derive initial merge hub-oracle reference index",
            cause:
              "hub-oracle reference input missing from merge reference set",
          }),
        );
      }
      const initialLayout: MergeRedeemerLayout = {
        headerNodeInputIndex,
        confirmedStateInputIndex,
        confirmedStateOutputIndex: 0,
        settlementOutputIndex: 1,
        stateQueueRedeemerIndex: initialStateQueueRedeemerIndex,
        settlementRedeemerIndex: initialSettlementRedeemerIndex,
        hubOracleRefInputIndex: initialHubOracleRefInputIndex,
      };
      const derivedLayoutResult = yield* Effect.either(
        Effect.tryPromise({
          try: async () => {
            const initialEncodedRedeemers = await Effect.runPromise(
              encodeMergeRedeemers(initialLayout),
            );
            const draftTxBuilder = await withStubbedProviderEvaluation(
              lucid,
              () =>
                makeMergeTxWithScripts(
                  initialEncodedRedeemers.encodedStateQueueMergeRedeemer,
                  initialEncodedRedeemers.encodedSettlementSpawnRedeemer,
                ).complete({ localUPLCEval: true }),
            );
            const draftTx = draftTxBuilder.toTransaction();
            const txBody = draftTx.body();
            const inputList = collectSortedInputOutRefs(txBody.inputs());
            const referenceInputList = txBody.reference_inputs();
            if (referenceInputList === undefined) {
              throw new Error(
                "Draft merge tx did not include reference inputs",
              );
            }
            const sortedReferenceInputList =
              collectSortedInputOutRefs(referenceInputList);
            const headerInput = findOutRefIndex(inputList, firstBlockUTxO.utxo);
            const confirmedInput = findOutRefIndex(
              inputList,
              confirmedUTxO.utxo,
            );
            const hubOracleRefInputIndex = findOutRefIndex(
              sortedReferenceInputList,
              hubOracleRefInput,
            );
            if (
              headerInput === undefined ||
              confirmedInput === undefined ||
              hubOracleRefInputIndex === undefined
            ) {
              throw new Error(
                `Draft merge tx missing expected input index mapping (header=${headerInput},confirmed=${confirmedInput},hub_ref=${hubOracleRefInputIndex})`,
              );
            }
            const indexedOutputs = collectIndexedOutputs(txBody.outputs());
            const confirmedOutput = indexedOutputs.find(
              (output) =>
                output.address === fetchConfig.stateQueueAddress &&
                output.datum === encodedConfirmedNodeDatum,
            );
            const settlementOutput = indexedOutputs.find(
              (output) =>
                output.address === contracts.settlement.spendingScriptAddress &&
                output.datum === encodedSettlementDatum &&
                (output.assets[settlementUnit] ?? 0n) === 1n,
            );
            if (
              confirmedOutput === undefined ||
              settlementOutput === undefined
            ) {
              throw new Error(
                `Draft merge tx missing expected outputs (confirmed=${confirmedOutput?.index ?? "missing"},settlement=${settlementOutput?.index ?? "missing"})`,
              );
            }
            const redeemerPointers = getRedeemerPointersInContextOrder(draftTx);
            const txInfoRedeemerIndexes =
              getTxInfoRedeemerIndexes(redeemerPointers);
            const stateQueueRedeemerContextIndex = redeemerPointers.findIndex(
              (pointer) =>
                pointer.tag === CML.RedeemerTag.Mint &&
                pointer.index === BigInt(stateQueueMintPointerIndex),
            );
            const settlementRedeemerContextIndex = redeemerPointers.findIndex(
              (pointer) =>
                pointer.tag === CML.RedeemerTag.Mint &&
                pointer.index === BigInt(settlementMintPointerIndex),
            );
            if (
              stateQueueRedeemerContextIndex < 0 ||
              settlementRedeemerContextIndex < 0
            ) {
              throw new Error(
                `Draft merge tx missing expected mint redeemers (state_queue_context=${stateQueueRedeemerContextIndex},settlement_context=${settlementRedeemerContextIndex})`,
              );
            }
            const stateQueueRedeemerPointer =
              redeemerPointers[stateQueueRedeemerContextIndex];
            const settlementRedeemerPointer =
              redeemerPointers[settlementRedeemerContextIndex];
            const stateQueueRedeemerIndex =
              txInfoRedeemerIndexes[stateQueueRedeemerContextIndex];
            const settlementRedeemerIndex =
              txInfoRedeemerIndexes[settlementRedeemerContextIndex];
            if (
              stateQueueRedeemerIndex === undefined ||
              stateQueueRedeemerIndex < 0 ||
              settlementRedeemerIndex === undefined ||
              settlementRedeemerIndex < 0
            ) {
              throw new Error(
                `Draft merge tx missing tx-info redeemer indexes (state_queue=${stateQueueRedeemerIndex},settlement=${settlementRedeemerIndex})`,
              );
            }
            const stateQueueRedeemerCbor = findRedeemerDataCbor(
              draftTx,
              stateQueueRedeemerPointer,
            );
            const settlementRedeemerCbor = findRedeemerDataCbor(
              draftTx,
              settlementRedeemerPointer,
            );
            return {
              layout: {
                headerNodeInputIndex: headerInput,
                confirmedStateInputIndex: confirmedInput,
                confirmedStateOutputIndex: confirmedOutput.index,
                settlementOutputIndex: settlementOutput.index,
                stateQueueRedeemerIndex,
                settlementRedeemerIndex,
                hubOracleRefInputIndex,
              } satisfies MergeRedeemerLayout,
              diagnostics: {
                initialLayout,
                redeemerPointersContextOrder: redeemerPointers.map(
                  (pointer, index) =>
                    `${index}:${pointer.tag.toString()}:${pointer.index.toString()}`,
                ),
                redeemerPointersTxInfoOrder: redeemerPointers
                  .map((pointer, contextIndex) => ({
                    pointer,
                    contextIndex,
                    txInfoIndex: txInfoRedeemerIndexes[contextIndex] ?? -1,
                  }))
                  .sort((a, b) => a.txInfoIndex - b.txInfoIndex)
                  .map(
                    ({ pointer, contextIndex, txInfoIndex }) =>
                      `${txInfoIndex}:${pointer.tag.toString()}:${pointer.index.toString()}(context=${contextIndex})`,
                  ),
                stateQueueRedeemerTxInfoIndex: stateQueueRedeemerIndex,
                settlementRedeemerTxInfoIndex: settlementRedeemerIndex,
                stateQueueRedeemerPointer:
                  stateQueueRedeemerPointer === undefined
                    ? "missing"
                    : `${stateQueueRedeemerPointer.tag.toString()}:${stateQueueRedeemerPointer.index.toString()}`,
                settlementRedeemerPointer:
                  settlementRedeemerPointer === undefined
                    ? "missing"
                    : `${settlementRedeemerPointer.tag.toString()}:${settlementRedeemerPointer.index.toString()}`,
                stateQueueRedeemerCbor: stateQueueRedeemerCbor ?? "missing",
                settlementRedeemerCbor: settlementRedeemerCbor ?? "missing",
                stateQueueRedeemerShape:
                  stateQueueRedeemerCbor === undefined
                    ? "missing"
                    : (() => {
                        try {
                          return Data.from(
                            stateQueueRedeemerCbor,
                            SDK.StateQueueRedeemer,
                          );
                        } catch {
                          return `decode-failed:${stateQueueRedeemerCbor}`;
                        }
                      })(),
                settlementRedeemerShape:
                  settlementRedeemerCbor === undefined
                    ? "missing"
                    : (() => {
                        try {
                          return Data.from(
                            settlementRedeemerCbor,
                            SettlementMintRedeemer,
                          );
                        } catch {
                          return `decode-failed:${settlementRedeemerCbor}`;
                        }
                      })(),
              },
            };
          },
          catch: (cause) =>
            new SDK.StateQueueError({
              message: `Failed to derive merge redeemer layout from balanced draft tx: ${formatUnknownError(cause)}`,
              cause,
            }),
        }),
      );

      if (derivedLayoutResult._tag === "Left") {
        const layoutDerivationCause = formatUnknownError(
          derivedLayoutResult.left,
        );
        return yield* failMergeWithCode(
          "E_MERGE_LAYOUT_DERIVATION_FAILED",
          `Failed to derive merge redeemer layout from balanced draft tx: ${layoutDerivationCause}`,
          {
            cause: layoutDerivationCause,
            initialLayout,
          },
        );
      }
      const finalLayout = derivedLayoutResult.right.layout;
      yield* Effect.logDebug(
        `🔸 Draft merge redeemer diagnostics: ${JSON.stringify(
          derivedLayoutResult.right.diagnostics,
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
        )}`,
      );
      yield* Effect.logInfo(
        `🔸 Merge redeemer layout: header_input=${finalLayout.headerNodeInputIndex},confirmed_input=${finalLayout.confirmedStateInputIndex},confirmed_output=${finalLayout.confirmedStateOutputIndex},settlement_output=${finalLayout.settlementOutputIndex},hub_ref_input=${finalLayout.hubOracleRefInputIndex},state_queue_redeemer_index=${finalLayout.stateQueueRedeemerIndex},settlement_redeemer_index=${finalLayout.settlementRedeemerIndex}`,
      );
      const finalEncodedRedeemers = yield* encodeMergeRedeemers(finalLayout);
      const decodedRedeemersEither = yield* Effect.either(
        Effect.try({
          try: () => ({
            stateQueue: Data.from(
              finalEncodedRedeemers.encodedStateQueueMergeRedeemer,
              SDK.StateQueueRedeemer,
            ) as SDK.StateQueueRedeemer,
            settlement: Data.from(
              finalEncodedRedeemers.encodedSettlementSpawnRedeemer,
              SettlementMintRedeemer,
            ) as SettlementMintRedeemer,
          }),
          catch: (cause) => cause,
        }),
      );
      if (decodedRedeemersEither._tag === "Left") {
        return yield* failMergeWithCode(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "Failed to decode merge redeemers for invariant checks",
          {
            cause: formatUnknownError(decodedRedeemersEither.left),
            layout: finalLayout,
          },
        );
      }
      const decodedStateQueueRedeemer = decodedRedeemersEither.right.stateQueue;
      const decodedSettlementRedeemer = decodedRedeemersEither.right.settlement;
      const redeemerInvariantMismatches: string[] = [];
      let decodedStateQueueMerge:
        | SDK.StateQueueRedeemer["MergeToConfirmedState"]
        | undefined;
      if (!("MergeToConfirmedState" in decodedStateQueueRedeemer)) {
        redeemerInvariantMismatches.push(
          `state_queue variant mismatch: expected=MergeToConfirmedState,got=${Object.keys(decodedStateQueueRedeemer).join("|") || "unknown"}`,
        );
      } else {
        decodedStateQueueMerge =
          decodedStateQueueRedeemer.MergeToConfirmedState;
      }
      let decodedSettlementSpawn: SettlementMintRedeemer["Spawn"] | undefined;
      if (!("Spawn" in decodedSettlementRedeemer)) {
        redeemerInvariantMismatches.push(
          `settlement variant mismatch: expected=Spawn,got=${Object.keys(decodedSettlementRedeemer).join("|") || "unknown"}`,
        );
      } else {
        decodedSettlementSpawn = decodedSettlementRedeemer.Spawn;
      }
      if (decodedStateQueueMerge !== undefined) {
        if (decodedStateQueueMerge.header_node_key !== headerNodeKey) {
          redeemerInvariantMismatches.push(
            `state_queue.header_node_key mismatch: expected=${headerNodeKey},got=${decodedStateQueueMerge.header_node_key}`,
          );
        }
        if (
          decodedStateQueueMerge.header_node_input_index !==
          BigInt(finalLayout.headerNodeInputIndex)
        ) {
          redeemerInvariantMismatches.push(
            `state_queue.header_node_input_index mismatch: expected=${finalLayout.headerNodeInputIndex},got=${decodedStateQueueMerge.header_node_input_index.toString()}`,
          );
        }
        if (
          decodedStateQueueMerge.confirmed_state_input_index !==
          BigInt(finalLayout.confirmedStateInputIndex)
        ) {
          redeemerInvariantMismatches.push(
            `state_queue.confirmed_state_input_index mismatch: expected=${finalLayout.confirmedStateInputIndex},got=${decodedStateQueueMerge.confirmed_state_input_index.toString()}`,
          );
        }
        if (
          decodedStateQueueMerge.confirmed_state_output_index !==
          BigInt(finalLayout.confirmedStateOutputIndex)
        ) {
          redeemerInvariantMismatches.push(
            `state_queue.confirmed_state_output_index mismatch: expected=${finalLayout.confirmedStateOutputIndex},got=${decodedStateQueueMerge.confirmed_state_output_index.toString()}`,
          );
        }
        if (
          decodedStateQueueMerge.m_settlement_redeemer_index !==
          BigInt(finalLayout.settlementRedeemerIndex)
        ) {
          redeemerInvariantMismatches.push(
            `state_queue.m_settlement_redeemer_index mismatch: expected=${finalLayout.settlementRedeemerIndex},got=${decodedStateQueueMerge.m_settlement_redeemer_index?.toString() ?? "null"}`,
          );
        }
        if (
          decodedStateQueueMerge.merged_block_transactions_root !==
          blockHeader.transactionsRoot
        ) {
          redeemerInvariantMismatches.push(
            `state_queue.merged_block_transactions_root mismatch: expected=${blockHeader.transactionsRoot},got=${decodedStateQueueMerge.merged_block_transactions_root}`,
          );
        }
        if (
          decodedStateQueueMerge.merged_block_deposits_root !==
          blockHeader.depositsRoot
        ) {
          redeemerInvariantMismatches.push(
            `state_queue.merged_block_deposits_root mismatch: expected=${blockHeader.depositsRoot},got=${decodedStateQueueMerge.merged_block_deposits_root}`,
          );
        }
        if (
          decodedStateQueueMerge.merged_block_withdrawals_root !==
          blockHeader.withdrawalsRoot
        ) {
          redeemerInvariantMismatches.push(
            `state_queue.merged_block_withdrawals_root mismatch: expected=${blockHeader.withdrawalsRoot},got=${decodedStateQueueMerge.merged_block_withdrawals_root}`,
          );
        }
      }
      if (decodedSettlementSpawn !== undefined) {
        if (decodedSettlementSpawn.settlement_id !== headerNodeKey) {
          redeemerInvariantMismatches.push(
            `settlement.settlement_id mismatch: expected=${headerNodeKey},got=${decodedSettlementSpawn.settlement_id}`,
          );
        }
        if (
          decodedSettlementSpawn.output_index !==
          BigInt(finalLayout.settlementOutputIndex)
        ) {
          redeemerInvariantMismatches.push(
            `settlement.output_index mismatch: expected=${finalLayout.settlementOutputIndex},got=${decodedSettlementSpawn.output_index.toString()}`,
          );
        }
        if (
          decodedSettlementSpawn.state_queue_merge_redeemer_index !==
          BigInt(finalLayout.stateQueueRedeemerIndex)
        ) {
          redeemerInvariantMismatches.push(
            `settlement.state_queue_merge_redeemer_index mismatch: expected=${finalLayout.stateQueueRedeemerIndex},got=${decodedSettlementSpawn.state_queue_merge_redeemer_index.toString()}`,
          );
        }
        if (
          decodedSettlementSpawn.hub_ref_input_index !==
          BigInt(finalLayout.hubOracleRefInputIndex)
        ) {
          redeemerInvariantMismatches.push(
            `settlement.hub_ref_input_index mismatch: expected=${finalLayout.hubOracleRefInputIndex},got=${decodedSettlementSpawn.hub_ref_input_index.toString()}`,
          );
        }
      }
      if (
        decodedStateQueueMerge !== undefined &&
        decodedSettlementSpawn !== undefined &&
        decodedStateQueueMerge.header_node_key !==
          decodedSettlementSpawn.settlement_id
      ) {
        redeemerInvariantMismatches.push(
          `cross-check mismatch: state_queue.header_node_key=${decodedStateQueueMerge.header_node_key},settlement.settlement_id=${decodedSettlementSpawn.settlement_id}`,
        );
      }
      if (redeemerInvariantMismatches.length > 0) {
        return yield* failMergeWithCode(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "Failed merge redeemer invariant checks",
          {
            mismatches: redeemerInvariantMismatches,
            layout: finalLayout,
          },
        );
      }

      const remoteEvalResult = yield* Effect.either(
        Effect.tryPromise({
          try: () =>
            makeMergeTxWithScripts(
              finalEncodedRedeemers.encodedStateQueueMergeRedeemer,
              finalEncodedRedeemers.encodedSettlementSpawnRedeemer,
            ).complete({
              localUPLCEval: true,
            }),
          catch: (cause) =>
            new SDK.StateQueueError({
              message:
                "Failed to finalize the transaction for merging oldest block into confirmed state",
              cause,
            }),
        }),
      );
      if (remoteEvalResult._tag === "Left") {
        const localEvalDiagnostic = yield* Effect.either(
          Effect.tryPromise({
            try: () =>
              makeMergeTxWithScripts(
                finalEncodedRedeemers.encodedStateQueueMergeRedeemer,
                finalEncodedRedeemers.encodedSettlementSpawnRedeemer,
              ).complete({
                localUPLCEval: true,
              }),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Local UPLC evaluation diagnostic failed for merge transaction",
                cause,
              }),
          }),
        );
        const localDiagnosticMessage =
          localEvalDiagnostic._tag === "Left"
            ? formatUnknownError(localEvalDiagnostic.left)
            : "local UPLC eval unexpectedly succeeded";
        return yield* failMergeWithCode(
          "E_MERGE_UPLC_EVAL_FAILED",
          "Failed to finalize the transaction for merging oldest block into confirmed state",
          {
            remote: formatUnknownError(remoteEvalResult.left),
            local: localDiagnosticMessage,
            layout: finalLayout,
          },
        );
      }
      const txBuilder: TxSignBuilder = remoteEvalResult.right;

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
                )
                  // .map((u) => utxoToOutRefAndCBORArray(u)),
                  .pipe(Effect.withSpan(`confirmed-ledger-insert-${i}`));
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
