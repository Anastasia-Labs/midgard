/**
 * State-queue merge transaction for advancing committed blocks into confirmed
 * state.
 *
 * 1. Fetches the confirmed state and the block it points to (the oldest block
 *    in the queue).
 * 2. Verifies queue length, maturity, and header-hash invariants.
 * 3. Builds the merge tx that:
 *      - spends the confirmed-state node + the first queued block,
 *      - burns the state-queue node token,
 *      - mints a settlement token at the headerNodeKey,
 *      - pays out the updated confirmed-state node and a settlement output,
 *      - reads the hub-oracle UTxO as a reference input.
 * 4. Submits the tx, then locally clears spent UTxOs / inserts produced
 *    UTxOs / clears the merged block from BlocksTxsDB.
 */

import { BlocksTxsDB, ConfirmedLedgerDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database, Globals, NodeConfig } from "@/services/index.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
  fetchFirstBlockTxs,
  handleSignSubmit,
} from "@/transactions/utils.js";
import {
  collectIndexedOutputs,
  collectSortedInputOutRefs,
  compareOutRefs,
  findOutRefIndex,
  outRefLabel,
  type OutRefLike,
} from "@/tx-context.js";
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
import { Entry as LedgerEntry } from "@/database/utils/ledger.js";
import { breakDownTx } from "@/utils.js";
import { alignedUnixTimeStrictlyAfter } from "@/workers/utils/commit-end-time.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Address,
  CML,
  Data,
  LucidEvolution,
  TxSignBuilder,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect, Metric, Ref } from "effect";

const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merged blocks",
  bigint: true,
  incremental: true,
});

// 30 minutes.
const MAX_LIFE_OF_LOCAL_SYNC: number = 1_800_000;

const MIN_SETTLEMENT_OUTPUT_LOVELACE = 5_000_000n;
// Matches the on-chain `state_queue` maturity rule before a queued block
// can be merged into confirmed state.
const STATE_QUEUE_MATURITY_DURATION_MS = 30;
// Buffer past the maturity boundary to absorb provider slot/time drift and
// avoid `invalid-before` rejections right at the boundary.
const MERGE_MATURITY_DELAY_BUFFER_MS = 20_000;

type MergeOptions = {
  readonly bypassQueueLengthGuard?: boolean;
};

type MergeRedeemerLayout = {
  readonly headerNodeInputIndex: number;
  readonly confirmedStateInputIndex: number;
  readonly confirmedStateOutputIndex: number;
  readonly settlementOutputIndex: number;
  readonly stateQueueRedeemerIndex: number;
  readonly settlementRedeemerIndex: number;
  readonly hubOracleRefInputIndex: number;
};

const formatUnknownError = (error: unknown): string => {
  if (error instanceof Error) {
    return `${error.name}: ${error.message}`;
  }
  return `${error}`;
};

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
      return Math.max(0, stateQueueUtxos.length - 1);
    } else {
      return yield* Ref.get(globals.BLOCKS_IN_QUEUE);
    }
  });

const comparePolicyIds = (a: string, b: string): number =>
  Buffer.from(a, "hex").compare(Buffer.from(b, "hex"));

type InitialMergeRedeemerSeedIndexes = {
  readonly stateQueueMintPointerIndex: number;
  readonly settlementMintPointerIndex: number;
  readonly stateQueueRedeemerIndex: number;
  readonly settlementRedeemerIndex: number;
};

/**
 * Seed redeemer indexes assuming the canonical purpose order (spends first,
 * mints sorted by policy id). The actual final indexes are re-derived from
 * the draft tx; the seeds are only used to encode the *first* draft.
 */
const deriveInitialMergeRedeemerSeedIndexes = ({
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

/**
 * Picks the largest pure-ADA wallet UTxO as the fee input. We pre-select so
 * the same UTxO appears in both draft and final tx, ensuring the indexes we
 * derive from the draft remain valid.
 */
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

/**
 * Build and submit the merge transaction.
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
  | TxConfirmError
  | TxSubmitError
  | TxSignError,
  Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const currentStateQueueLength = yield* getStateQueueLength(
      lucid,
      fetchConfig.stateQueueAddress,
    );
    const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (
      (!options?.bypassQueueLengthGuard &&
        currentStateQueueLength < nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING) ||
      RESET_IN_PROGRESS
    ) {
      return;
    }

    yield* Effect.logInfo("🔸 Merging of oldest block started.");
    yield* Effect.logInfo(
      "🔸 Fetching confirmed state and the first block in queue from L1...",
    );
    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      yield* SDK.fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig);
    if (!firstBlockUTxO) {
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );
      yield* Effect.logInfo("🔸 No blocks found in queue.");
      return;
    }

    yield* Effect.logInfo(
      `🔸 First block found: ${firstBlockUTxO.utxo.txHash}#${firstBlockUTxO.utxo.outputIndex}`,
    );
    const { txs: firstBlockTxs, headerHash } = yield* fetchFirstBlockTxs(
      firstBlockUTxO,
    ).pipe(Effect.withSpan("fetchFirstBlockTxs"));

    if (firstBlockUTxO.datum.key === "Empty") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to build merge transaction",
          cause: "first queued block cannot be a root node",
        }),
      );
    }
    const headerNodeKey = firstBlockUTxO.datum.key.Key.key;
    const blockHeader: SDK.Header = yield* SDK.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
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

    yield* Effect.logInfo("🔸 Building merge transaction...");
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
    const settlementUnit = toUnit(contracts.settlement.policyId, headerNodeKey);
    const settlementAssetsToMint = { [settlementUnit]: 1n };
    const settlementOutputAssets = {
      lovelace: MIN_SETTLEMENT_OUTPUT_LOVELACE,
      ...settlementAssetsToMint,
    };
    const settlementOutputDatum: SDK.SettlementDatum = {
      deposits_root: blockHeader.depositsRoot,
      withdrawals_root: blockHeader.withdrawalsRoot,
      transactions_root: blockHeader.transactionsRoot,
      resolution_claim: null,
    };

    const hubOracleAddress = contracts.hubOracle.spendingScriptAddress;
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
    const mergeReferenceInputs = [hubOracleRefInput];

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
      try: () => Data.to(settlementOutputDatum, SDK.SettlementDatum),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to encode settlement output datum for merge transaction",
          cause,
        }),
    });

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
            m_settlement_redeemer_index: BigInt(layout.settlementRedeemerIndex),
            merged_block_transactions_root: blockHeader.transactionsRoot,
            merged_block_deposits_root: blockHeader.depositsRoot,
            merged_block_withdrawals_root: blockHeader.withdrawalsRoot,
          },
        };
        const settlementSpawnRedeemer: SDK.SettlementMintRedeemer = {
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
          try: () =>
            Data.to(settlementSpawnRedeemer, SDK.SettlementMintRedeemer),
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
          { kind: "inline", value: encodedConfirmedNodeDatum },
          confirmedUTxO.utxo.assets,
        )
        .pay.ToContract(
          contracts.settlement.spendingScriptAddress,
          { kind: "inline", value: encodedSettlementDatum },
          settlementOutputAssets,
        )
        .mintAssets(stateQueueAssetsToBurn, encodedStateQueueMergeRedeemer)
        .mintAssets(settlementAssetsToMint, encodedSettlementSpawnRedeemer)
        .attach.Script(contracts.stateQueue.spendingScript)
        .attach.Script(contracts.stateQueue.mintingScript)
        .attach.Script(contracts.settlement.mintingScript);

    // --- Seed indexes (used only for the draft) ---
    const initialSeedIndexes = yield* Effect.try({
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
    } = initialSeedIndexes;
    const orderedInputsSeed = [
      confirmedUTxO.utxo,
      firstBlockUTxO.utxo,
      feeInput,
    ].sort(compareOutRefs);
    const seedConfirmedStateInputIndex = findInputIndex(
      orderedInputsSeed,
      confirmedUTxO.utxo,
    );
    const seedHeaderNodeInputIndex = findInputIndex(
      orderedInputsSeed,
      firstBlockUTxO.utxo,
    );
    const seedHubOracleRefInputIndex = findOutRefIndex(
      [...mergeReferenceInputs].sort(compareOutRefs),
      hubOracleRefInput,
    );
    if (
      seedConfirmedStateInputIndex < 0 ||
      seedHeaderNodeInputIndex < 0 ||
      seedHubOracleRefInputIndex === undefined
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to derive initial merge layout indexes",
          cause: `confirmed=${seedConfirmedStateInputIndex},header=${seedHeaderNodeInputIndex},hub_ref=${seedHubOracleRefInputIndex}`,
        }),
      );
    }
    const initialLayout: MergeRedeemerLayout = {
      headerNodeInputIndex: seedHeaderNodeInputIndex,
      confirmedStateInputIndex: seedConfirmedStateInputIndex,
      confirmedStateOutputIndex: 0,
      settlementOutputIndex: 1,
      stateQueueRedeemerIndex: initialStateQueueRedeemerIndex,
      settlementRedeemerIndex: initialSettlementRedeemerIndex,
      hubOracleRefInputIndex: seedHubOracleRefInputIndex,
    };

    // --- Draft tx → re-derive indexes from actual sorted body ---
    const draftLayoutResult = yield* Effect.either(
      Effect.tryPromise({
        try: async () => {
          const seedRedeemers = await Effect.runPromise(
            encodeMergeRedeemers(initialLayout),
          );
          const draftTxBuilder = await withStubbedProviderEvaluation(
            lucid,
            () =>
              makeMergeTx(
                seedRedeemers.encodedStateQueueMergeRedeemer,
                seedRedeemers.encodedSettlementSpawnRedeemer,
              ).complete({ localUPLCEval: true }),
          );
          const draftTx = draftTxBuilder.toTransaction();
          const txBody = draftTx.body();
          const inputList = collectSortedInputOutRefs(txBody.inputs());
          const referenceInputList = txBody.reference_inputs();
          if (referenceInputList === undefined) {
            throw new Error("Draft merge tx did not include reference inputs");
          }
          const sortedReferenceInputList =
            collectSortedInputOutRefs(referenceInputList);
          const headerInput = findOutRefIndex(inputList, firstBlockUTxO.utxo);
          const confirmedInput = findOutRefIndex(inputList, confirmedUTxO.utxo);
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
          if (confirmedOutput === undefined || settlementOutput === undefined) {
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
          // Read draft redeemer CBORs purely for diagnostics in case of a
          // later mismatch.
          findRedeemerDataCbor(
            draftTx,
            redeemerPointers[stateQueueRedeemerContextIndex],
          );
          findRedeemerDataCbor(
            draftTx,
            redeemerPointers[settlementRedeemerContextIndex],
          );
          return {
            headerNodeInputIndex: headerInput,
            confirmedStateInputIndex: confirmedInput,
            confirmedStateOutputIndex: confirmedOutput.index,
            settlementOutputIndex: settlementOutput.index,
            stateQueueRedeemerIndex,
            settlementRedeemerIndex,
            hubOracleRefInputIndex,
          } satisfies MergeRedeemerLayout;
        },
        catch: (cause) =>
          new SDK.StateQueueError({
            message: `Failed to derive merge redeemer layout from balanced draft tx: ${formatUnknownError(cause)}`,
            cause,
          }),
      }),
    );
    if (draftLayoutResult._tag === "Left") {
      return yield* Effect.fail(draftLayoutResult.left);
    }
    const finalLayout = draftLayoutResult.right;

    yield* Effect.logInfo(
      `🔸 Merge redeemer layout: header_input=${finalLayout.headerNodeInputIndex},confirmed_input=${finalLayout.confirmedStateInputIndex},confirmed_output=${finalLayout.confirmedStateOutputIndex},settlement_output=${finalLayout.settlementOutputIndex},hub_ref_input=${finalLayout.hubOracleRefInputIndex},state_queue_redeemer_index=${finalLayout.stateQueueRedeemerIndex},settlement_redeemer_index=${finalLayout.settlementRedeemerIndex} (fee_input=${outRefLabel(feeInput)},seed=h${seedHeaderNodeInputIndex}/c${seedConfirmedStateInputIndex})`,
    );

    const finalEncodedRedeemers = yield* encodeMergeRedeemers(finalLayout);

    // --- Final tx ---
    const txBuilder: TxSignBuilder = yield* Effect.tryPromise({
      try: () =>
        makeMergeTx(
          finalEncodedRedeemers.encodedStateQueueMergeRedeemer,
          finalEncodedRedeemers.encodedSettlementSpawnRedeemer,
        ).complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to finalize the transaction for merging oldest block into confirmed state",
          cause,
        }),
    });

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
    yield* Effect.logInfo("🔸 Merge transaction submitted, updating the db...");

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

    const bs = 100;
    yield* Effect.logInfo("🔸 Clear confirmed ledger db...");
    for (let i = 0; i < spentOutRefs.length; i += bs) {
      yield* ConfirmedLedgerDB.clearUTxOs(spentOutRefs.slice(i, i + bs)).pipe(
        Effect.withSpan(`confirmed-ledger-clearUTxOs-${i}`),
      );
    }
    yield* Effect.logInfo("🔸 Insert produced UTxOs...");
    for (let i = 0; i < producedUTxOs.length; i += bs) {
      yield* ConfirmedLedgerDB.insertMultiple(
        producedUTxOs.slice(i, i + bs),
      ).pipe(Effect.withSpan(`confirmed-ledger-insert-${i}`));
    }
    yield* Effect.logInfo("🔸 Clear block from BlocksTxsDB...");
    yield* BlocksTxsDB.clearBlock(headerHash).pipe(
      Effect.withSpan("clear-block-from-BlocksTxsDB"),
    );
    yield* Effect.logInfo("🔸 ☑️  Merge transaction completed.");

    yield* Metric.increment(mergeBlockCounter).pipe(
      Effect.withSpan("increment-merge-block-counter"),
    );
    yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => Math.max(0, n - 1));
  });
