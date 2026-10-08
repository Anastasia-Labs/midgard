import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  LucidEvolution,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Duration, Effect, Metric, Ref } from "effect";

import { DatabaseError } from "../../database/utils/common.js";
import { emitQueueStateMetrics } from "../../fibers/queue-metrics.js";
import { Database, Globals, NodeConfig } from "../../services/index.js";
import {
  type IntentJournal,
  journaledIntent,
  openPlan,
} from "../../services/intent-journal.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../reference-scripts.js";
import {
  fetchFirstBlockTxs,
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "../utils.js";
import { readSelectedWalletView } from "../utils.wallet-view.js";
import {
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeSubmitValidityEvidence,
} from "./merge-readiness.js";
import {
  captureMergeLocalLedgerGate,
  mergeSubmitRecoveryOptions,
  registerMergeNoInlineSubmitDueWork,
} from "./merge-to-confirmed-state.capture-merge-local-ledger-gate.js";
import {
  failMergeWithCode,
  fetchCanonicalMergeCandidateReadiness,
  getStateQueueLength,
  mergeSemanticSkipResult,
  type MergeTxResult,
  preflightDecodeBlockTxs,
  slotFromUnixTime,
} from "./merge-to-confirmed-state.fetch-canonical-merge-candidate-readiness.js";
import {
  finalizeConfirmedMergeProgram,
  landedMergeOf,
  mergeBlockCounter,
} from "./merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import {
  diagnoseMissingBlockTxs,
  finalizeMergesLandedThrough,
  mergeBlockTxDecodeFailureCounter,
  mergeDurationTimer,
  mergeFailureCounter,
  mergeLocalFinalizationFailureCounter,
  type MergeOptions,
} from "./merge-to-confirmed-state.landed-unfinalized-merges.js";

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param contracts - Midgard script bundle used for state_queue and settlement.
 * @returns An Effect that resolves to either a submitted merge transaction or a
 *          structured skip result explaining why no merge was attempted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
  contracts: SDK.MidgardValidators,
  options?: MergeOptions,
): Effect.Effect<
  MergeTxResult,
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
  Database | Globals | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    const mergeStartedAt = Date.now();
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    // S5: the plan opens before the merge's first L1 read.
    const plan = yield* openPlan;
    const currentStateQueueLength = yield* getStateQueueLength(fetchConfig);
    const minQueueLengthForMerging =
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING;
    const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (resetInProgress) {
      return {
        status: "skipped_reset_in_progress",
        reason: "reset_in_progress=true",
        queueLength: currentStateQueueLength,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }
    if (currentStateQueueLength <= 0) {
      return {
        status: "no_queued_block",
        reason: `queue_length=${currentStateQueueLength.toString()}`,
        queueLength: currentStateQueueLength,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }
    // Avoid a merge tx if the queue is too short (performing a merge with such
    // conditions has a chance of wasting the work done for root computations).
    if (
      !options?.bypassQueueLengthGuard &&
      currentStateQueueLength < minQueueLengthForMerging
    ) {
      return {
        status: "skipped_below_min_queue_length",
        reason: `queue_length=${currentStateQueueLength.toString()},min_queue_length=${minQueueLengthForMerging.toString()}`,
        queueLength: currentStateQueueLength,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }

    yield* Effect.logInfo("🔸 Merging of oldest block started.");

    yield* Effect.logInfo(
      "🔸 Fetching confirmed state and the first block in queue from L1...",
    );
    const candidate = yield* fetchCanonicalMergeCandidateReadiness(
      lucid,
      fetchConfig,
      contracts,
    );
    if (candidate.status === "candidate") {
      const {
        confirmedUTxO,
        firstBlockUTxO,
        blockHeader,
        readiness: oldestBlockReadiness,
      } = candidate;
      yield* Effect.logInfo(
        `🔸 First block found: ${firstBlockUTxO.utxo.txHash}#${firstBlockUTxO.utxo.outputIndex}`,
      );
      if (oldestBlockReadiness.status !== "ready") {
        if (oldestBlockReadiness.status === "skipped_oldest_block_unattested") {
          yield* Effect.logInfo(
            `🔸 Skipping merge because oldest block is not DA-attested yet (${oldestBlockReadiness.reason}).`,
          );
        } else if (
          oldestBlockReadiness.status === "skipped_oldest_block_proven_fraud"
        ) {
          yield* Effect.logInfo(
            `🔸 Skipping merge because completed fraud requires state correction (${oldestBlockReadiness.reason}).`,
          );
        } else {
          yield* Effect.logInfo(
            `🔸 Oldest block is not mature enough for merge yet (${oldestBlockReadiness.reason}).`,
          );
        }
        return mergeSemanticSkipResult(oldestBlockReadiness);
      }
      const mergeMaturity = {
        validFromUnixTime: oldestBlockReadiness.validFromUnixTime,
        readyAfterUnixTime: oldestBlockReadiness.readyAfterUnixTime,
      };
      const recomputedHeaderHash = oldestBlockReadiness.headerHash;
      // The merge spends exactly `confirmedUTxO`, so every merge up to its
      // header is finalized first. A previous merge that landed after this
      // attempt's catch-up read L1 would otherwise have its ledger delta
      // folded into this block's finalization and never be finalized itself.
      yield* finalizeMergesLandedThrough(
        (yield* SDK.getConfirmedStateFromStateQueueDatum(confirmedUTxO.datum))
          .data,
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
      yield* Effect.logInfo(
        `🔸 Preflight decoded ${preflightDecodedBlockTxs.length} block tx(s) successfully (header=${headerHash.toString("hex")}).`,
      );
      yield* Effect.logInfo("🔸 Building merge transaction...");

      const localLedgerGate = yield* captureMergeLocalLedgerGate({
        lucid,
        nodeConfig,
        validFromUnixTime: mergeMaturity.validFromUnixTime,
        leaseToken: options?.leaseToken,
        headerHash: recomputedHeaderHash,
        candidateIdentity: oldestBlockReadiness.candidateIdentity,
        submitSlotSnapshot: options?.submitSlotSnapshot,
      });
      if (localLedgerGate.status === "retry_later") {
        return {
          status: "skipped_oldest_block_local_ledger_not_ready",
          headerHash: recomputedHeaderHash,
          reason: localLedgerGate.reason,
          readyAfterUnixTime: mergeMaturity.readyAfterUnixTime,
          nowUnixTime: oldestBlockReadiness.nowUnixTime,
        } satisfies MergeTxResult;
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
      const correctionLockRefInput = yield* SDK.fetchCorrectionLockUTxOProgram(
        lucid,
        {
          correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
          hubOraclePolicyId: contracts.hubOracle.policyId,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message:
                "Failed to fetch authenticated correction-lock witness for merge transaction",
              cause,
            }),
        ),
      );

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
                  name: "state-queue merge withdrawal",
                  script: contracts.stateQueue.yields.merge.withdrawalScript,
                },
                {
                  name: "settlement minting",
                  script: contracts.settlement.mintingScript,
                },
              ],
              contracts.referenceScriptAuth,
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
      const stateQueueMergeYieldRefInput = referenceScriptByName(
        resolvedReferenceScripts,
        "state-queue merge withdrawal",
      );

      const operatorWalletView = yield* readSelectedWalletView(lucid).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message: `Failed to read the merge wallet view: ${cause.message}`,
              cause,
            }),
        ),
      );
      const presetWalletInputs = yield* SDK.requireOperatorWalletInputs(
        operatorWalletView.utxos,
        "state_queue merge tx",
      );
      yield* Effect.logInfo(
        `🔸 Using ${presetWalletInputs.length.toString()} operator wallet view input(s) for merge tx (source=${operatorWalletView.source}, held=${operatorWalletView.held.size.toString()}).`,
      );

      const builtMerge = yield* SDK.buildMergeToConfirmedStateTxProgram({
        lucid,
        fetchConfig,
        contracts,
        confirmedUTxO,
        firstBlockUTxO,
        validFrom: mergeMaturity.validFromUnixTime,
        presetWalletInputs,
        hubOracleRefInput,
        correctionLockRefInput,
        stateQueueMergeYieldRefInput,
        referenceScripts,
      }).pipe(Effect.tapError(() => Metric.increment(mergeFailureCounter)));
      const txBuilder = builtMerge.tx;

      // Submit the transaction
      /**
       * Normalizes transaction-submission failures during confirmed-state merging.
       */
      const onSubmitFailure = (err: TxSubmitError) =>
        Effect.gen(function* () {
          yield* Effect.logError(`Submit tx error: ${err.message}`);
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
            `Confirm tx error: ${err.message}; refusing local merge finalization until L1 confirmation is verified`,
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
      const txHash = txBuilder.toHash();
      const submitValidFromSlot = yield* slotFromUnixTime(
        lucid,
        mergeMaturity.validFromUnixTime,
      );
      const submitDueWorkEvidence = mergeSubmitValidityEvidence({
        headerHash: recomputedHeaderHash,
        validFromSlot: submitValidFromSlot,
        candidateIdentity: oldestBlockReadiness.candidateIdentity,
      });
      const submitRecoveryOptions = mergeSubmitRecoveryOptions(
        nodeConfig,
        submitDueWorkEvidence,
        options?.submitSlotSnapshot,
        options?.confirmationDeadlineMs,
      );
      if (options?.assertSubmitAuthority !== undefined) {
        yield* options.assertSubmitAuthority();
      }
      const finalizeLocalMergeLogged = finalizeConfirmedMergeProgram(
        landedMergeOf(headerHash, blockHeader),
      ).pipe(
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
      // Only signing, submission and the L1 confirmation wait stay
      // interruptible; the wait ends by `confirmationDeadlineMs` so the
      // finalization still fits in the caller's hold. Once the merge is
      // confirmed, its local finalization starts at once and runs to
      // completion (or records a failed job) even if the caller is interrupted
      // meanwhile. A merge that lands after the wait stopped, or whose
      // finalization failed, is finalized by the next attempt's
      // finalizeLandedMergesProgram. The exit goes to
      // `onConfirmedFinalization`, so an interrupting caller can still report
      // what actually happened.
      const submitOutcome = yield* Effect.uninterruptibleMask((restore) =>
        restore(
          handleSignSubmit(
            lucid,
            txBuilder,
            journaledIntent(
              "merge",
              `merge:head=${headerHash.toString("hex")}`,
              plan,
              headerHash,
            ),
            submitRecoveryOptions,
          ).pipe(
            Effect.as({ status: "submitted" } as const),
            Effect.catchTag("NoInlineSubmitDefer", (defer) =>
              Effect.succeed({ status: "deferred", defer } as const),
            ),
            Effect.catchTag("TxSubmitError", onSubmitFailure),
            Effect.catchTag("TxConfirmError", onConfirmFailure),
            Effect.withSpan("handleSignSubmit-merge-tx"),
          ),
        ).pipe(
          Effect.map(
            (rawSubmitOutcome) =>
              rawSubmitOutcome ?? ({ status: "submitted" } as const),
          ),
          Effect.tap((outcome) =>
            outcome.status === "submitted"
              ? Effect.logInfo(
                  "🔸 Merge transaction submitted, updating the db...",
                ).pipe(
                  Effect.zipRight(finalizeLocalMergeLogged),
                  Effect.onExit((exit) =>
                    options?.onConfirmedFinalization === undefined
                      ? Effect.void
                      : options.onConfirmedFinalization({
                          headerHash: headerHash.toString("hex"),
                          txHash,
                          exit,
                        }),
                  ),
                )
              : Effect.void,
          ),
        ),
      );
      if (submitOutcome.status === "deferred") {
        const dueWork = yield* registerMergeNoInlineSubmitDueWork(
          submitOutcome.defer,
          nodeConfig,
          options?.submitSlotSnapshot,
        );
        yield* Effect.logInfo(
          `🔸 Skipping merge after no-inline submit defer (kind=${dueWork.kind},key=${dueWork.key},callerLabel=${dueWork.callerLabel},deferKind=${submitOutcome.defer.kind},current_slot=${dueWork.observedSlot.toString()},due_slot=${dueWork.dueSlot.toString()},wait_ms=${dueWork.waitMs.toString()},slot_source=${dueWork.slotSource},dependency_key=${dueWork.dependencyKey},invalidation_key=${dueWork.invalidationKey},leaseToken=${options?.leaseToken ?? "none"},headerHash=${recomputedHeaderHash}).`,
        );
        return {
          status: "skipped_oldest_block_local_ledger_not_ready",
          headerHash: recomputedHeaderHash,
          reason: `no_inline_submit_defer,kind=${submitOutcome.defer.kind},current_slot=${submitOutcome.defer.currentSlot.toString()},target_slot=${submitOutcome.defer.targetSlot.toString()},due_slot=${submitOutcome.defer.dueSlot.toString()},wait_ms=${submitOutcome.defer.waitMs.toString()},slot_source=${submitOutcome.defer.slotSource}`,
          readyAfterUnixTime: mergeMaturity.readyAfterUnixTime,
          nowUnixTime: oldestBlockReadiness.nowUnixTime,
        } satisfies MergeTxResult;
      }
      yield* Effect.logInfo("🔸 ☑️  Merge transaction completed.");

      yield* Metric.increment(mergeBlockCounter).pipe(
        Effect.withSpan("increment-merge-block-counter"),
      );
      yield* mergeDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - mergeStartedAt)),
      );

      yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => Math.max(0, n - 1));
      yield* emitQueueStateMetrics;
      return {
        status: "merged",
        headerHash: headerHash.toString("hex"),
        txHash,
      } satisfies MergeTxResult;
    } else {
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
      yield* emitQueueStateMetrics;
      yield* Effect.logInfo("🔸 No blocks found in queue.");
      yield* mergeDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - mergeStartedAt)),
      );
      return {
        status: "no_queued_block",
        reason: "no_first_block_utxo",
        queueLength: 0,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }
  });
