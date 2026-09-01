import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { OperatorWalletView } from "@/operator-wallet-view.js";
import {
  type ContractDeploymentIdentityValue,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import {
  handleSignSubmitNoConfirmation,
  type NoInlineSubmitRecoveryOptions,
  type TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  type CommitTimingBudget,
  commitTimingBudget,
  formatCommitTimingBudget,
  makeSubmitSlotAnchoredClock,
  resolveAlignedCommitEndTime,
  resolveCommitValidityInterval,
} from "@/workers/utils/commit-end-time.js";
import {
  type CommitTimingDueWork,
  fetchRealStateQueueWitnessContext,
  type RealStateQueueWitnessContext,
} from "@/workers/utils/scheduler-refresh.js";

import {
  getLatestBlockDatumEndTime,
  hashBlockHeaderV1Local,
  updateLatestBlocksDatumAndGetTheNewHeaderV1Local,
} from "./state-queue.js";

const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;
const COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS = 4;

export type BuiltCommitTx = {
  readonly newHeaderHash: string;
  readonly newHeader: SDK.HeaderV1;
  readonly newHeaderCbor: Buffer;
  readonly blockEndTimeMs: number;
  readonly txValidFromMs: number;
  readonly txValidToMs: number;
  readonly signAndSubmitProgram: Effect.Effect<
    string,
    TxSignError | TxSubmitError
  >;
  readonly txSize: number;
};

export type CommitBuildResult = BuiltCommitTx | CommitTimingDueWork;

const isCommitTimingDueWork = (
  value: RealStateQueueWitnessContext | CommitTimingDueWork,
): value is CommitTimingDueWork => "type" in value;

export const buildUnsignedCommitTx = (
  contracts: SDK.MidgardValidators,
  latestBlock: SDK.StateQueueUTxO,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  withdrawalsRoot: string,
  transitionCommitments: SDK.HeaderTransitionCommitmentsV1,
  _consensusProfile: ContractDeploymentIdentityValue["consensusProfile"],
  endDate: Date,
  initialOperatorWalletView?: OperatorWalletView,
  maximumEndTimeMs?: number,
): Effect.Effect<
  CommitBuildResult,
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HeaderTransitionCommitmentsError
  | SDK.HashingError
  | SDK.LucidError
  | TxSignError
  | TxSubmitError,
  Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;
    const submitSlotSnapshot = lucid.submitSlotSnapshot;
    const initialSubmitSlotSnapshot = yield* submitSlotSnapshot().pipe(
      Effect.mapError(
        (cause) =>
          new SDK.LucidError({
            message:
              "Failed to acquire the submit-slot snapshot for commit timing",
            cause,
          }),
      ),
    );
    const anchoredNowMs = makeSubmitSlotAnchoredClock(
      initialSubmitSlotSnapshot.observedAtMs,
    );
    const latestEndTime = Number(
      (yield* getLatestBlockDatumEndTime(latestBlock.datum)).getTime(),
    );
    const candidateEndTimeMs = endDate.getTime();
    if (!Number.isSafeInteger(candidateEndTimeMs)) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Refusing to build a block with an invalid semantic end-time",
          cause: `candidate_end_time_ms=${String(candidateEndTimeMs)}`,
        }),
      );
    }

    // The worker's Lucid service starts without a selected wallet. Select the
    // operator wallet before any scheduler refresh or witness lookup that
    // depends on wallet address or spendable operator inputs.
    yield* lucid.switchToOperatorsMainWallet;
    let commitValidityResolutionNow = anchoredNowMs();
    const resolveCommitValidityWindow = () =>
      resolveAlignedCommitEndTime({
        lucid: lucid.api,
        latestEndTime,
        candidateEndTime: candidateEndTimeMs,
        nowMs: commitValidityResolutionNow,
        minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      });
    const enforceCommitValidityCap = (
      commitValidityWindow: ReturnType<typeof resolveCommitValidityWindow>,
      stage: string,
    ) =>
      maximumEndTimeMs !== undefined &&
      commitValidityWindow.resolvedEndTime - 1 > maximumEndTimeMs
        ? Effect.fail(
            new SDK.StateQueueError({
              message:
                "Resolved commit transaction validity exceeds the selected scheduler window cap",
              cause: `stage=${stage},resolved_valid_to_ms=${commitValidityWindow.resolvedEndTime.toString()},resolved_header_end_time_ms=${(commitValidityWindow.resolvedEndTime - 1).toString()},maximum_end_time_ms=${maximumEndTimeMs.toString()},candidate_end_time_ms=${candidateEndTimeMs.toString()},aligned_candidate_valid_to_ms=${commitValidityWindow.alignedCandidateEndTime.toString()},minimum_monotonic_valid_to_ms=${commitValidityWindow.minimumMonotonicEndTime.toString()},minimum_current_time_valid_to_ms=${commitValidityWindow.minimumCurrentTimeEndTime.toString()}`,
            }),
          )
        : Effect.void;
    let commitValidityWindow = resolveCommitValidityWindow();
    yield* enforceCommitValidityCap(commitValidityWindow, "initial_resolution");
    let witnessContext: RealStateQueueWitnessContext | undefined;
    let lastFailedBudget: CommitTimingBudget | undefined;
    for (
      let stabilizationAttempts = 1;
      stabilizationAttempts <= COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS;
      stabilizationAttempts += 1
    ) {
      const preWitnessBudget = commitTimingBudget({
        checkpoint: "pre_witness",
        resolvedEndTimeMs: commitValidityWindow.resolvedEndTime,
        nowMs: anchoredNowMs(),
      });
      if (!preWitnessBudget.satisfied) {
        lastFailedBudget = preWitnessBudget;
        commitValidityResolutionNow = anchoredNowMs();
        const refreshedCommitValidityWindow = resolveCommitValidityWindow();
        yield* Effect.logWarning(
          `Commit timing budget too low before witness assembly; rebuilding with refreshed transaction validity (${formatCommitTimingBudget(preWitnessBudget)},candidate_end=${candidateEndTimeMs},next_valid_to=${refreshedCommitValidityWindow.resolvedEndTime},attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
        );
        yield* enforceCommitValidityCap(
          refreshedCommitValidityWindow,
          "pre_witness_refresh",
        );
        commitValidityWindow = refreshedCommitValidityWindow;
        continue;
      }

      const witnessEndTime = commitValidityWindow.resolvedEndTime;
      yield* enforceCommitValidityCap(
        commitValidityWindow,
        "before_witness_lookup",
      );
      const witnessResult = yield* fetchRealStateQueueWitnessContext(
        lucid.api,
        contracts,
        witnessEndTime,
        witnessContext?.operatorWalletView ?? initialOperatorWalletView,
        lucid.referenceScriptsAddress,
        submitSlotSnapshot,
        false,
      );
      if (isCommitTimingDueWork(witnessResult)) {
        return witnessResult;
      }
      witnessContext = witnessResult;
      const preBuildBudget = commitTimingBudget({
        checkpoint: "pre_build",
        resolvedEndTimeMs: witnessEndTime,
        nowMs: anchoredNowMs(),
      });
      if (!preBuildBudget.satisfied) {
        lastFailedBudget = preBuildBudget;
        commitValidityResolutionNow = anchoredNowMs();
        const refreshedCommitValidityWindow = resolveCommitValidityWindow();
        yield* Effect.logWarning(
          `Commit timing budget too low after witness assembly; rebuilding with refreshed transaction validity (${formatCommitTimingBudget(preBuildBudget)},candidate_end=${candidateEndTimeMs},previous_valid_to=${commitValidityWindow.resolvedEndTime},next_valid_to=${refreshedCommitValidityWindow.resolvedEndTime},attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
        );
        yield* enforceCommitValidityCap(
          refreshedCommitValidityWindow,
          "pre_build_refresh",
        );
        commitValidityWindow = refreshedCommitValidityWindow;
        continue;
      }

      const {
        alignedCandidateEndTime,
        minimumMonotonicEndTime,
        resolvedEndTime: txValidToMs,
      } = commitValidityWindow;
      const {
        validFromMs: txValidFromMs,
        inclusiveUpperBoundMs: blockEndTimeMs,
      } = resolveCommitValidityInterval({
        lucid: lucid.api,
        submitSlotSnapshot: initialSubmitSlotSnapshot,
        validToMs: txValidToMs,
      });
      if (txValidToMs !== alignedCandidateEndTime) {
        yield* Effect.logWarning(
          `Adjusted commit transaction validity and bound the committed header to its inclusive upper bound (candidate_end=${candidateEndTimeMs},header_end=${blockEndTimeMs},candidate_valid_to=${alignedCandidateEndTime},minimum=${minimumMonotonicEndTime},selected_valid_from=${txValidFromMs},selected_valid_to=${txValidToMs},latest_block_end=${latestEndTime}).`,
        );
      }
      yield* enforceCommitValidityCap(
        commitValidityWindow,
        "before_header_build",
      );
      yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
      const { nodeDatum: updatedNodeDatum, header: newHeader } =
        yield* updateLatestBlocksDatumAndGetTheNewHeaderV1Local(
          lucid.api,
          latestBlock.datum,
          utxosRoot,
          txsRoot,
          depositsRoot,
          withdrawalsRoot,
          transitionCommitments,
          BigInt(blockEndTimeMs),
          {
            blockSlot: BigInt(lucid.api.unixTimeToSlot(blockEndTimeMs)),
            expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
            minFeeA: nodeConfig.MIN_FEE_A,
            minFeeB: nodeConfig.MIN_FEE_B,
          },
        );

      const newHeaderHash = yield* hashBlockHeaderV1Local(newHeader);
      yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);
      yield* Effect.logInfo(
        "🔹 Building commitment with real state_queue witness context.",
      );
      yield* Effect.logInfo("🔹 Building block commitment transaction...");

      const { tx: txBuilder } =
        yield* SDK.buildProductionCommitBlockHeaderTxProgram({
          lucid: lucid.api,
          contracts,
          latestBlock,
          updatedNodeDatum,
          newHeader,
          validFrom: txValidFromMs,
          validTo: txValidToMs,
          witness: witnessContext,
          headerNodeLovelace: STATE_QUEUE_HEADER_NODE_LOVELACE,
        });

      const txSize = txBuilder.toCBOR().length / 2;
      yield* Effect.logInfo(
        `🔹 Transaction built successfully. Size: ${txSize}`,
      );

      const preSubmitBudget = commitTimingBudget({
        checkpoint: "pre_submit",
        resolvedEndTimeMs: txValidToMs,
        nowMs: anchoredNowMs(),
      });
      if (!preSubmitBudget.satisfied) {
        lastFailedBudget = preSubmitBudget;
        commitValidityResolutionNow = anchoredNowMs();
        const refreshedCommitValidityWindow = resolveCommitValidityWindow();
        yield* Effect.logWarning(
          `Commit timing budget too low before submission; rebuilding transaction validity before pending journal preparation (${formatCommitTimingBudget(preSubmitBudget)},candidate_end=${candidateEndTimeMs},previous_valid_to=${commitValidityWindow.resolvedEndTime},next_valid_to=${refreshedCommitValidityWindow.resolvedEndTime},attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
        );
        yield* enforceCommitValidityCap(
          refreshedCommitValidityWindow,
          "pre_submit_refresh",
        );
        commitValidityWindow = refreshedCommitValidityWindow;
        continue;
      }

      yield* enforceCommitValidityCap(
        commitValidityWindow,
        "before_pending_journal_preparation",
      );

      const submitRecoveryOptions: NoInlineSubmitRecoveryOptions = {
        label: "commit-block",
        slotSnapshot: submitSlotSnapshot,
        requireSlotForBoundedTx: true,
        maxPreSubmitWaitMs: preSubmitBudget.remainingBudgetMs,
        inlineWaitPolicy: "defer_positive_wait",
        noInlineSubmitDefer: {
          key: `commit-block:${newHeaderHash}`,
          dependencyKey: `commit-block:${newHeaderHash}`,
          invalidationKey: `commit-block:${newHeaderHash}`,
        },
      };
      const signAndSubmitProgram = handleSignSubmitNoConfirmation(
        lucid.api,
        txBuilder,
        submitRecoveryOptions,
      )
        .pipe(
          Effect.flatMap((result) =>
            result.status === "submitted"
              ? Effect.succeed(result.txHash)
              : Effect.fail(
                  new TxSubmitError({
                    message:
                      "Commit block submit deferred in no-inline mode before submission",
                    txHash: newHeaderHash,
                    cause: result.defer,
                  }),
                ),
          ),
        )
        .pipe(Effect.withSpan("handleSignSubmit-commit-block"));

      return {
        newHeaderHash,
        newHeader,
        newHeaderCbor: Buffer.from(
          LucidData.to(newHeader as never, SDK.HeaderV1 as never),
          "hex",
        ),
        blockEndTimeMs,
        txValidFromMs,
        txValidToMs,
        signAndSubmitProgram,
        txSize,
      };
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Failed to stabilize the commit timing budget before building the block commitment transaction",
        cause: `attempts=${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS},candidate_end_time_ms=${candidateEndTimeMs.toString()},last_failed_checkpoint=${lastFailedBudget?.checkpoint ?? "none"},last_selected_valid_to_ms=${commitValidityWindow.resolvedEndTime},last_budget=${lastFailedBudget === undefined ? "none" : formatCommitTimingBudget(lastFailedBudget)},witness_context=${witnessContext === undefined ? "missing" : "present"},clock_anchor_observed_at_ms=${initialSubmitSlotSnapshot.observedAtMs},clock_now_ms=${anchoredNowMs()}`,
      }),
    );
  });
