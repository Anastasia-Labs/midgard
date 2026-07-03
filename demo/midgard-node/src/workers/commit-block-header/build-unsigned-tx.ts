import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { OperatorWalletView } from "@/operator-wallet-view.js";
import { Lucid } from "@/services/index.js";
import {
  handleSignSubmitNoConfirmation,
  type TxSignError,
  type TxSubmitError,
} from "@/transactions/utils.js";
import {
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  commitTimingBudget,
  formatCommitTimingBudget,
  resolveAlignedCommitEndTime,
} from "@/workers/utils/commit-end-time.js";
import {
  type CommitTimingDueWork,
  fetchRealStateQueueWitnessContext,
  type RealStateQueueWitnessContext,
} from "@/workers/utils/scheduler-refresh.js";

import {
  getLatestBlockDatumEndTime,
  hashBlockHeaderLocal,
  updateLatestBlocksDatumAndGetTheNewHeaderLocal,
} from "./state-queue.js";

const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;
const COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS = 4;

export type BuiltCommitTx = {
  readonly newHeaderHash: string;
  readonly newHeader: SDK.Header;
  readonly newHeaderCbor: Buffer;
  readonly blockEndTimeMs: number;
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
  transitionCommitments: SDK.HeaderTransitionCommitments,
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
  Lucid
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const submitSlotSnapshot = lucid.submitSlotSnapshot;
    const latestEndTime = Number(
      (yield* getLatestBlockDatumEndTime(latestBlock.datum)).getTime(),
    );

    // The worker's Lucid service starts without a selected wallet. Select the
    // operator wallet before any scheduler refresh or witness lookup that
    // depends on wallet address or spendable operator inputs.
    yield* lucid.switchToOperatorsMainWallet;
    let commitWindowResolutionNow = Date.now();
    const resolveCommitWindow = () =>
      resolveAlignedCommitEndTime({
        lucid: lucid.api,
        latestEndTime,
        candidateEndTime: endDate.getTime(),
        nowMs: commitWindowResolutionNow,
        minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      });
    const enforceCommitWindowCap = (
      commitWindow: ReturnType<typeof resolveCommitWindow>,
      stage: string,
    ) =>
      maximumEndTimeMs !== undefined &&
      commitWindow.resolvedEndTime > maximumEndTimeMs
        ? Effect.fail(
            new SDK.StateQueueError({
              message:
                "Resolved commit end-time exceeds the selected scheduler window cap",
              cause: `stage=${stage},resolved_end_time_ms=${commitWindow.resolvedEndTime.toString()},maximum_end_time_ms=${maximumEndTimeMs.toString()},aligned_candidate_end_time_ms=${commitWindow.alignedCandidateEndTime.toString()},minimum_monotonic_end_time_ms=${commitWindow.minimumMonotonicEndTime.toString()},minimum_current_time_end_time_ms=${commitWindow.minimumCurrentTimeEndTime.toString()}`,
            }),
          )
        : Effect.void;
    let commitWindow = resolveCommitWindow();
    yield* enforceCommitWindowCap(commitWindow, "initial_resolution");
    let witnessContext: RealStateQueueWitnessContext | undefined;
    for (
      let stabilizationAttempts = 1;
      stabilizationAttempts <= COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS;
      stabilizationAttempts += 1
    ) {
      const preWitnessBudget = commitTimingBudget({
        checkpoint: "pre_witness",
        resolvedEndTimeMs: commitWindow.resolvedEndTime,
      });
      if (!preWitnessBudget.satisfied) {
        commitWindowResolutionNow = Date.now();
        const refreshedCommitWindow = resolveCommitWindow();
        yield* Effect.logWarning(
          `Commit timing budget too low before witness assembly; rebuilding with refreshed window (${formatCommitTimingBudget(preWitnessBudget)},next=${refreshedCommitWindow.resolvedEndTime},attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
        );
        yield* enforceCommitWindowCap(
          refreshedCommitWindow,
          "pre_witness_refresh",
        );
        commitWindow = refreshedCommitWindow;
        continue;
      }

      const witnessEndTime = commitWindow.resolvedEndTime;
      yield* enforceCommitWindowCap(commitWindow, "before_witness_lookup");
      const witnessResult = yield* fetchRealStateQueueWitnessContext(
        lucid.api,
        contracts,
        witnessEndTime,
        witnessContext?.operatorWalletView ?? initialOperatorWalletView,
        lucid.referenceScriptsAddress,
        submitSlotSnapshot,
      );
      if (isCommitTimingDueWork(witnessResult)) {
        return witnessResult;
      }
      witnessContext = witnessResult;
      const preBuildBudget = commitTimingBudget({
        checkpoint: "pre_build",
        resolvedEndTimeMs: witnessEndTime,
      });
      if (!preBuildBudget.satisfied) {
        commitWindowResolutionNow = Date.now();
        const refreshedCommitWindow = resolveCommitWindow();
        yield* Effect.logWarning(
          `Commit timing budget too low after witness assembly; rebuilding with refreshed window (${formatCommitTimingBudget(preBuildBudget)},previous=${commitWindow.resolvedEndTime},next=${refreshedCommitWindow.resolvedEndTime},attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
        );
        yield* enforceCommitWindowCap(
          refreshedCommitWindow,
          "pre_build_refresh",
        );
        commitWindow = refreshedCommitWindow;
        continue;
      }

      const {
        alignedCandidateEndTime,
        minimumMonotonicEndTime,
        resolvedEndTime: alignedEndTime,
      } = commitWindow;
      if (alignedEndTime !== alignedCandidateEndTime) {
        yield* Effect.logWarning(
          `Adjusted commit end-time to maintain monotonic header timing (candidate=${alignedCandidateEndTime}, minimum=${minimumMonotonicEndTime}, selected=${alignedEndTime}, latestEnd=${latestEndTime}).`,
        );
      }
      yield* enforceCommitWindowCap(commitWindow, "before_header_build");
      yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
      const { nodeDatum: updatedNodeDatum, header: newHeader } =
        yield* updateLatestBlocksDatumAndGetTheNewHeaderLocal(
          lucid.api,
          latestBlock.datum,
          utxosRoot,
          txsRoot,
          depositsRoot,
          withdrawalsRoot,
          transitionCommitments,
          BigInt(alignedEndTime),
        );

      const newHeaderHash = yield* hashBlockHeaderLocal(newHeader);
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
          validTo: alignedEndTime,
          witness: witnessContext,
          headerNodeLovelace: STATE_QUEUE_HEADER_NODE_LOVELACE,
        });

      const txSize = txBuilder.toCBOR().length / 2;
      yield* Effect.logInfo(
        `🔹 Transaction built successfully. Size: ${txSize}`,
      );

      const preSubmitBudget = commitTimingBudget({
        checkpoint: "pre_submit",
        resolvedEndTimeMs: alignedEndTime,
      });
      if (!preSubmitBudget.satisfied) {
        commitWindowResolutionNow = Date.now();
        const refreshedCommitWindow = resolveCommitWindow();
        yield* Effect.logWarning(
          `Commit timing budget too low before submission; rebuilding before pending journal preparation (${formatCommitTimingBudget(preSubmitBudget)},previous=${commitWindow.resolvedEndTime},next=${refreshedCommitWindow.resolvedEndTime},attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
        );
        yield* enforceCommitWindowCap(
          refreshedCommitWindow,
          "pre_submit_refresh",
        );
        commitWindow = refreshedCommitWindow;
        continue;
      }

      yield* enforceCommitWindowCap(
        commitWindow,
        "before_pending_journal_preparation",
      );

      const signAndSubmitProgram = handleSignSubmitNoConfirmation(
        lucid.api,
        txBuilder,
        {
          label: "commit-block",
          slotSnapshot: submitSlotSnapshot,
          requireSlotForBoundedTx: true,
          maxPreSubmitWaitMs: preSubmitBudget.remainingBudgetMs,
        },
      ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

      return {
        newHeaderHash,
        newHeader,
        newHeaderCbor: Buffer.from(
          LucidData.to(newHeader as never, SDK.Header as never),
          "hex",
        ),
        blockEndTimeMs: alignedEndTime,
        signAndSubmitProgram,
        txSize,
      };
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Failed to stabilize the commit timing budget before building the block commitment transaction",
        cause: `attempts=${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS},last_selected_end_time=${commitWindow.resolvedEndTime},witness_context=${witnessContext === undefined ? "missing" : "present"}`,
      }),
    );
  });
