import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import type { OperatorWalletView } from "@/operator-wallet-view.js";
import { Lucid } from "@/services/index.js";
import {
  handleSignSubmitNoConfirmation,
  type TxSignError,
  type TxSubmitError,
} from "@/transactions/utils.js";
import { resolveAlignedCommitEndTime } from "@/workers/utils/commit-end-time.js";
import {
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
  readonly blockEndTimeMs: number;
  readonly signAndSubmitProgram: Effect.Effect<
    string,
    TxSignError | TxSubmitError
  >;
  readonly txSize: number;
};

export const buildUnsignedCommitTx = (
  contracts: SDK.MidgardValidators,
  latestBlock: SDK.StateQueueUTxO,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  withdrawalsRoot: string,
  endDate: Date,
  initialOperatorWalletView?: OperatorWalletView,
): Effect.Effect<
  BuiltCommitTx,
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | TxSignError
  | TxSubmitError,
  Lucid
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const latestEndTime = Number(
      (yield* getLatestBlockDatumEndTime(latestBlock.datum)).getTime(),
    );

    // The worker's Lucid service starts without a selected wallet. Select the
    // operator wallet before any scheduler refresh or witness lookup that
    // depends on wallet address or spendable operator inputs.
    yield* lucid.switchToOperatorsMainWallet;
    const resolveCommitWindow = () =>
      resolveAlignedCommitEndTime({
        lucid: lucid.api,
        latestEndTime,
        candidateEndTime: endDate.getTime(),
      });
    let commitWindow = resolveCommitWindow();
    let witnessContext: RealStateQueueWitnessContext | undefined;
    let stabilizationAttempts = 0;
    let commitWindowStabilized = false;
    while (stabilizationAttempts < COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS) {
      const witnessEndTime = commitWindow.resolvedEndTime;
      witnessContext = yield* fetchRealStateQueueWitnessContext(
        lucid.api,
        contracts,
        witnessEndTime,
        witnessContext?.operatorWalletView ?? initialOperatorWalletView,
        lucid.referenceScriptsAddress,
      );
      const refreshedCommitWindow = resolveCommitWindow();
      if (refreshedCommitWindow.resolvedEndTime === witnessEndTime) {
        commitWindow = refreshedCommitWindow;
        commitWindowStabilized = true;
        break;
      }
      stabilizationAttempts += 1;
      yield* Effect.logWarning(
        `Commit end-time advanced while preparing scheduler-aligned witness context; rebuilding with refreshed window (previous=${commitWindow.resolvedEndTime}, next=${refreshedCommitWindow.resolvedEndTime}, candidate=${refreshedCommitWindow.alignedCandidateEndTime}, latestEnd=${latestEndTime}, attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
      );
      commitWindow = refreshedCommitWindow;
    }
    if (witnessContext === undefined || !commitWindowStabilized) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to stabilize the commit window before building the block commitment transaction",
          cause: `attempts=${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS},last_selected_end_time=${commitWindow.resolvedEndTime}`,
        }),
      );
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
    yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* updateLatestBlocksDatumAndGetTheNewHeaderLocal(
        lucid.api,
        latestBlock.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        withdrawalsRoot,
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
    yield* Effect.logInfo(`🔹 Transaction built successfully. Size: ${txSize}`);

    const signAndSubmitProgram = handleSignSubmitNoConfirmation(
      lucid.api,
      txBuilder,
    ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

    return {
      newHeaderHash,
      blockEndTimeMs: alignedEndTime,
      signAndSubmitProgram,
      txSize,
    };
  });
