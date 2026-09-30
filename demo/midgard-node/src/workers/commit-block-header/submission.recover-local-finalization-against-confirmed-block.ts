import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  PendingBlockFinalizationsDB,
  TxUtils as TxTable,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { type MidgardMpf } from "../../mpf/index.js";
import {
  type ContractDeploymentIdentityValue,
  Database,
} from "../../services/index.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "../utils/commit-block-header.js";
import {
  skippedSubmissionProgram,
  successfulLocalFinalizationRecoveryProgram,
} from "../utils/commit-submission.js";
import {
  getHeaderFromStateQueueDatumLocal,
  hashBlockHeaderLocal,
} from "./state-queue.js";

export const deferProcessedCommitPayloadUntilConfirmation = ({
  processedMempoolTxs,
  mempoolTxHashes,
  mempoolTxsCount,
  sizeOfProcessedTxs,
}: {
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly mempoolTxsCount: number;
  readonly sizeOfProcessedTxs: number;
}) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔹 No confirmed blocks available. Transferring to ProcessedMempoolDB...",
    );
    const transferResult = yield* Effect.either(
      skippedSubmissionProgram(processedMempoolTxs, mempoolTxHashes),
    );
    if (transferResult._tag === "Left") {
      const detail = formatUnknownError(transferResult.left);
      yield* Effect.logError(
        `🔹 Failed to defer processed txs while waiting for confirmation: ${detail}`,
      );
      return {
        type: "FailureOutput",
        error: `Failed to transfer deferred commit payload to ProcessedMempoolDB: ${detail}`,
      } satisfies WorkerOutput;
    }
    return {
      type: "SkippedSubmissionOutput",
      mempoolTxsCount,
      sizeOfProcessedTxs,
    } satisfies WorkerOutput;
  });

export const recoverLocalFinalizationAgainstConfirmedBlock = ({
  latestBlock,
  transactionsMpf,
  processedMempoolTxs,
  mempoolTxHashes,
  workerInput,
  sizeOfProcessedTxs,
  beforeTransactionsMpfReset,
  consensusProfile,
}: {
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly transactionsMpf: MidgardMpf;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
  readonly beforeTransactionsMpfReset?: Effect.Effect<
    void,
    DatabaseError,
    Database
  >;
  readonly consensusProfile: ContractDeploymentIdentityValue["consensusProfile"];
}): Effect.Effect<WorkerOutput, unknown, Database> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔹 Attempting local finalization recovery against confirmed block roots...",
    );
    if (latestBlock.datum.key === "Empty") {
      return {
        type: "FailureOutput",
        error:
          "Confirmed block datum does not contain a recoverable header for local finalization",
      } satisfies WorkerOutput;
    }
    const confirmedHeader = yield* getHeaderFromStateQueueDatumLocal(
      latestBlock.datum,
    );
    const confirmedHeaderHash = yield* hashBlockHeaderLocal(confirmedHeader);
    const pendingRecord =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(fromHex(confirmedHeaderHash)),
      );
    if (Option.isNone(pendingRecord)) {
      return {
        type: "FailureOutput",
        error:
          "Local finalization recovery aborted: no durable pending journal exists for the confirmed block",
      } satisfies WorkerOutput;
    }
    const record = pendingRecord.value;
    const rootsMatch =
      record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID] ===
        consensusProfile.profileId &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT] ===
        confirmedHeader.utxosRoot &&
      record[
        PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT
      ] === confirmedHeader.forcedTransactionsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT] ===
        confirmedHeader.transactionsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT] ===
        confirmedHeader.depositsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT] ===
        confirmedHeader.withdrawalsRoot &&
      record[
        PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT
      ] === confirmedHeader.validationTracesRoot &&
      record[
        PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT
      ] === confirmedHeader.validationTraceCount;
    if (!rootsMatch) {
      return {
        type: "FailureOutput",
        error:
          "Local finalization recovery aborted: journal expected roots do not match the confirmed block header",
      } satisfies WorkerOutput;
    }
    if (
      workerInput.nativeMpf !== undefined &&
      workerInput.nativeMpf.durableRoot !== confirmedHeader.utxosRoot
    ) {
      return {
        type: "FailureOutput",
        error:
          "Local finalization recovery requires the native durable root to match the confirmed header",
      } satisfies WorkerOutput;
    }
    return yield* successfulLocalFinalizationRecoveryProgram(
      transactionsMpf,
      processedMempoolTxs,
      mempoolTxHashes,
      confirmedHeaderHash,
      workerInput,
      sizeOfProcessedTxs,
      beforeTransactionsMpfReset,
    );
  });
