import { DepositsDB, PendingBlockFinalizationsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database, Globals } from "@/services/index.js";
import { Data, Effect, Option, Ref, Schedule } from "effect";
import { Worker } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import {
  WorkerInput as BlockConfirmationWorkerInput,
  WorkerOutput as BlockConfirmationWorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { WorkerError } from "@/workers/utils/common.js";
import { deserializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { emitQueueStateMetrics } from "./queue-metrics.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";

type ConfirmationWorkerRunner = (
  input: BlockConfirmationWorkerInput,
) => Effect.Effect<BlockConfirmationWorkerOutput, WorkerError, never>;

class ConfirmationInvariantError extends Data.TaggedError(
  "ConfirmationInvariantError",
)<{
  readonly message: string;
  readonly cause: string;
}> {}

const stateQueueTipMetadata = (
  blocksUTxO: Parameters<typeof deserializeStateQueueUTxO>[0],
): Effect.Effect<
  { readonly endTimeMs: number; readonly headerHash: Buffer | null },
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError,
  never
> =>
  Effect.gen(function* () {
    const latestBlock = yield* deserializeStateQueueUTxO(blocksUTxO);
    if (latestBlock.datum.key === "Empty") {
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(
        latestBlock.datum,
      );
      return {
        endTimeMs: Number(data.endTime),
        headerHash: null,
      };
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum);
    const headerHash = yield* SDK.hashBlockHeader(header);
    return {
      endTimeMs: Number(header.endTime),
      headerHash: Buffer.from(headerHash, "hex"),
    };
  });

const toPendingWorkerInput = (
  pending: Option.Option<PendingBlockFinalizationsDB.Record>,
) =>
  Option.match(pending, {
    onNone: () => null,
    onSome: (record) => ({
      expectedHeaderHash:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
      submittedTxHash:
        record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
          "hex",
        ) ?? "",
      blockEndTimeMs:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME].getTime(),
      updatedAtMs:
        record[PendingBlockFinalizationsDB.Columns.UPDATED_AT].getTime(),
    }),
  });

const pendingRecordRequiresLocalFinalizationRecovery = (
  record: PendingBlockFinalizationsDB.Record,
): boolean => {
  const status = record[PendingBlockFinalizationsDB.Columns.STATUS];
  const hasLocalPayloadMembers =
    record.depositEventIds.length > 0 || record.mempoolTxIds.length > 0;
  return (
    hasLocalPayloadMembers &&
    (status === PendingBlockFinalizationsDB.Status.PendingSubmission ||
      status ===
        PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending ||
      status === PendingBlockFinalizationsDB.Status.ObservedWaitingStability)
  );
};

const observeConfirmedPendingBlock = (
  record: PendingBlockFinalizationsDB.Record,
  recoveredSubmittedTxHash: Buffer | null,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const journalHeaderHash =
      record[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
    yield* DepositsDB.markProjectedByEventIds(
      record.depositEventIds,
      journalHeaderHash,
    );
    const requiresLocalFinalizationRecovery =
      pendingRecordRequiresLocalFinalizationRecovery(record);
    yield* requiresLocalFinalizationRecovery
      ? PendingBlockFinalizationsDB.markObservedWaitingStability(
          journalHeaderHash,
          BigInt(Date.now()),
          recoveredSubmittedTxHash ?? undefined,
        )
      : PendingBlockFinalizationsDB.markFinalized(journalHeaderHash);
    return requiresLocalFinalizationRecovery;
  });

const abandonPendingBlockIfPresent = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const headerHash = record[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
    yield* DepositsDB.clearProjectedHeaderAssignmentByEventIds(
      record.depositEventIds,
      headerHash,
    );
    yield* PendingBlockFinalizationsDB.markAbandoned(headerHash);
  });

const runConfirmationWorkerInThread: ConfirmationWorkerRunner = (input) =>
  Effect.async<BlockConfirmationWorkerOutput, WorkerError, never>((resume) => {
    Effect.runSync(Effect.logInfo("🔍 Starting block confirmation worker..."));
    const worker = new Worker(
      resolveWorkerEntry(import.meta.url, "confirm-block-commitments.js"),
      {
        workerData: input,
      },
    );
    worker.on("message", (output: BlockConfirmationWorkerOutput) => {
      if (output.type === "FailedConfirmationOutput") {
        resume(
          Effect.fail(
            new WorkerError({
              worker: "confirm-block-commitments",
              message: "Confirmation worker failed.",
              cause: output.error,
            }),
          ),
        );
      } else {
        resume(Effect.succeed(output));
      }
      worker.terminate();
    });
    worker.on("error", (error: Error) => {
      resume(
        Effect.fail(
          new WorkerError({
            worker: "confirm-block-commitments",
            message: `Error in confirmation worker: ${error}`,
            cause: error,
          }),
        ),
      );
      worker.terminate();
    });
    worker.on("exit", (code: number) => {
      if (code !== 0) {
        resume(
          Effect.fail(
            new WorkerError({
              worker: "confirm-block-commitments",
              message: `Confirmation worker exited with code: ${code}`,
              cause: `exit code ${code}`,
            }),
          ),
        );
      }
    });
    return Effect.sync(() => {
      worker.terminate();
    });
  });

export const buildBlockConfirmationAction = (
  runWorker: ConfirmationWorkerRunner = runConfirmationWorkerInThread,
): Effect.Effect<
  void,
  WorkerError | DatabaseError | ConfirmationInvariantError,
  Globals | Database
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.HEARTBEAT_BLOCK_CONFIRMATION, Date.now());
    const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (resetInProgress) {
      return;
    }

    const availableConfirmedBlock = yield* Ref.get(
      globals.AVAILABLE_CONFIRMED_BLOCK,
    );
    const pending = yield* PendingBlockFinalizationsDB.retrieveActive();

    yield* Effect.logInfo("🔍 New block confirmation process started.");
    const workerOutput = yield* runWorker({
      data: {
        firstRun: Option.isNone(pending) && availableConfirmedBlock === "",
        pendingBlock: toPendingWorkerInput(pending),
      },
    });
    switch (workerOutput.type) {
      case "SuccessfulConfirmationOutput": {
        const metadata = yield* stateQueueTipMetadata(
          workerOutput.latestBlocksUTxO,
        ).pipe(Effect.orDie);
        let nextLocalBlockBoundaryMs = metadata.endTimeMs;
        if (
          Option.isSome(pending) &&
          workerOutput.matchedPendingBlocksUTxO === null
        ) {
          return yield* Effect.fail(
            new ConfirmationInvariantError({
              message:
                "Confirmation worker reported success without resolving the active pending-finalization journal",
              cause: `pending_header_hash=${pending.value[
                PendingBlockFinalizationsDB.Columns.HEADER_HASH
              ].toString("hex")}`,
            }),
          );
        }
        if (
          Option.isNone(pending) &&
          workerOutput.matchedPendingBlocksUTxO !== null
        ) {
          return yield* Effect.fail(
            new ConfirmationInvariantError({
              message:
                "Confirmation worker returned a matched pending block even though no active pending-finalization journal exists",
              cause: "matched_pending_block_without_journal",
            }),
          );
        }
        if (
          Option.isSome(pending) &&
          workerOutput.matchedPendingBlocksUTxO !== null
        ) {
          const matchedMetadata = yield* stateQueueTipMetadata(
            workerOutput.matchedPendingBlocksUTxO,
          ).pipe(Effect.orDie);
          const matchedBlock = yield* deserializeStateQueueUTxO(
            workerOutput.matchedPendingBlocksUTxO,
          ).pipe(Effect.orDie);
          const journalHeaderHash =
            pending.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
          if (
            matchedMetadata.headerHash === null ||
            !matchedMetadata.headerHash.equals(journalHeaderHash)
          ) {
            return yield* Effect.fail(
              new ConfirmationInvariantError({
                message:
                  "Confirmed block header does not match the persisted pending-finalization journal",
                cause: `expected_header_hash=${journalHeaderHash.toString(
                  "hex",
                )},matched_header_hash=${
                  matchedMetadata.headerHash === null
                    ? "null"
                    : matchedMetadata.headerHash.toString("hex")
                }`,
              }),
            );
          }
          const localFinalizationRecoveryPending =
            yield* observeConfirmedPendingBlock(
              pending.value,
              Buffer.from(matchedBlock.utxo.txHash, "hex"),
            );
          yield* Ref.set(
            globals.LOCAL_FINALIZATION_PENDING,
            localFinalizationRecoveryPending,
          );
          yield* Ref.set(
            globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            localFinalizationRecoveryPending
              ? workerOutput.matchedPendingBlocksUTxO
              : "",
          );
          nextLocalBlockBoundaryMs =
            pending.value[
              PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
            ].getTime();
        } else {
          yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
          yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        }
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          nextLocalBlockBoundaryMs,
        );
        yield* Ref.set(
          globals.AVAILABLE_CONFIRMED_BLOCK,
          workerOutput.latestBlocksUTxO,
        );
        yield* Effect.logInfo("🔍 ☑️  Submitted block confirmed.");
        break;
      }
      case "StaleUnconfirmedRecoveryOutput": {
        const metadata = yield* stateQueueTipMetadata(
          workerOutput.latestBlocksUTxO,
        ).pipe(Effect.orDie);
        if (
          Option.isSome(pending) &&
          pending.value[
            PendingBlockFinalizationsDB.Columns.HEADER_HASH
          ].toString("hex") === workerOutput.stalePendingHeaderHash
        ) {
          yield* abandonPendingBlockIfPresent(pending.value);
        }
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          metadata.endTimeMs,
        );
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => Math.max(0, n - 1));
        yield* Ref.set(
          globals.AVAILABLE_CONFIRMED_BLOCK,
          workerOutput.latestBlocksUTxO,
        );
        yield* Effect.logWarning(
          `🔍 ⚠️  Abandoning stale pending block submission ${workerOutput.stalePendingHeaderHash} (submitted_tx=${workerOutput.staleSubmittedTxHash || "unknown"}); recovered canonical chain tip and resumed commitment flow.`,
        );
        break;
      }
      case "NoTxForConfirmationOutput": {
        break;
      }
      case "FailedConfirmationOutput": {
        break;
      }
    }

    yield* emitQueueStateMetrics;
  });

export const blockConfirmationAction = buildBlockConfirmationAction();

export const blockConfirmationFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Globals | Database> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟫 Block confirmation fiber started.");
    const action = blockConfirmationAction.pipe(
      Effect.withSpan("block-confirmation-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
