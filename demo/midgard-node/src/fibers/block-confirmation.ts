import {
  BlocksDB,
  PendingBlockFinalizationsDB,
} from "@/database/index.js";
import { DatabaseError, NotFoundError } from "@/database/utils/common.js";
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
import { finalizeConfirmedBlock } from "./block-submission.js";
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
  | WorkerError
  | DatabaseError
  | NotFoundError
  | ConfirmationInvariantError
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError,
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
          // Phase 2 of submission: now that the L1 commit tx is on chain,
          // apply the local DB mutations (mempool clear, ledger update,
          // address history, BlocksTxsDB / ImmutableDB transfer, BlocksDB
          // status). Submission Phase 1 deliberately deferred all of this so
          // that an expired/unincluded tx leaves no local state to roll back.
          yield* finalizeConfirmedBlock(journalHeaderHash);
          yield* PendingBlockFinalizationsDB.markFinalized(journalHeaderHash);
          // The pending submission this worker resolved is now finalized
          // on-chain. Only in this branch may we open the submission gate,
          // since `pending` was snapshotted before the worker ran and a
          // newer pending submission may have been recorded in the meantime
          // (matched=null only proves this run did not confirm anything).
          yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
          yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
        }
        yield* Ref.set(
          globals.AVAILABLE_CONFIRMED_BLOCK,
          workerOutput.latestBlocksUTxO,
        );
        yield* Effect.logInfo("🔍 ☑️  Submitted block confirmed.");
        break;
      }
      case "StaleUnconfirmedRecoveryOutput": {
        if (
          Option.isSome(pending) &&
          pending.value[
            PendingBlockFinalizationsDB.Columns.HEADER_HASH
          ].toString("hex") === workerOutput.stalePendingHeaderHash
        ) {
          yield* PendingBlockFinalizationsDB.markAbandoned(
            pending.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
          );
        }
        // Drop the now-abandoned block from BlocksDB. Local state was never
        // mutated for it (Phase 2 was gated behind on-chain confirmation),
        // so there is nothing to roll back; simply removing the row makes
        // the next worker iteration build a fresh block on top of the real
        // chain tip instead of the ghost one.
        yield* BlocksDB.deleteByBlocks([
          Buffer.from(workerOutput.stalePendingHeaderHash, "hex"),
        ]);
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
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
