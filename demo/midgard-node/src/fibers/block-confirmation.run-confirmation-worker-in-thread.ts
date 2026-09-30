import { Effect } from "effect";
import { Worker } from "worker_threads";

import {
  DepositsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  reviveEarliestCanonicalPayloadJournal,
  withCanonicalHeaderJournals,
} from "../services/canonical-journal-recovery.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
import { Database } from "../services/index.js";
import { WorkerError } from "../workers/utils/common.js";
import {
  SerializedCanonicalCommittedHeader,
  WorkerOutput as BlockConfirmationWorkerOutput,
} from "../workers/utils/confirm-block-commitments.js";
import { type ConfirmationWorkerRunner } from "./block-confirmation.record-confirmed-pending-block.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";
import { makeAwaitedWorkerTerminator } from "./worker-lifecycle.js";

export const abandonUnsubmittedPendingBlockIfStillPresent = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* PendingBlockFinalizationsDB.assertCanonicalEventMembers(record);
    const headerHash = record[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
    const abandoned =
      yield* PendingBlockFinalizationsDB.markUnsubmittedAbandoned(headerHash);
    if (!abandoned) {
      return false;
    }
    yield* DepositsDB.clearProjectedHeaderAssignmentByEventIds(
      record.depositEventIds,
      headerHash,
    );
    yield* ForcedTransactionsDB.clearProjectedHeaderAssignmentByEventIds(
      record.forcedTransactionEventIds,
      headerHash,
    );
    yield* WithdrawalsDB.clearProjectedHeaderAssignmentByEventIds(
      record.withdrawalEventIds,
      headerHash,
    );
    return true;
  }).pipe(withHistoryWrite);

export const reviveCanonicalPayloadJournalFromWorkerSnapshot = (
  canonicalHeaders: readonly SerializedCanonicalCommittedHeader[],
) =>
  Effect.gen(function* () {
    const headersWithJournals = yield* withCanonicalHeaderJournals(
      canonicalHeaders.map((header) => ({
        headerHash: Buffer.from(header.headerHash, "hex"),
        endTimeMs: header.endTimeMs,
        blockUTxO: header.blockUTxO,
      })),
    );
    return yield* reviveEarliestCanonicalPayloadJournal({
      canonicalHeaders: headersWithJournals,
      logPrefix: "Steady-state confirmation",
    });
  });

export const runConfirmationWorkerInThread: ConfirmationWorkerRunner = (
  input,
) =>
  Effect.async<BlockConfirmationWorkerOutput, WorkerError, never>((resume) => {
    Effect.runSync(Effect.logInfo("🔍 Starting block confirmation worker..."));
    const worker = new Worker(
      resolveWorkerEntry(import.meta.url, "confirm-block-commitments.js"),
      {
        workerData: input,
      },
    );
    const terminate = makeAwaitedWorkerTerminator(worker);
    let settled = false;
    const cleanup = () => {
      worker.off("message", onMessage);
      worker.off("error", onError);
      worker.off("exit", onExit);
    };
    const settle = (
      result: Effect.Effect<BlockConfirmationWorkerOutput, WorkerError>,
    ) => {
      if (settled) return;
      settled = true;
      cleanup();
      void terminate().then(
        () => resume(result),
        (cause) =>
          resume(
            Effect.fail(
              new WorkerError({
                worker: "confirm-block-commitments",
                message: "Failed to terminate confirmation worker.",
                cause,
              }),
            ),
          ),
      );
    };
    const onMessage = (output: BlockConfirmationWorkerOutput) => {
      if (output.type === "FailedConfirmationOutput") {
        settle(
          Effect.fail(
            new WorkerError({
              worker: "confirm-block-commitments",
              message: "Confirmation worker failed.",
              cause: output.error,
            }),
          ),
        );
      } else {
        settle(Effect.succeed(output));
      }
    };
    const onError = (error: Error) => {
      settle(
        Effect.fail(
          new WorkerError({
            worker: "confirm-block-commitments",
            message: `Error in confirmation worker: ${error}`,
            cause: error,
          }),
        ),
      );
    };
    const onExit = (code: number) => {
      settle(
        Effect.fail(
          new WorkerError({
            worker: "confirm-block-commitments",
            message: `Confirmation worker exited before producing output with code: ${code}`,
            cause: `exit code ${code}`,
          }),
        ),
      );
    };
    worker.on("message", onMessage);
    worker.on("error", onError);
    worker.on("exit", onExit);
    return Effect.promise(async () => {
      if (!settled) {
        settled = true;
        cleanup();
      }
      await terminate();
    });
  });
