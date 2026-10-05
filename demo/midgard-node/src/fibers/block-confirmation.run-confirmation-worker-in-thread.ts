import { Effect } from "effect";

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
import {
  makeAwaitedWorkerTerminator,
  type SpawnWorker,
  spawnWorkerThread,
  WORKER_TERMINATION_WAIT_MS,
} from "./worker-lifecycle.js";

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

/** Old-generation cap of one confirmation worker. Loading its module graph
 * takes about 140 MiB of heap; a worker past the cap fails with a
 * WorkerError, retried on the next tick, instead of growing the process. */
export const CONFIRMATION_WORKER_HEAP_MB = 1024;

/** Longest one confirmation job may run before its worker is terminated and
 * the job fails. Above the worker's own provider waits, below the
 * confirmation fiber's 180 s L1 control-plane hold. */
export const CONFIRMATION_WORKER_JOB_TIMEOUT_MS = 150_000;

const confirmationWorkerError = (message: string, cause: unknown) =>
  new WorkerError({ worker: "confirm-block-commitments", message, cause });

/**
 * Runs one confirmation job in a fresh worker thread. Every exit path
 * terminates the worker and waits for that at most
 * `WORKER_TERMINATION_WAIT_MS`: a worker that does not stop in time fails
 * the job (settled) or is left to stop on its own (interrupted), so neither
 * path can hold the L1 control plane past its bound.
 */
export const makeConfirmationWorkerRunner =
  (
    options: {
      readonly workerEntry?: string | URL;
      readonly jobTimeoutMs?: number;
      readonly terminationWaitMs?: number;
      readonly heapMb?: number;
      readonly spawnWorker?: SpawnWorker;
    } = {},
  ): ConfirmationWorkerRunner =>
  (input) =>
    Effect.async<BlockConfirmationWorkerOutput, WorkerError, never>(
      (resume) => {
        Effect.runSync(
          Effect.logInfo("🔍 Starting block confirmation worker..."),
        );
        const worker = (options.spawnWorker ?? spawnWorkerThread)(
          options.workerEntry ??
            resolveWorkerEntry(import.meta.url, "confirm-block-commitments.js"),
          {
            workerData: input,
            resourceLimits: {
              maxOldGenerationSizeMb:
                options.heapMb ?? CONFIRMATION_WORKER_HEAP_MB,
            },
          },
        );
        const terminate = makeAwaitedWorkerTerminator(worker, undefined, {
          waitTimeoutMs:
            options.terminationWaitMs ?? WORKER_TERMINATION_WAIT_MS,
        });
        const jobTimeoutMs =
          options.jobTimeoutMs ?? CONFIRMATION_WORKER_JOB_TIMEOUT_MS;
        let settled = false;
        const cleanup = () => {
          clearTimeout(jobTimer);
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
                  confirmationWorkerError(
                    "Failed to terminate confirmation worker.",
                    cause,
                  ),
                ),
              ),
          );
        };
        const onMessage = (output: BlockConfirmationWorkerOutput) => {
          if (output.type === "FailedConfirmationOutput") {
            settle(
              Effect.fail(
                confirmationWorkerError(
                  "Confirmation worker failed.",
                  output.error,
                ),
              ),
            );
          } else {
            settle(Effect.succeed(output));
          }
        };
        const onError = (error: Error) => {
          settle(
            Effect.fail(
              confirmationWorkerError(
                `Error in confirmation worker: ${error}`,
                error,
              ),
            ),
          );
        };
        const onExit = (code: number) => {
          settle(
            Effect.fail(
              confirmationWorkerError(
                `Confirmation worker exited before producing output with code: ${code}`,
                `exit code ${code}`,
              ),
            ),
          );
        };
        const jobTimer = setTimeout(
          () =>
            settle(
              Effect.fail(
                confirmationWorkerError(
                  `Confirmation worker produced no output within ${jobTimeoutMs.toString()} ms.`,
                  `timeout_ms=${jobTimeoutMs.toString()}`,
                ),
              ),
            ),
          jobTimeoutMs,
        );
        worker.on("message", onMessage);
        worker.on("error", onError);
        worker.on("exit", onExit);
        // An interrupted job stops waiting for the worker after the bounded
        // termination wait. The worker holds no lease, so nothing is released
        // early; a worker that never stops is logged, not awaited forever.
        return Effect.tryPromise(() => {
          if (!settled) {
            settled = true;
            cleanup();
          }
          return terminate();
        }).pipe(
          Effect.catchAll((error) =>
            Effect.logError(
              `Confirmation worker termination after interruption was not confirmed: ${String(error.cause)}`,
            ),
          ),
          Effect.asVoid,
        );
      },
    );

export const runConfirmationWorkerInThread: ConfirmationWorkerRunner =
  makeConfirmationWorkerRunner();
