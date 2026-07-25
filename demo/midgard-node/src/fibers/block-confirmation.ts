import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Duration,
  Effect,
  Metric,
  Option,
  Queue,
  Ref,
  Schedule,
} from "effect";
import { Worker } from "worker_threads";

import {
  DepositsDB,
  ForcedTransactionsDB,
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  reviveEarliestCanonicalPayloadJournal,
  withCanonicalHeaderJournals,
} from "@/services/canonical-journal-recovery.js";
import {
  Database,
  Globals,
  Lucid,
  NodeConfig,
  publishMempoolLedgerDelta,
  withL1ControlPlane,
} from "@/services/index.js";
import {
  resolveTransactionConfirmationMetadata,
  type TransactionConfirmationMetadata,
} from "@/transaction-confirmation-metadata.js";
import { deserializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { WorkerError } from "@/workers/utils/common.js";
import {
  SerializedCanonicalCommittedHeader,
  WorkerInput as BlockConfirmationWorkerInput,
  WorkerOutput as BlockConfirmationWorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";

import { emitQueueStateMetrics } from "./queue-metrics.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";
import { invalidateSpeculativeCommitCandidate } from "./speculative-commit-builder.js";
import { makeAwaitedWorkerTerminator } from "./worker-lifecycle.js";

const confirmationDetectionLagTimer = Metric.timer(
  "confirmation_detection_lag_ms",
  "Milliseconds from the authoritative Kupo creation slot to AVAILABLE_CONFIRMED_BLOCK publication",
);
const confirmationDetectionLagUnavailableCounter = Metric.counter(
  "confirmation_detection_lag_unavailable_total",
  {
    description:
      "First confirmation observations without valid authoritative Kupo creation metadata",
  },
);

/**
 * Computes milliseconds from the authoritative Kupo creation slot to the
 * instant AVAILABLE_CONFIRMED_BLOCK is published. Provider clock skew can put
 * the slot slightly in the future, so negative samples clamp to zero.
 */
export const resolveConfirmationDetectionLagMs = ({
  confirmationSlotUnixMs,
  availableConfirmedSetAtMs,
}: {
  readonly confirmationSlotUnixMs: number;
  readonly availableConfirmedSetAtMs: number;
}): number => Math.max(0, availableConfirmedSetAtMs - confirmationSlotUnixMs);

export const shouldObserveConfirmationDetectionLag = (
  observedConfirmedAtMs: bigint | null,
): boolean => observedConfirmedAtMs === null;

export type ActivePendingFinalizationIdentity = {
  readonly headerHash: string;
  readonly submittedTxHash: string | null;
  readonly status: PendingBlockFinalizationsDB.Status;
};

const activePendingFinalizationIdentity = (
  pending: Option.Option<PendingBlockFinalizationsDB.Row>,
): ActivePendingFinalizationIdentity | null =>
  Option.match(pending, {
    onNone: () => null,
    onSome: (row) => ({
      headerHash:
        row[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
      submittedTxHash:
        row[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
          "hex",
        ) ?? null,
      status: row[PendingBlockFinalizationsDB.Columns.STATUS],
    }),
  });

export const confirmationPendingSnapshotChanged = ({
  captured,
  current,
}: {
  readonly captured: ActivePendingFinalizationIdentity | null;
  readonly current: ActivePendingFinalizationIdentity | null;
}): boolean =>
  captured?.headerHash !== current?.headerHash ||
  captured?.submittedTxHash !== current?.submittedTxHash ||
  captured?.status !== current?.status;

export const staleRecoveryMustPreserveNewActiveJournal = ({
  captured,
  current,
}: {
  readonly captured: ActivePendingFinalizationIdentity | null;
  readonly current: ActivePendingFinalizationIdentity | null;
}): boolean =>
  current !== null && confirmationPendingSnapshotChanged({ captured, current });

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
    const header = yield* SDK.getHeaderV1FromStateQueueDatum(latestBlock.datum);
    const headerHash = yield* SDK.hashBlockHeaderV1(header);
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
    record.depositEventIds.length > 0 ||
    record.forcedTransactionEventIds.length > 0 ||
    record.withdrawalEventIds.length > 0 ||
    record.mempoolTxIds.length > 0;
  return (
    status === PendingBlockFinalizationsDB.Status.PendingSubmission ||
    status ===
      PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending ||
    (hasLocalPayloadMembers &&
      status === PendingBlockFinalizationsDB.Status.ObservedWaitingStability)
  );
};

const observeConfirmedPendingBlock = (
  record: PendingBlockFinalizationsDB.Record,
  recoveredSubmittedTxHash: Buffer | null,
): Effect.Effect<boolean, DatabaseError, Database | Globals | NodeConfig> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    const journalHeaderHash =
      record[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
    yield* DepositsDB.markProjectedByEventIds(
      record.depositEventIds,
      journalHeaderHash,
    );
    yield* ForcedTransactionsDB.markProjectedByEventIds(
      record.forcedTransactionEventIds,
      journalHeaderHash,
    );
    yield* WithdrawalsDB.markProjectedByEventIds(
      record.withdrawalEventIds,
      journalHeaderHash,
    );
    if (record.depositEventIds.length > 0) {
      const projectedEntries = yield* MempoolLedgerDB.retrieveBySourceEventIds(
        record.depositEventIds,
      );
      yield* publishMempoolLedgerDelta(
        globals,
        {
          full: false,
          upserts: projectedEntries.map((entry) => [
            entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
            entry[MempoolLedgerDB.Columns.OUTPUT],
          ]),
          deletes: [],
        },
        config.VALIDATION_LEDGER_DELTA_LOG_MAX,
      );
    }
    const requiresLocalFinalizationRecovery =
      pendingRecordRequiresLocalFinalizationRecovery(record);
    if (!requiresLocalFinalizationRecovery) {
      yield* WithdrawalsDB.markFinalizedByEventIds(
        record.withdrawalEventIds,
        journalHeaderHash,
      );
      yield* ForcedTransactionsDB.markFinalizedByEventIds(
        record.forcedTransactionEventIds,
        journalHeaderHash,
      );
    }
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
    yield* ForcedTransactionsDB.clearProjectedHeaderAssignmentByEventIds(
      record.forcedTransactionEventIds,
      headerHash,
    );
    yield* WithdrawalsDB.clearProjectedHeaderAssignmentByEventIds(
      record.withdrawalEventIds,
      headerHash,
    );
    yield* PendingBlockFinalizationsDB.markAbandoned(headerHash);
  });

const abandonUnsubmittedPendingBlockIfStillPresent = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
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
  });

const reviveCanonicalPayloadJournalFromWorkerSnapshot = (
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

const runConfirmationWorkerInThread: ConfirmationWorkerRunner = (input) =>
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

export const buildBlockConfirmationAction = (
  runWorker: ConfirmationWorkerRunner = runConfirmationWorkerInThread,
  options: {
    readonly afterPendingSnapshotGuard?: () => Effect.Effect<void>;
    readonly afterStaleRecoveryJournalTransition?: () => Effect.Effect<void>;
  } = {},
): Effect.Effect<
  void,
  WorkerError | DatabaseError | ConfirmationInvariantError,
  Globals | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    yield* Ref.set(globals.HEARTBEAT_BLOCK_CONFIRMATION, Date.now());
    const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (resetInProgress) {
      if (config.SPECULATIVE_COMMIT_BUILD) {
        yield* invalidateSpeculativeCommitCandidate(globals, config, "T5");
      }
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
    const currentPending = yield* PendingBlockFinalizationsDB.retrieveActive();
    const capturedPendingIdentity = activePendingFinalizationIdentity(pending);
    const currentPendingIdentity =
      activePendingFinalizationIdentity(currentPending);
    if (
      confirmationPendingSnapshotChanged({
        captured: capturedPendingIdentity,
        current: currentPendingIdentity,
      })
    ) {
      yield* Effect.logWarning(
        `🔍 Discarding stale confirmation worker output because the active pending-finalization journal changed while the worker was running (captured=${JSON.stringify(capturedPendingIdentity)},current=${JSON.stringify(currentPendingIdentity)}).`,
      );
      return;
    }
    yield* options.afterPendingSnapshotGuard?.() ?? Effect.void;
    const postGuardPending =
      yield* PendingBlockFinalizationsDB.retrieveActive();
    const postGuardPendingIdentity =
      activePendingFinalizationIdentity(postGuardPending);
    if (
      confirmationPendingSnapshotChanged({
        captured: capturedPendingIdentity,
        current: postGuardPendingIdentity,
      })
    ) {
      yield* Effect.logWarning(
        `🔍 Discarding stale confirmation worker output because the active pending-finalization journal changed after the initial snapshot guard (captured=${JSON.stringify(capturedPendingIdentity)},current=${JSON.stringify(postGuardPendingIdentity)}).`,
      );
      return;
    }
    switch (workerOutput.type) {
      case "SuccessfulConfirmationOutput": {
        const confirmationObservedAtMs = Date.now();
        let confirmationMetadata: TransactionConfirmationMetadata | undefined;
        const submittedAtMs = yield* Ref.get(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
        );
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
          const matchedHeader = yield* SDK.getHeaderV1FromStateQueueDatum(
            matchedBlock.datum,
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
          const journalRootsMatch =
            pending.value[
              PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
            ] === matchedHeader.utxosRoot &&
            pending.value[
              PendingBlockFinalizationsDB.Columns
                .EXPECTED_FORCED_TRANSACTIONS_ROOT
            ] === matchedHeader.forcedTransactionsRoot &&
            pending.value[
              PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT
            ] === matchedHeader.transactionsRoot &&
            pending.value[
              PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT
            ] === matchedHeader.depositsRoot &&
            pending.value[
              PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT
            ] === matchedHeader.withdrawalsRoot;
          if (!journalRootsMatch) {
            return yield* Effect.fail(
              new ConfirmationInvariantError({
                message:
                  "Confirmed block roots do not match the persisted pending-finalization journal",
                cause: `header_hash=${journalHeaderHash.toString("hex")}`,
              }),
            );
          }
          if (
            shouldObserveConfirmationDetectionLag(
              pending.value[
                PendingBlockFinalizationsDB.Columns.OBSERVED_CONFIRMED_AT_MS
              ],
            )
          ) {
            const lucid = yield* Effect.serviceOption(Lucid);
            if (Option.isNone(lucid)) {
              yield* Metric.increment(
                confirmationDetectionLagUnavailableCounter,
              );
              yield* Effect.logWarning(
                `Confirmation detection lag unavailable for ${matchedBlock.utxo.txHash}#${matchedBlock.utxo.outputIndex.toString()}: lucid_service_unavailable`,
              );
            } else {
              const metadataResolution = yield* Effect.promise(() =>
                resolveTransactionConfirmationMetadata({
                  lucid: lucid.value.api,
                  txHash: matchedBlock.utxo.txHash,
                }),
              );
              if (metadataResolution.type === "Available") {
                confirmationMetadata = metadataResolution.metadata;
              } else {
                yield* Metric.increment(
                  confirmationDetectionLagUnavailableCounter,
                );
                yield* Effect.logWarning(
                  `Confirmation detection lag unavailable for ${matchedBlock.utxo.txHash}#${matchedBlock.utxo.outputIndex.toString()}: ${metadataResolution.reason}`,
                );
              }
            }
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
          const revivedCanonicalPayloadJournal =
            yield* reviveCanonicalPayloadJournalFromWorkerSnapshot(
              workerOutput.canonicalHeaders,
            );
          if (Option.isSome(revivedCanonicalPayloadJournal)) {
            const recoveryBlock =
              revivedCanonicalPayloadJournal.value.blockUTxO;
            if (recoveryBlock === undefined) {
              return yield* Effect.fail(
                new ConfirmationInvariantError({
                  message:
                    "Canonical payload journal recovery target is missing its serialized state-queue UTxO",
                  cause: `header_hash=${revivedCanonicalPayloadJournal.value.headerHash.toString(
                    "hex",
                  )}`,
                }),
              );
            }
            const journalBoundaryMs =
              revivedCanonicalPayloadJournal.value.journal.pipe(
                Option.match({
                  onNone: () => revivedCanonicalPayloadJournal.value.endTimeMs,
                  onSome: (journal) =>
                    journal[
                      PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
                    ].getTime(),
                }),
              );
            nextLocalBlockBoundaryMs = Math.max(
              nextLocalBlockBoundaryMs,
              revivedCanonicalPayloadJournal.value.endTimeMs,
              journalBoundaryMs,
            );
            yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
            yield* Ref.set(
              globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
              recoveryBlock,
            );
          }
        }
        if (Option.isSome(pending)) {
          yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
          yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
        }
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          nextLocalBlockBoundaryMs,
        );
        const availableConfirmedSetAtMs = Date.now();
        yield* Ref.set(
          globals.AVAILABLE_CONFIRMED_BLOCK,
          workerOutput.latestBlocksUTxO,
        );
        if (confirmationMetadata !== undefined) {
          yield* Metric.update(
            confirmationDetectionLagTimer,
            Duration.millis(
              resolveConfirmationDetectionLagMs({
                confirmationSlotUnixMs: confirmationMetadata.confirmedAtMs,
                availableConfirmedSetAtMs,
              }),
            ),
          );
        }
        if (config.SPECULATIVE_COMMIT_BUILD && metadata.headerHash !== null) {
          yield* Queue.offer(globals.COMMIT_SUBMIT_WAKE_QUEUE, {
            confirmedHeaderHash: metadata.headerHash.toString("hex"),
            confirmedTip: workerOutput.latestBlocksUTxO,
            confirmationObservedAtMs,
            confirmationWaitMs:
              submittedAtMs === 0
                ? 0
                : Math.max(0, confirmationObservedAtMs - submittedAtMs),
          });
        }
        yield* Effect.logInfo(
          Option.isSome(pending)
            ? "🔍 ☑️  Submitted block confirmed."
            : "🔍 ☑️  Canonical state_queue snapshot refreshed.",
        );
        break;
      }
      case "StaleUnconfirmedRecoveryOutput": {
        const metadata = yield* stateQueueTipMetadata(
          workerOutput.latestBlocksUTxO,
        ).pipe(Effect.orDie);
        let appliedStaleRecovery = true;
        if (
          Option.isSome(pending) &&
          pending.value[
            PendingBlockFinalizationsDB.Columns.HEADER_HASH
          ].toString("hex") === workerOutput.stalePendingHeaderHash
        ) {
          if (workerOutput.staleSubmittedTxHash === "") {
            appliedStaleRecovery =
              yield* abandonUnsubmittedPendingBlockIfStillPresent(
                pending.value,
              );
          } else {
            yield* abandonPendingBlockIfPresent(pending.value);
          }
        }
        if (!appliedStaleRecovery) {
          yield* Effect.logInfo(
            `🔍 Skipping stale unsubmitted recovery for ${workerOutput.stalePendingHeaderHash} because the pending-finalization row changed after the confirmation worker snapshot.`,
          );
          break;
        }
        yield* options.afterStaleRecoveryJournalTransition?.() ?? Effect.void;
        const activeAfterStaleRecovery =
          yield* PendingBlockFinalizationsDB.retrieveActive();
        const activeAfterStaleRecoveryIdentity =
          activePendingFinalizationIdentity(activeAfterStaleRecovery);
        if (
          staleRecoveryMustPreserveNewActiveJournal({
            captured: capturedPendingIdentity,
            current: activeAfterStaleRecoveryIdentity,
          })
        ) {
          yield* Effect.logWarning(
            `🔍 Preserving newer submission globals after stale recovery transitioned the captured journal (captured=${JSON.stringify(capturedPendingIdentity)},current=${JSON.stringify(activeAfterStaleRecoveryIdentity)}).`,
          );
          break;
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
        if (config.SPECULATIVE_COMMIT_BUILD) {
          yield* invalidateSpeculativeCommitCandidate(globals, config, "T1");
        }
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
): Effect.Effect<void, never, Globals | Database | NodeConfig> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo("🟫 Block confirmation fiber started.");
    const action = withL1ControlPlane(
      globals,
      { scope: "block_confirmation", maxHoldMs: 180_000 },
      blockConfirmationAction,
    ).pipe(
      Effect.withSpan("block-confirmation-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
