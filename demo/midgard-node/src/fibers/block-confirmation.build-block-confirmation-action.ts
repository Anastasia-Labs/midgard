import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect, Metric, Option, Queue, Ref } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { SignedIntentReplacementIntegrityError } from "../services/canonical-journal-recovery.js";
import { logOnStateChange } from "../services/globals.liveness-reasons.js";
import { Database, Globals, Lucid, NodeConfig } from "../services/index.js";
import {
  resolveTransactionConfirmationMetadata,
  type TransactionConfirmationMetadata,
} from "../transaction-confirmation-metadata.js";
import { deserializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import {
  recordConfirmationTickIdleness,
  skipIdleConfirmationTick,
} from "./block-confirmation.idle-backoff.js";
import {
  abandonPendingBlockIfPresent,
  activePendingFinalizationIdentity,
  confirmationDetectionLagTimer,
  confirmationDetectionLagUnavailableCounter,
  ConfirmationInvariantError,
  confirmationPendingSnapshotChanged,
  type ConfirmationWorkerRunner,
  observeConfirmedPendingBlock,
  resolveConfirmationDetectionLagMs,
  shouldObserveConfirmationDetectionLag,
  staleRecoveryMustPreserveNewActiveJournal,
  stateQueueTipMetadata,
  toPendingWorkerInput,
} from "./block-confirmation.record-confirmed-pending-block.js";
import {
  abandonUnsubmittedPendingBlockIfStillPresent,
  reviveCanonicalPayloadJournalFromWorkerSnapshot,
  runConfirmationWorkerInThread,
} from "./block-confirmation.run-confirmation-worker-in-thread.js";
import { emitQueueStateMetrics } from "./queue-metrics.js";
import { invalidateSpeculativeCommitCandidate } from "./speculative-commit-builder.js";

export const buildBlockConfirmationAction = (
  runWorker: ConfirmationWorkerRunner = runConfirmationWorkerInThread,
  options: {
    readonly afterPendingSnapshotGuard?: () => Effect.Effect<void>;
    readonly afterStaleRecoveryJournalTransition?: () => Effect.Effect<void>;
  } = {},
): Effect.Effect<
  void,
  | WorkerError
  | DatabaseError
  | ConfirmationInvariantError
  | SignedIntentReplacementIntegrityError,
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
    // A provably idle node refreshes its snapshot with growing gaps instead of
    // spawning a worker every tick; the heartbeat above stays fresh.
    if (yield* skipIdleConfirmationTick(globals, pending)) return;

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
          const matchedHeader = yield* SDK.getHeaderFromStateQueueDatum(
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
        if (Option.isSome(pending)) {
          yield* Effect.logInfo("🔍 ☑️  Submitted block confirmed.");
        } else {
          yield* logOnStateChange(
            globals,
            "block_confirmation_refresh",
            metadata.headerHash?.toString("hex") ?? "no_header",
            "🔍 ☑️  Canonical state_queue snapshot refreshed.",
          );
        }
        yield* recordConfirmationTickIdleness(
          globals,
          pending,
          config.WAIT_BETWEEN_BLOCK_CONFIRMATION,
        );
        break;
      }
      case "StaleUnconfirmedRecoveryOutput": {
        if (
          Option.isSome(pending) &&
          pending.value[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] !=
            null
        ) {
          // A signed commit is replaced only by the history owner, from its
          // authenticated view at an exact point (whichever block holds the
          // tail node's slot wins).
          yield* Effect.logInfo(
            "Signed commit intent is unresolved; the history owner's signed-intent reconciliation decides whether it is confirmed, replaced or revived.",
          );
          return;
        }
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
