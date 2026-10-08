import * as SDK from "@al-ft/midgard-sdk";
import { Data, Effect, Metric, Option } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import {
  Database,
  Globals,
  NodeConfig,
  publishMempoolLedgerDelta,
} from "../services/index.js";
import { deserializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import {
  WorkerInput as BlockConfirmationWorkerInput,
  WorkerOutput as BlockConfirmationWorkerOutput,
} from "../workers/utils/confirm-block-commitments.js";

export const confirmationDetectionLagTimer = Metric.timer(
  "confirmation_detection_lag_ms",
  "Milliseconds from the authoritative Kupo creation slot to AVAILABLE_CONFIRMED_BLOCK publication",
);

export const confirmationDetectionLagUnavailableCounter = Metric.counter(
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
  readonly intendedTxHash?: string | null;
  readonly status: PendingBlockFinalizationsDB.Status;
};

export const activePendingFinalizationIdentity = (
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
      intendedTxHash:
        row[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH]?.toString(
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
  captured?.intendedTxHash !== current?.intendedTxHash ||
  captured?.status !== current?.status;

export const staleRecoveryMustPreserveNewActiveJournal = ({
  captured,
  current,
}: {
  readonly captured: ActivePendingFinalizationIdentity | null;
  readonly current: ActivePendingFinalizationIdentity | null;
}): boolean =>
  current !== null && confirmationPendingSnapshotChanged({ captured, current });

export type ConfirmationWorkerRunner = (
  input: BlockConfirmationWorkerInput,
) => Effect.Effect<BlockConfirmationWorkerOutput, WorkerError, never>;

export class ConfirmationInvariantError extends Data.TaggedError(
  "ConfirmationInvariantError",
)<{
  readonly message: string;
  readonly cause: string;
}> {}

export const stateQueueTipMetadata = (
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

export const toPendingWorkerInput = (
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
      intendedTxHash:
        record[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH]?.toString(
          "hex",
        ) ?? null,
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

/**
 * Records the L1 observation of the active journal's block: its members are
 * assigned to it and it moves to observed (or straight to finalized when it
 * has nothing left to finalize locally). Returns whether local finalization
 * still has to run. SQL only; the caller publishes cache state.
 */
export const recordConfirmedPendingBlock = (
  record: PendingBlockFinalizationsDB.Record,
  recoveredSubmittedTxHash: Buffer | null,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const journalHeaderHash =
      record[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
    return yield* withFollowerWrite(
      Effect.gen(function* () {
        yield* PendingBlockFinalizationsDB.assertCanonicalEventMembers(record);
        yield* DepositsDB.markProjectedByEventIds(
          record.depositEventIds,
          journalHeaderHash,
        );
        yield* ForcedTransactionsDB.markProjectedByEventIds(
          record.forcedTransactionEventIds,
          journalHeaderHash,
        );
        yield* WithdrawalsDB.markProjectedByEventIds(
          record.withdrawalMembers.map(
            PendingBlockFinalizationsDB.withdrawalMemberToAssignment,
          ),
          journalHeaderHash,
        );
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
              // A linked-list continuation changes the current outref, not the
              // transaction that originally signed this pending commitment.
              record[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] !=
                null &&
                !record[
                  PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH
                ]!.equals(recoveredSubmittedTxHash ?? Buffer.alloc(0))
                ? undefined
                : (recoveredSubmittedTxHash ?? undefined),
            )
          : PendingBlockFinalizationsDB.markFinalized(journalHeaderHash);
        return requiresLocalFinalizationRecovery;
      }),
    );
  });

/** Publishes the header-assigned deposit rows to the validation cache. */
const publishProjectedDeposits = (
  depositEventIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database | Globals | NodeConfig> =>
  Effect.gen(function* () {
    if (depositEventIds.length === 0) return;
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    const projectedEntries =
      yield* MempoolLedgerDB.retrieveBySourceEventIds(depositEventIds);
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
  });

export const observeConfirmedPendingBlock = (
  record: PendingBlockFinalizationsDB.Record,
  recoveredSubmittedTxHash: Buffer | null,
): Effect.Effect<boolean, DatabaseError, Database | Globals | NodeConfig> =>
  Effect.gen(function* () {
    const requiresLocalFinalizationRecovery =
      yield* recordConfirmedPendingBlock(record, recoveredSubmittedTxHash);
    yield* publishProjectedDeposits(record.depositEventIds);
    return requiresLocalFinalizationRecovery;
  });

export const abandonPendingBlockIfPresent = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* PendingBlockFinalizationsDB.assertCanonicalEventMembers(record);
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
  }).pipe(withFollowerWrite);
