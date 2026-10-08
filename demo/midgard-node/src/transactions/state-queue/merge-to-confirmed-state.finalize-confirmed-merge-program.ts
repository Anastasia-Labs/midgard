import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Metric, Option, Ref } from "effect";

import {
  BlocksDB,
  DepositsDB,
  ForcedTransactionsDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../../database/index.js";
import * as MempoolInclusionsDB from "../../database/mempoolInclusions.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../../database/utils/common.js";
import { formatLandedStateQueue } from "../../l1-state-queue/index.js";
import { recordSettlements } from "../../landed-blocks/settlements.js";
import { withHistoryWrite } from "../../services/event-history-producer.js";
import { Database, Globals } from "../../services/index.js";
import {
  readLandedStateQueue,
  stateQueueContractOf,
} from "../../services/landed-state-queue.js";
import {
  assertNativeMpfHashHex,
  type NativeMpfOwnerService,
} from "../../services/mpf-native-owner/index.js";
import {
  applyConfirmedLedgerDeltaChainTransaction,
  type ConfirmedLedgerSnapshot,
  materializeConfirmedMergeLedgerSnapshot,
} from "./confirmed-ledger-snapshot.js";

export const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merged blocks",
  bigint: true,
  incremental: true,
});

export type ConfirmedMergeNativeOwnerObservation = {
  readonly confirmedLedgerEntryCount: number;
  readonly confirmedLedgerRoot: string;
  readonly durableLedgerRoot: string;
  readonly activeGenerations: number;
};

/**
 * Folds an own merged block into `confirmed_ledger` in one transaction. The
 * block's transactions (`includedTxIds`, its journal members) settle their
 * receipt members for good (recorded here too, so a block that folds with
 * no rebase between still records them), and the pending-table rows the
 * block marked are deleted.
 */
export const finalizeConfirmedMergeTransaction = ({
  headerHash,
  snapshot,
  projectedDepositEventIds,
  projectedWithdrawalEventIds,
  projectedForcedTransactionEventIds,
  includedTxIds,
}: {
  readonly headerHash: Buffer;
  readonly snapshot: ConfirmedLedgerSnapshot;
  readonly projectedDepositEventIds: readonly Buffer[];
  readonly projectedWithdrawalEventIds: readonly Buffer[];
  readonly projectedForcedTransactionEventIds: readonly Buffer[];
  readonly includedTxIds: readonly Buffer[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* Effect.logInfo(
          `🔸 Apply finalized confirmed-ledger V1 delta chain (deltas=${snapshot.deltaChain.length.toString()},root=${snapshot.root})...`,
        );
        yield* applyConfirmedLedgerDeltaChainTransaction(snapshot);
        yield* Effect.logInfo("🔸 Clear block from BlocksDB...");
        yield* BlocksDB.clearBlock(headerHash).pipe(
          Effect.withSpan("clear-block-from-BlocksDB"),
        );
        yield* DepositsDB.markConsumedByEventIds(projectedDepositEventIds).pipe(
          Effect.withSpan("mark-merged-deposits-consumed"),
        );
        yield* WithdrawalsDB.markFinalizedByEventIds(
          projectedWithdrawalEventIds,
          headerHash,
        ).pipe(Effect.withSpan("mark-merged-withdrawals-finalized"));
        yield* ForcedTransactionsDB.markFinalizedByEventIds(
          projectedForcedTransactionEventIds,
          headerHash,
        ).pipe(Effect.withSpan("mark-merged-forced-transactions-finalized"));
        yield* recordSettlements([
          { headerHash: headerHash.toString("hex"), txIds: includedTxIds },
        ]);
        yield* MempoolInclusionsDB.deleteIncluded(headerHash);
      }),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      "confirmed_merge_finalization",
      "Failed to finalize confirmed-state merge locally",
    ),
  );

/**
 * A confirmed-state merge advances the L1 queue head but does not change the
 * latest committed L2 tail. The node therefore retains the root already
 * promoted by its single native owner and verifies that owner is live instead
 * of reopening the owner's LevelDB path through MidgardMpf.
 */
export const observeNativeOwnerAfterConfirmedMerge = ({
  nativeMpfOwner,
  confirmedLedgerEntryCount,
  confirmedLedgerRoot,
}: {
  readonly nativeMpfOwner:
    | Pick<NativeMpfOwnerService, "diagnostics">
    | undefined;
  readonly confirmedLedgerEntryCount: number;
  readonly confirmedLedgerRoot: string;
}): Effect.Effect<ConfirmedMergeNativeOwnerObservation, Error> =>
  Effect.tryPromise({
    try: async () => {
      if (nativeMpfOwner === undefined) {
        throw new Error(
          "Architecture G native owner is not initialized during confirmed-state merge finalization",
        );
      }
      const diagnostics = await nativeMpfOwner.diagnostics();
      assertNativeMpfHashHex(
        diagnostics.durableRoot,
        "Architecture G durable root",
      );
      return {
        confirmedLedgerEntryCount,
        confirmedLedgerRoot,
        durableLedgerRoot: diagnostics.durableRoot,
        activeGenerations: diagnostics.activeGenerations,
      };
    },
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Architecture G owner observation failed", { cause }),
  });

/**
 * Finalizes one L1-confirmed merge into the local database under its
 * confirmed-merge job: folds the header's ledger delta chain into the
 * confirmed ledger, clears its block rows, marks its projected events, then
 * confirms the native MPF owner is live (see
 * observeNativeOwnerAfterConfirmedMerge). `headerUtxosRoot` is the header's committed
 * UTxO root, which the folded ledger must reach.
 *
 * Idempotent, so a failed or interrupted attempt can simply run again: a
 * ledger already at the journal's expected root is not folded twice, and every
 * other step converges. A failure records the job as failed.
 */
export const finalizeConfirmedMergeProgram = ({
  headerHash,
  headerUtxosRoot,
}: {
  readonly headerHash: Buffer;
  readonly headerUtxosRoot: string;
}): Effect.Effect<void, DatabaseError, Database | Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const jobId = MutationJobsDB.confirmedMergeFinalizationJobId(
      headerHash.toString("hex"),
    );
    const projectedDepositEntries =
      yield* DepositsDB.retrieveByProjectedHeaderHash(headerHash);
    const projectedForcedTransactionEntries =
      yield* ForcedTransactionsDB.retrieveByProjectedHeaderHash(headerHash);
    const projectedWithdrawalEntries =
      yield* WithdrawalsDB.retrieveByProjectedHeaderHash(headerHash);
    const projectedDepositEventIds = projectedDepositEntries.map(
      (entry) => entry[DepositsDB.Columns.ID],
    );
    const projectedWithdrawalEventIds = projectedWithdrawalEntries.map(
      (entry) => entry[WithdrawalsDB.Columns.ID],
    );
    const projectedForcedTransactionEventIds =
      projectedForcedTransactionEntries.map(
        (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
      );
    const finalizedJournal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    if (Option.isNone(finalizedJournal)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Failed to finalize confirmed-state merge locally because the pending-finalization journal is missing",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
    const confirmedLedgerSnapshot =
      yield* materializeConfirmedMergeLedgerSnapshot(finalizedJournal.value);
    const confirmedLedgerSnapshotRoot = confirmedLedgerSnapshot.root;
    const expectedSnapshotRoot =
      finalizedJournal.value[
        PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
      ];
    if (
      confirmedLedgerSnapshotRoot !== expectedSnapshotRoot ||
      confirmedLedgerSnapshotRoot !== headerUtxosRoot
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Failed to finalize confirmed-state merge locally because the durable UTxO snapshot root does not match the confirmed block",
          cause: `header_hash=${headerHash.toString(
            "hex",
          )},snapshot_root=${confirmedLedgerSnapshotRoot},journal_expected_root=${expectedSnapshotRoot},confirmed_header_root=${headerUtxosRoot}`,
        }),
      );
    }
    yield* MutationJobsDB.start({
      jobId,
      kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
      payload: {
        headerHash: headerHash.toString("hex"),
        depositEventCount: projectedDepositEventIds.length,
        forcedTransactionEventCount: projectedForcedTransactionEventIds.length,
        withdrawalEventCount: projectedWithdrawalEventIds.length,
        confirmedLedgerSnapshotRoot,
        ledgerDeltaSpentCount: confirmedLedgerSnapshot.delta.spent.length,
        ledgerDeltaProducedCount: confirmedLedgerSnapshot.delta.produced.length,
      },
    });
    yield* finalizeConfirmedMergeTransaction({
      headerHash,
      snapshot: confirmedLedgerSnapshot,
      projectedDepositEventIds,
      projectedWithdrawalEventIds,
      projectedForcedTransactionEventIds,
      includedTxIds: finalizedJournal.value.mempoolTxIds,
    });
    const ownerObservation = yield* observeNativeOwnerAfterConfirmedMerge({
      nativeMpfOwner: yield* Ref.get(globals.NATIVE_MPF_OWNER),
      confirmedLedgerEntryCount: confirmedLedgerSnapshot.entries.length,
      confirmedLedgerRoot: confirmedLedgerSnapshotRoot,
    }).pipe(
      Effect.mapError(
        (error) =>
          new DatabaseError({
            table: "confirmed_merge_finalization",
            message:
              "Failed to observe the native MPF owner after confirmed-state merge",
            cause: formatUnknownError(error),
          }),
      ),
    );
    yield* Effect.logInfo(
      `🔸 Retained Architecture G owner after merge local finalization (header=${headerHash.toString(
        "hex",
      )},confirmed_ledger_entries=${ownerObservation.confirmedLedgerEntryCount.toString()},confirmed_ledger_root=${ownerObservation.confirmedLedgerRoot},durable_tail_root=${ownerObservation.durableLedgerRoot},active_generations=${ownerObservation.activeGenerations.toString()}).`,
    );
    yield* MutationJobsDB.markCompleted(jobId);
  }).pipe(
    Effect.tapError((error) =>
      MutationJobsDB.markFailed(
        MutationJobsDB.confirmedMergeFinalizationJobId(
          headerHash.toString("hex"),
        ),
        formatUnknownError(error),
      ).pipe(Effect.catchAll(() => Effect.void)),
    ),
  );

/** A landed merge whose local finalization the catch-up has to run. */
export type LandedUnfinalizedMerge = {
  readonly headerHash: Buffer;
  readonly headerUtxosRoot: string;
};

/**
 * Upper bound on the headers one catch-up walks back through. Every merge
 * attempt runs the catch-up first and fails while it fails, so at most the
 * merges that landed while the node was down or recovering are unfinalized.
 */
export const MAX_LANDED_MERGE_CATCH_UP = 1_000;

/** The confirmed state in the landed queue's root (P1, never L1). */
export const fetchLandedConfirmedState = (
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<
  SDK.ConfirmedState,
  SDK.StateQueueError | SDK.DataCoercionError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const read = yield* readLandedStateQueue(stateQueueContractOf(fetchConfig));
    const root = read.kind === "ok" ? read.queue.root : null;
    if (root === null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "The landed state queue has no single root",
          cause:
            read.kind === "ok"
              ? formatLandedStateQueue(read.queue)
              : `${read.kind}: ${read.detail}`,
        }),
      );
    }
    return (yield* SDK.getConfirmedStateFromStateQueueDatum(root.element.datum))
      .data;
  });
