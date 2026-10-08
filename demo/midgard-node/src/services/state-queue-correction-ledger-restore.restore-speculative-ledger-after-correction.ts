import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  MempoolInclusionsDB,
  MempoolLedgerDB,
} from "../database/index.js";
import {
  type DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import type { Database } from "./database.js";
import { Globals } from "./globals.globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import {
  ancestorRow,
  depositRow,
  type LedgerRestoreResult,
  REJECTIONS,
  type WithdrawalLedgerRestore,
} from "./state-queue-correction-ledger-restore.load-pending-txs.js";
import {
  byteaArray,
  closeRejections,
  confirmedRow,
  failure,
  findUndecidedBatchMember,
  hex,
  insertRows,
  type LedgerRow,
  loadPendingTxs,
  presentOutRefs,
  producedByRejections,
  recordRejections,
  table,
} from "./working-ledger-recompute.js";

/** The liveness source `correction_restore_batch_undecided` is raised under. */
export const CORRECTION_RESTORE_BATCH_SOURCE = "correction_restore_batch";
/**
 * The restore's batch closure reached an acceptance receipt with a member
 * that is neither pending, settled, recorded rejected, nor concluded, so
 * the restore fails and its correction step retries it. The reason clears
 * once a restore completes.
 */
export const CORRECTION_RESTORE_BATCH_UNDECIDED =
  "correction_restore_batch_undecided";

/**
 * Names an `UndecidedBatchMember` failure of `restore` as
 * `correction_restore_batch_undecided` and clears it when a restore
 * completes; the failure itself propagates unchanged. Without `Globals`
 * the restore runs as it is.
 */
const withBatchReadiness = <A, E, R>(restore: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const globals = yield* Effect.serviceOption(Globals);
    if (Option.isNone(globals)) return yield* restore;
    return yield* restore.pipe(
      Effect.tapErrorCause((cause) => {
        const undecided = findUndecidedBatchMember(cause);
        return undecided === undefined
          ? Effect.void
          : raiseLivenessIncident(
              globals.value,
              CORRECTION_RESTORE_BATCH_SOURCE,
              CORRECTION_RESTORE_BATCH_UNDECIDED,
              formatUnknownError(undecided),
            );
      }),
      Effect.tap(() =>
        clearLivenessIncident(globals.value, CORRECTION_RESTORE_BATCH_SOURCE),
      ),
    );
  });

/**
 * Restores the speculative ledger after a state-queue correction reopened
 * withdrawals and deposits, and rejects the pending transactions the
 * reopened deposits invalidate (with the batch closure; a co-member a
 * block's inclusion mark holds is settled by that block).
 */
export const restoreSpeculativeLedgerAfterCorrection = (input: {
  readonly withdrawals: readonly WithdrawalLedgerRestore[];
  readonly reopenedDepositEventIds: readonly Buffer[];
}): Effect.Effect<LedgerRestoreResult, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    if (
      input.withdrawals.length === 0 &&
      input.reopenedDepositEventIds.length === 0
    )
      return {
        restoredWithdrawalOutputs: 0,
        rejectedTransactions: [],
      } satisfies LedgerRestoreResult;
    const pending = yield* loadPendingTxs;
    const spentByPending = new Set(
      pending.flatMap(({ spent }) => spent.map(hex)),
    );

    // 1. Withdrawn outputs. An output a pending transaction spent was never
    // deleted by the withdrawal, and one already present needs nothing.
    const present = yield* presentOutRefs(
      input.withdrawals.map(({ outRef }) => outRef),
    );
    const withdrawalRows: LedgerRow[] = [];
    const withdrawalDeposits: Buffer[] = [];
    const restoredWithdrawals = new Set<string>();
    for (const withdrawal of input.withdrawals) {
      const key = hex(withdrawal.outRef);
      if (
        present.has(key) ||
        spentByPending.has(key) ||
        restoredWithdrawals.has(key)
      )
        continue;
      const deposit = yield* depositRow(
        withdrawal.l2OutRefData,
        withdrawal.outRef,
      );
      const row =
        deposit?.row ??
        (yield* confirmedRow(withdrawal.outRef)) ??
        (yield* ancestorRow(withdrawal.outRef, withdrawal.baseTailHeaderHash));
      if (row === undefined)
        return yield* Effect.fail(
          failure(
            "Cannot restore a reopened withdrawal's L2 output: no retained before-image",
            key,
          ),
        );
      if (deposit !== undefined)
        withdrawalDeposits.push(deposit.deposit[DepositsDB.Columns.ID]);
      restoredWithdrawals.add(key);
      withdrawalRows.push(row);
    }
    yield* insertRows(withdrawalRows);
    yield* DepositsDB.unconsumeByEventIds(withdrawalDeposits);

    // 2. Pending transactions that depend on a reopened deposit's output.
    const reopenedDepositOutRefs = new Map<string, LedgerRow>();
    for (const eventId of input.reopenedDepositEventIds) {
      const deposit = yield* DepositsDB.retrieveByEventId(eventId);
      if (Option.isNone(deposit))
        return yield* Effect.fail(
          failure("A reopened deposit is missing", hex(eventId)),
        );
      const entry = yield* DepositsDB.toMempoolLedgerEntry(deposit.value);
      reopenedDepositOutRefs.set(hex(entry[Ledger.Columns.OUTREF]), {
        [MempoolLedgerDB.Columns.TX_ID]: entry[Ledger.Columns.TX_ID],
        [MempoolLedgerDB.Columns.OUTREF]: entry[Ledger.Columns.OUTREF],
        [MempoolLedgerDB.Columns.OUTPUT]: entry[Ledger.Columns.OUTPUT],
        [MempoolLedgerDB.Columns.ADDRESS]: entry[Ledger.Columns.ADDRESS],
        [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: entry.source_event_id,
      });
    }
    const tainted = new Set(reopenedDepositOutRefs.keys());
    const settled = yield* MempoolInclusionsDB.markedTxIds;
    const rejected = yield* closeRejections({
      pending,
      settled: new Set(settled.map(hex)),
      onReject: (tx) => {
        for (const row of tx.produced)
          tainted.add(hex(row[MempoolLedgerDB.Columns.OUTREF]));
      },
      // Spending closure over reopened deposit outputs.
      spread: (reject, done) => {
        let changed = false;
        for (const tx of pending) {
          if (done.has(hex(tx.entry[Tx.Columns.TX_ID]))) continue;
          const spent = tx.spent.map(hex);
          if (!spent.some((outRef) => tainted.has(outRef))) continue;
          reject(
            tx,
            spent.some((outRef) => reopenedDepositOutRefs.has(outRef))
              ? "direct"
              : "dependent",
          );
          changed = true;
        }
        return changed;
      },
    });
    if (rejected.size === 0)
      return {
        restoredWithdrawalOutputs: withdrawalRows.length,
        rejectedTransactions: [],
      } satisfies LedgerRestoreResult;

    const rejectedTxs = [...rejected.values()].map(({ tx }) => tx);
    const rejectedIds = rejectedTxs.map((tx) => tx.entry[Tx.Columns.TX_ID]);
    const producedByRejected = producedByRejections(rejected);
    const external = new Map<string, Buffer>();
    for (const tx of rejectedTxs)
      for (const outRef of tx.spent)
        if (!producedByRejected.has(hex(outRef)))
          external.set(hex(outRef), outRef);

    // Outputs of the rejected set are either unspent, or spent inside it.
    yield* sql`DELETE FROM mempool_ledger
      WHERE outref = ANY(${pg.array(byteaArray([...producedByRejected.values()].map((row) => row[MempoolLedgerDB.Columns.OUTREF])))}::bytea[])`;

    const externalRefs = [...external.values()];
    const occupied = yield* presentOutRefs(externalRefs);
    if (occupied.size !== 0)
      return yield* Effect.fail(
        failure(
          "A rejected transaction's input is still unspent in the ledger",
          [...occupied].join(","),
        ),
      );
    // Exact before-images first: the acceptance receipts that retained them.
    const fromReceipts = yield* sql<{ outref: Buffer }>`
      INSERT INTO mempool_ledger
      SELECT DISTINCT ON (old.outref) old.*
      FROM event_history_l2_ledger_receipts r,
        LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_before) old
      WHERE r.reversed_at_revision IS NULL
        AND r.tx_ids && ${pg.array(byteaArray(rejectedIds))}::bytea[]
        AND old.outref = ANY(${pg.array(byteaArray(externalRefs))}::bytea[])
      ORDER BY old.outref, r.sequence DESC
      RETURNING outref`;
    const restored = new Set(fromReceipts.map((row) => hex(row.outref)));
    const producedByPending = new Map(
      pending.flatMap((tx) =>
        tx.produced.map(
          (row) => [hex(row[MempoolLedgerDB.Columns.OUTREF]), row] as const,
        ),
      ),
    );
    const rebuilt: LedgerRow[] = [];
    for (const [key, outRef] of external) {
      if (restored.has(key)) continue;
      const row =
        reopenedDepositOutRefs.get(key) ??
        producedByPending.get(key) ??
        (yield* confirmedRow(outRef));
      if (row === undefined)
        return yield* Effect.fail(
          failure(
            "Cannot reverse a rejected transaction: an input has no retained before-image",
            key,
          ),
        );
      rebuilt.push(row);
    }
    yield* insertRows(rebuilt);
    // Acceptance marked a spent deposit consumed; its output is back.
    const restoredDepositIds = yield* sql<{ source_event_id: Buffer }>`
      SELECT source_event_id FROM mempool_ledger
      WHERE outref = ANY(${pg.array(byteaArray(externalRefs))}::bytea[])
        AND source_event_id IS NOT NULL`;
    yield* DepositsDB.unconsumeByEventIds(
      restoredDepositIds.map((row) => row.source_event_id),
    );

    // Undo the acceptance itself: the terminal admission, its address
    // history, and the batch receipts, which no longer describe a pending
    // ledger overlay (the before-images above were read from them first).
    yield* recordRejections(rejected, REJECTIONS, settled);
    return {
      restoredWithdrawalOutputs: withdrawalRows.length,
      rejectedTransactions: rejectedIds,
    } satisfies LedgerRestoreResult;
  }).pipe(
    sqlErrorToDatabaseError(
      table,
      "Failed to restore the speculative ledger after a state-queue correction",
    ),
    withBatchReadiness,
  );
