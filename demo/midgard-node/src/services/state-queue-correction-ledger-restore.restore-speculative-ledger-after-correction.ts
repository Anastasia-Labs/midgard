import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Option } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import {
  DepositsDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import type { Database } from "./database.js";
import {
  ancestorRow,
  byteaArray,
  confirmedRow,
  depositRow,
  failure,
  hex,
  insertRows,
  type LedgerRestoreResult,
  type LedgerRow,
  loadPendingTxs,
  type PendingTx,
  presentOutRefs,
  type RejectionReason,
  REJECTIONS,
  table,
  type WithdrawalLedgerRestore,
} from "./state-queue-correction-ledger-restore.load-pending-txs.js";

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
    const pendingById = new Map(
      pending.map((tx) => [hex(tx.entry[Tx.Columns.TX_ID]), tx] as const),
    );
    const rejected = new Map<
      string,
      { tx: PendingTx; reason: RejectionReason }
    >();
    const reject = (tx: PendingTx, reason: RejectionReason) => {
      rejected.set(hex(tx.entry[Tx.Columns.TX_ID]), { tx, reason });
      for (const row of tx.produced)
        tainted.add(hex(row[MempoolLedgerDB.Columns.OUTREF]));
    };
    for (let widened = true; widened; ) {
      widened = false;
      // Spending closure over reopened deposit outputs.
      for (let changed = true; changed; ) {
        changed = false;
        for (const tx of pending) {
          if (rejected.has(hex(tx.entry[Tx.Columns.TX_ID]))) continue;
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
      }
      if (rejected.size === 0) break;
      // Batch closure: every co-member of an unreversed acceptance receipt.
      const coMembers = yield* sql<{ sequence: string; tx_id: Buffer }>`
        SELECT r.sequence::text AS sequence, ids.tx_id
        FROM event_history_l2_ledger_receipts r, unnest(r.tx_ids) AS ids(tx_id)
        WHERE r.reversed_at_revision IS NULL
          AND r.tx_ids && ${pg.array(byteaArray([...rejected.values()].map(({ tx }) => tx.entry[Tx.Columns.TX_ID])))}::bytea[]
        ORDER BY r.sequence, ids.tx_id`;
      for (const { sequence, tx_id } of coMembers) {
        const id = hex(tx_id);
        if (rejected.has(id)) continue;
        const tx = pendingById.get(id);
        if (tx === undefined)
          return yield* Effect.fail(
            failure(
              "A rejected transaction was accepted in one batch with a transaction that is no longer pending, so the batch's acceptance cannot be reversed",
              { receipt: sequence, txId: id },
            ),
          );
        reject(tx, "batch");
        widened = true;
      }
    }
    if (rejected.size === 0)
      return {
        restoredWithdrawalOutputs: withdrawalRows.length,
        rejectedTransactions: [],
      } satisfies LedgerRestoreResult;

    const rejectedTxs = [...rejected.values()].map(({ tx }) => tx);
    const rejectedIds = rejectedTxs.map((tx) => tx.entry[Tx.Columns.TX_ID]);
    const producedByRejected = new Map(
      rejectedTxs.flatMap((tx) =>
        tx.produced.map(
          (row) => [hex(row[MempoolLedgerDB.Columns.OUTREF]), row] as const,
        ),
      ),
    );
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

    const mempoolIds = rejectedTxs
      .filter(({ source }) => source === "mempool")
      .map((tx) => tx.entry[Tx.Columns.TX_ID]);
    const processedIds = rejectedTxs
      .filter(({ source }) => source === "processed")
      .map((tx) => tx.entry[Tx.Columns.TX_ID]);
    if (mempoolIds.length > 0) yield* MempoolDB.clearTxs(mempoolIds);
    if (processedIds.length > 0) {
      yield* ProcessedMempoolDB.clearTxs(processedIds);
      yield* MempoolTxDeltasDB.clearTxs(processedIds);
    }
    const rejections = [...rejected.entries()].map(([id, { reason }]) => ({
      txId: Buffer.from(id, "hex"),
      ...REJECTIONS[reason],
    }));
    yield* TxRejectionsDB.insertMany(
      rejections.map(({ txId, code, detail }) => ({
        [TxRejectionsDB.Columns.TX_ID]: txId,
        [TxRejectionsDB.Columns.REJECT_CODE]: code,
        [TxRejectionsDB.Columns.REJECT_DETAIL]: detail,
      })),
    );
    // Undo the acceptance itself: the terminal admission, its address
    // history, and the batch receipts, which no longer describe a pending
    // ledger overlay (the before-images above were read from them first).
    yield* TxAdmissionsDB.markAcceptedRejectedAfterCorrection(rejections);
    yield* sql`DELETE FROM address_history
      WHERE tx_id = ANY(${pg.array(byteaArray(rejectedIds))}::bytea[])`;
    yield* sql`UPDATE event_history_l2_ledger_receipts r
      SET reversed_at_revision = c.revision
      FROM event_history_cursor c
      WHERE c.binding_digest = r.binding_digest
        AND r.reversed_at_revision IS NULL
        AND r.tx_ids <@ ${pg.array(byteaArray(rejectedIds))}::bytea[]`;
    const unreversed = yield* sql<{ sequence: string }>`
      SELECT sequence::text AS sequence FROM event_history_l2_ledger_receipts
      WHERE reversed_at_revision IS NULL
        AND tx_ids && ${pg.array(byteaArray(rejectedIds))}::bytea[]
      LIMIT 1`;
    if (unreversed.length !== 0)
      return yield* Effect.fail(
        failure(
          "A rejected transaction's acceptance receipt could not be reversed",
          unreversed[0]?.sequence,
        ),
      );
    yield* CekProgramMaterialDB.releaseAdmissionOwnership(rejectedIds);
    return {
      restoredWithdrawalOutputs: withdrawalRows.length,
      rejectedTransactions: rejectedIds,
    } satisfies LedgerRestoreResult;
  }).pipe(
    sqlErrorToDatabaseError(
      table,
      "Failed to restore the speculative ledger after a state-queue correction",
    ),
  );
