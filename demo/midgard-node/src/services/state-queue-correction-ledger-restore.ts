import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Option } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import {
  DepositsDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  PendingBlockFinalizationsDB,
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
import { resolveTxDeltaForCommit } from "../mpf/commit-rejection.js";
import type { Database } from "./database.js";

/**
 * Speculative-ledger repair after a state-queue correction reopened removed
 * blocks. Runs inside the reinclusion transaction, after every removed
 * block's payloads returned to the pending sets.
 *
 * Two local effects outlive a removed block and are undone here:
 *
 * 1. Local finalization deleted each valid withdrawal's L2 output from
 *    `mempool_ledger`. The withdrawal is pending again, so its output is
 *    restored with its exact bytes (and deposit origin, when it had one).
 * 2. A reopened deposit's output is no longer spendable until the deposit is
 *    committed again, and a transaction spending it in the same block is
 *    refused at commit. Every pending transaction that spends such an output,
 *    directly or through another refused transaction's output, is rejected
 *    now, and its ledger effects are reversed from exact before-images.
 *
 * A rejection undoes the whole acceptance: the admission becomes terminally
 * rejected, its address history goes, and every acceptance receipt it belongs
 * to is marked reversed. A receipt is the inverse of one accepted batch and
 * cannot be split, so a rejection widens to every member of a batch it
 * touches (`E_REWIND_BATCH_MEMBER`); a batch with a member that already left
 * the pending sets cannot be reversed at all, and the repair refuses.
 *
 * A before-image this node cannot prove is never guessed: the repair fails and
 * the whole reinclusion transaction rolls back.
 */

export const REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT =
  "E_REWIND_REOPENED_DEPOSIT_INPUT";
export const REWIND_REJECT_CODE_DEPENDENT_INPUT = "E_REWIND_DEPENDENT_INPUT";
/** A transaction accepted in one batch with a rejected transaction: the
 * batch's acceptance receipt is reversed as a whole. */
export const REWIND_REJECT_CODE_BATCH_MEMBER = "E_REWIND_BATCH_MEMBER";

type RejectionReason = "direct" | "dependent" | "batch";
const REJECTIONS: Readonly<
  Record<RejectionReason, Readonly<{ code: string; detail: string }>>
> = {
  direct: {
    code: REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
    detail:
      "Transaction spends the output of a deposit reopened by a state-queue correction",
  },
  dependent: {
    code: REWIND_REJECT_CODE_DEPENDENT_INPUT,
    detail:
      "Transaction spends an output of a transaction rejected after a state-queue correction",
  },
  batch: {
    code: REWIND_REJECT_CODE_BATCH_MEMBER,
    detail:
      "Transaction was accepted in one batch with a transaction rejected after a state-queue correction",
  },
};

const table = MempoolLedgerDB.tableName;
const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });
const hex = (value: Buffer) => value.toString("hex");
const byteaArray = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);
const ROOT_TAIL_HEADER_HASH = Buffer.alloc(28);

export type WithdrawalLedgerRestore = Readonly<{
  /** 38-byte ledger key of the withdrawn L2 output. */
  outRef: Buffer;
  /** The withdrawal's l2_outref (Data OutputReference); a deposit's event id
   * uses the same encoding. */
  l2OutRefData: Buffer;
  /** Header whose base state held the output. */
  baseTailHeaderHash: Buffer;
}>;

export type LedgerRestoreResult = Readonly<{
  restoredWithdrawalOutputs: number;
  rejectedTransactions: readonly Buffer[];
}>;

type LedgerRow = MempoolLedgerDB.EntryNoTimeStamp;

type PendingTx = Readonly<{
  entry: Tx.EntryWithTimeStamp;
  source: "mempool" | "processed";
  spent: readonly Buffer[];
  produced: readonly LedgerRow[];
}>;

const producedRow = (
  txId: Buffer,
  entry: Ledger.MinimalEntry,
): Effect.Effect<LedgerRow, DatabaseError> =>
  Effect.try({
    try: () => ({
      [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(txId),
      [MempoolLedgerDB.Columns.OUTREF]: Buffer.from(
        entry[Ledger.Columns.OUTREF],
      ),
      [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from(
        entry[Ledger.Columns.OUTPUT],
      ),
      [MempoolLedgerDB.Columns.ADDRESS]: encodeMidgardAddressText(
        decodeMidgardTxOutput(entry[Ledger.Columns.OUTPUT]).address,
      ),
      [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
    }),
    catch: (cause) =>
      failure("Pending transaction output is not a canonical ledger output", {
        txId: hex(txId),
        cause,
      }),
  });

/** Every pending transaction whose ledger effects are in `mempool_ledger`,
 * in admission order, with its exact spends and outputs. */
const loadPendingTxs = Effect.gen(function* () {
  const mempool = yield* Tx.retrieveAllEntries(MempoolDB.tableName);
  const processed = yield* Tx.retrieveAllEntries(ProcessedMempoolDB.tableName);
  const seen = new Set<string>();
  const ordered: {
    entry: Tx.EntryWithTimeStamp;
    source: PendingTx["source"];
  }[] = [];
  for (const [entries, source] of [
    [mempool, "mempool"],
    [processed, "processed"],
  ] as const)
    for (const entry of entries) {
      const id = hex(entry[Tx.Columns.TX_ID]);
      if (seen.has(id)) continue;
      seen.add(id);
      ordered.push({ entry, source });
    }
  ordered.sort(
    (left, right) =>
      left.entry[Tx.Columns.TIMESTAMPTZ].getTime() -
        right.entry[Tx.Columns.TIMESTAMPTZ].getTime() ||
      Buffer.compare(
        left.entry[Tx.Columns.TX_ID],
        right.entry[Tx.Columns.TX_ID],
      ),
  );
  const deltas = yield* MempoolTxDeltasDB.retrieveByTxIds(
    ordered.map(({ entry }) => entry[Tx.Columns.TX_ID]),
  );
  const pending: PendingTx[] = [];
  for (const { entry, source } of ordered) {
    const txId = entry[Tx.Columns.TX_ID];
    const resolved = yield* resolveTxDeltaForCommit(
      entry,
      deltas.get(hex(txId)),
    );
    if (resolved._tag === "Rejected")
      return yield* Effect.fail(
        failure(
          "A pending transaction cannot be decoded, so its dependence on reopened state cannot be decided",
          hex(txId),
        ),
      );
    const produced: LedgerRow[] = [];
    for (const output of resolved.produced)
      produced.push(yield* producedRow(txId, output));
    pending.push({ entry, source, spent: resolved.spent, produced });
  }
  return pending;
});

const presentOutRefs = (outRefs: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (outRefs.length === 0) return new Set<string>();
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const rows = yield* sql<{ outref: Buffer }>`
      SELECT outref FROM mempool_ledger
      WHERE outref = ANY(${pg.array(byteaArray(outRefs))}::bytea[])`;
    return new Set(rows.map((row) => hex(row.outref)));
  });

/** The deposit whose ledger output is `outRef`, if `eventId` names one. */
const depositRow = (eventId: Buffer, outRef: Buffer) =>
  Effect.gen(function* () {
    const deposit = yield* DepositsDB.retrieveByEventId(eventId);
    if (Option.isNone(deposit)) return undefined;
    const entry = yield* DepositsDB.toMempoolLedgerEntry(deposit.value);
    if (!entry[Ledger.Columns.OUTREF].equals(outRef)) return undefined;
    return {
      row: {
        [MempoolLedgerDB.Columns.TX_ID]: entry[Ledger.Columns.TX_ID],
        [MempoolLedgerDB.Columns.OUTREF]: entry[Ledger.Columns.OUTREF],
        [MempoolLedgerDB.Columns.OUTPUT]: entry[Ledger.Columns.OUTPUT],
        [MempoolLedgerDB.Columns.ADDRESS]: entry[Ledger.Columns.ADDRESS],
        [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: entry.source_event_id,
      } satisfies LedgerRow,
      deposit: deposit.value,
    };
  });

/** A merged output: the confirmed ledger holds every merged block's state. */
const confirmedRow = (outRef: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Ledger.EntryNoTimeStamp>`
      SELECT tx_id, outref, output, address FROM confirmed_ledger
      WHERE outref = ${outRef}`;
    const row = rows[0];
    return row === undefined
      ? undefined
      : ({
          [MempoolLedgerDB.Columns.TX_ID]: row[Ledger.Columns.TX_ID],
          [MempoolLedgerDB.Columns.OUTREF]: row[Ledger.Columns.OUTREF],
          [MempoolLedgerDB.Columns.OUTPUT]: row[Ledger.Columns.OUTPUT],
          [MempoolLedgerDB.Columns.ADDRESS]: row[Ledger.Columns.ADDRESS],
          [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
        } satisfies LedgerRow);
  });

/** An output produced by an unmerged ancestor block, from its journal's
 * retained ledger delta. The walk follows base-tail links to the root. */
const ancestorRow = (outRef: Buffer, baseTailHeaderHash: Buffer) =>
  Effect.gen(function* () {
    const seen = new Set<string>();
    let cursor = baseTailHeaderHash;
    while (!cursor.equals(ROOT_TAIL_HEADER_HASH) && !seen.has(hex(cursor))) {
      seen.add(hex(cursor));
      const journal =
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(cursor);
      if (Option.isNone(journal)) return undefined;
      const record = journal.value;
      const member = record.ledgerDelta.produced.find((produced) =>
        produced[PendingBlockFinalizationsDB.UtxoColumns.OUTREF].equals(outRef),
      );
      if (member !== undefined) {
        const output = member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT];
        return yield* Effect.try({
          try: () =>
            ({
              [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(
                decodeMidgardSpendInputItem(outRef).txId,
              ),
              [MempoolLedgerDB.Columns.OUTREF]: Buffer.from(outRef),
              [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from(output),
              [MempoolLedgerDB.Columns.ADDRESS]: encodeMidgardAddressText(
                decodeMidgardTxOutput(output).address,
              ),
              [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
            }) satisfies LedgerRow,
          catch: (cause) =>
            failure("Retained journal output is not canonical", {
              outRef: hex(outRef),
              cause,
            }),
        });
      }
      cursor =
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH];
    }
    return undefined;
  });

/** Restores a deposit a restored output belongs to: it is spendable again
 * exactly when its header assignment says so. */
const unconsumeDeposits = (eventIds: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (eventIds.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE deposits_utxos
      SET status = ${DepositsDB.Status.Projected}
      WHERE event_id IN ${sql.in(eventIds)}
        AND status = ${DepositsDB.Status.Consumed}`;
  });

const insertRows = (rows: readonly LedgerRow[]) =>
  Effect.gen(function* () {
    if (rows.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const inserted = yield* sql<{ outref: Buffer }>`
      INSERT INTO mempool_ledger ${sql.insert([...rows])}
      ON CONFLICT (outref) DO NOTHING RETURNING outref`;
    if (inserted.length !== rows.length)
      return yield* Effect.fail(
        failure("A restored ledger output is already present"),
      );
  });

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
    yield* unconsumeDeposits(withdrawalDeposits);

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
    yield* unconsumeDeposits(
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
