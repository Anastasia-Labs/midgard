import { SqlClient, type Statement } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import {
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Tx from "../database/utils/tx.js";
import {
  byteaArray,
  failure,
  hex,
  type PendingTx,
  table,
} from "./working-ledger-recompute.pending-txs.js";

/** Why a pending transaction left the working ledger: its own input became
 * unavailable, it spends a rejected transaction's output, or it was accepted
 * in one batch with a rejected transaction. */
export type RejectionReason = "direct" | "dependent" | "batch";

export type RejectionCodes = Readonly<
  Record<RejectionReason, Readonly<{ code: string; detail: string }>>
>;

export type Rejection = Readonly<{ tx: PendingTx; reason: RejectionReason }>;

/** The code and detail one rejection is recorded with, by hex transaction id. */
export type RejectionCodeOf = (
  id: string,
  rejection: Rejection,
) => Readonly<{ code: string; detail: string }>;

/** Rejections keyed by hex transaction id. */
export type Rejections = ReadonlyMap<string, Rejection>;

export const txIdHex = (tx: PendingTx) => hex(tx.entry[Tx.Columns.TX_ID]);

/**
 * A rejection reaches an unreversed receipt with a member that is neither
 * pending, settled, nor recorded rejected on it: the closure cannot decide
 * that member, so the batch's acceptance cannot be reversed.
 */
export class UndecidedBatchMember extends DatabaseError {}

type ReceiptMember = {
  sequence: string;
  tx_id: Buffer;
  settled: boolean;
  rejected_earlier: boolean;
};

/** The members of the unreversed receipts `which` selects, receipt by receipt. */
const receiptMembers = (
  sql: SqlClient.SqlClient,
  which: Statement.Fragment,
) => sql<ReceiptMember>`
  SELECT r.sequence::text AS sequence, ids.tx_id,
    EXISTS (SELECT 1 FROM event_history_l2_ledger_receipt_settlements s
      WHERE s.receipt_sequence = r.sequence AND s.tx_id = ids.tx_id)
      AS settled,
    EXISTS (SELECT 1 FROM event_history_l2_ledger_receipt_rejections x
      WHERE x.receipt_sequence = r.sequence AND x.tx_id = ids.tx_id)
      AS rejected_earlier
  FROM event_history_l2_ledger_receipts r, unnest(r.tx_ids) AS ids(tx_id)
  WHERE r.reversed_at_revision IS NULL AND ${which}
  ORDER BY r.sequence, ids.tx_id`;

/**
 * The transitive rejection closure over the pending transactions.
 *
 * `spread` runs one pass over the pending set and calls `reject` for every
 * transaction the rejections so far make invalid ("direct" or "dependent");
 * it returns whether it rejected anything. Passes repeat until none rejects.
 * Then every co-member of an unreversed acceptance receipt that holds a
 * rejected transaction is rejected as "batch" (a receipt is the inverse of
 * one accepted batch and cannot be split), and spreading resumes, until
 * nothing widens. A co-member in `settled` (one a base block includes), or
 * one a landed block settled in an earlier rebuild (a row of
 * `event_history_l2_ledger_receipt_settlements`), is settled by that block,
 * not rejected. A co-member that is not pending and is recorded rejected on
 * the receipt (`event_history_l2_ledger_receipt_rejections`, migration 0016)
 * left the batch earlier. Any other co-member that is no longer pending is
 * undecided: the closure fails with `UndecidedBatchMember`.
 *
 * With `repairRecordedRejections`, after the first spreading the closure
 * rejects as "batch" the pending members of every unreversed receipt that
 * records a rejected member and has no undecided member;
 * `recordRejections` then reverses it. A receipt with an undecided member
 * is left as it is.
 *
 * `onReject` observes every rejection, including batch ones, in order.
 */
export const closeRejections = (input: {
  readonly pending: readonly PendingTx[];
  readonly spread: (
    reject: (tx: PendingTx, reason: "direct" | "dependent") => void,
    rejected: Rejections,
  ) => boolean;
  readonly onReject?: (tx: PendingTx) => void;
  /** Hex ids of transactions a base block includes (none by default). */
  readonly settled?: ReadonlySet<string>;
  readonly repairRecordedRejections?: boolean;
}) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const pendingById = new Map(
      input.pending.map((tx) => [txIdHex(tx), tx] as const),
    );
    const rejected = new Map<string, Rejection>();
    const reject = (tx: PendingTx, reason: RejectionReason) => {
      rejected.set(txIdHex(tx), { tx, reason });
      input.onReject?.(tx);
    };
    // A member is decided when it is rejected, settled, or recorded
    // rejected on the receipt and no longer pending; a pending one is the
    // batch member to reject.
    const decide = (member: ReceiptMember) => {
      const id = hex(member.tx_id);
      if (rejected.has(id) || member.settled || input.settled?.has(id) === true)
        return "decided" as const;
      const tx = pendingById.get(id);
      if (tx !== undefined) return tx;
      return member.rejected_earlier ? ("decided" as const) : undefined;
    };
    while (input.spread(reject, rejected));
    if (input.repairRecordedRejections === true) {
      const recorded = yield* receiptMembers(
        sql,
        sql`r.sequence IN (SELECT receipt_sequence
          FROM event_history_l2_ledger_receipt_rejections)`,
      );
      const bySequence = new Map<string, ReceiptMember[]>();
      for (const member of recorded) {
        const members = bySequence.get(member.sequence) ?? [];
        members.push(member);
        bySequence.set(member.sequence, members);
      }
      for (const members of bySequence.values()) {
        const decisions = members.map(decide);
        if (decisions.includes(undefined)) continue;
        for (const decision of decisions)
          if (typeof decision === "object") reject(decision, "batch");
      }
    }
    for (let widened = true; widened; ) {
      widened = false;
      while (input.spread(reject, rejected));
      if (rejected.size === 0) break;
      const coMembers = yield* receiptMembers(
        sql,
        sql`r.tx_ids && ${pg.array(byteaArray([...rejected.values()].map(({ tx }) => tx.entry[Tx.Columns.TX_ID])))}::bytea[]`,
      );
      for (const member of coMembers) {
        const decision = decide(member);
        if (decision === "decided") continue;
        if (decision === undefined)
          return yield* Effect.fail(
            new UndecidedBatchMember({
              table,
              message:
                "A rejected transaction was accepted in one batch with a transaction that is no longer pending, so the batch's acceptance cannot be reversed",
              cause: { receipt: member.sequence, txId: hex(member.tx_id) },
            }),
          );
        reject(decision, "batch");
        widened = true;
      }
    }
    return rejected;
  });

/** Every output the rejected set produced, keyed by hex outref. */
export const producedByRejections = (rejected: Rejections) =>
  new Map(
    [...rejected.values()].flatMap(({ tx }) =>
      tx.produced.map(
        (row) => [hex(row[MempoolLedgerDB.Columns.OUTREF]), row] as const,
      ),
    ),
  );

/**
 * Removes rejected transactions from the pending sets and undoes their
 * acceptance: a terminal rejection row and admission (coded by reason, or per
 * transaction), no address history, and
 * every acceptance receipt they belong to reversed. A receipt whose other
 * members are all in `settled` (transactions a base block includes),
 * recorded as settled on it, or recorded rejected on it is reversed with
 * them: those members are settled by the base, or left the batch earlier.
 * Every unreversed receipt that records a rejected member and whose members
 * are all decided that way is reversed too, rejections or not, and its
 * recorded rows go with it. Ledger rows are the caller's: run this after
 * any read of the receipts' before-images.
 */
export const recordRejections = (
  rejected: Rejections,
  codes: RejectionCodes | RejectionCodeOf,
  settled: readonly Buffer[] = [],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const rejectedTxs = [...rejected.values()].map(({ tx }) => tx);
    const rejectedIds = rejectedTxs.map((tx) => tx.entry[Tx.Columns.TX_ID]);
    const decided = pg.array(byteaArray([...rejectedIds, ...settled]));
    const reverse = (which: Statement.Fragment) =>
      sql`UPDATE event_history_l2_ledger_receipts r
        SET reversed_at_revision = c.revision
        FROM event_history_cursor c
        WHERE c.binding_digest = r.binding_digest
          AND r.reversed_at_revision IS NULL
          AND ${which}
          AND NOT EXISTS (
            SELECT 1 FROM unnest(r.tx_ids) AS member(tx_id)
            WHERE member.tx_id <> ALL(${decided}::bytea[])
              AND NOT EXISTS (
                SELECT 1 FROM event_history_l2_ledger_receipt_settlements s
                WHERE s.receipt_sequence = r.sequence
                  AND s.tx_id = member.tx_id)
              AND NOT EXISTS (
                SELECT 1 FROM event_history_l2_ledger_receipt_rejections x
                WHERE x.receipt_sequence = r.sequence
                  AND x.tx_id = member.tx_id))`;
    if (rejected.size > 0) {
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
      const rejections = [...rejected.entries()].map(([id, rejection]) => ({
        txId: Buffer.from(id, "hex"),
        ...(typeof codes === "function"
          ? codes(id, rejection)
          : codes[rejection.reason]),
      }));
      yield* TxRejectionsDB.insertMany(
        rejections.map(({ txId, code, detail }) => ({
          [TxRejectionsDB.Columns.TX_ID]: txId,
          [TxRejectionsDB.Columns.REJECT_CODE]: code,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: detail,
        })),
      );
      yield* TxAdmissionsDB.markAcceptedRejectedAfterCorrection(rejections);
      yield* sql`DELETE FROM address_history
        WHERE tx_id = ANY(${pg.array(byteaArray(rejectedIds))}::bytea[])`;
      yield* reverse(
        sql`r.tx_ids && ${pg.array(byteaArray(rejectedIds))}::bytea[]`,
      );
    }
    yield* reverse(
      sql`r.sequence IN (SELECT receipt_sequence
        FROM event_history_l2_ledger_receipt_rejections)`,
    );
    yield* sql`DELETE FROM event_history_l2_ledger_receipt_rejections x
      USING event_history_l2_ledger_receipts r
      WHERE r.sequence = x.receipt_sequence
        AND r.reversed_at_revision IS NOT NULL`;
    if (rejected.size === 0) return [] as readonly Buffer[];
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
    return rejectedIds;
  });
