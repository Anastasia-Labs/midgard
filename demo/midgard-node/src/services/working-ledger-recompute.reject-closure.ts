import { SqlClient } from "@effect/sql";
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
import * as Tx from "../database/utils/tx.js";
import {
  byteaArray,
  failure,
  hex,
  type PendingTx,
} from "./working-ledger-recompute.pending-txs.js";

/** Why a pending transaction left the working ledger: its own input became
 * unavailable, it spends a rejected transaction's output, or it was accepted
 * in one batch with a rejected transaction. */
export type RejectionReason = "direct" | "dependent" | "batch";

export type RejectionCodes = Readonly<
  Record<RejectionReason, Readonly<{ code: string; detail: string }>>
>;

export type Rejection = Readonly<{ tx: PendingTx; reason: RejectionReason }>;

/** Rejections keyed by hex transaction id. */
export type Rejections = ReadonlyMap<string, Rejection>;

export const txIdHex = (tx: PendingTx) => hex(tx.entry[Tx.Columns.TX_ID]);

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
 * not rejected. Any other co-member that is no longer pending makes the
 * batch irreversible, and the closure fails.
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
    for (let widened = true; widened; ) {
      widened = false;
      while (input.spread(reject, rejected));
      if (rejected.size === 0) break;
      const coMembers = yield* sql<{
        sequence: string;
        tx_id: Buffer;
        settled: boolean;
      }>`
        SELECT r.sequence::text AS sequence, ids.tx_id,
          EXISTS (SELECT 1 FROM event_history_l2_ledger_receipt_settlements s
            WHERE s.receipt_sequence = r.sequence AND s.tx_id = ids.tx_id)
            AS settled
        FROM event_history_l2_ledger_receipts r, unnest(r.tx_ids) AS ids(tx_id)
        WHERE r.reversed_at_revision IS NULL
          AND r.tx_ids && ${pg.array(byteaArray([...rejected.values()].map(({ tx }) => tx.entry[Tx.Columns.TX_ID])))}::bytea[]
        ORDER BY r.sequence, ids.tx_id`;
      for (const { sequence, tx_id, settled } of coMembers) {
        const id = hex(tx_id);
        if (rejected.has(id) || settled || input.settled?.has(id) === true)
          continue;
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
 * acceptance: a terminal rejection row and admission, no address history, and
 * every acceptance receipt they belong to reversed. A receipt whose other
 * members are all in `settled` (transactions a base block includes) or
 * recorded as settled on it is reversed with them: those members are
 * settled by the base. Ledger rows are the caller's: run this after any
 * read of the receipts' before-images.
 */
export const recordRejections = (
  rejected: Rejections,
  codes: RejectionCodes,
  settled: readonly Buffer[] = [],
) =>
  Effect.gen(function* () {
    if (rejected.size === 0) return [] as readonly Buffer[];
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const rejectedTxs = [...rejected.values()].map(({ tx }) => tx);
    const rejectedIds = rejectedTxs.map((tx) => tx.entry[Tx.Columns.TX_ID]);
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
      ...codes[reason],
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
    yield* sql`UPDATE event_history_l2_ledger_receipts r
      SET reversed_at_revision = c.revision
      FROM event_history_cursor c
      WHERE c.binding_digest = r.binding_digest
        AND r.reversed_at_revision IS NULL
        AND r.tx_ids && ${pg.array(byteaArray(rejectedIds))}::bytea[]
        AND NOT EXISTS (
          SELECT 1 FROM unnest(r.tx_ids) AS member(tx_id)
          WHERE member.tx_id <> ALL(${pg.array(byteaArray([...rejectedIds, ...settled]))}::bytea[])
            AND NOT EXISTS (
              SELECT 1 FROM event_history_l2_ledger_receipt_settlements s
              WHERE s.receipt_sequence = r.sequence
                AND s.tx_id = member.tx_id))`;
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
