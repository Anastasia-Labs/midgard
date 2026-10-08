/**
 * What decides an acceptance-receipt member that is not pending (plan §7.3,
 * N3), as SQL conditions on `member.tx_id`, and the admission and address
 * history of a member recorded rejected on its receipt (migration 0014).
 *
 * A member is settled by a block when this node's own block records place
 * it in a landed or folded block (`settledByBlock`), the evidence
 * `eventHistoryLedgerRepair.ts` reads as committed:
 *
 * - a `blocks` row whose header is a processed landed row
 *   (`node_landed_blocks`, state `processed`): the local finalization of
 *   this node's block writes its `blocks` and `immutable` rows in one
 *   transaction, the one that now writes the block's inclusion marks; the
 *   reopening of the block by a correction deletes them with the marks;
 * - an `immutable` row with no `blocks` row: the merge finalization of the
 *   block clears its `blocks` rows and keeps `immutable`, while a reopening
 *   deletes both (`immutable` only where no `blocks` row holds the member),
 *   so only a folded block leaves an `immutable` row alone;
 * - a `pending_block_finalization_txs` row of a journal that landed
 *   (`observed_waiting_stability` or `locally_applied`, the statuses outside
 *   `UNLANDED_STATUSES`).
 *
 * A member is concluded (`concluded`) when it is in neither pending table,
 * its admission is terminal, and no journal of an unlanded own block holds
 * it. A row enters a pending table only through its admission, which a
 * terminal admission never repeats (`ON CONFLICT DO NOTHING`), or through
 * the reopening of a journal that holds it; a concluded member is in no
 * unlanded block's journal, so only a correction that reopens a landed
 * block brings it back, as it does a member that block settles. Whether a
 * block included it or it was rejected, its batch is decided the same way:
 * its pending co-members are rejected as batch members and the receipt is
 * reversed.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import {
  PendingBlockFinalizationsDB,
  TxAdmissionsDB,
} from "../database/index.js";
import { byteaArray } from "./working-ledger-recompute.pending-txs.js";

const Status = PendingBlockFinalizationsDB.Status;

/** A landed or folded block of this node includes `member.tx_id`. */
export const settledByBlock = (sql: SqlClient.SqlClient) => sql`(
  EXISTS (SELECT 1 FROM blocks b
    JOIN node_landed_blocks l
      ON l.header_hash = b.header_hash AND l.state = 'processed'
    WHERE b.tx_id = member.tx_id)
  OR (EXISTS (SELECT 1 FROM immutable i WHERE i.tx_id = member.tx_id)
    AND NOT EXISTS (SELECT 1 FROM blocks b WHERE b.tx_id = member.tx_id))
  OR EXISTS (SELECT 1 FROM pending_block_finalization_txs p
    JOIN pending_block_finalizations f ON f.header_hash = p.header_hash
    WHERE p.member_id = member.tx_id
      AND f.status IN (${Status.ObservedWaitingStability}, ${Status.LocallyApplied})))`;

/** `member.tx_id` left the pending tables for good. */
export const concluded = (sql: SqlClient.SqlClient) => sql`(
  NOT EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = member.tx_id)
  AND NOT EXISTS (SELECT 1 FROM processed_mempool p WHERE p.tx_id = member.tx_id)
  AND EXISTS (SELECT 1 FROM tx_admissions a
    WHERE a.tx_id = member.tx_id AND a.status IN ('accepted', 'rejected'))
  AND NOT EXISTS (SELECT 1 FROM pending_block_finalization_txs p
    JOIN pending_block_finalizations f ON f.header_hash = p.header_hash
    WHERE p.member_id = member.tx_id
      AND f.status IN (${Status.PendingSubmission},
        ${Status.SubmittedLocalFinalizationPending},
        ${Status.SubmittedUnconfirmed})))`;

/**
 * Every member recorded rejected on a receipt that is in neither pending
 * table gets the admission and address history of a rejected transaction:
 * an accepted admission becomes rejected with the recorded code, and its
 * address history goes.
 */
export const settleRecordedRejections = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const pg = sql as PgClient;
  const recorded = yield* sql<{
    tx_id: Buffer;
    reject_code: string;
    reject_detail: string | null;
    accepted: boolean;
  }>`SELECT DISTINCT ON (x.tx_id) x.tx_id, x.reject_code, x.reject_detail,
      EXISTS (SELECT 1 FROM tx_admissions a
        WHERE a.tx_id = x.tx_id AND a.status = 'accepted') AS accepted
    FROM event_history_l2_ledger_receipt_rejections x
    WHERE NOT EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = x.tx_id)
      AND NOT EXISTS (SELECT 1 FROM processed_mempool p WHERE p.tx_id = x.tx_id)
    ORDER BY x.tx_id`;
  if (recorded.length === 0) return;
  yield* TxAdmissionsDB.markAcceptedRejectedAfterCorrection(
    recorded
      .filter(({ accepted }) => accepted)
      .map((row) => ({
        txId: row.tx_id,
        code: row.reject_code,
        detail: row.reject_detail ?? "",
      })),
  );
  yield* sql`DELETE FROM address_history WHERE tx_id = ANY(${pg.array(
    byteaArray(recorded.map((row) => row.tx_id)),
  )}::bytea[])`;
});
