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
  hex,
  type PendingTx,
} from "./working-ledger-recompute.pending-txs.js";

/** Why a pending transaction left the working ledger: its own input became
 * unavailable, or it spends (in a rebuild, also reads by reference) a
 * rejected transaction's output. */
export type RejectionReason = "direct" | "dependent";

export type RejectionCodes = Readonly<
  Record<RejectionReason, Readonly<{ code: string; detail: string }>>
>;

/**
 * `causes` are the hex ids of the rejected transactions a "dependent"
 * rejection follows from: the producers of the rejected outputs it spends
 * or reads.
 * A rejection without them is not traced to another transaction.
 */
export type Rejection = Readonly<{
  tx: PendingTx;
  reason: RejectionReason;
  causes?: readonly string[];
}>;

/** Rejects `tx` for `reason`, after the rejected transactions `causes`. */
export type Reject = (
  tx: PendingTx,
  reason: RejectionReason,
  causes?: readonly string[],
) => void;

/** The code and detail one rejection is recorded with, by hex transaction id. */
export type RejectionCodeOf = (
  id: string,
  rejection: Rejection,
) => Readonly<{ code: string; detail: string }>;

/** Rejections keyed by hex transaction id. */
export type Rejections = ReadonlyMap<string, Rejection>;

export const txIdHex = (tx: PendingTx) => hex(tx.entry[Tx.Columns.TX_ID]);

/**
 * The transitive rejection closure over the pending transactions.
 *
 * `spread` runs one pass over the pending set and calls `reject` for every
 * transaction the rejections so far make invalid ("direct" or "dependent");
 * it returns whether it rejected anything. Passes repeat until none rejects.
 * Each pending transaction carries its own ledger effects (its per-transaction
 * delta), so a transaction that spends a rejected one's output is found as a
 * dependent whichever acceptance batch either came in.
 *
 * `onReject` observes every rejection, in order.
 */
export const closeRejections = (input: {
  readonly pending: readonly PendingTx[];
  readonly spread: (reject: Reject, rejected: Rejections) => boolean;
  readonly onReject?: (tx: PendingTx) => void;
}) =>
  Effect.sync(() => {
    const rejected = new Map<string, Rejection>();
    const reject: Reject = (tx, reason, causes) => {
      rejected.set(
        txIdHex(tx),
        causes === undefined || causes.length === 0
          ? { tx, reason }
          : { tx, reason, causes },
      );
      input.onReject?.(tx);
    };
    while (input.spread(reject, rejected));
    return rejected as Rejections;
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
 * transaction), no address history, and their script material released.
 * Ledger rows are the caller's. Returns the rejected transaction ids.
 */
export const recordRejections = (
  rejected: Rejections,
  codes: RejectionCodes | RejectionCodeOf,
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
    yield* TxRejectionsDB.insertCauses(
      [...rejected.entries()].flatMap(([id, { causes }]) =>
        (causes ?? []).map((cause) => ({
          txId: Buffer.from(id, "hex"),
          causeTxId: Buffer.from(cause, "hex"),
        })),
      ),
    );
    yield* TxAdmissionsDB.markAcceptedRejectedAfterCorrection(rejections);
    yield* sql`DELETE FROM address_history
      WHERE tx_id = ANY(${pg.array(byteaArray(rejectedIds))}::bytea[])`;
    yield* CekProgramMaterialDB.releaseAdmissionOwnership(rejectedIds);
    return rejectedIds;
  });
