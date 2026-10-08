import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import {
  MempoolDB,
  MempoolInclusionsDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import { resolveTxDeltaForCommit } from "../mpf/commit-rejection.js";

/**
 * The pending working ledger: every admitted transaction not yet in a
 * processed block, with its exact spends and outputs, and the helpers that
 * read and write `mempool_ledger` rows. Shared by every path that recomputes
 * the working ledger after its base changed (a landed-block rebase, a
 * state-queue correction).
 */

export const table = MempoolLedgerDB.tableName;

export const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });

export const hex = (value: Buffer) => value.toString("hex");

export const byteaArray = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

export type LedgerRow = MempoolLedgerDB.EntryNoTimeStamp;

export type PendingTx = Readonly<{
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

/** Every pending (unmarked) transaction whose ledger effects are in
 * `mempool_ledger`, in admission order, with its exact spends and outputs. A
 * row a block's inclusion mark holds is in that block, not pending. */
export const loadPendingTxs = Effect.gen(function* () {
  const mempool = yield* MempoolInclusionsDB.retrievePendingEntries(
    MempoolDB.tableName,
  );
  const processed = yield* MempoolInclusionsDB.retrievePendingEntries(
    ProcessedMempoolDB.tableName,
  );
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

export const presentOutRefs = (outRefs: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (outRefs.length === 0) return new Set<string>();
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const rows = yield* sql<{ outref: Buffer }>`
      SELECT outref FROM mempool_ledger
      WHERE outref = ANY(${pg.array(byteaArray(outRefs))}::bytea[])`;
    return new Set(rows.map((row) => hex(row.outref)));
  });

/** A merged output: the confirmed ledger holds every merged block's state. */
export const confirmedRow = (outRef: Buffer) =>
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

export const insertRows = (rows: readonly LedgerRow[]) =>
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
