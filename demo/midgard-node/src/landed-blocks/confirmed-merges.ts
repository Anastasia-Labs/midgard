/**
 * `confirmed_ledger` is temporal (plan §10.5, §5.5 P10, N5): every landed
 * block folded into it keeps a stored delta keyed by its header
 * (`node_confirmed_merges` and `node_confirmed_ledger_spent`, migration
 * 0014), so the fold of a merge a rollback undid is unfolded in O(delta),
 * and the frontier only ever moves along landed header identities.
 *
 * - A fold checks its base first: the frontier is the block's parent header
 *   with the parent's root, and `confirmed_ledger` holds what the block
 *   spends and none of what it produces. A wrong base is refused
 *   (`ConfirmedLedgerBaseMismatch`) and writes nothing.
 * - It then marks the block's events terminal (recording exactly the rows it
 *   moved), moves the spent rows aside verbatim, inserts the produced rows,
 *   drops the block's landed row and moves the frontier to the block. It
 *   reads no whole ledger, computes no root and locks only the frontier row;
 *   every caller runs in a follower-gated write (`follower-write-gate.ts`).
 * - An unfold of the frontier reverses exactly that: the produced rows go,
 *   the spent rows come back, the events it moved reopen, the landed row is
 *   processed again with its stored flags, and the frontier is the parent.
 * - A row lives while its merge can roll back: once the follower's prune
 *   boundary reaches the slot of the merge that made its header the queue
 *   root, it is deleted. A fold whose merge point is not known yet (the
 *   merge fiber's, which reads no queue history) keeps it null, and landed
 *   processing fills it in.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Data, Effect } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import { releaseFinalFolds } from "./final-folds.js";
import { depositOutputs, ledgerRows } from "./ledger.js";
import {
  bytea,
  decodeRow,
  deleteRows,
  Frontier,
  type HeaderRoot,
  insertRow,
  type LandedBlockRow,
  type RawRow,
} from "./store.js";

export const mergesTableName = "node_confirmed_merges";

/** The fold's base is not the frontier or `confirmed_ledger` it needs. */
export class ConfirmedLedgerBaseMismatch extends Data.TaggedError(
  "ConfirmedLedgerBaseMismatch",
)<{ readonly detail: string }> {}

/** The queue root output a merge created for a header. */
export type MergePoint = Readonly<{ slot: number; outRef: string }>;

/** A folded block's link to its parent, and its merge point. */
export type MergeLink = Readonly<{
  headerHash: string;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  utxosRoot: string;
  merge: MergePoint | null;
}>;

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

const mismatch = (detail: string) =>
  Effect.fail(new ConfirmedLedgerBaseMismatch({ detail }));

/** The frontier, row-locked for the rest of the transaction. */
const lockedFrontier = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [row] = yield* sql<{ header_hash: Buffer; utxos_root: string }>`
    SELECT header_hash, utxos_root FROM node_confirmed_ledger_frontier
    FOR UPDATE`;
  return row === undefined
    ? undefined
    : ({
        headerHash: hex(row.header_hash),
        utxosRoot: row.utxos_root,
      } satisfies HeaderRoot);
});

const array = (sql: SqlClient.SqlClient, values: readonly Buffer[]) =>
  (sql as PgClient).array(bytea(values));

/** The ids of the events `table` projected to `header` (an own block's). */
const projectedTo = (table: string, idColumn: string, header: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ id: Buffer }>`
      SELECT ${sql(idColumn)} AS id FROM ${sql(table)}
      WHERE projected_header_hash = ${header}`;
    return rows.map((row) => Buffer.from(row.id));
  });

/** Consumes the projected deposits `ids`; the ids it moved. */
const consumeDeposits = (ids: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (ids.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const moved = yield* sql<{ id: Buffer }>`
      UPDATE deposits_utxos SET status = ${DepositsDB.Status.Consumed}
      WHERE event_id = ANY(${array(sql, ids)}::bytea[])
        AND status = ${DepositsDB.Status.Projected}
      RETURNING event_id AS id`;
    return moved.map((row) => Buffer.from(row.id));
  });

/**
 * Finalizes the events `ids` of `table`, every one of which must be
 * projected (or finalized) to `header`; the ids it moved.
 */
const finalizeEvents = (
  table: string,
  idColumn: string,
  ids: readonly Buffer[],
  header: Buffer,
) =>
  Effect.gen(function* () {
    if (ids.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const matched = yield* sql<{ id: Buffer }>`
      SELECT ${sql(idColumn)} AS id FROM ${sql(table)}
      WHERE ${sql(idColumn)} = ANY(${array(sql, ids)}::bytea[])
        AND status IN ('projected', 'finalized')
        AND projected_header_hash = ${header}
      FOR UPDATE`;
    if (matched.length !== ids.length)
      return yield* Effect.fail(
        new DatabaseError({
          table,
          message:
            "A landed block's event is missing, unprojected, or projected to a different header",
          cause: `requested=${ids.length.toString()},matched=${matched.length.toString()},header_hash=${hex(header)}`,
        }),
      );
    const moved = yield* sql<{ id: Buffer }>`
      UPDATE ${sql(table)} SET status = 'finalized', updated_at = NOW()
      WHERE ${sql(idColumn)} = ANY(${array(sql, ids)}::bytea[])
        AND status = 'projected' AND projected_header_hash = ${header}
      RETURNING ${sql(idColumn)} AS id`;
    return moved.map((row) => Buffer.from(row.id));
  });

/** The terminal marks of `row`'s events, recording the rows they moved. */
const markEvents = (row: LandedBlockRow, header: Buffer) =>
  Effect.gen(function* () {
    const own = row.kind === "own";
    const depositIds = own
      ? yield* projectedTo(DepositsDB.tableName, DepositsDB.Columns.ID, header)
      : row.depositIds;
    const withdrawalIds = own
      ? yield* projectedTo(
          WithdrawalsDB.tableName,
          WithdrawalsDB.Columns.ID,
          header,
        )
      : row.withdrawals.map((member) => Buffer.from(member.id, "hex"));
    const forcedIds = own
      ? yield* projectedTo(
          ForcedTransactionsDB.tableName,
          ForcedTransactionsDB.Columns.TX_ORDER_ID,
          header,
        )
      : row.forcedIds;
    return {
      deposits: yield* consumeDeposits(depositIds),
      withdrawals: yield* finalizeEvents(
        WithdrawalsDB.tableName,
        WithdrawalsDB.Columns.ID,
        withdrawalIds,
        header,
      ),
      forced: yield* finalizeEvents(
        ForcedTransactionsDB.tableName,
        ForcedTransactionsDB.Columns.TX_ORDER_ID,
        forcedIds,
        header,
      ),
    };
  });

/**
 * Folds `row` into `confirmed_ledger` at the frontier, in the caller's
 * write transaction; `merge` is the root output its merge created, when
 * known. Fails `ConfirmedLedgerBaseMismatch` (writing nothing) when the
 * frontier is not `row`'s parent or `confirmed_ledger` is not its base.
 */
export const foldMerge = (row: LandedBlockRow, merge: MergePoint | null) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const frontier = yield* lockedFrontier;
    if (
      frontier?.headerHash !== row.parentHeaderHash ||
      frontier.utxosRoot !== row.parentUtxosRoot
    )
      return yield* mismatch(
        `block ${row.headerHash} folds on ${row.parentHeaderHash} (${row.parentUtxosRoot}), the confirmed-ledger frontier is ${frontier === undefined ? "unset" : `${frontier.headerHash} (${frontier.utxosRoot})`}`,
      );
    const header = Buffer.from(row.headerHash, "hex");
    const marked = yield* markEvents(row, header);
    yield* sql`INSERT INTO node_confirmed_merges (header_hash,
        parent_header_hash, parent_utxos_root, utxos_root, kind, applied,
        spent, produced_outrefs, produced_outputs, deposit_ids, withdrawals,
        forced_ids, tx_ids, consumed_deposit_ids, finalized_withdrawal_ids,
        finalized_forced_ids, merge_slot, merge_out_ref)
      VALUES (${header}, ${Buffer.from(row.parentHeaderHash, "hex")},
        ${row.parentUtxosRoot}, ${row.utxosRoot}, ${row.kind}, ${row.applied},
        ${array(sql, row.spent)}::bytea[],
        ${array(
          sql,
          row.produced.map((entry) => entry.outref),
        )}::bytea[],
        ${array(
          sql,
          row.produced.map((entry) => entry.output),
        )}::bytea[],
        ${array(sql, row.depositIds)}::bytea[],
        CAST(${JSON.stringify(row.withdrawals)} AS TEXT)::jsonb,
        ${array(sql, row.forcedIds)}::bytea[],
        ${array(sql, row.txIds)}::bytea[],
        ${array(sql, marked.deposits)}::bytea[],
        ${array(sql, marked.withdrawals)}::bytea[],
        ${array(sql, marked.forced)}::bytea[],
        ${merge?.slot ?? null}, ${merge?.outRef ?? null})`;
    if (row.spent.length > 0) {
      const moved = yield* sql<{ outref: Buffer }>`
        WITH gone AS (
          DELETE FROM confirmed_ledger
          WHERE outref = ANY(${array(sql, row.spent)}::bytea[])
          RETURNING tx_id, outref, output, address, time_stamp_tz
        )
        INSERT INTO node_confirmed_ledger_spent (header_hash, tx_id, outref,
          output, address, time_stamp_tz)
        SELECT ${header}, tx_id, outref, output, address, time_stamp_tz
        FROM gone
        RETURNING outref`;
      if (moved.length !== row.spent.length)
        return yield* mismatch(
          `confirmed_ledger holds ${moved.length.toString()} of the ${row.spent.length.toString()} outputs block ${row.headerHash} spends`,
        );
    }
    if (row.produced.length > 0) {
      const deposits = yield* depositOutputs(row.depositIds);
      const rows = yield* ledgerRows(
        row.produced,
        new Map([...deposits].map(([outRef, { txId }]) => [outRef, txId])),
      );
      const inserted = yield* sql<{ outref: Buffer }>`
        INSERT INTO confirmed_ledger (tx_id, outref, output, address)
        SELECT * FROM unnest(
          ${array(
            sql,
            rows.map((entry) => entry[Ledger.Columns.TX_ID]),
          )}::bytea[],
          ${array(
            sql,
            rows.map((entry) => entry[Ledger.Columns.OUTREF]),
          )}::bytea[],
          ${array(
            sql,
            rows.map((entry) => entry[Ledger.Columns.OUTPUT]),
          )}::bytea[],
          ${pg.array(rows.map((entry) => entry[Ledger.Columns.ADDRESS]))}::text[])
        ON CONFLICT (outref) DO NOTHING
        RETURNING outref`;
      if (inserted.length !== row.produced.length)
        return yield* mismatch(
          `confirmed_ledger already holds ${(row.produced.length - inserted.length).toString()} of the outputs block ${row.headerHash} produces`,
        );
    }
    yield* deleteRows([row.headerHash]);
    yield* Frontier.upsert(row);
  }).pipe(
    sqlErrorToDatabaseError(mergesTableName, "Failed to fold a landed block"),
  );

type RawMerge = Omit<RawRow, "state"> & {
  consumed_deposit_ids: unknown;
  finalized_withdrawal_ids: unknown;
  finalized_forced_ids: unknown;
};

const buffers = (value: unknown): Buffer[] =>
  ((value ?? []) as Uint8Array[]).map((item) => Buffer.from(item));

/**
 * Unfolds the frontier `expected` in the caller's write transaction: back
 * to its parent, its events reopened and its landed row processed again.
 * Fails `ConfirmedLedgerBaseMismatch` when the frontier moved or the ledger
 * does not hold what the fold wrote.
 */
export const unfoldFrontier = (expected: HeaderRoot) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const frontier = yield* lockedFrontier;
    if (
      frontier?.headerHash !== expected.headerHash ||
      frontier.utxosRoot !== expected.utxosRoot
    )
      return yield* mismatch(
        `the confirmed-ledger frontier moved off ${expected.headerHash} before its unfold`,
      );
    const header = Buffer.from(expected.headerHash, "hex");
    const [raw] = yield* sql<RawMerge>`
      SELECT * FROM node_confirmed_merges WHERE header_hash = ${header}`;
    if (raw === undefined)
      return yield* mismatch(
        `the confirmed-ledger frontier ${expected.headerHash} has no retained fold to unfold`,
      );
    const row = decodeRow({ ...raw, state: "processed" });
    if (row.produced.length > 0) {
      const gone = yield* sql<{ outref: Buffer }>`
        DELETE FROM confirmed_ledger
        WHERE outref = ANY(${array(
          sql,
          row.produced.map((entry) => entry.outref),
        )}::bytea[])
        RETURNING outref`;
      if (gone.length !== row.produced.length)
        return yield* mismatch(
          `confirmed_ledger holds ${gone.length.toString()} of the ${row.produced.length.toString()} outputs the fold of ${row.headerHash} produced`,
        );
    }
    if (row.spent.length > 0) {
      const back = yield* sql<{ outref: Buffer }>`
        INSERT INTO confirmed_ledger (tx_id, outref, output, address,
          time_stamp_tz)
        SELECT tx_id, outref, output, address, time_stamp_tz
        FROM node_confirmed_ledger_spent WHERE header_hash = ${header}
        ON CONFLICT (outref) DO NOTHING
        RETURNING outref`;
      if (back.length !== row.spent.length)
        return yield* mismatch(
          `the unfold of ${row.headerHash} restores ${back.length.toString()} of the ${row.spent.length.toString()} outputs its fold spent`,
        );
    }
    const consumed = buffers(raw.consumed_deposit_ids);
    if (consumed.length > 0)
      yield* sql`UPDATE deposits_utxos SET status = ${DepositsDB.Status.Projected}
        WHERE event_id = ANY(${array(sql, consumed)}::bytea[])
          AND status = ${DepositsDB.Status.Consumed}`;
    for (const [table, idColumn, ids] of [
      [
        WithdrawalsDB.tableName,
        WithdrawalsDB.Columns.ID,
        buffers(raw.finalized_withdrawal_ids),
      ],
      [
        ForcedTransactionsDB.tableName,
        ForcedTransactionsDB.Columns.TX_ORDER_ID,
        buffers(raw.finalized_forced_ids),
      ],
    ] as const)
      if (ids.length > 0)
        yield* sql`UPDATE ${sql(table)} SET status = 'projected',
            updated_at = NOW()
          WHERE ${sql(idColumn)} = ANY(${array(sql, ids)}::bytea[])
            AND status = 'finalized' AND projected_header_hash = ${header}`;
    yield* sql`DELETE FROM node_confirmed_merges WHERE header_hash = ${header}`;
    yield* insertRow(row);
    const parent = {
      headerHash: row.parentHeaderHash,
      utxosRoot: row.parentUtxosRoot,
    } satisfies HeaderRoot;
    yield* Frontier.upsert(parent);
    return parent;
  }).pipe(
    sqlErrorToDatabaseError(mergesTableName, "Failed to unfold a landed block"),
  );

/** Every retained fold's link, by header. */
export const retrieveMergeLinks = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    parent_header_hash: Buffer;
    parent_utxos_root: string;
    utxos_root: string;
    merge_slot: string | number | null;
    merge_out_ref: string | null;
  }>`SELECT header_hash, parent_header_hash, parent_utxos_root, utxos_root,
      merge_slot, merge_out_ref
    FROM node_confirmed_merges`;
  return new Map(
    rows.map((row) => {
      const link: MergeLink = {
        headerHash: hex(row.header_hash),
        parentHeaderHash: hex(row.parent_header_hash),
        parentUtxosRoot: row.parent_utxos_root,
        utxosRoot: row.utxos_root,
        merge:
          row.merge_slot === null || row.merge_out_ref === null
            ? null
            : { slot: Number(row.merge_slot), outRef: row.merge_out_ref },
      };
      return [link.headerHash, link] as const;
    }),
  );
}).pipe(
  sqlErrorToDatabaseError(mergesTableName, "Failed to read the retained folds"),
);

/**
 * The frontier and the headers it can unfold back to, newest first: each
 * retained fold's parent, while the fold is retained.
 */
export const retainedAncestors = (
  frontier: HeaderRoot,
  links: ReadonlyMap<string, MergeLink>,
): HeaderRoot[] => {
  const chain: HeaderRoot[] = [frontier];
  const seen = new Set([frontier.headerHash]);
  for (
    let link = links.get(frontier.headerHash);
    link !== undefined && !seen.has(link.parentHeaderHash);
    link = links.get(link.parentHeaderHash)
  ) {
    seen.add(link.parentHeaderHash);
    chain.push({
      headerHash: link.parentHeaderHash,
      utxosRoot: link.parentUtxosRoot,
    });
  }
  return chain;
};

/** Records the merge points of retained folds. */
export const setMergePoints = (points: ReadonlyMap<string, MergePoint>) =>
  Effect.gen(function* () {
    if (points.size === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const entries = [...points];
    yield* sql`UPDATE node_confirmed_merges m
      SET merge_slot = p.slot, merge_out_ref = p.out_ref
      FROM unnest(
          ${array(
            sql,
            entries.map(([hash]) => Buffer.from(hash, "hex")),
          )}::bytea[],
          ${pg.array(entries.map(([, point]) => point.slot.toString()))}::bigint[],
          ${pg.array(entries.map(([, point]) => point.outRef))}::text[]
        ) AS p(header_hash, slot, out_ref)
      WHERE m.header_hash = p.header_hash`;
  }).pipe(
    sqlErrorToDatabaseError(mergesTableName, "Failed to record merge points"),
  );

/**
 * Deletes the folds whose merge is at or below the follower's prune
 * boundary `throughSlot`: no rollback reaches them, so they never unfold.
 * What each fold left until it is final is released in the same
 * transaction (`releaseFinalFolds`). Returns the released headers.
 */
export const pruneMerges = (throughSlot: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pruned = yield* sql<{ header_hash: Buffer }>`
      DELETE FROM node_confirmed_merges
      WHERE merge_slot IS NOT NULL AND merge_slot <= ${throughSlot}
      RETURNING header_hash`;
    const headers = pruned.map((row) => Buffer.from(row.header_hash));
    yield* releaseFinalFolds(headers);
    return headers.map((header) => header.toString("hex"));
  }).pipe(
    sqlErrorToDatabaseError(mergesTableName, "Failed to prune retained folds"),
  );
