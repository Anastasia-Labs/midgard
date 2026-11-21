/**
 * `confirmed_ledger`'s trie leaves fed in path order to a roots session
 * (`mpf/confirmed-ledger-roots.ts`), a page at a time, so neither thread
 * holds the whole ledger. The trie orders leaves by the hash of their
 * outref, which no index of `confirmed_ledger` follows: the entries are
 * read a page at a time by outref, the session hashes each page to its
 * leaves, and the leaves go to a temporary table of the transaction, read
 * back a page at a time by path. Run in `onOneWritableSnapshot`, every
 * page is of the one snapshot the run reads the frontier and the landed
 * rows on.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";

import { ConfirmedLedgerDB } from "../database/index.js";
import { sqlErrorToDatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  type ConfirmedLedgerRootsSession,
  LEAF_BYTES,
} from "../mpf/confirmed-ledger-roots.js";

/** Entries per page of `confirmed_ledger`, and leaves per page of the
 * temporary table: each insert binds two parameters per leaf. */
export const CONFIRMED_LEDGER_ENTRY_PAGE = 2_000;
export const CONFIRMED_LEDGER_LEAF_PAGE = 10_000;

const PATH_BYTES = 32;

/** Outrefs per lookup of the entries the deltas name. */
const NAMED_PAGE = 10_000;

/** The `confirmed_ledger` entries at `outRefs`, read by outref (not a full
 * read). */
export const confirmedLedgerEntriesNamed = (outRefs: readonly Buffer[]) =>
  Effect.forEach(
    Array.from({ length: Math.ceil(outRefs.length / NAMED_PAGE) }, (_, index) =>
      outRefs.slice(index * NAMED_PAGE, (index + 1) * NAMED_PAGE),
    ),
    (page) =>
      Ledger.retrieveEntriesByOutRefs(ConfirmedLedgerDB.tableName, page),
  ).pipe(Effect.map((pages) => pages.flat()));

/**
 * Runs `read` on one snapshot, as `onOneSnapshot` (`ledger.ts`) does, in a
 * `REPEATABLE READ` transaction that may write: a temporary table needs
 * it. Nothing but that table is written.
 */
export const onOneWritableSnapshot = <A, E, R>(read: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.zipRight(
        sql`SET TRANSACTION ISOLATION LEVEL REPEATABLE READ`,
        read,
      ),
    );
  });

type LeafRow = { path: Buffer; value_digest: Buffer };

const leafRows = (leaves: Uint8Array): LeafRow[] =>
  Array.from({ length: leaves.length / LEAF_BYTES }, (_, index) => {
    const at = index * LEAF_BYTES;
    return {
      path: Buffer.from(leaves.subarray(at, at + PATH_BYTES)),
      value_digest: Buffer.from(
        leaves.subarray(at + PATH_BYTES, at + LEAF_BYTES),
      ),
    };
  });

const packedLeaves = (rows: readonly LeafRow[]): Uint8Array => {
  const leaves = new Uint8Array(rows.length * LEAF_BYTES);
  rows.forEach((row, index) => {
    leaves.set(row.path, index * LEAF_BYTES);
    leaves.set(row.value_digest, index * LEAF_BYTES + PATH_BYTES);
  });
  return leaves;
};

/**
 * Feeds every `confirmed_ledger` leaf to `session` in path order and
 * returns how many there were: one full read of `confirmed_ledger`
 * (`confirmed_ledger_full_scan_total`).
 */
export const feedConfirmedLedgerLeaves = (
  session: ConfirmedLedgerRootsSession,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Metric.increment(ConfirmedLedgerDB.fullScanCounter);
    yield* sql`CREATE TEMPORARY TABLE confirmed_ledger_leaves (
      path bytea PRIMARY KEY, value_digest bytea NOT NULL) ON COMMIT DROP`;
    let entries = 0;
    for (let after: Buffer | undefined; ; ) {
      const page = yield* after === undefined
        ? sql<Ledger.MinimalEntry>`SELECT outref, output
              FROM ${sql(ConfirmedLedgerDB.tableName)}
              ORDER BY outref LIMIT ${CONFIRMED_LEDGER_ENTRY_PAGE}`
        : sql<Ledger.MinimalEntry>`SELECT outref, output
              FROM ${sql(ConfirmedLedgerDB.tableName)} WHERE outref > ${after}
              ORDER BY outref LIMIT ${CONFIRMED_LEDGER_ENTRY_PAGE}`;
      if (page.length === 0) break;
      const leaves = yield* session.leaves(page);
      if (leaves.length !== page.length * LEAF_BYTES)
        return yield* Effect.fail(
          new Error(
            `the roots session hashed ${page.length.toString()} entries to ${(leaves.length / LEAF_BYTES).toString()} leaves`,
          ),
        );
      yield* sql`INSERT INTO confirmed_ledger_leaves ${sql.insert(leafRows(leaves))}`;
      entries += page.length;
      after = page.at(-1)!.outref;
    }
    for (let after: Buffer | undefined; ; ) {
      const page = yield* after === undefined
        ? sql<LeafRow>`SELECT path, value_digest FROM confirmed_ledger_leaves
              ORDER BY path LIMIT ${CONFIRMED_LEDGER_LEAF_PAGE}`
        : sql<LeafRow>`SELECT path, value_digest FROM confirmed_ledger_leaves
              WHERE path > ${after} ORDER BY path LIMIT ${CONFIRMED_LEDGER_LEAF_PAGE}`;
      if (page.length === 0) break;
      yield* session.feed(packedLeaves(page));
      after = page.at(-1)!.path;
    }
    return entries;
  }).pipe(
    sqlErrorToDatabaseError(
      ConfirmedLedgerDB.tableName,
      "Failed to read confirmed_ledger's leaves in path order",
    ),
  );
