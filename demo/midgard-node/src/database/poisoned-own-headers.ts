/**
 * The poisoned own headers: every processed own landed block with an
 * own-landed orphan (a deposit or withdrawal assigned to it whose follower
 * admission left the chain, `follower-orphan-repair.ts`), and every
 * processed landed block built on one (its descendants in the landed queue,
 * by `parent_header_hash`).
 *
 * A landed own block whose event left the chain holds commits and its own
 * merge until it leaves the landed queue: a landed correction or a rollback
 * that removes the header ends the hold, with no operator step. The
 * projection is derived from the database on every read; nothing stores
 * it.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  type AdmissionKind,
  orphanedAdmission,
} from "./l1-admission-identity.js";
import { sqlErrorToDatabaseError } from "./utils/common.js";

export type PoisonedOwnHeaders = Readonly<{
  /** The own landed headers with an orphaned event, and how many each has. */
  orphaned: ReadonlyArray<Readonly<{ headerHash: string; orphans: number }>>;
  /** Those headers and every processed landed header descending from one. */
  headers: ReadonlySet<string>;
}>;

const ORPHAN_TABLES: ReadonlyArray<
  Readonly<{ table: string; kind: AdmissionKind }>
> = [
  { table: "deposits_utxos", kind: "deposit" },
  { table: "withdrawal_utxos", kind: "withdrawal" },
];

/** The poisoned own headers, read in the caller's transaction. */
export const readPoisonedOwnHeaders = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const orphans = sql.join(
    " + ",
    false,
  )(
    ORPHAN_TABLES.map(
      ({ table, kind }) => sql`(SELECT count(*) FROM ${sql(table)} o
        WHERE o.projected_header_hash = b.header_hash
          AND ${orphanedAdmission(sql, "o", kind)})`,
    ),
  );
  const rows = yield* sql<{
    header_hash: Buffer;
    orphans: string | null;
  }>`WITH RECURSIVE seeds AS (
      SELECT header_hash, orphans FROM (
        SELECT b.header_hash, ${orphans} AS orphans
        FROM node_landed_blocks b
        WHERE b.kind = 'own' AND b.state = 'processed') counted
      WHERE orphans > 0),
    poisoned(header_hash) AS (
      SELECT header_hash FROM seeds
      UNION
      SELECT c.header_hash FROM node_landed_blocks c
        JOIN poisoned p ON c.parent_header_hash = p.header_hash
        WHERE c.state = 'processed')
    SELECT p.header_hash, s.orphans::text AS orphans
    FROM poisoned p LEFT JOIN seeds s ON s.header_hash = p.header_hash
    ORDER BY p.header_hash`;
  return {
    orphaned: rows.flatMap((row) =>
      row.orphans === null
        ? []
        : [
            {
              headerHash: row.header_hash.toString("hex"),
              orphans: Number(row.orphans),
            },
          ],
    ),
    headers: new Set(rows.map((row) => row.header_hash.toString("hex"))),
  } satisfies PoisonedOwnHeaders;
}).pipe(
  sqlErrorToDatabaseError(
    "node_landed_blocks",
    "Failed to read the own landed headers whose events left the chain",
  ),
);

/** The headers that name a hold of `poisoned`, with their orphan counts. */
export const describePoisonedOwnHeaders = (
  poisoned: PoisonedOwnHeaders,
): string => {
  const orphans = poisoned.orphaned.reduce((sum, row) => sum + row.orphans, 0);
  const named = poisoned.orphaned
    .slice(0, 3)
    .map((row) => `${row.headerHash} (${row.orphans.toString()})`)
    .join(", ");
  const more =
    poisoned.orphaned.length > 3
      ? ` and ${(poisoned.orphaned.length - 3).toString()} more`
      : "";
  const descendants = poisoned.headers.size - poisoned.orphaned.length;
  return `own landed block ${named}${more}: ${orphans.toString()} event(s) whose L1 admission left the chain${descendants > 0 ? `; ${descendants.toString()} landed block(s) built on it` : ""}`;
};

/**
 * The merge hold of `headerHash`: set when it is a poisoned own header or
 * built on one (`eventOrphaned` of `classifyOldestQueuedBlockReadiness`).
 */
export const poisonedHeaderHold = (
  poisoned: PoisonedOwnHeaders,
  headerHash: string,
): string | undefined =>
  poisoned.headers.has(headerHash)
    ? describePoisonedOwnHeaders(poisoned)
    : undefined;
