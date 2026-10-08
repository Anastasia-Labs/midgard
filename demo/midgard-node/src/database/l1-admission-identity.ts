/**
 * The follower admission identity an event row carries (plan §5.4, N1): the
 * event key and the immutable admission output, as the L1 follower's
 * never-reuse key set `l1_event_keys` records them. It replaces the
 * event-history journal's incarnation association. An admission is canonical
 * while `l1_event_keys` holds the same (kind, key, origin_outref), live or
 * retired; a follower rewind past it deletes the key, which orphans the row.
 * The follower store shares the node database, so every check here runs in
 * the caller's SQL transaction.
 */
import type { SqlClient } from "@effect/sql";

export type AdmissionKind = "deposit" | "withdrawal";

export const IdentityColumns = {
  EVENT_KEY: "l1_event_key",
  ORIGIN_OUTREF: "l1_origin_outref",
} as const;

export type AdmissionIdentity = Readonly<{
  l1_event_key: Buffer | null;
  l1_origin_outref: Buffer | null;
}>;

export const NO_IDENTITY: AdmissionIdentity = {
  l1_event_key: null,
  l1_origin_outref: null,
};

/** The admission kind of each event row table. */
export const ADMISSION_KIND_OF: Readonly<Record<string, AdmissionKind>> = {
  deposits_utxos: "deposit",
  withdrawal_utxos: "withdrawal",
  pending_block_finalization_deposits: "deposit",
  pending_block_finalization_withdrawals: "withdrawal",
};

/**
 * SQL condition: the row aliased `alias` carries an identity whose admission
 * is canonical in the follower's key set. False for a null identity.
 */
export const canonicalAdmission = (
  sql: SqlClient.SqlClient,
  alias: string,
  kind: AdmissionKind,
) =>
  sql`EXISTS (SELECT 1 FROM l1_event_keys k WHERE k.kind = ${kind}
    AND k.key = ${sql(alias)}.l1_event_key AND k.origin_outref = ${sql(alias)}.l1_origin_outref)`;

/** SQL condition: the rows aliased `a` and `b` carry the same identity. */
export const sameAdmission = (sql: SqlClient.SqlClient, a: string, b: string) =>
  sql`${sql(a)}.l1_event_key = ${sql(b)}.l1_event_key AND ${sql(a)}.l1_origin_outref = ${sql(b)}.l1_origin_outref`;

/** SQL condition: the row aliased `alias` carries an identity the follower no longer holds (an orphan). */
export const orphanedAdmission = (
  sql: SqlClient.SqlClient,
  alias: string,
  kind: AdmissionKind,
) =>
  sql`${sql(alias)}.l1_event_key IS NOT NULL AND NOT ${canonicalAdmission(sql, alias, kind)}`;

/**
 * The forced kind (N10b). A forced row (`forced_transaction_utxos`) carries
 * no identity columns: its admission is its order output, so its key and its
 * origin outref are both that outref in the follower's 34-byte encoding (the
 * 32-byte transaction hash, then the output index as u16 big-endian). The
 * forced-order derivation enters it into `l1_event_keys` under this kind; a
 * follower rewind past the order's block deletes it.
 */
export const FORCED_ADMISSION_KIND = "forced";

const forcedOutRef = (sql: SqlClient.SqlClient, alias: string) =>
  sql`(${sql(alias)}.tx_order_l1_tx_hash || substring(int4send(${sql(alias)}.tx_order_l1_output_index) from 3 for 2))`;

/**
 * SQL condition: the forced row aliased `alias` is backed by its order, in
 * the follower's key set or as the projection's order row (a row ingested
 * before its key was written is backed by its order row).
 */
export const canonicalForcedAdmission = (
  sql: SqlClient.SqlClient,
  alias: string,
) =>
  sql`(EXISTS (SELECT 1 FROM l1_event_keys k WHERE k.kind = ${FORCED_ADMISSION_KIND}
      AND k.key = ${forcedOutRef(sql, alias)} AND k.origin_outref = ${forcedOutRef(sql, alias)})
    OR EXISTS (SELECT 1 FROM node_l1_forced_order_fields o
      WHERE o.order_tx_hash = ${sql(alias)}.tx_order_l1_tx_hash
        AND o.order_output_index = ${sql(alias)}.tx_order_l1_output_index))`;

/** SQL condition: the forced row aliased `alias` is a member of a block journal neither finalized nor abandoned. */
export const forcedInActiveBlock = (sql: SqlClient.SqlClient, alias: string) =>
  sql`EXISTS (SELECT 1 FROM pending_block_finalization_forced_transactions m
    JOIN pending_block_finalizations p ON p.header_hash = m.header_hash
    WHERE m.member_id = ${sql(alias)}.tx_order_id
      AND p.status NOT IN ('locally_applied', 'abandoned'))`;

/**
 * SQL condition: the forced row aliased `alias` has no header, its order is
 * gone, and no unfinished block journal holds it. The forced-order hook
 * deletes such a row.
 */
export const abandonedForcedAdmission = (
  sql: SqlClient.SqlClient,
  alias: string,
) =>
  sql`${sql(alias)}.projected_header_hash IS NULL
    AND NOT ${canonicalForcedAdmission(sql, alias)}
    AND NOT ${forcedInActiveBlock(sql, alias)}`;

/**
 * SQL condition: the forced row aliased `alias` has no header and its order
 * is gone, but an unfinished block journal holds it: an orphan for the
 * event-history recovery, as a deposit or withdrawal orphan is. A row with
 * a header is its header's to settle.
 */
export const orphanedForcedAdmission = (
  sql: SqlClient.SqlClient,
  alias: string,
) =>
  sql`${sql(alias)}.projected_header_hash IS NULL
    AND NOT ${canonicalForcedAdmission(sql, alias)}
    AND ${forcedInActiveBlock(sql, alias)}`;
