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
