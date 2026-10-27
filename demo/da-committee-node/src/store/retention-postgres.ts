import type { Pool, PoolClient } from "pg";

import type { DaPayloadRetentionRecord } from "../store.committee-store.js";

/**
 * The retention metadata of `committee_da_payloads` rows. The fields are
 * extracted in Postgres, so the payload bytes never leave the server.
 */
const RETENTION_COLUMNS = `header_hash,
  record->'headerHash' AS record_header_hash,
  record->'fetchedAt' AS fetched_at,
  record->'deploymentFingerprint' AS deployment_fingerprint`;

type RetentionRow = {
  readonly header_hash: string;
  readonly record_header_hash: unknown;
  readonly fetched_at: unknown;
  readonly deployment_fingerprint: unknown;
};

const retentionRecordOf = (row: RetentionRow): DaPayloadRetentionRecord => {
  if (row.record_header_hash !== row.header_hash)
    throw new Error(
      "Postgres DA stored payload row key does not match record identity",
    );
  if (
    typeof row.fetched_at !== "string" ||
    typeof row.deployment_fingerprint !== "string"
  )
    throw new Error(
      "Postgres DA stored payload row has no string fetchedAt and deploymentFingerprint",
    );
  return {
    headerHash: row.header_hash,
    fetchedAt: row.fetched_at,
    deploymentFingerprint: row.deployment_fingerprint,
  };
};

/** One keyset page of retained payloads' retention metadata. */
export const listDaPayloadRetentionPage = async (
  pool: Pool,
  args: { readonly afterHeaderHash?: string; readonly limit: number },
): Promise<readonly DaPayloadRetentionRecord[]> => {
  if (!Number.isSafeInteger(args.limit) || args.limit < 1)
    throw new Error("retention page limit must be a positive safe integer");
  // Two shapes, so each page is an index range scan on the primary key.
  const result =
    args.afterHeaderHash === undefined
      ? await pool.query<RetentionRow>(
          `SELECT ${RETENTION_COLUMNS} FROM committee_da_payloads
           ORDER BY header_hash LIMIT $1`,
          [args.limit],
        )
      : await pool.query<RetentionRow>(
          `SELECT ${RETENTION_COLUMNS} FROM committee_da_payloads
           WHERE header_hash > $1 ORDER BY header_hash LIMIT $2`,
          [args.afterHeaderHash, args.limit],
        );
  return result.rows.map(retentionRecordOf);
};

/** One payload's retention metadata, its row locked for the deletion decision. */
export const lockDaPayloadRetentionRecord = async (
  client: PoolClient,
  headerHash: string,
): Promise<DaPayloadRetentionRecord | undefined> => {
  const row = (
    await client.query<RetentionRow>(
      `SELECT ${RETENTION_COLUMNS} FROM committee_da_payloads
       WHERE header_hash = $1 FOR UPDATE`,
      [headerHash],
    )
  ).rows[0];
  return row === undefined ? undefined : retentionRecordOf(row);
};
