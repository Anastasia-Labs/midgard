import type { Pool } from "pg";

export type PromiseStoreResourceUsage = Readonly<{
  /** All durable store rows, including unrelated/retired metadata. */
  storeRecords: number;
  /** Backend serialization bytes: all Postgres record texts. */
  storeEncodedBytes: number;
}>;
export type PromiseStoreResourceLimits = Readonly<{
  storeRecords: number;
  storeEncodedBytes: number;
}>;
export const assertPromiseStoreResourceUsage = (
  value: PromiseStoreResourceUsage,
  limits?: PromiseStoreResourceLimits,
): PromiseStoreResourceUsage => {
  if (
    limits &&
    (!Number.isSafeInteger(limits.storeRecords) ||
      limits.storeRecords <= 0 ||
      !Number.isSafeInteger(limits.storeEncodedBytes) ||
      limits.storeEncodedBytes <= 0 ||
      value.storeRecords > limits.storeRecords ||
      value.storeEncodedBytes > limits.storeEncodedBytes)
  )
    throw new Error("Retained store exceeds the adopted resource domain");
  return value;
};
const usage = (records: unknown, bytes: unknown): PromiseStoreResourceUsage => {
  const storeRecords = Number(records);
  const storeEncodedBytes = Number(bytes);
  if (
    !Number.isSafeInteger(storeRecords) ||
    storeRecords < 0 ||
    !Number.isSafeInteger(storeEncodedBytes) ||
    storeEncodedBytes < 0
  )
    throw new Error("Retained store resource usage is out of range");
  return { storeRecords, storeEncodedBytes };
};
/** One SQL snapshot counts all retained rows without decoding actor subsets. */
export const PROMISE_STORE_RESOURCE_SQL = `
SELECT COUNT(*)::text AS records,
       COALESCE(SUM(octet_length(serialized)),0)::text AS bytes
FROM (
  ${[
    "committee_retirement_metadata",
    "committee_state_queue_headers",
    "committee_l1_source_state",
    "committee_decision_outbox",
    "committee_da_payloads",
    "committee_promise_capacity_evidence",
    "committee_da_signatures",
    "committee_da_conflict_evidence",
    "committee_da_attestation_candidates",
    "committee_l1_submissions",
    "committee_peer_broadcasts",
    "committee_peer_health",
    "committee_peer_nonces",
  ]
    .map((table) => `SELECT record::text AS serialized FROM ${table}`)
    .join(" UNION ALL ")}
  UNION ALL SELECT row_to_json(d)::text AS serialized FROM committee_deployment d
) retained`;
export const postgresPromiseStoreResourceUsage = (
  row: Readonly<{ records: string; bytes: string }> | undefined,
): PromiseStoreResourceUsage => {
  if (
    row === undefined ||
    !/^\d+$/u.test(row.records) ||
    !/^\d+$/u.test(row.bytes)
  )
    throw new Error("Retained store resource count is unavailable");
  return usage(row.records, row.bytes);
};

export const postgresPromiseResources =
  (pool: () => Pick<Pool, "query">) =>
  async (limits?: PromiseStoreResourceLimits) => {
    const result = await pool().query<{ records: string; bytes: string }>(
      PROMISE_STORE_RESOURCE_SQL,
    );
    return assertPromiseStoreResourceUsage(
      postgresPromiseStoreResourceUsage(result.rows[0]),
      limits,
    );
  };
