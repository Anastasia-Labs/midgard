import { open, readFile } from "node:fs/promises";

import type { Pool } from "pg";

export type PromiseStoreResourceUsage = Readonly<{
  /** All durable store rows, including unrelated/retired metadata. */
  storeRecords: number;
  /** Backend serialization bytes: exact JSON file or all PG record texts. */
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
const boundedJsonBytes = async (
  filePath: string,
  maximum: number,
): Promise<Buffer> => {
  if (!Number.isSafeInteger(maximum) || maximum <= 0)
    throw new Error("Invalid store byte limit");
  const file = await open(filePath, "r");
  try {
    if ((await file.stat()).size > maximum)
      throw new Error("Retained store exceeds the adopted byte domain");
    const chunks: Buffer[] = [];
    let total = 0;
    while (true) {
      const chunk = Buffer.alloc(Math.min(64 * 1024, maximum - total + 1));
      const { bytesRead } = await file.read(chunk, 0, chunk.byteLength, null);
      if (bytesRead === 0) return Buffer.concat(chunks, total);
      total += bytesRead;
      if (total > maximum)
        throw new Error("Retained store grew beyond the adopted byte domain");
      chunks.push(chunk.subarray(0, bytesRead));
    }
  } finally {
    await file.close();
  }
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
export const jsonPromiseStoreResourceUsage = async (
  filePath: string,
  limits?: PromiseStoreResourceLimits,
): Promise<PromiseStoreResourceUsage> => {
  const bytes = limits
    ? await boundedJsonBytes(filePath, limits.storeEncodedBytes)
    : await readFile(filePath);
  const data = JSON.parse(bytes.toString("utf8")) as Record<string, unknown>;
  if (data === null || typeof data !== "object" || Array.isArray(data))
    throw new Error("Retained store resource data is malformed");
  let records = 0;
  for (const [key, value] of Object.entries(data)) {
    if (value === undefined) continue;
    if (
      key === "deployment" ||
      key === "chainCursor" ||
      key === "retirementFloor"
    ) {
      records++;
    } else {
      if (value === null || typeof value !== "object" || Array.isArray(value))
        throw new Error("Retained store resource map is malformed");
      records += Object.keys(value).length;
    }
  }
  return assertPromiseStoreResourceUsage(
    usage(records, bytes.byteLength),
    limits,
  );
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

export const jsonPromiseResources =
  (filePath: () => string) => (limits?: PromiseStoreResourceLimits) =>
    jsonPromiseStoreResourceUsage(filePath(), limits);
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
