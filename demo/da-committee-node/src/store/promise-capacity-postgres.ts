import type { Pool, PoolClient } from "pg";

import {
  mergePromiseCapacityEvidence,
  parsePromiseCapacityEvidence,
  type PromiseCapacityEvidence,
  promiseCapacityEvidenceKey,
} from "../availability/promise-capacity-evidence.js";
import {
  decodeRecord,
  encodeRecord,
  lockL1SourceState,
} from "./postgres.assert-postgres-decision-retry.js";

export const reader =
  (pool: () => Pick<Pool, "query">) =>
  async (key: string): Promise<PromiseCapacityEvidence | undefined> => {
    const result = await pool().query<{
      evidence_key: string;
      record: unknown;
    }>(
      "SELECT evidence_key, record FROM committee_promise_capacity_evidence WHERE evidence_key=$1",
      [key],
    );
    if (result.rows.length === 0) return undefined;
    const parsed = parsePromiseCapacityEvidence(
      decodeRecord(result.rows[0]!.record),
    );
    if (promiseCapacityEvidenceKey(parsed) !== result.rows[0]!.evidence_key)
      throw new Error("Capacity evidence row identity mismatch");
    return parsed;
  };

export const writer =
  (
    withClient: <T>(run: (client: PoolClient) => Promise<T>) => Promise<T>,
    assertHeld: (client: PoolClient) => Promise<void>,
  ) =>
  async (
    record: PromiseCapacityEvidence,
    expectedPointId?: string,
  ): Promise<PromiseCapacityEvidence> => {
    const canonical = parsePromiseCapacityEvidence(record);
    const key = promiseCapacityEvidenceKey(canonical);
    return withClient(async (client) => {
      const state = await lockL1SourceState(client);
      if (state?.status === "quarantined")
        throw new Error(
          "Cannot persist capacity evidence while source is quarantined",
        );
      const rows = await client.query<{ record: unknown }>(
        "SELECT record FROM committee_promise_capacity_evidence WHERE evidence_key=$1 FOR UPDATE",
        [key],
      );
      const existing =
        rows.rows[0] === undefined
          ? undefined
          : parsePromiseCapacityEvidence(decodeRecord(rows.rows[0].record));
      const saved = mergePromiseCapacityEvidence(
        existing,
        canonical,
        expectedPointId,
      );
      await assertHeld(client);
      await client.query(
        "INSERT INTO committee_promise_capacity_evidence(evidence_key,record) VALUES($1,$2::jsonb) ON CONFLICT(evidence_key) DO UPDATE SET record=EXCLUDED.record",
        [key, encodeRecord(saved)],
      );
      return saved;
    });
  };
