import type { PoolClient } from "pg";

import type { StoreData } from "../store.committee-store.js";
import {
  conflictEvidenceKey,
  peerBroadcastKey,
  peerNonceKey,
  signatureKey,
} from "../store.committee-store.js";
import { normalizeStoreData } from "../store.normalize-store-data.js";
import { emptyStoreData } from "../store.parse-decision-outbox-record.js";
import {
  decodeRecord,
  encodeRecord,
} from "./postgres.assert-postgres-decision-retry.js";
import {
  postgresPromiseStoreResourceUsage,
  PROMISE_STORE_RESOURCE_SQL,
} from "./promise-resource-usage.js";
import {
  type CommitteeRetirementFloor,
  parseRetirementFloor,
} from "./retirement-model.js";
import {
  applyRetirementPlan,
  type CommitteeRetirementPlan,
} from "./retirement-transition.js";

const maps = [
  ["stateQueueHeaders", "committee_state_queue_headers"],
  ["daPayloads", "committee_da_payloads"],
  ["daSignatures", "committee_da_signatures"],
  ["daConflictEvidence", "committee_da_conflict_evidence"],
  ["daAttestationCandidates", "committee_da_attestation_candidates"],
  ["l1Submissions", "committee_l1_submissions"],
  ["peerBroadcasts", "committee_peer_broadcasts"],
  ["peerHealth", "committee_peer_health"],
  ["peerNonces", "committee_peer_nonces"],
  ["decisionOutbox", "committee_decision_outbox"],
  ["promiseCapacityEvidence", "committee_promise_capacity_evidence"],
] as const;
const identity = (
  family: (typeof maps)[number][0],
  record: Record<string, unknown>,
  row: Record<string, unknown>,
): string => {
  if (family === "peerHealth") return String(record.peerId);
  if (family === "decisionOutbox") return String(record.effectId);
  if (family === "promiseCapacityEvidence") return String(row.evidence_key);
  if (family === "daSignatures")
    return signatureKey(
      String(record.headerHash),
      String(record.availabilityCommitmentDigest),
      Number(record.signerIndex),
    );
  if (family === "daConflictEvidence")
    return conflictEvidenceKey({
      deploymentFingerprint: String(record.deploymentFingerprint),
      evidenceHash: String(record.evidenceHash),
    });
  if (family === "peerBroadcasts")
    return peerBroadcastKey(
      String(record.peerId),
      String(record.headerHash),
      String(record.availabilityCommitmentDigest),
      Number(record.signerIndex),
    );
  if (family === "peerNonces")
    return peerNonceKey(
      String(record.deploymentFingerprint),
      Number(record.signerIndex),
      String(record.nonce),
    );
  if (family === "l1Submissions")
    return `${String(record.headerHash)}:${String(record.txKind)}:${String(record.txHash)}`;
  if (family === "daAttestationCandidates")
    return `${String(record.headerHash)}:${String(record.outRef)}`;
  return String(record.headerHash);
};
/** The metadata row is authoritative under the same advisory transaction lock.
 * Ordinary stores without a floor do not enter the bounded retirement domain. */
export const readPostgresRetirementFloor = async (
  client: Pick<PoolClient, "query">,
): Promise<CommitteeRetirementFloor | undefined> => {
  const result = await client.query<{ record: unknown }>(
    "SELECT record FROM committee_retirement_metadata WHERE id=1",
  );
  return result.rows[0] === undefined
    ? undefined
    : parseRetirementFloor(decodeRecord(result.rows[0].record));
};

/** Called under the same write transaction lock; no filtered actor or row family. */
export const readPostgresRetirementData = async (
  client: PoolClient,
): Promise<StoreData> => {
  const data: Record<string, unknown> = { ...emptyStoreData() };
  for (const [family, table] of maps) {
    const result = await client.query<Record<string, unknown>>(
      `SELECT * FROM ${table} LIMIT 513`,
    );
    if (result.rows.length > 512)
      throw new Error("Retirement store family exceeds the bounded row domain");
    const rows: Record<string, unknown> = {};
    for (const row of result.rows) {
      const record = decodeRecord<Record<string, unknown>>(row.record);
      if (
        record === null ||
        typeof record !== "object" ||
        Array.isArray(record)
      )
        throw new Error("Retirement store row is malformed");
      const key = identity(family, record, row);
      if (rows[key] !== undefined)
        throw new Error("Retirement store row identity is duplicated");
      if (
        row.header_hash !== undefined &&
        row.header_hash !== record.headerHash
      )
        throw new Error("Retirement physical header identity differs");
      if (row.effect_id !== undefined && row.effect_id !== record.effectId)
        throw new Error("Retirement physical effect identity differs");
      if (row.tx_hash !== undefined && row.tx_hash !== record.txHash)
        throw new Error("Retirement physical transaction identity differs");
      if (row.peer_id !== undefined && row.peer_id !== record.peerId)
        throw new Error("Retirement physical peer identity differs");
      if (
        row.commitment_digest !== undefined &&
        row.commitment_digest !== record.availabilityCommitmentDigest &&
        row.commitment_digest !== record.commitmentDigest
      )
        throw new Error("Retirement physical commitment identity differs");
      rows[key] = record;
    }
    data[family] = rows;
  }
  const source = await client.query<{ record: unknown }>(
    "SELECT record FROM committee_l1_source_state WHERE id=1",
  );
  if (source.rows[0]) data.chainCursor = decodeRecord(source.rows[0].record);
  const floor = await readPostgresRetirementFloor(client);
  if (floor !== undefined) data.retirementFloor = floor;
  const d = await client.query<Record<string, unknown>>(
    "SELECT * FROM committee_deployment WHERE id=1",
  );
  if (d.rows[0]) {
    const r = d.rows[0];
    data.deployment = {
      marker: {
        schemaVersion: r.marker_schema_version,
        manifestId: r.manifest_id,
      },
      manifestSha256: r.manifest_sha256,
      contractDeploymentInfoSha256: r.contract_deployment_info_sha256,
      manifestRaw: r.manifest_raw,
    };
  }
  return normalizeStoreData(data);
};
export const writePostgresRetirementPlan = async (
  client: PoolClient,
  data: StoreData,
  plan: CommitteeRetirementPlan,
): Promise<StoreData> => {
  const next = applyRetirementPlan(data, plan);
  const hashes = [...plan.headerHashes];
  for (const [family, table] of maps) {
    if (family === "peerHealth")
      await client.query(
        `DELETE FROM ${table} WHERE NOT(peer_id=ANY($1::text[]))`,
        [[...plan.floor.binding.peerIds]],
      );
    else if (family === "peerNonces")
      await client.query(
        `DELETE FROM ${table} WHERE (record->>'timestampMs')::bigint <= $1`,
        [plan.floor.nonceTimeFloorMs],
      );
    else if (family === "promiseCapacityEvidence")
      await client.query(
        `DELETE FROM ${table} WHERE record->>'headerHash'=ANY($1::text[])`,
        [hashes],
      );
    else
      await client.query(
        `DELETE FROM ${table} WHERE header_hash=ANY($1::text[])`,
        [hashes],
      );
  }
  await client.query(
    "UPDATE committee_l1_source_state SET record=$1::jsonb,updated_at=NOW() WHERE id=1",
    [encodeRecord(next.chainCursor)],
  );
  await client.query(
    "INSERT INTO committee_retirement_metadata(id,record) VALUES(1,$1::jsonb) ON CONFLICT(id) DO UPDATE SET record=EXCLUDED.record",
    [encodeRecord(plan.floor)],
  );
  return next;
};
export const RETIREMENT_SCHEMA_SQL =
  "CREATE TABLE IF NOT EXISTS committee_retirement_metadata(id integer PRIMARY KEY CHECK(id=1),record jsonb NOT NULL)";

export const assertPostgresRetirementResources = async (
  client: PoolClient,
  data: StoreData,
): Promise<void> => {
  const b = data.retirementFloor?.binding;
  if (!b) return;
  const result = await client.query<{ records: string; bytes: string }>(
    PROMISE_STORE_RESOURCE_SQL,
  );
  const usage = postgresPromiseStoreResourceUsage(result.rows[0]);
  if (
    usage.storeRecords > b.maximumRecords ||
    usage.storeEncodedBytes > b.maximumEncodedBytes
  )
    throw new Error(
      "Postgres retained store exceeds the unchanged bounded profile",
    );
};
