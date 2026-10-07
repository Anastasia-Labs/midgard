import { Pool, type PoolClient } from "pg";

import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
  DaStoredConflictEvidenceRecord,
} from "../domain.js";
import {
  type DecisionOutboxRecord,
  jsonReplacer,
  type L1SourceState,
  mergeL1SourceState,
  parseL1SourceState,
  parseStoredJson,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "../store.js";
import type { PostgresStoreInstanceLockEvents } from "./postgres.instance-lock.js";

export type JsonRecordRow = {
  readonly record: unknown;
  readonly deployment_fingerprint?: unknown;
  readonly evidence_hash?: unknown;
  readonly header_hash?: unknown;
  readonly commitment_digest?: unknown;
  readonly conflicting_header_hash?: unknown;
  readonly conflicting_commitment_digest?: unknown;
  readonly reporter_peer_id?: unknown;
  readonly signer_index?: unknown;
  readonly effect_id?: unknown;
};

export const COMMITTEE_TABLES = [
  "deployment",
  "state_queue_headers",
  "l1_source_state",
  "decision_outbox",
  "da_payloads",
  "da_signatures",
  "da_conflict_evidence",
  "da_attestation_candidates",
  "l1_submissions",
  "peer_broadcasts",
  "peer_health",
  "peer_nonces",
] as const;

/** The instance lock's events; see `PostgresStoreInstanceLock`. */
export type PostgresCommitteeStoreOptions = PostgresStoreInstanceLockEvents;

export const ensureL1SourceStateRow = async (
  client: PoolClient,
  proposed: L1SourceState,
): Promise<void> => {
  await client.query(
    `INSERT INTO committee_l1_source_state (id, record, updated_at)
     VALUES (1, $1::jsonb, NOW())
     ON CONFLICT (id) DO NOTHING`,
    [encodeRecord(proposed)],
  );
};

export const lockL1SourceState = async (
  client: PoolClient,
): Promise<L1SourceState> => {
  const result = await client.query<JsonRecordRow>(
    "SELECT record FROM committee_l1_source_state WHERE id = 1 FOR UPDATE",
  );
  const decoded = decodeRow<unknown>(result.rows[0]);
  if (decoded === undefined) {
    throw new Error("decision outbox lacks durable L1 source state");
  }
  return parseL1SourceState(decoded);
};

export const mergeLockedL1SourceState = async (
  client: PoolClient,
  proposed: L1SourceState,
): Promise<L1SourceState> => {
  await ensureL1SourceStateRow(client, proposed);
  const current = await lockL1SourceState(client);
  const merged = mergeL1SourceState(current, proposed);
  await client.query(
    `UPDATE committee_l1_source_state
     SET record = $1::jsonb, updated_at = NOW()
     WHERE id = 1`,
    [encodeRecord(merged)],
  );
  return merged;
};

export const upsertRecordWithPool = async <T>(
  pool: Pool,
  tableName: string,
  headerHash: string,
  record: T,
): Promise<void> => {
  await pool.query(
    `INSERT INTO ${tableName} (header_hash, record, updated_at)
     VALUES ($1, $2::jsonb, NOW())
     ON CONFLICT (header_hash) DO UPDATE SET
       record = EXCLUDED.record,
       updated_at = NOW()`,
    [headerHash, encodeRecord(record)],
  );
};

export const upsertRecordWithClient = async <T>(
  client: PoolClient,
  tableName: string,
  headerHash: string,
  record: T,
): Promise<void> => {
  await client.query(
    `INSERT INTO ${tableName} (header_hash, record, updated_at)
     VALUES ($1, $2::jsonb, NOW())
     ON CONFLICT (header_hash) DO UPDATE SET
       record = EXCLUDED.record,
       updated_at = NOW()`,
    [headerHash, encodeRecord(record)],
  );
};

export const upsertSignatureWithClient = async (
  client: PoolClient,
  record: DaSignatureRecordV1,
): Promise<void> => {
  // The member's own signature is its signed decision (class B): the
  // header's end time is kept in its row, for the obligations projection.
  const endTimeMs =
    record.source === "local" ? signedEndTimeMs(record).toString() : null;
  await client.query(
    `INSERT INTO committee_da_signatures
       (header_hash, commitment_digest, signer_index, record, end_time_ms, updated_at)
     VALUES ($1, $2, $3, $4::jsonb, $5, NOW())
     ON CONFLICT (header_hash, commitment_digest, signer_index) DO UPDATE SET
       record = EXCLUDED.record,
       end_time_ms = EXCLUDED.end_time_ms,
       updated_at = NOW()`,
    [
      record.headerHash,
      record.availabilityCommitmentDigest,
      record.signerIndex,
      encodeRecord(record),
      endTimeMs,
    ],
  );
};

const signedEndTimeMs = (record: DaSignatureRecordV1): bigint => {
  const text = record.validation.l1Header.endTime;
  if (!/^(0|[1-9][0-9]*)$/u.test(text))
    throw new Error(
      `local DA signature for ${record.headerHash} has no end time in milliseconds`,
    );
  return BigInt(text);
};

export const queryOne = async <T>(
  client: PoolClient,
  query: string,
  values: readonly unknown[],
  parseRecord: (record: unknown) => T,
  validateRow: (row: JsonRecordRow, record: T) => void,
): Promise<T | undefined> => {
  const result = await client.query<JsonRecordRow>(query, [...values]);
  return decodeParsedRow(result.rows[0], parseRecord, validateRow);
};

export const encodeRecord = (record: unknown): string =>
  JSON.stringify(record, jsonReplacer);

export const decodeRow = <T>(row: JsonRecordRow | undefined): T | undefined =>
  row === undefined ? undefined : decodeRecord<T>(row.record);

export const decodeParsedRow = <T>(
  row: JsonRecordRow | undefined,
  parseRecord: (record: unknown) => T,
  validateRow: (row: JsonRecordRow, record: T) => void,
): T | undefined =>
  row === undefined
    ? undefined
    : validateParsedRow(row, parseRecord, validateRow);

const validateParsedRow = <T>(
  row: JsonRecordRow,
  parseRecord: (record: unknown) => T,
  validateRow: (row: JsonRecordRow, record: T) => void,
): T => {
  const record = parseRecord(decodeRecord<unknown>(row.record));
  validateRow(row, record);
  return record;
};

export const assertPayloadRowIdentity = (
  row: JsonRecordRow,
  record: DaPayloadRecord,
): void => {
  if (row.header_hash !== record.headerHash) {
    throw new Error(
      "Postgres DA stored payload row key does not match record identity",
    );
  }
};

export const assertSignatureRowIdentity = (
  row: JsonRecordRow,
  record: DaSignatureRecord,
): void => {
  if (
    row.header_hash !== record.headerHash ||
    row.commitment_digest !== record.availabilityCommitmentDigest ||
    row.signer_index !== record.signerIndex
  ) {
    throw new Error(
      "Postgres DA signature row key does not match record identity",
    );
  }
};

export const assertConflictEvidenceRowIdentity = (
  row: JsonRecordRow,
  record: DaStoredConflictEvidenceRecord,
): void => {
  if (
    row.deployment_fingerprint !== record.deploymentFingerprint ||
    row.evidence_hash !== record.evidenceHash ||
    row.header_hash !== record.headerHash ||
    row.commitment_digest !== record.commitmentDigest ||
    record.conflictingHeaderHash !== record.headerHash ||
    row.conflicting_commitment_digest !== record.conflictingCommitmentDigest ||
    row.signer_index !== record.signerIndex ||
    row.reporter_peer_id !== record.reporterPeerId
  ) {
    throw new Error(
      "Postgres DA conflict evidence row key does not match record identity",
    );
  }
};

export const assertDecisionOutboxRowIdentity = (
  row: JsonRecordRow,
  record: DecisionOutboxRecord,
): void => {
  if (
    row.effect_id !== record.effectId ||
    row.header_hash !== record.headerHash
  ) {
    throw new Error(
      "Postgres decision outbox row key does not match record identity",
    );
  }
};

export const assertPostgresDecisionRetry = (
  existing: DecisionOutboxRecord | undefined,
  next: DecisionOutboxRecord,
): void => {
  if (existing === undefined) {
    return;
  }
  if (
    existing.effectId !== next.effectId ||
    existing.deploymentFingerprint !== next.deploymentFingerprint ||
    existing.sourceMode !== next.sourceMode ||
    existing.network !== next.network ||
    existing.effectKind !== next.effectKind ||
    existing.headerHash !== next.headerHash ||
    existing.stateQueueOutRef !== next.stateQueueOutRef ||
    existing.signerIndex !== next.signerIndex ||
    existing.slot !== next.slot ||
    existing.blockHash !== next.blockHash ||
    existing.finalized !== next.finalized ||
    existing.createdAt !== next.createdAt ||
    next.attemptCount !== existing.attemptCount + 1
  ) {
    throw new Error("decision outbox retry does not match durable identity");
  }
};

export const assertPostgresDecisionSourceState = (
  effect: DecisionOutboxRecord,
  sourceState: L1SourceState,
): void => {
  const observation = sourceState.observations.find(
    ({ headerHash }) => headerHash === effect.headerHash,
  );
  if (
    sourceState.status !== "healthy" ||
    sourceState.sourceMode !== effect.sourceMode ||
    sourceState.network !== effect.network ||
    observation?.stateQueueOutRef !== effect.stateQueueOutRef ||
    observation.stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS ||
    observation.finalized !== true ||
    observation.hasPersistedDecision !== true ||
    observation.slot !== effect.slot ||
    observation.blockHash !== effect.blockHash
  ) {
    throw new Error("decision outbox lacks matching durable L1 observation");
  }
};

export const assertPostgresDecisionSignature = (
  effect: DecisionOutboxRecord,
  signature: DaSignatureRecordV1 | undefined,
): void => {
  if (
    (effect.effectKind === "signature_publish" &&
      (signature === undefined ||
        signature.deploymentFingerprint !== effect.deploymentFingerprint ||
        signature.headerHash !== effect.headerHash ||
        signature.signerIndex !== effect.signerIndex ||
        signature.validation.stateQueueOutRef !== effect.stateQueueOutRef ||
        signature.l1ChainPoint.slot !== effect.slot ||
        signature.l1ChainPoint.blockHash !== effect.blockHash)) ||
    (effect.effectKind === "l1_reconcile" && signature !== undefined)
  ) {
    throw new Error("decision outbox signature does not match effect identity");
  }
};

export const decodeRecord = <T>(record: unknown): T =>
  parseStoredJson(JSON.stringify(record)) as T;
