import {
  assertDeploymentMarkerV1Matches,
  type DeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
  parseDeploymentMarkerV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { Pool, type PoolClient } from "pg";

import type {
  DaAttestationCandidateRecord,
  DaPayloadRecord,
  DaPeerBroadcastRecord,
  DaPeerHealthRecord,
  DaPeerNonceRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
  DaStoredConflictEvidenceRecordV1,
  DaStoredPayloadRecordV1,
  L1SubmissionRecord,
  StateQueueHeaderRecord,
} from "../domain.js";
import {
  parseDaSignatureRecordV1,
  parseDaStoredConflictEvidenceRecordV1,
  parseDaStoredPayloadRecordV1,
} from "../domain.js";
import {
  DECISION_EFFECT_PENDING_LEASE_MS,
  type DecisionOutboxRecord,
  type DecisionOutboxStatus,
  jsonReplacer,
  jsonReviver,
  type L1SourceState,
  mergeL1SourceState,
  mergeQuarantinedL1SourceState,
  parseDecisionOutboxRecord,
  parseL1SourceState,
  resolveDaPayloadSave,
  type WatcherDeploymentRecord,
  type WatcherStore,
} from "../store.js";

type JsonRecordRow = {
  readonly record: unknown;
  readonly deployment_fingerprint?: unknown;
  readonly evidence_hash?: unknown;
  readonly header_hash?: unknown;
  readonly conflicting_header_hash?: unknown;
  readonly reporter_peer_id?: unknown;
  readonly signer_index?: unknown;
  readonly effect_id?: unknown;
};

export class PostgresWatcherStore implements WatcherStore {
  private readonly pool: Pool;

  private constructor(pool: Pool) {
    this.pool = pool;
  }

  static async open(databaseUrl: string): Promise<PostgresWatcherStore> {
    const parsed = new URL(databaseUrl);
    if (parsed.protocol !== "postgres:" && parsed.protocol !== "postgresql:") {
      throw new Error(
        "WATCHER_DATABASE_URL must be a postgres:// or postgresql:// URL",
      );
    }
    const store = new PostgresWatcherStore(
      new Pool({
        connectionString: databaseUrl,
        max: 10,
      }),
    );
    await store.initSchema();
    return store;
  }

  async close(): Promise<void> {
    await this.pool.end();
  }

  async initDeployment(args: {
    readonly marker: DeploymentMarkerV1;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void> {
    const marker = parseDeploymentMarkerV1(args.marker);
    const result = await this.pool.query<{
      readonly marker_schema_version: string;
      readonly manifest_id: string;
    }>(
      "SELECT marker_schema_version, manifest_id FROM watcher_deployment WHERE id = 1",
    );
    const existing = result.rows[0];
    if (existing !== undefined) {
      try {
        assertDeploymentMarkerV1Matches(
          marker,
          {
            schemaVersion: existing.marker_schema_version,
            manifestId: existing.manifest_id,
          },
          "DA Postgres store",
        );
      } catch {
        throw new Error(
          `stale_deployment_state_requires_fresh_redeploy: stored_manifest_id=${existing.manifest_id}, canonical_manifest_id=${marker.manifestId}, contract_deployment_info_sha256=${args.contractDeploymentInfoSha256}; refusing to reuse stale watcher state; perform an explicit fresh redeploy/reset before deleting local watcher state.`,
        );
      }
    }
    await this.pool.query(
      `INSERT INTO watcher_deployment (
         id,
         marker_schema_version,
         manifest_id,
         manifest_sha256,
         contract_deployment_info_sha256,
         manifest_raw,
         updated_at
       )
       VALUES (1, $1, $2, $3, $4, $5, NOW())
       ON CONFLICT (id) DO UPDATE SET
         marker_schema_version = EXCLUDED.marker_schema_version,
         manifest_id = EXCLUDED.manifest_id,
         manifest_sha256 = EXCLUDED.manifest_sha256,
         contract_deployment_info_sha256 = EXCLUDED.contract_deployment_info_sha256,
         manifest_raw = EXCLUDED.manifest_raw,
         updated_at = NOW()`,
      [
        marker.schemaVersion,
        marker.manifestId,
        args.manifestSha256,
        args.contractDeploymentInfoSha256,
        args.manifestRaw,
      ],
    );
  }

  async getDeployment(): Promise<WatcherDeploymentRecord | undefined> {
    const result = await this.pool.query<{
      readonly marker_schema_version: string;
      readonly manifest_id: string;
      readonly manifest_sha256: string;
      readonly contract_deployment_info_sha256: string;
      readonly manifest_raw: string;
    }>(
      `SELECT marker_schema_version,
              manifest_id,
              manifest_sha256,
              contract_deployment_info_sha256,
              manifest_raw
       FROM watcher_deployment
       WHERE id = 1`,
    );
    const row = result.rows[0];
    if (row === undefined) {
      return undefined;
    }
    return {
      marker: parseDeploymentMarkerV1({
        schemaVersion: row.marker_schema_version,
        manifestId: row.manifest_id,
      }),
      manifestSha256: row.manifest_sha256,
      contractDeploymentInfoSha256: row.contract_deployment_info_sha256,
      manifestRaw: row.manifest_raw,
    };
  }

  async getL1SourceState(): Promise<L1SourceState | undefined> {
    const result = await this.pool.query<JsonRecordRow>(
      "SELECT record FROM watcher_l1_source_state WHERE id = 1",
    );
    const decoded = decodeRow<unknown>(result.rows[0]);
    return decoded === undefined ? undefined : parseL1SourceState(decoded);
  }

  async saveL1SourceState(state: L1SourceState): Promise<void> {
    const canonical = parseL1SourceState(state);
    await this.withClient(async (client) => {
      await client.query("BEGIN");
      try {
        await mergeLockedL1SourceState(client, canonical);
        await client.query("COMMIT");
      } catch (error) {
        await client.query("ROLLBACK");
        throw error;
      }
    });
  }

  async getDecisionOutbox(
    effectId: string,
  ): Promise<DecisionOutboxRecord | undefined> {
    return this.getParsedRecord(
      "SELECT effect_id, header_hash, record FROM watcher_decision_outbox WHERE effect_id = $1",
      [effectId],
      parseDecisionOutboxRecord,
      assertDecisionOutboxRowIdentity,
    );
  }

  async listDecisionOutbox(
    headerHash?: string,
  ): Promise<readonly DecisionOutboxRecord[]> {
    return this.listParsedRecords(
      headerHash === undefined
        ? `SELECT effect_id, header_hash, record FROM watcher_decision_outbox
           ORDER BY effect_id`
        : `SELECT effect_id, header_hash, record FROM watcher_decision_outbox
           WHERE header_hash = $1 ORDER BY effect_id`,
      headerHash === undefined ? [] : [headerHash],
      parseDecisionOutboxRecord,
      assertDecisionOutboxRowIdentity,
    );
  }

  async beginDecisionEffect(args: {
    readonly effect: DecisionOutboxRecord;
    readonly sourceState: L1SourceState;
    readonly signature?: DaSignatureRecord;
  }): Promise<void> {
    const effect = parseDecisionOutboxRecord(args.effect);
    const proposedSourceState = parseL1SourceState(args.sourceState);
    if (effect.status !== "pending") {
      throw new Error("decision outbox begin requires pending status");
    }
    const signature =
      args.signature === undefined
        ? undefined
        : parseDaSignatureRecordV1(args.signature);
    assertPostgresDecisionSignature(effect, signature);
    await this.withClient(async (client) => {
      await client.query("BEGIN");
      try {
        const sourceState = await mergeLockedL1SourceState(
          client,
          proposedSourceState,
        );
        assertPostgresDecisionSourceState(effect, sourceState);
        const current = await queryOne(
          client,
          "SELECT effect_id, header_hash, record FROM watcher_decision_outbox WHERE effect_id = $1 FOR UPDATE",
          [effect.effectId],
          parseDecisionOutboxRecord,
          assertDecisionOutboxRowIdentity,
        );
        assertPostgresDecisionRetry(current, effect);
        if (current?.status === "pending") {
          const lease = await client.query<{ lease_expired: boolean }>(
            `SELECT updated_at +
                    ($2::bigint * INTERVAL '1 millisecond') <= NOW()
                    AS lease_expired
             FROM watcher_decision_outbox
             WHERE effect_id = $1`,
            [effect.effectId, DECISION_EFFECT_PENDING_LEASE_MS],
          );
          if (lease.rows[0]?.lease_expired !== true) {
            throw new Error(
              "decision outbox pending attempt lease has not expired",
            );
          }
        }
        await client.query(
          `INSERT INTO watcher_decision_outbox
             (effect_id, header_hash, record, updated_at)
           VALUES ($1, $2, $3::jsonb, NOW())
           ON CONFLICT (effect_id) DO UPDATE SET
             record = EXCLUDED.record, updated_at = NOW()`,
          [effect.effectId, effect.headerHash, encodeRecord(effect)],
        );
        if (signature !== undefined) {
          await upsertSignatureWithClient(client, signature);
        }
        await client.query("COMMIT");
      } catch (error) {
        await client.query("ROLLBACK");
        throw error;
      }
    });
  }

  async completeDecisionEffect(args: {
    readonly effectId: string;
    readonly expectedAttemptCount: number;
    readonly status: Exclude<DecisionOutboxStatus, "pending">;
    readonly updatedAt: string;
    readonly lastError?: string;
    readonly signature?: DaSignatureRecord;
  }): Promise<void> {
    await this.withClient(async (client) => {
      await client.query("BEGIN");
      try {
        const sourceState = await lockL1SourceState(client);
        const existing = await queryOne(
          client,
          "SELECT effect_id, header_hash, record FROM watcher_decision_outbox WHERE effect_id = $1 FOR UPDATE",
          [args.effectId],
          parseDecisionOutboxRecord,
          assertDecisionOutboxRowIdentity,
        );
        if (existing === undefined) {
          throw new Error(`decision outbox effect ${args.effectId} is missing`);
        }
        if (
          existing.status !== "pending" ||
          existing.attemptCount !== args.expectedAttemptCount ||
          existing.quarantineReason !== undefined ||
          existing.quarantinedAt !== undefined
        ) {
          throw new Error(
            "decision outbox completion does not match the pending attempt",
          );
        }
        assertPostgresDecisionSourceState(existing, sourceState);
        const signature =
          args.signature === undefined
            ? undefined
            : parseDaSignatureRecordV1(args.signature);
        assertPostgresDecisionSignature(existing, signature);
        const completed = parseDecisionOutboxRecord({
          ...existing,
          status: args.status,
          updatedAt: args.updatedAt,
          ...(args.lastError === undefined
            ? { lastError: undefined }
            : { lastError: args.lastError }),
        });
        await client.query(
          `UPDATE watcher_decision_outbox
           SET record = $2::jsonb, updated_at = NOW()
           WHERE effect_id = $1`,
          [args.effectId, encodeRecord(completed)],
        );
        if (signature !== undefined) {
          await upsertSignatureWithClient(client, signature);
        }
        await client.query("COMMIT");
      } catch (error) {
        await client.query("ROLLBACK");
        throw error;
      }
    });
  }

  async quarantineL1Decisions(state: L1SourceState): Promise<void> {
    const canonical = parseL1SourceState(state);
    if (canonical.status !== "quarantined") {
      throw new Error(
        "L1 decision quarantine requires quarantined source state",
      );
    }
    await this.withClient(async (client) => {
      await client.query("BEGIN");
      try {
        await ensureL1SourceStateRow(client, canonical);
        const current = await lockL1SourceState(client);
        const quarantined = mergeQuarantinedL1SourceState(current, canonical);
        const headerHashes = quarantined.observations
          .filter(({ hasPersistedDecision }) => hasPersistedDecision)
          .map(({ headerHash }) => headerHash);
        const reason = `l1_source_quarantined:${quarantined.quarantineReason!}`;
        await client.query(
          `UPDATE watcher_l1_source_state
           SET record = $1::jsonb, updated_at = NOW()
           WHERE id = 1`,
          [encodeRecord(quarantined)],
        );
        if (headerHashes.length > 0) {
          await client.query(
            `UPDATE watcher_state_queue_headers
             SET record =
                   record ||
                   jsonb_build_object(
                     'status', 'conflicted',
                     'validationErrors',
                       COALESCE(record->'validationErrors', '[]'::jsonb) ||
                       to_jsonb($2::text),
                     'updatedAt', $3::text
                   ),
                 updated_at = NOW()
             WHERE header_hash = ANY($1::text[])`,
            [headerHashes, reason, quarantined.quarantinedAt],
          );
          await client.query(
            `UPDATE watcher_da_payloads
             SET record =
                   record ||
                   jsonb_build_object(
                     'validationStatus', 'conflicted',
                     'validationError', $2::text
                   ),
                 updated_at = NOW()
             WHERE header_hash = ANY($1::text[])`,
            [headerHashes, reason],
          );
          await client.query(
            `UPDATE watcher_da_signatures
             SET record = record || jsonb_build_object(
                   'broadcastStatus', 'post_failed'
                 ),
                 updated_at = NOW()
             WHERE header_hash = ANY($1::text[])`,
            [headerHashes],
          );
          await client.query(
            `UPDATE watcher_l1_submissions
             SET record =
                   record ||
                   jsonb_build_object(
                     'resultStatus', 'failed',
                     'failureCause', $2::text
                   ),
                 updated_at = NOW()
             WHERE header_hash = ANY($1::text[])`,
            [headerHashes, reason],
          );
          await client.query(
            `UPDATE watcher_peer_broadcasts
             SET record =
                   (record - 'nextAttemptAt') ||
                   jsonb_build_object(
                     'status', 'failed',
                     'lastError', $2::text,
                     'updatedAt', $3::text
                   ),
                 updated_at = NOW()
             WHERE header_hash = ANY($1::text[])`,
            [headerHashes, reason, quarantined.quarantinedAt],
          );
          await client.query(
            `UPDATE watcher_decision_outbox
             SET record =
                   record ||
                   jsonb_build_object(
                     'status', 'failed',
                     'lastError', $2::text,
                     'quarantineReason', $3::text,
                     'quarantinedAt', $4::text,
                     'updatedAt', $4::text
                   ),
                 updated_at = NOW()
             WHERE header_hash = ANY($1::text[])`,
            [
              headerHashes,
              reason,
              quarantined.quarantineReason,
              quarantined.quarantinedAt,
            ],
          );
        }
        await client.query("COMMIT");
      } catch (error) {
        await client.query("ROLLBACK");
        throw error;
      }
    });
  }

  async upsertStateQueueHeader(record: StateQueueHeaderRecord): Promise<void> {
    await this.upsertRecord(
      "watcher_state_queue_headers",
      record.headerHash,
      record,
    );
  }

  async listStateQueueHeaders(): Promise<readonly StateQueueHeaderRecord[]> {
    return this.listRecords<StateQueueHeaderRecord>(
      "SELECT record FROM watcher_state_queue_headers ORDER BY header_hash",
    );
  }

  async getStateQueueHeader(
    headerHash: string,
  ): Promise<StateQueueHeaderRecord | undefined> {
    return this.getRecord<StateQueueHeaderRecord>(
      "SELECT record FROM watcher_state_queue_headers WHERE header_hash = $1",
      [headerHash],
    );
  }

  async saveDaPayload(
    record: DaPayloadRecord,
  ): Promise<DaStoredPayloadRecordV1> {
    const canonicalRecord = parseDaStoredPayloadRecordV1(record);
    return this.withClient(async (client) => {
      await client.query("BEGIN");
      try {
        const existing = await queryOne(
          client,
          "SELECT header_hash, record FROM watcher_da_payloads WHERE header_hash = $1 FOR UPDATE",
          [canonicalRecord.headerHash],
          parseDaStoredPayloadRecordV1,
          assertPayloadRowIdentity,
        );
        const saved = resolveDaPayloadSave(existing, canonicalRecord);
        await upsertRecordWithClient(
          client,
          "watcher_da_payloads",
          canonicalRecord.headerHash,
          saved,
        );
        await client.query("COMMIT");
        return saved;
      } catch (error) {
        await client.query("ROLLBACK");
        throw error;
      }
    });
  }

  async getDaPayload(
    headerHash: string,
  ): Promise<DaStoredPayloadRecordV1 | undefined> {
    return this.getParsedRecord(
      "SELECT header_hash, record FROM watcher_da_payloads WHERE header_hash = $1",
      [headerHash],
      parseDaStoredPayloadRecordV1,
      assertPayloadRowIdentity,
    );
  }

  async saveDaSignature(record: DaSignatureRecord): Promise<void> {
    const canonicalRecord = parseDaSignatureRecordV1(record);
    await this.withClient(async (client) => {
      await client.query("BEGIN");
      try {
        const sourceState = await lockL1SourceState(client);
        if (sourceState?.status === "quarantined") {
          throw new Error(
            "cannot persist a DA signature while the L1 source is quarantined",
          );
        }
        await upsertSignatureWithClient(client, canonicalRecord);
        await client.query("COMMIT");
      } catch (error) {
        await client.query("ROLLBACK");
        throw error;
      }
    });
  }

  async getDaSignature(args: {
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecordV1 | undefined> {
    return this.getParsedRecord(
      `SELECT header_hash, signer_index, record FROM watcher_da_signatures
       WHERE header_hash = $1 AND signer_index = $2`,
      [args.headerHash, args.signerIndex],
      parseDaSignatureRecordV1,
      assertSignatureRowIdentity,
    );
  }

  async listDaSignatures(
    headerHash?: string,
  ): Promise<readonly DaSignatureRecordV1[]> {
    return this.listParsedRecords(
      headerHash === undefined
        ? `SELECT header_hash, signer_index, record
           FROM watcher_da_signatures
           ORDER BY header_hash, signer_index`
        : `SELECT header_hash, signer_index, record
           FROM watcher_da_signatures
           WHERE header_hash = $1
           ORDER BY header_hash, signer_index`,
      headerHash === undefined ? [] : [headerHash],
      parseDaSignatureRecordV1,
      assertSignatureRowIdentity,
    );
  }

  async saveDaConflictEvidence(
    record: DaStoredConflictEvidenceRecordV1,
  ): Promise<boolean> {
    const canonicalRecord = parseDaStoredConflictEvidenceRecordV1(record);
    const result = await this.pool.query(
      `INSERT INTO watcher_da_conflict_evidence (
         deployment_fingerprint,
         evidence_hash,
         header_hash,
         conflicting_header_hash,
         signer_index,
         reporter_peer_id,
         record,
         created_at
       )
       VALUES ($1, $2, $3, $4, $5, $6, $7::jsonb, NOW())
       ON CONFLICT (deployment_fingerprint, evidence_hash) DO NOTHING`,
      [
        canonicalRecord.deploymentFingerprint,
        canonicalRecord.evidenceHash,
        canonicalRecord.headerHash,
        canonicalRecord.conflictingHeaderHash,
        canonicalRecord.signerIndex,
        canonicalRecord.reporterPeerId,
        encodeRecord(canonicalRecord),
      ],
    );
    return result.rowCount === 1;
  }

  async listDaConflictEvidence(
    headerHash?: string,
  ): Promise<readonly DaStoredConflictEvidenceRecordV1[]> {
    return this.listParsedRecords(
      headerHash === undefined
        ? `SELECT deployment_fingerprint, evidence_hash, header_hash,
                  conflicting_header_hash, signer_index, reporter_peer_id,
                  record
           FROM watcher_da_conflict_evidence
           ORDER BY header_hash, signer_index, evidence_hash`
        : `SELECT deployment_fingerprint, evidence_hash, header_hash,
                  conflicting_header_hash, signer_index, reporter_peer_id,
                  record
           FROM watcher_da_conflict_evidence
           WHERE header_hash = $1
           ORDER BY header_hash, signer_index, evidence_hash`,
      headerHash === undefined ? [] : [headerHash],
      parseDaStoredConflictEvidenceRecordV1,
      assertConflictEvidenceRowIdentity,
    );
  }

  async saveDaAttestationCandidate(
    record: DaAttestationCandidateRecord,
  ): Promise<void> {
    await this.pool.query(
      `INSERT INTO watcher_da_attestation_candidates (
         header_hash,
         out_ref,
         record,
         updated_at
       )
       VALUES ($1, $2, $3::jsonb, NOW())
       ON CONFLICT (header_hash, out_ref) DO UPDATE SET
         record = EXCLUDED.record,
         updated_at = NOW()`,
      [record.headerHash, record.outRef, encodeRecord(record)],
    );
  }

  async listDaAttestationCandidates(
    headerHash?: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    return this.listRecords<DaAttestationCandidateRecord>(
      headerHash === undefined
        ? `SELECT record FROM watcher_da_attestation_candidates
           ORDER BY header_hash, out_ref`
        : `SELECT record FROM watcher_da_attestation_candidates
           WHERE header_hash = $1
           ORDER BY header_hash, out_ref`,
      headerHash === undefined ? [] : [headerHash],
    );
  }

  async saveL1Submission(record: L1SubmissionRecord): Promise<void> {
    await this.pool.query(
      `INSERT INTO watcher_l1_submissions (
         header_hash,
         tx_kind,
         tx_hash,
         record,
         updated_at
       )
       VALUES ($1, $2, $3, $4::jsonb, NOW())
       ON CONFLICT (header_hash, tx_kind, tx_hash) DO UPDATE SET
         record = EXCLUDED.record,
         updated_at = NOW()`,
      [record.headerHash, record.txKind, record.txHash, encodeRecord(record)],
    );
  }

  async listL1Submissions(): Promise<readonly L1SubmissionRecord[]> {
    return this.listRecords<L1SubmissionRecord>(
      `SELECT record FROM watcher_l1_submissions
       ORDER BY header_hash, tx_kind, tx_hash`,
    );
  }

  async savePeerBroadcast(record: DaPeerBroadcastRecord): Promise<void> {
    await this.pool.query(
      `INSERT INTO watcher_peer_broadcasts (
         peer_id,
         header_hash,
         signer_index,
         record,
         updated_at
       )
       VALUES ($1, $2, $3, $4::jsonb, NOW())
       ON CONFLICT (peer_id, header_hash, signer_index) DO UPDATE SET
         record = EXCLUDED.record,
         updated_at = NOW()`,
      [
        record.peerId,
        record.headerHash,
        record.signerIndex,
        encodeRecord(record),
      ],
    );
  }

  async getPeerBroadcast(args: {
    readonly peerId: string;
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaPeerBroadcastRecord | undefined> {
    return this.getRecord<DaPeerBroadcastRecord>(
      `SELECT record FROM watcher_peer_broadcasts
       WHERE peer_id = $1 AND header_hash = $2 AND signer_index = $3`,
      [args.peerId, args.headerHash, args.signerIndex],
    );
  }

  async listPeerBroadcasts(
    headerHash?: string,
  ): Promise<readonly DaPeerBroadcastRecord[]> {
    return this.listRecords<DaPeerBroadcastRecord>(
      headerHash === undefined
        ? `SELECT record FROM watcher_peer_broadcasts
           ORDER BY header_hash, signer_index, peer_id`
        : `SELECT record FROM watcher_peer_broadcasts
           WHERE header_hash = $1
           ORDER BY header_hash, signer_index, peer_id`,
      headerHash === undefined ? [] : [headerHash],
    );
  }

  async savePeerHealth(record: DaPeerHealthRecord): Promise<void> {
    await this.pool.query(
      `INSERT INTO watcher_peer_health (
         peer_id,
         record,
         updated_at
       )
       VALUES ($1, $2::jsonb, NOW())
       ON CONFLICT (peer_id) DO UPDATE SET
         record = EXCLUDED.record,
         updated_at = NOW()`,
      [record.peerId, encodeRecord(record)],
    );
  }

  async listPeerHealth(): Promise<readonly DaPeerHealthRecord[]> {
    return this.listRecords<DaPeerHealthRecord>(
      `SELECT record FROM watcher_peer_health ORDER BY peer_id`,
    );
  }

  async recordPeerNonce(record: DaPeerNonceRecord): Promise<boolean> {
    const result = await this.pool.query(
      `INSERT INTO watcher_peer_nonces (
         deployment_fingerprint,
         signer_index,
         nonce,
         record,
         created_at
       )
       VALUES ($1, $2, $3, $4::jsonb, NOW())
       ON CONFLICT (deployment_fingerprint, signer_index, nonce) DO NOTHING`,
      [
        record.deploymentFingerprint,
        record.signerIndex,
        record.nonce,
        encodeRecord(record),
      ],
    );
    return result.rowCount === 1;
  }

  private async initSchema(): Promise<void> {
    await this.pool.query(`
      CREATE TABLE IF NOT EXISTS watcher_deployment (
        id integer PRIMARY KEY CHECK (id = 1),
        marker_schema_version text NOT NULL CHECK (marker_schema_version = '${MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION}'),
        manifest_id text NOT NULL CHECK (manifest_id ~ '^[0-9a-f]{64}$'),
        manifest_sha256 text NOT NULL CHECK (manifest_sha256 ~ '^[0-9a-f]{64}$'),
        contract_deployment_info_sha256 text NOT NULL CHECK (contract_deployment_info_sha256 ~ '^[0-9a-f]{64}$'),
        manifest_raw text NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS watcher_state_queue_headers (
        header_hash text PRIMARY KEY,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS watcher_l1_source_state (
        id integer PRIMARY KEY CHECK (id = 1),
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS watcher_decision_outbox (
        effect_id text PRIMARY KEY,
        header_hash text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS watcher_da_payloads (
        header_hash text PRIMARY KEY,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS watcher_da_signatures (
        header_hash text NOT NULL,
        signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (header_hash, signer_index)
      );

      CREATE TABLE IF NOT EXISTS watcher_da_conflict_evidence (
        deployment_fingerprint text NOT NULL CHECK (deployment_fingerprint ~ '^[0-9a-f]{64}$'),
        evidence_hash text NOT NULL CHECK (evidence_hash ~ '^[0-9a-f]{64}$'),
        header_hash text NOT NULL CHECK (header_hash ~ '^[0-9a-f]{56}$'),
        conflicting_header_hash text NOT NULL CHECK (conflicting_header_hash ~ '^[0-9a-f]{56}$' AND conflicting_header_hash > header_hash),
        signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
        reporter_peer_id text NOT NULL CHECK (length(reporter_peer_id) > 0),
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (deployment_fingerprint, evidence_hash)
      );

      CREATE TABLE IF NOT EXISTS watcher_da_attestation_candidates (
        header_hash text NOT NULL,
        out_ref text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (header_hash, out_ref)
      );

      CREATE TABLE IF NOT EXISTS watcher_l1_submissions (
        header_hash text NOT NULL,
        tx_kind text NOT NULL,
        tx_hash text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (header_hash, tx_kind, tx_hash)
      );

      CREATE TABLE IF NOT EXISTS watcher_peer_broadcasts (
        peer_id text NOT NULL,
        header_hash text NOT NULL,
        signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (peer_id, header_hash, signer_index)
      );

      CREATE TABLE IF NOT EXISTS watcher_peer_health (
        peer_id text PRIMARY KEY,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS watcher_peer_nonces (
        deployment_fingerprint text NOT NULL,
        signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
        nonce text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (deployment_fingerprint, signer_index, nonce)
      );
    `);
  }

  private async upsertRecord<T extends { readonly headerHash: string }>(
    tableName: string,
    headerHash: string,
    record: T,
  ): Promise<void> {
    await upsertRecordWithPool(this.pool, tableName, headerHash, record);
  }

  private async getRecord<T>(
    query: string,
    values: readonly unknown[],
  ): Promise<T | undefined> {
    const result = await this.pool.query<JsonRecordRow>(query, [...values]);
    return decodeRow<T>(result.rows[0]);
  }

  private async getParsedRecord<T>(
    query: string,
    values: readonly unknown[],
    parseRecord: (record: unknown) => T,
    validateRow: (row: JsonRecordRow, record: T) => void,
  ): Promise<T | undefined> {
    const result = await this.pool.query<JsonRecordRow>(query, [...values]);
    return decodeParsedRow(result.rows[0], parseRecord, validateRow);
  }

  private async listRecords<T>(
    query: string,
    values: readonly unknown[] = [],
  ): Promise<readonly T[]> {
    const result = await this.pool.query<JsonRecordRow>(query, [...values]);
    return result.rows.map((row) => decodeRecord<T>(row.record));
  }

  private async listParsedRecords<T>(
    query: string,
    values: readonly unknown[],
    parseRecord: (record: unknown) => T,
    validateRow: (row: JsonRecordRow, record: T) => void,
  ): Promise<readonly T[]> {
    const result = await this.pool.query<JsonRecordRow>(query, [...values]);
    return result.rows.map((row) => {
      const record = parseRecord(decodeRecord<unknown>(row.record));
      validateRow(row, record);
      return record;
    });
  }

  private async withClient<T>(
    action: (client: PoolClient) => Promise<T>,
  ): Promise<T> {
    const client = await this.pool.connect();
    try {
      return await action(client);
    } finally {
      client.release();
    }
  }
}

const ensureL1SourceStateRow = async (
  client: PoolClient,
  proposed: L1SourceState,
): Promise<void> => {
  await client.query(
    `INSERT INTO watcher_l1_source_state (id, record, updated_at)
     VALUES (1, $1::jsonb, NOW())
     ON CONFLICT (id) DO NOTHING`,
    [encodeRecord(proposed)],
  );
};

const lockL1SourceState = async (
  client: PoolClient,
): Promise<L1SourceState> => {
  const result = await client.query<JsonRecordRow>(
    "SELECT record FROM watcher_l1_source_state WHERE id = 1 FOR UPDATE",
  );
  const decoded = decodeRow<unknown>(result.rows[0]);
  if (decoded === undefined) {
    throw new Error("decision outbox lacks durable L1 source state");
  }
  return parseL1SourceState(decoded);
};

const mergeLockedL1SourceState = async (
  client: PoolClient,
  proposed: L1SourceState,
): Promise<L1SourceState> => {
  await ensureL1SourceStateRow(client, proposed);
  const current = await lockL1SourceState(client);
  const merged = mergeL1SourceState(current, proposed);
  await client.query(
    `UPDATE watcher_l1_source_state
     SET record = $1::jsonb, updated_at = NOW()
     WHERE id = 1`,
    [encodeRecord(merged)],
  );
  return merged;
};

const upsertRecordWithPool = async <T>(
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

const upsertRecordWithClient = async <T>(
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

const upsertSignatureWithClient = async (
  client: PoolClient,
  record: DaSignatureRecordV1,
): Promise<void> => {
  await client.query(
    `INSERT INTO watcher_da_signatures
       (header_hash, signer_index, record, updated_at)
     VALUES ($1, $2, $3::jsonb, NOW())
     ON CONFLICT (header_hash, signer_index) DO UPDATE SET
       record = EXCLUDED.record, updated_at = NOW()`,
    [record.headerHash, record.signerIndex, encodeRecord(record)],
  );
};

const queryOne = async <T>(
  client: PoolClient,
  query: string,
  values: readonly unknown[],
  parseRecord: (record: unknown) => T,
  validateRow: (row: JsonRecordRow, record: T) => void,
): Promise<T | undefined> => {
  const result = await client.query<JsonRecordRow>(query, [...values]);
  return decodeParsedRow(result.rows[0], parseRecord, validateRow);
};

const encodeRecord = (record: unknown): string =>
  JSON.stringify(record, jsonReplacer);

const decodeRow = <T>(row: JsonRecordRow | undefined): T | undefined =>
  row === undefined ? undefined : decodeRecord<T>(row.record);

const decodeParsedRow = <T>(
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

const assertPayloadRowIdentity = (
  row: JsonRecordRow,
  record: DaPayloadRecord,
): void => {
  if (row.header_hash !== record.headerHash) {
    throw new Error(
      "Postgres DA stored payload row key does not match record identity",
    );
  }
};

const assertSignatureRowIdentity = (
  row: JsonRecordRow,
  record: DaSignatureRecord,
): void => {
  if (
    row.header_hash !== record.headerHash ||
    row.signer_index !== record.signerIndex
  ) {
    throw new Error(
      "Postgres DA signature row key does not match record identity",
    );
  }
};

const assertConflictEvidenceRowIdentity = (
  row: JsonRecordRow,
  record: DaStoredConflictEvidenceRecordV1,
): void => {
  if (
    row.deployment_fingerprint !== record.deploymentFingerprint ||
    row.evidence_hash !== record.evidenceHash ||
    row.header_hash !== record.headerHash ||
    row.conflicting_header_hash !== record.conflictingHeaderHash ||
    row.signer_index !== record.signerIndex ||
    row.reporter_peer_id !== record.reporterPeerId
  ) {
    throw new Error(
      "Postgres DA conflict evidence row key does not match record identity",
    );
  }
};

const assertDecisionOutboxRowIdentity = (
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

const assertPostgresDecisionRetry = (
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

const assertPostgresDecisionSourceState = (
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
    observation.finalized !== true ||
    observation.hasPersistedDecision !== true ||
    observation.slot !== effect.slot ||
    observation.blockHash !== effect.blockHash
  ) {
    throw new Error("decision outbox lacks matching durable L1 observation");
  }
};

const assertPostgresDecisionSignature = (
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

const decodeRecord = <T>(record: unknown): T =>
  JSON.parse(JSON.stringify(record), jsonReviver) as T;
