import { Pool, type PoolClient } from "pg";

import type {
  DaAttestationCandidateRecord,
  DaPayloadRecord,
  DaPeerBroadcastRecord,
  DaPeerHealthRecord,
  DaPeerNonceRecord,
  DaSignatureRecord,
  L1SubmissionRecord,
  StateQueueHeaderRecord,
} from "../domain.js";
import {
  jsonReplacer,
  jsonReviver,
  resolveDaPayloadSave,
  type WatcherDeploymentRecord,
  type WatcherStore,
} from "../store.js";

type JsonRecordRow = {
  readonly record: unknown;
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
    readonly fingerprint: string;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void> {
    const result = await this.pool.query<{
      readonly fingerprint: string;
    }>("SELECT fingerprint FROM watcher_deployment WHERE id = 1");
    const existing = result.rows[0];
    if (existing !== undefined && existing.fingerprint !== args.fingerprint) {
      throw new Error(
        `stale_deployment_state_requires_fresh_redeploy: stored_fingerprint=${existing.fingerprint}, canonical_manifest_id=${args.fingerprint}, contract_deployment_info_sha256=${args.contractDeploymentInfoSha256}; refusing to reuse stale watcher state; perform an explicit fresh redeploy/reset before deleting local watcher state.`,
      );
    }
    await this.pool.query(
      `INSERT INTO watcher_deployment (
         id,
         fingerprint,
         manifest_sha256,
         contract_deployment_info_sha256,
         manifest_raw,
         updated_at
       )
       VALUES (1, $1, $2, $3, $4, NOW())
       ON CONFLICT (id) DO UPDATE SET
         fingerprint = EXCLUDED.fingerprint,
         manifest_sha256 = EXCLUDED.manifest_sha256,
         contract_deployment_info_sha256 = EXCLUDED.contract_deployment_info_sha256,
         manifest_raw = EXCLUDED.manifest_raw,
         updated_at = NOW()`,
      [
        args.fingerprint,
        args.manifestSha256,
        args.contractDeploymentInfoSha256,
        args.manifestRaw,
      ],
    );
  }

  async getDeployment(): Promise<WatcherDeploymentRecord | undefined> {
    const result = await this.pool.query<{
      readonly fingerprint: string;
      readonly manifest_sha256: string;
      readonly contract_deployment_info_sha256: string | null;
      readonly manifest_raw: string;
    }>(
      `SELECT fingerprint,
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
      fingerprint: row.fingerprint,
      manifestSha256: row.manifest_sha256,
      ...(row.contract_deployment_info_sha256 === null
        ? {}
        : {
            contractDeploymentInfoSha256: row.contract_deployment_info_sha256,
          }),
      manifestRaw: row.manifest_raw,
    };
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

  async saveDaPayload(record: DaPayloadRecord): Promise<DaPayloadRecord> {
    return this.withClient(async (client) => {
      await client.query("BEGIN");
      try {
        const existing = await queryOne<DaPayloadRecord>(
          client,
          "SELECT record FROM watcher_da_payloads WHERE header_hash = $1 FOR UPDATE",
          [record.headerHash],
        );
        const saved = resolveDaPayloadSave(existing, record);
        await upsertRecordWithClient(
          client,
          "watcher_da_payloads",
          record.headerHash,
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

  async getDaPayload(headerHash: string): Promise<DaPayloadRecord | undefined> {
    return this.getRecord<DaPayloadRecord>(
      "SELECT record FROM watcher_da_payloads WHERE header_hash = $1",
      [headerHash],
    );
  }

  async saveDaSignature(record: DaSignatureRecord): Promise<void> {
    await this.pool.query(
      `INSERT INTO watcher_da_signatures (
         header_hash,
         signer_index,
         record,
         updated_at
       )
       VALUES ($1, $2, $3::jsonb, NOW())
       ON CONFLICT (header_hash, signer_index) DO UPDATE SET
         record = EXCLUDED.record,
         updated_at = NOW()`,
      [record.headerHash, record.signerIndex, encodeRecord(record)],
    );
  }

  async getDaSignature(args: {
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecord | undefined> {
    return this.getRecord<DaSignatureRecord>(
      `SELECT record FROM watcher_da_signatures
       WHERE header_hash = $1 AND signer_index = $2`,
      [args.headerHash, args.signerIndex],
    );
  }

  async listDaSignatures(
    headerHash?: string,
  ): Promise<readonly DaSignatureRecord[]> {
    return this.listRecords<DaSignatureRecord>(
      headerHash === undefined
        ? `SELECT record FROM watcher_da_signatures
           ORDER BY header_hash, signer_index`
        : `SELECT record FROM watcher_da_signatures
           WHERE header_hash = $1
           ORDER BY header_hash, signer_index`,
      headerHash === undefined ? [] : [headerHash],
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
        fingerprint text NOT NULL,
        manifest_sha256 text NOT NULL,
        contract_deployment_info_sha256 text,
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

  private async listRecords<T>(
    query: string,
    values: readonly unknown[] = [],
  ): Promise<readonly T[]> {
    const result = await this.pool.query<JsonRecordRow>(query, [...values]);
    return result.rows.map((row) => decodeRecord<T>(row.record));
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

const queryOne = async <T>(
  client: PoolClient,
  query: string,
  values: readonly unknown[],
): Promise<T | undefined> => {
  const result = await client.query<JsonRecordRow>(query, [...values]);
  return decodeRow<T>(result.rows[0]);
};

const encodeRecord = (record: unknown): string =>
  JSON.stringify(record, jsonReplacer);

const decodeRow = <T>(row: JsonRecordRow | undefined): T | undefined =>
  row === undefined ? undefined : decodeRecord<T>(row.record);

const decodeRecord = <T>(record: unknown): T =>
  JSON.parse(JSON.stringify(record), jsonReviver) as T;
