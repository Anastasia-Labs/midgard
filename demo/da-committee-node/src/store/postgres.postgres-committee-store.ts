import {
  assertDeploymentMarkerMatches,
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { Pool, type PoolClient } from "pg";

import {
  type DaAttestationCandidateRecord,
  type DaPayloadRecord,
  type DaPeerBroadcastRecord,
  type DaPeerHealthRecord,
  type DaPeerNonceRecord,
  type DaSignatureRecord,
  type DaSignatureRecordV1,
  type DaStoredConflictEvidenceRecord,
  type DaStoredPayloadRecord,
  type L1SubmissionRecord,
  parseDaSignatureRecord,
  parseDaStoredConflictEvidenceRecord,
  parseDaStoredPayloadRecord,
  type StateQueueHeaderRecord,
} from "../domain.js";
import type { SignedHeader } from "../l1/follower/obligations.js";
import {
  type CommitteeDeploymentRecord,
  type CommitteeStore,
  type CommitteeStoreReadinessCounts,
  type DecisionOutboxRecord,
  type DecisionOutboxStatus,
  InFlightDecisionAttempts,
  type L1SourceState,
  parseDecisionOutboxRecord,
  parseL1SourceState,
  resolveDaPayloadSave,
  type RetainedPayloadPruneRequest,
} from "../store.js";
import {
  prunePostgresSignedDecisionsUnlessRetiring as pruneSignedDecisions,
  releasePostgresHeaderRows,
} from "./decision-pruning-postgres.js";
import {
  assertConflictEvidenceRowIdentity,
  assertDecisionOutboxRowIdentity,
  assertPayloadRowIdentity,
  assertPostgresDecisionRetry,
  assertPostgresDecisionSignature,
  assertPostgresDecisionSourceState,
  assertSignatureRowIdentity,
  COMMITTEE_TABLES,
  decodeParsedRow,
  decodeRecord,
  decodeRow,
  encodeRecord,
  type JsonRecordRow,
  lockL1SourceState,
  mergeLockedL1SourceState,
  type PostgresCommitteeStoreOptions,
  queryOne,
  upsertRecordWithClient,
  upsertSignatureWithClient,
} from "./postgres.assert-postgres-decision-retry.js";
import { PostgresStoreInstanceLock } from "./postgres.instance-lock.js";
import {
  COMMITTEE_READINESS_COUNTS_SQL,
  initializeCommitteeSchema,
  UNSETTLED_HEADER_PREDICATE,
} from "./postgres.schema.js";
import { readCommitteeL1PinTargets } from "./postgres.stored-l1-points.js";
import * as capacity from "./promise-capacity-postgres.js";
import { postgresPromiseResources } from "./promise-resource-usage.js";
import { retainedPayloadPruneDecision } from "./retention.js";
import { type CommitteeRetirementCertificate } from "./retirement-certificate.js";
import {
  type CommitteeRetirementBreachPoint,
  CommitteeRetirementController,
  type CommitteeRetirementFloor,
  type CommitteeRetirementGuard,
  type CommitteeRetirementSnapshot,
} from "./retirement-model.js";
import {
  assertPostgresRetirementResources,
  readPostgresRetirementData,
  readPostgresRetirementFloor,
} from "./retirement-postgres.js";
import { PostgresRetirementOperations } from "./retirement-postgres-operations.js";
import {
  assertRetirementWrite,
  retirementStoreDigest,
} from "./retirement-transition.js";

export class PostgresCommitteeStore implements CommitteeStore {
  promiseStoreResourceUsage = postgresPromiseResources(() => this.pool);
  private readonly retirement = new CommitteeRetirementController();
  private readonly retirementOperations: PostgresRetirementOperations;
  private readonly pool: Pool;
  readonly instanceLock: PostgresStoreInstanceLock;
  private readonly inFlightDecisions = new InFlightDecisionAttempts();

  private constructor(pool: Pool, instanceLock: PostgresStoreInstanceLock) {
    this.pool = pool;
    this.instanceLock = instanceLock;
    this.retirementOperations = new PostgresRetirementOperations(
      pool,
      instanceLock,
      this.retirement,
      this.inFlightDecisions,
    );
  }

  /** Holds the instance lock for this store's lifetime; refuses another live holder. */
  static async open(
    databaseUrl: string,
    options: PostgresCommitteeStoreOptions = {},
  ): Promise<PostgresCommitteeStore> {
    const parsed = new URL(databaseUrl);
    if (parsed.protocol !== "postgres:" && parsed.protocol !== "postgresql:") {
      throw new Error(
        "DA_COMMITTEE_DATABASE_URL must be a postgres:// or postgresql:// URL",
      );
    }
    const instanceLock = await PostgresStoreInstanceLock.acquire(
      databaseUrl,
      options,
    );
    const pool = new Pool({
      connectionString: databaseUrl,
      max: 10,
    });
    // Idle connection loss is re-emitted here and would crash the node unhandled.
    // The pool discards that client; the next query reconnects.
    pool.on("error", (error) => {
      process.stderr.write(
        `${JSON.stringify({ event: "committee_store_pool_error", error: error.message })}\n`,
      );
    });
    const store = new PostgresCommitteeStore(pool, instanceLock);
    try {
      await store.renameLegacyTables();
      await initializeCommitteeSchema(pool, instanceLock, options.openChecks);
      store.retirement.load(await store.getRetirementFloor());
    } catch (error) {
      await store.close().catch(() => undefined);
      throw error;
    }
    return store;
  }

  retirementDiscoveryActive(): boolean {
    return this.retirement.discoveryActive();
  }
  async withRetirementDiscovery<T>(run: () => Promise<T>): Promise<T> {
    const release = this.retirement.discovery();
    try {
      return await run();
    } finally {
      release();
    }
  }
  captureRetirementGuard(): CommitteeRetirementGuard {
    return this.retirement.capture();
  }
  assertRetirementGuard(
    token: CommitteeRetirementGuard,
    record?: StateQueueHeaderRecord,
  ): void {
    this.retirement.assert(token, record);
  }
  async getRetirementFloor(): Promise<CommitteeRetirementFloor | undefined> {
    return readPostgresRetirementFloor(this.pool);
  }
  async readRetirementSnapshot(): Promise<CommitteeRetirementSnapshot> {
    const guard = this.captureRetirementGuard();
    const client = await this.pool.connect();
    try {
      await client.query("BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY");
      const data = await readPostgresRetirementData(client);
      await client.query("COMMIT");
      this.assertRetirementGuard(guard);
      return { data, digest: retirementStoreDigest(data), guard };
    } catch (error) {
      await client.query("ROLLBACK");
      throw error;
    } finally {
      client.release();
    }
  }
  /** The L1 history the stored records name: the follower's pins. */
  readonly readL1PinTargets = () => readCommitteeL1PinTargets(this.pool);
  async withRetainedHeaderPin<T>(
    headerHash: string,
    run: () => Promise<T>,
  ): Promise<T> {
    const release = this.retirement.pin(headerHash);
    try {
      return await run();
    } finally {
      release();
    }
  }
  applyRetirementCertificate(
    certificate: CommitteeRetirementCertificate,
  ): Promise<readonly string[]> {
    return this.retirementOperations.applyRetirementCertificate(certificate);
  }
  recordRetirementBreach(
    reason: string,
    observedAt: CommitteeRetirementBreachPoint,
  ): Promise<void> {
    return this.retirementOperations.recordRetirementBreach(reason, observedAt);
  }

  async close(): Promise<void> {
    try {
      await this.pool.end();
    } finally {
      await this.instanceLock.release();
    }
  }

  async initDeployment(args: {
    readonly marker: DeploymentMarker;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void> {
    return this.withClient(async (client) => {
      const marker = parseDeploymentMarker(args.marker);
      const result = await client.query<{
        readonly marker_schema_version: string;
        readonly manifest_id: string;
      }>(
        "SELECT marker_schema_version, manifest_id FROM committee_deployment WHERE id = 1",
      );
      const existing = result.rows[0];
      if (existing !== undefined) {
        try {
          assertDeploymentMarkerMatches(
            marker,
            {
              schemaVersion: existing.marker_schema_version,
              manifestId: existing.manifest_id,
            },
            "DA Postgres store",
          );
        } catch {
          throw new Error(
            `stale_deployment_state_requires_fresh_redeploy: stored_manifest_id=${existing.manifest_id}, canonical_manifest_id=${marker.manifestId}, contract_deployment_info_sha256=${args.contractDeploymentInfoSha256}; refusing to reuse stale committee node state; perform an explicit fresh redeploy/reset before deleting local committee node state.`,
          );
        }
      }
      await client.query(
        `INSERT INTO committee_deployment (
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
    });
  }

  async getDeployment(): Promise<CommitteeDeploymentRecord | undefined> {
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
       FROM committee_deployment
       WHERE id = 1`,
    );
    const row = result.rows[0];
    if (row === undefined) {
      return undefined;
    }
    return {
      marker: parseDeploymentMarker({
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
      "SELECT record FROM committee_l1_source_state WHERE id = 1",
    );
    const decoded = decodeRow<unknown>(result.rows[0]);
    return decoded === undefined ? undefined : parseL1SourceState(decoded);
  }

  async saveL1SourceState(state: L1SourceState): Promise<void> {
    const canonical = parseL1SourceState(state);
    await this.withClient(async (client) => {
      await mergeLockedL1SourceState(client, canonical);
    });
  }

  async getDecisionOutbox(
    effectId: string,
  ): Promise<DecisionOutboxRecord | undefined> {
    return this.getParsedRecord(
      "SELECT effect_id, header_hash, record FROM committee_decision_outbox WHERE effect_id = $1",
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
        ? `SELECT effect_id, header_hash, record FROM committee_decision_outbox
           ORDER BY effect_id`
        : `SELECT effect_id, header_hash, record FROM committee_decision_outbox
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
        : parseDaSignatureRecord(args.signature);
    assertPostgresDecisionSignature(effect, signature);
    this.instanceLock.assertHeld();
    let claimed = false;
    await this.withClient(async (client) => {
      try {
        const sourceState = await mergeLockedL1SourceState(
          client,
          proposedSourceState,
        );
        assertPostgresDecisionSourceState(effect, sourceState);
        const current = await queryOne(
          client,
          "SELECT effect_id, header_hash, record FROM committee_decision_outbox WHERE effect_id = $1 FOR UPDATE",
          [effect.effectId],
          parseDecisionOutboxRecord,
          assertDecisionOutboxRowIdentity,
        );
        assertPostgresDecisionRetry(current, effect);
        // A pending attempt from the previous lock holder is now ours.
        // Claim before COMMIT so a concurrent begin waiting on the row sees it.
        await this.instanceLock.assertHeldAtServer(client);
        this.inFlightDecisions.claim(effect);
        claimed = true;
        await client.query(
          `INSERT INTO committee_decision_outbox
             (effect_id, header_hash, record, updated_at)
           VALUES ($1, $2, $3::jsonb, NOW())
           ON CONFLICT (effect_id) DO UPDATE SET
             record = EXCLUDED.record, updated_at = NOW()`,
          [effect.effectId, effect.headerHash, encodeRecord(effect)],
        );
        if (signature !== undefined) {
          await upsertSignatureWithClient(client, signature);
        }
      } catch (error) {
        if (claimed) {
          this.inFlightDecisions.release(effect.effectId, effect.attemptCount);
        }
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
    try {
      this.instanceLock.assertHeld();
      await this.completeDecisionEffectRow(args);
    } finally {
      this.inFlightDecisions.release(args.effectId, args.expectedAttemptCount);
    }
  }

  private async completeDecisionEffectRow(args: {
    readonly effectId: string;
    readonly expectedAttemptCount: number;
    readonly status: Exclude<DecisionOutboxStatus, "pending">;
    readonly updatedAt: string;
    readonly lastError?: string;
    readonly signature?: DaSignatureRecord;
  }): Promise<void> {
    await this.withClient(async (client) => {
      const sourceState = await lockL1SourceState(client);
      const existing = await queryOne(
        client,
        "SELECT effect_id, header_hash, record FROM committee_decision_outbox WHERE effect_id = $1 FOR UPDATE",
        [args.effectId],
        parseDecisionOutboxRecord,
        assertDecisionOutboxRowIdentity,
      );
      if (existing === undefined) {
        throw new Error(`decision outbox effect ${args.effectId} is missing`);
      }
      if (
        existing.status !== "pending" ||
        existing.attemptCount !== args.expectedAttemptCount
      ) {
        throw new Error(
          "decision outbox completion does not match the pending attempt",
        );
      }
      assertPostgresDecisionSourceState(existing, sourceState);
      const signature =
        args.signature === undefined
          ? undefined
          : parseDaSignatureRecord(args.signature);
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
        `UPDATE committee_decision_outbox
           SET record = $2::jsonb, updated_at = NOW()
           WHERE effect_id = $1`,
        [args.effectId, encodeRecord(completed)],
      );
      if (signature !== undefined) {
        await upsertSignatureWithClient(client, signature);
      }
    });
  }

  async upsertStateQueueHeader(record: StateQueueHeaderRecord): Promise<void> {
    await this.upsertRecord(
      "committee_state_queue_headers",
      record.headerHash,
      record,
    );
  }

  async listUnsettledStateQueueHeaders(): Promise<
    readonly StateQueueHeaderRecord[]
  > {
    return this.listRecords<StateQueueHeaderRecord>(
      `SELECT record FROM committee_state_queue_headers
       WHERE ${UNSETTLED_HEADER_PREDICATE} ORDER BY header_hash`,
    );
  }

  async getStateQueueHeaders(
    headerHashes: readonly string[],
  ): Promise<readonly StateQueueHeaderRecord[]> {
    if (headerHashes.length === 0) return [];
    return this.listRecords<StateQueueHeaderRecord>(
      `SELECT record FROM committee_state_queue_headers
       WHERE header_hash = ANY($1::text[]) ORDER BY header_hash`,
      [[...new Set(headerHashes)]],
    );
  }

  async readinessCounts(): Promise<CommitteeStoreReadinessCounts> {
    const result = await this.pool.query<{
      readonly totals: Readonly<Record<string, string | number>> | null;
      readonly missing_payloads: string;
      readonly verified_missing_l1_attestation: string;
    }>(COMMITTEE_READINESS_COUNTS_SQL);
    const row = result.rows[0];
    const total = (name: string): number => {
      const value = Number(row?.totals?.[name]);
      if (!Number.isSafeInteger(value) || value < 0)
        throw new Error(`committee store counter ${name} is unavailable`);
      return value;
    };
    return {
      discoveredHeaders: total("headers"),
      missingPayloads: Number(row?.missing_payloads ?? 0),
      verifiedPayloads: total("verified_payloads"),
      verifiedPayloadsMissingL1Attestation: Number(
        row?.verified_missing_l1_attestation ?? 0,
      ),
      signatures: total("signatures"),
      l1AttestationSubmissions: total("l1_submissions"),
      submittedOrConfirmedL1Attestations: total(
        "submitted_or_confirmed_headers",
      ),
    };
  }

  async listSignedDecisions(): Promise<readonly SignedHeader[]> {
    const result = await this.pool.query<{
      readonly header_hash: string;
      readonly end_time_ms: string;
    }>(
      `SELECT DISTINCT ON (header_hash)
              header_hash, end_time_ms::text AS end_time_ms
       FROM committee_da_signatures
       WHERE end_time_ms IS NOT NULL
       ORDER BY header_hash`,
    );
    return result.rows.map((row) => ({
      headerHash: row.header_hash,
      endTimeMs: BigInt(row.end_time_ms),
    }));
  }

  async getStateQueueHeader(
    headerHash: string,
  ): Promise<StateQueueHeaderRecord | undefined> {
    return this.getRecord<StateQueueHeaderRecord>(
      "SELECT record FROM committee_state_queue_headers WHERE header_hash = $1",
      [headerHash],
    );
  }

  async saveDaPayload(record: DaPayloadRecord): Promise<DaStoredPayloadRecord> {
    const canonicalRecord = parseDaStoredPayloadRecord(record);
    return this.withClient(async (client) => {
      const existing = await queryOne(
        client,
        "SELECT header_hash, record FROM committee_da_payloads WHERE header_hash = $1 FOR UPDATE",
        [canonicalRecord.headerHash],
        parseDaStoredPayloadRecord,
        assertPayloadRowIdentity,
      );
      const saved = resolveDaPayloadSave(existing, canonicalRecord);
      await upsertRecordWithClient(
        client,
        "committee_da_payloads",
        canonicalRecord.headerHash,
        saved,
      );
      return saved;
    });
  }

  async getDaPayload(
    headerHash: string,
  ): Promise<DaStoredPayloadRecord | undefined> {
    return this.getParsedRecord(
      "SELECT header_hash, record FROM committee_da_payloads WHERE header_hash = $1",
      [headerHash],
      parseDaStoredPayloadRecord,
      assertPayloadRowIdentity,
    );
  }

  async listDaPayloads(): Promise<readonly DaStoredPayloadRecord[]> {
    return this.listParsedRecords(
      "SELECT header_hash, record FROM committee_da_payloads ORDER BY header_hash",
      [],
      parseDaStoredPayloadRecord,
      assertPayloadRowIdentity,
    );
  }
  async deleteDaPayloadIfPrunable(
    request: RetainedPayloadPruneRequest,
  ): Promise<boolean> {
    return this.withClient(async (client) => {
      if ((await readPostgresRetirementFloor(client)) !== undefined)
        return false;
      // Payload and header locks decide a concurrently written header here,
      // or hold its writer until this deletion decision commits.
      const payload = await queryOne(
        client,
        "SELECT header_hash, record FROM committee_da_payloads WHERE header_hash = $1 FOR UPDATE",
        [request.headerHash],
        parseDaStoredPayloadRecord,
        assertPayloadRowIdentity,
      );
      const header =
        payload === undefined
          ? undefined
          : decodeRow<StateQueueHeaderRecord>(
              (
                await client.query<JsonRecordRow>(
                  "SELECT record FROM committee_state_queue_headers WHERE header_hash = $1 FOR SHARE",
                  [request.headerHash],
                )
              ).rows[0],
            );
      const prune =
        payload !== undefined &&
        retainedPayloadPruneDecision(payload, header, request).decision ===
          "prune";
      const deleted =
        prune &&
        ((
          await client.query(
            "DELETE FROM committee_da_payloads WHERE header_hash = $1",
            [request.headerHash],
          )
        ).rowCount ?? 0) > 0;
      if (deleted && request.releaseHeader === true)
        await releasePostgresHeaderRows(
          client,
          request.headerHash,
          this.inFlightDecisions,
        );
      return deleted;
    });
  }

  pruneSignedDecisions = (headerHashes: readonly string[]) =>
    this.withClient((client) =>
      pruneSignedDecisions(client, headerHashes, this.inFlightDecisions),
    );

  getPromiseCapacityEvidence = capacity.reader(() => this.pool);
  savePromiseCapacityEvidence = capacity.writer(
    (run) => this.withClient(run),
    (client) => this.instanceLock.assertHeldAtServer(client),
  );
  async saveDaSignature(record: DaSignatureRecord): Promise<void> {
    const canonicalRecord = parseDaSignatureRecord(record);
    await this.withClient(async (client) => {
      await upsertSignatureWithClient(client, canonicalRecord);
    });
  }
  async getDaSignature(args: {
    readonly headerHash: string;
    readonly availabilityCommitmentDigest: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecordV1 | undefined> {
    return this.getParsedRecord(
      `SELECT header_hash, commitment_digest, signer_index, record FROM committee_da_signatures
       WHERE header_hash = $1 AND commitment_digest = $2 AND signer_index = $3`,
      [args.headerHash, args.availabilityCommitmentDigest, args.signerIndex],
      parseDaSignatureRecord,
      assertSignatureRowIdentity,
    );
  }
  async listDaSignatures(
    headerHash?: string,
  ): Promise<readonly DaSignatureRecordV1[]> {
    return this.listParsedRecords(
      headerHash === undefined
        ? `SELECT header_hash, commitment_digest, signer_index, record
           FROM committee_da_signatures
           ORDER BY header_hash, commitment_digest, signer_index`
        : `SELECT header_hash, commitment_digest, signer_index, record
           FROM committee_da_signatures
           WHERE header_hash = $1
           ORDER BY header_hash, commitment_digest, signer_index`,
      headerHash === undefined ? [] : [headerHash],
      parseDaSignatureRecord,
      assertSignatureRowIdentity,
    );
  }
  async saveDaConflictEvidence(
    record: DaStoredConflictEvidenceRecord,
  ): Promise<boolean> {
    return this.withClient(async (client) => {
      const canonicalRecord = parseDaStoredConflictEvidenceRecord(record);
      const result = await client.query(
        `INSERT INTO committee_da_conflict_evidence (
         deployment_fingerprint,
         evidence_hash,
         header_hash,
         commitment_digest,
         conflicting_commitment_digest,
         signer_index,
         reporter_peer_id,
         record,
         created_at
       )
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8::jsonb, NOW())
       ON CONFLICT (deployment_fingerprint, evidence_hash) DO NOTHING`,
        [
          canonicalRecord.deploymentFingerprint,
          canonicalRecord.evidenceHash,
          canonicalRecord.headerHash,
          canonicalRecord.commitmentDigest,
          canonicalRecord.conflictingCommitmentDigest,
          canonicalRecord.signerIndex,
          canonicalRecord.reporterPeerId,
          encodeRecord(canonicalRecord),
        ],
      );
      return result.rowCount === 1;
    });
  }
  async listDaConflictEvidence(
    headerHash?: string,
  ): Promise<readonly DaStoredConflictEvidenceRecord[]> {
    return this.listParsedRecords(
      headerHash === undefined
        ? `SELECT deployment_fingerprint, evidence_hash, header_hash,
                  commitment_digest,
                  conflicting_commitment_digest, signer_index, reporter_peer_id,
                  record
           FROM committee_da_conflict_evidence
           ORDER BY header_hash, signer_index, evidence_hash`
        : `SELECT deployment_fingerprint, evidence_hash, header_hash,
                  commitment_digest,
                  conflicting_commitment_digest, signer_index, reporter_peer_id,
                  record
           FROM committee_da_conflict_evidence
           WHERE header_hash = $1
           ORDER BY header_hash, signer_index, evidence_hash`,
      headerHash === undefined ? [] : [headerHash],
      parseDaStoredConflictEvidenceRecord,
      assertConflictEvidenceRowIdentity,
    );
  }
  async saveDaAttestationCandidate(
    record: DaAttestationCandidateRecord,
  ): Promise<void> {
    return this.withClient(async (client) => {
      await client.query(
        `INSERT INTO committee_da_attestation_candidates (
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
    });
  }
  async listDaAttestationCandidates(
    headerHash?: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    return this.listRecords<DaAttestationCandidateRecord>(
      headerHash === undefined
        ? `SELECT record FROM committee_da_attestation_candidates
           ORDER BY header_hash, out_ref`
        : `SELECT record FROM committee_da_attestation_candidates
           WHERE header_hash = $1
           ORDER BY header_hash, out_ref`,
      headerHash === undefined ? [] : [headerHash],
    );
  }
  async saveL1Submission(record: L1SubmissionRecord): Promise<void> {
    return this.withClient(async (client) => {
      await client.query(
        `INSERT INTO committee_l1_submissions (
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
    });
  }
  async listL1Submissions(): Promise<readonly L1SubmissionRecord[]> {
    return this.listRecords<L1SubmissionRecord>(
      `SELECT record FROM committee_l1_submissions
       ORDER BY header_hash, tx_kind, tx_hash`,
    );
  }
  async savePeerBroadcast(record: DaPeerBroadcastRecord): Promise<void> {
    return this.withClient(async (client) => {
      await client.query(
        `INSERT INTO committee_peer_broadcasts (
         peer_id,
         header_hash,
         commitment_digest,
         signer_index,
         record,
         updated_at
       )
       VALUES ($1, $2, $3, $4, $5::jsonb, NOW())
       ON CONFLICT (peer_id, header_hash, commitment_digest, signer_index) DO UPDATE SET
         record = EXCLUDED.record,
         updated_at = NOW()`,
        [
          record.peerId,
          record.headerHash,
          record.availabilityCommitmentDigest,
          record.signerIndex,
          encodeRecord(record),
        ],
      );
    });
  }
  async getPeerBroadcast(args: {
    readonly peerId: string;
    readonly headerHash: string;
    readonly availabilityCommitmentDigest: string;
    readonly signerIndex: number;
  }): Promise<DaPeerBroadcastRecord | undefined> {
    return this.getRecord<DaPeerBroadcastRecord>(
      `SELECT record FROM committee_peer_broadcasts
       WHERE peer_id = $1 AND header_hash = $2 AND commitment_digest = $3 AND signer_index = $4`,
      [
        args.peerId,
        args.headerHash,
        args.availabilityCommitmentDigest,
        args.signerIndex,
      ],
    );
  }
  async listPeerBroadcasts(
    headerHash?: string,
  ): Promise<readonly DaPeerBroadcastRecord[]> {
    return this.listRecords<DaPeerBroadcastRecord>(
      headerHash === undefined
        ? `SELECT record FROM committee_peer_broadcasts
           ORDER BY header_hash, commitment_digest, signer_index, peer_id`
        : `SELECT record FROM committee_peer_broadcasts
           WHERE header_hash = $1
           ORDER BY header_hash, commitment_digest, signer_index, peer_id`,
      headerHash === undefined ? [] : [headerHash],
    );
  }
  async savePeerHealth(record: DaPeerHealthRecord): Promise<void> {
    return this.withClient(async (client) => {
      await client.query(
        `INSERT INTO committee_peer_health (
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
    });
  }
  async listPeerHealth(): Promise<readonly DaPeerHealthRecord[]> {
    return this.listRecords<DaPeerHealthRecord>(
      `SELECT record FROM committee_peer_health ORDER BY peer_id`,
    );
  }
  async recordPeerNonce(record: DaPeerNonceRecord): Promise<boolean> {
    return this.withClient(async (client) => {
      const result = await client.query(
        `INSERT INTO committee_peer_nonces (
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
    });
  }

  /** Rename legacy watcher_* tables in place to preserve existing data.
   * Skip each rename when its committee_* table already exists. */
  private async renameLegacyTables(): Promise<void> {
    const statements = COMMITTEE_TABLES.map(
      (table) => `
      DO $$
      BEGIN
        IF to_regclass('committee_${table}') IS NULL
           AND to_regclass('watcher_${table}') IS NOT NULL THEN
          ALTER TABLE watcher_${table} RENAME TO committee_${table};
        END IF;
      END
      $$;`,
    );
    await this.pool.query(statements.join("\n"));
  }
  private async upsertRecord<T extends { readonly headerHash: string }>(
    tableName: string,
    headerHash: string,
    record: T,
  ): Promise<void> {
    await this.withClient((client) =>
      upsertRecordWithClient(client, tableName, headerHash, record),
    );
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
    const generation = this.retirement.generation();
    const client = await this.pool.connect();
    try {
      await client.query("BEGIN");
      await client.query("SELECT pg_advisory_xact_lock(172947,725726)");
      this.retirement.assertGeneration(generation);
      await this.instanceLock.assertHeldAtServer(client);
      const before =
        (await readPostgresRetirementFloor(client)) === undefined
          ? undefined
          : await readPostgresRetirementData(client);
      const result = await action(client);
      if (before !== undefined) {
        const after = await readPostgresRetirementData(client);
        assertRetirementWrite(before, after);
        await assertPostgresRetirementResources(client, after);
      } else if ((await readPostgresRetirementFloor(client)) !== undefined) {
        throw new Error("Retirement floor changed during an ordinary write");
      }
      this.retirement.assertGeneration(generation);
      await client.query("COMMIT");
      return result;
    } catch (error) {
      await client.query("ROLLBACK");
      throw error;
    } finally {
      client.release();
    }
  }
}
