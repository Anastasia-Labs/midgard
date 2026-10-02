import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import { daRetentionPruneDecision } from "@al-ft/midgard-core";
import {
  assertDeploymentMarkerMatches,
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import type {
  DaAttestationCandidateRecord,
  DaPayloadRecord,
  DaPeerBroadcastRecord,
  DaPeerHealthRecord,
  DaPeerNonceRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
  DaStoredConflictEvidenceRecord,
  DaStoredPayloadRecord,
  L1SubmissionRecord,
  StateQueueHeaderRecord,
} from "./domain.js";
import {
  parseDaSignatureRecord,
  parseDaStoredConflictEvidenceRecord,
  parseDaStoredPayloadRecord,
} from "./domain.js";
import type { StateQueueOutputStep } from "./l1/terminal-retention-observation.js";
import {
  type CommitteeDeploymentRecord,
  type CommitteeStore,
  conflictEvidenceKey,
  type DecisionOutboxRecord,
  type DecisionOutboxStatus,
  hasPayloadBytes,
  InFlightDecisionAttempts,
  type L1ObservedDecision,
  type L1SourceState,
  peerBroadcastKey,
  peerNonceKey,
  type RetainedPayloadPruneRequest,
  signatureKey,
  type StoreData,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "./store.committee-store.js";
import {
  JsonStoreLease,
  type JsonStoreLeaseOptions,
} from "./store.json-file-lease.js";
import {
  assertDecisionRetry,
  emptyStoreData,
  isCanonicalIsoTimestamp,
  parseCommitteeDeploymentRecord,
  parseDecisionOutboxRecord,
  withDerivedPayloadFetchStatus,
} from "./store.parse-decision-outbox-record.js";
import {
  committeeStoreFilePath,
  isNodeError,
  jsonReplacer,
  jsonReviver,
  parseStoredRecordMap,
} from "./store.parse-stored-record-map.js";
import {
  assertDecisionSignature,
  assertDecisionSourceState,
  mergeL1SourceState,
  mergeQuarantinedL1SourceState,
  parseStateQueueReplayAnchor,
} from "./store.persisted-decision-transition.js";
import {
  retentionBlockEndTimeMs,
  retentionQueueReference,
} from "./store/retention.js";

export class JsonFileCommitteeStore implements CommitteeStore {
  private readonly filePath: string;
  private readonly lease: JsonStoreLease;
  private writeQueue: Promise<void> = Promise.resolve();
  private readonly inFlightDecisions = new InFlightDecisionAttempts();
  private closePromise: Promise<void> | undefined;
  private closing = false;
  private closed = false;

  private constructor(args: {
    readonly filePath: string;
    readonly lease: JsonStoreLease;
  }) {
    this.filePath = args.filePath;
    this.lease = args.lease;
  }

  static async open(
    path: string,
    options: JsonStoreLeaseOptions = {},
  ): Promise<JsonFileCommitteeStore> {
    const filePath = path.endsWith(".json")
      ? path
      : await committeeStoreFilePath(path);
    await mkdir(dirname(filePath), { recursive: true });
    const lease = await JsonStoreLease.acquire(`${filePath}.lock`, options);
    const store = new JsonFileCommitteeStore({ filePath, lease });
    try {
      await store.read();
      return store;
    } catch (error) {
      await lease.release().catch(() => undefined);
      throw error;
    }
  }

  async close(): Promise<void> {
    if (this.closePromise === undefined) {
      this.closing = true;
      this.closePromise = (async () => {
        await this.writeQueue.catch(() => undefined);
        await this.lease.release();
        this.closed = true;
      })();
    }
    await this.closePromise;
  }

  async initDeployment(args: {
    readonly marker: DeploymentMarker;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void> {
    await this.mutate((data) => {
      const marker = parseDeploymentMarker(args.marker);
      if (data.deployment !== undefined) {
        try {
          assertDeploymentMarkerMatches(
            marker,
            data.deployment.marker,
            "DA file store",
          );
        } catch {
          throw new Error(
            `stale_deployment_state_requires_fresh_redeploy: stored_manifest_id=${data.deployment.marker.manifestId}, canonical_manifest_id=${marker.manifestId}, contract_deployment_info_sha256=${args.contractDeploymentInfoSha256}; refusing to reuse stale committee node state; perform an explicit fresh redeploy/reset before deleting local committee node state.`,
          );
        }
      }
      return {
        ...data,
        deployment: {
          marker,
          manifestSha256: args.manifestSha256,
          contractDeploymentInfoSha256: args.contractDeploymentInfoSha256,
          manifestRaw: args.manifestRaw,
        },
      };
    });
  }

  async getDeployment(): Promise<CommitteeDeploymentRecord | undefined> {
    const data = await this.read();
    return data.deployment;
  }

  async getL1SourceState(): Promise<L1SourceState | undefined> {
    const data = await this.read();
    return data.chainCursor;
  }

  async saveL1SourceState(state: L1SourceState): Promise<void> {
    const canonical = parseL1SourceState(state);
    await this.mutate((data) => ({
      ...data,
      chainCursor: mergeL1SourceState(data.chainCursor, canonical),
    }));
  }

  async getDecisionOutbox(
    effectId: string,
  ): Promise<DecisionOutboxRecord | undefined> {
    return (await this.read()).decisionOutbox[effectId];
  }

  async listDecisionOutbox(
    headerHash?: string,
  ): Promise<readonly DecisionOutboxRecord[]> {
    return Object.values((await this.read()).decisionOutbox)
      .filter(
        (record) =>
          headerHash === undefined || record.headerHash === headerHash,
      )
      .sort((left, right) => left.effectId.localeCompare(right.effectId));
  }

  async beginDecisionEffect(args: {
    readonly effect: DecisionOutboxRecord;
    readonly sourceState: L1SourceState;
    readonly signature?: DaSignatureRecord;
  }): Promise<void> {
    const effect = parseDecisionOutboxRecord(args.effect);
    if (effect.status !== "pending") {
      throw new Error("decision outbox begin requires pending status");
    }
    const proposedSourceState = parseL1SourceState(args.sourceState);
    const signature =
      args.signature === undefined
        ? undefined
        : parseDaSignatureRecord(args.signature);
    assertDecisionSignature(effect, signature);
    let claimed = false;
    try {
      await this.mutate((data) => {
        const sourceState = mergeL1SourceState(
          data.chainCursor,
          proposedSourceState,
        );
        assertDecisionSourceState(effect, sourceState);
        assertDecisionRetry(data.decisionOutbox[effect.effectId], effect);
        this.inFlightDecisions.claim(effect);
        claimed = true;
        return {
          ...data,
          chainCursor: sourceState,
          decisionOutbox: {
            ...data.decisionOutbox,
            [effect.effectId]: effect,
          },
          ...(signature === undefined
            ? {}
            : {
                daSignatures: {
                  ...data.daSignatures,
                  [signatureKey(
                    signature.headerHash,
                    signature.availabilityCommitmentDigest,
                    signature.signerIndex,
                  )]: signature,
                },
              }),
        };
      });
    } catch (error) {
      if (claimed) {
        this.inFlightDecisions.release(effect.effectId, effect.attemptCount);
      }
      throw error;
    }
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
      await this.completeDecisionEffectRecord(args);
    } finally {
      this.inFlightDecisions.release(args.effectId, args.expectedAttemptCount);
    }
  }

  private async completeDecisionEffectRecord(args: {
    readonly effectId: string;
    readonly expectedAttemptCount: number;
    readonly status: Exclude<DecisionOutboxStatus, "pending">;
    readonly updatedAt: string;
    readonly lastError?: string;
    readonly signature?: DaSignatureRecord;
  }): Promise<void> {
    await this.mutate((data) => {
      const existing = data.decisionOutbox[args.effectId];
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
      if (data.chainCursor === undefined) {
        throw new Error("decision outbox completion lacks L1 source state");
      }
      assertDecisionSourceState(existing, data.chainCursor);
      const signature =
        args.signature === undefined
          ? undefined
          : parseDaSignatureRecord(args.signature);
      assertDecisionSignature(existing, signature);
      const completed = parseDecisionOutboxRecord({
        ...existing,
        status: args.status,
        updatedAt: args.updatedAt,
        ...(args.lastError === undefined
          ? { lastError: undefined }
          : { lastError: args.lastError }),
      });
      return {
        ...data,
        decisionOutbox: {
          ...data.decisionOutbox,
          [args.effectId]: completed,
        },
        ...(signature === undefined
          ? {}
          : {
              daSignatures: {
                ...data.daSignatures,
                [signatureKey(
                  signature.headerHash,
                  signature.availabilityCommitmentDigest,
                  signature.signerIndex,
                )]: signature,
              },
            }),
      };
    });
  }

  async quarantineL1Decisions(state: L1SourceState): Promise<void> {
    const canonical = parseL1SourceState(state);
    if (canonical.status !== "quarantined") {
      throw new Error(
        "L1 decision quarantine requires quarantined source state",
      );
    }
    await this.mutate((data) => {
      const quarantined = mergeQuarantinedL1SourceState(
        data.chainCursor,
        canonical,
      );
      const affectedHeaders = new Set(
        quarantined.observations
          .filter(({ hasPersistedDecision }) => hasPersistedDecision)
          .map(({ headerHash }) => headerHash),
      );
      const reason = quarantined.quarantineReason!;
      const errorCode = `l1_source_quarantined:${reason}`;
      return {
        ...data,
        chainCursor: quarantined,
        stateQueueHeaders: Object.fromEntries(
          Object.entries(data.stateQueueHeaders).map(([key, record]) => [
            key,
            affectedHeaders.has(record.headerHash)
              ? {
                  ...record,
                  status: "conflicted",
                  validationErrors: [
                    ...new Set([...record.validationErrors, errorCode]),
                  ],
                  updatedAt: quarantined.quarantinedAt!,
                }
              : record,
          ]),
        ),
        daPayloads: Object.fromEntries(
          Object.entries(data.daPayloads).map(([key, record]) => [
            key,
            affectedHeaders.has(record.headerHash)
              ? {
                  ...record,
                  validationStatus: "conflicted",
                  validationError: errorCode,
                }
              : record,
          ]),
        ),
        daSignatures: Object.fromEntries(
          Object.entries(data.daSignatures).map(([key, record]) => [
            key,
            affectedHeaders.has(record.headerHash)
              ? { ...record, broadcastStatus: "post_failed" }
              : record,
          ]),
        ),
        l1Submissions: Object.fromEntries(
          Object.entries(data.l1Submissions).map(([key, record]) => [
            key,
            affectedHeaders.has(record.headerHash)
              ? {
                  ...record,
                  resultStatus: "failed",
                  failureCause: errorCode,
                }
              : record,
          ]),
        ),
        peerBroadcasts: Object.fromEntries(
          Object.entries(data.peerBroadcasts).map(([key, record]) => [
            key,
            affectedHeaders.has(record.headerHash)
              ? {
                  ...record,
                  status: "failed",
                  nextAttemptAt: undefined,
                  lastError: errorCode,
                  updatedAt: quarantined.quarantinedAt!,
                }
              : record,
          ]),
        ),
        decisionOutbox: Object.fromEntries(
          Object.entries(data.decisionOutbox).map(([key, record]) => [
            key,
            affectedHeaders.has(record.headerHash)
              ? {
                  ...record,
                  status: "failed",
                  lastError: errorCode,
                  quarantineReason: reason,
                  quarantinedAt: quarantined.quarantinedAt!,
                  updatedAt: quarantined.quarantinedAt!,
                }
              : record,
          ]),
        ),
      };
    });
  }

  async upsertStateQueueHeader(record: StateQueueHeaderRecord): Promise<void> {
    await this.mutate((data) => ({
      ...data,
      stateQueueHeaders: {
        ...data.stateQueueHeaders,
        [record.headerHash]: record,
      },
    }));
  }

  async listStateQueueHeaders(): Promise<readonly StateQueueHeaderRecord[]> {
    const data = await this.read();
    return Object.values(data.stateQueueHeaders).sort((left, right) =>
      left.headerHash.localeCompare(right.headerHash),
    );
  }

  async getStateQueueHeader(
    headerHash: string,
  ): Promise<StateQueueHeaderRecord | undefined> {
    const data = await this.read();
    return data.stateQueueHeaders[headerHash];
  }

  async saveDaPayload(record: DaPayloadRecord): Promise<DaStoredPayloadRecord> {
    const canonicalRecord = parseDaStoredPayloadRecord(record);
    let saved: DaStoredPayloadRecord = canonicalRecord;
    await this.mutate((data) => {
      const existing = data.daPayloads[canonicalRecord.headerHash];
      saved = resolveDaPayloadSave(existing, canonicalRecord);
      return {
        ...data,
        daPayloads: {
          ...data.daPayloads,
          [canonicalRecord.headerHash]: saved,
        },
      };
    });
    return saved;
  }

  async getDaPayload(
    headerHash: string,
  ): Promise<DaStoredPayloadRecord | undefined> {
    const data = await this.read();
    return data.daPayloads[headerHash];
  }

  async listDaPayloads(): Promise<readonly DaStoredPayloadRecord[]> {
    const data = await this.read();
    return Object.values(data.daPayloads).sort((left, right) =>
      left.headerHash.localeCompare(right.headerHash),
    );
  }

  async deleteDaPayloadIfPrunable(
    request: RetainedPayloadPruneRequest,
  ): Promise<boolean> {
    let deleted = false;
    await this.mutate((data) => {
      const payload = data.daPayloads[request.headerHash];
      if (payload === undefined) {
        return data;
      }
      const header = data.stateQueueHeaders[request.headerHash];
      const decision = daRetentionPruneDecision({
        nowMs: request.nowMs,
        blockEndTimeMs: retentionBlockEndTimeMs(payload, header),
        headerStatus: header?.status ?? "unobserved",
        queueReference: retentionQueueReference(request.headerHash, request),
        retentionDays: request.retentionDays,
      });
      if (decision.decision !== "prune") {
        return data;
      }
      const daPayloads = { ...data.daPayloads };
      delete daPayloads[request.headerHash];
      deleted = true;
      return { ...data, daPayloads };
    });
    return deleted;
  }

  async saveDaSignature(record: DaSignatureRecord): Promise<void> {
    const canonicalRecord = parseDaSignatureRecord(record);
    await this.mutate((data) => {
      if (data.chainCursor?.status === "quarantined") {
        throw new Error(
          "cannot persist a DA signature while the L1 source is quarantined",
        );
      }
      return {
        ...data,
        daSignatures: {
          ...data.daSignatures,
          [signatureKey(
            canonicalRecord.headerHash,
            canonicalRecord.availabilityCommitmentDigest,
            canonicalRecord.signerIndex,
          )]: canonicalRecord,
        },
      };
    });
  }

  async getDaSignature(args: {
    readonly headerHash: string;
    readonly availabilityCommitmentDigest: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecordV1 | undefined> {
    const data = await this.read();
    return data.daSignatures[
      signatureKey(
        args.headerHash,
        args.availabilityCommitmentDigest,
        args.signerIndex,
      )
    ];
  }

  async listDaSignatures(
    headerHash?: string,
  ): Promise<readonly DaSignatureRecordV1[]> {
    const data = await this.read();
    return Object.values(data.daSignatures)
      .filter(
        (record) =>
          headerHash === undefined || record.headerHash === headerHash,
      )
      .sort(
        (left, right) =>
          left.headerHash.localeCompare(right.headerHash) ||
          left.availabilityCommitmentDigest.localeCompare(
            right.availabilityCommitmentDigest,
          ) ||
          left.signerIndex - right.signerIndex,
      );
  }

  async saveDaConflictEvidence(
    record: DaStoredConflictEvidenceRecord,
  ): Promise<boolean> {
    const canonicalRecord = parseDaStoredConflictEvidenceRecord(record);
    let accepted = false;
    await this.mutate((data) => {
      const key = conflictEvidenceKey(canonicalRecord);
      if (data.daConflictEvidence[key] !== undefined) {
        return data;
      }
      accepted = true;
      return {
        ...data,
        daConflictEvidence: {
          ...data.daConflictEvidence,
          [key]: canonicalRecord,
        },
      };
    });
    return accepted;
  }

  async listDaConflictEvidence(
    headerHash?: string,
  ): Promise<readonly DaStoredConflictEvidenceRecord[]> {
    const data = await this.read();
    return Object.values(data.daConflictEvidence)
      .filter(
        (record) =>
          headerHash === undefined || record.headerHash === headerHash,
      )
      .sort(
        (left, right) =>
          left.headerHash.localeCompare(right.headerHash) ||
          left.signerIndex - right.signerIndex ||
          left.evidenceHash.localeCompare(right.evidenceHash),
      );
  }

  async saveDaAttestationCandidate(
    record: DaAttestationCandidateRecord,
  ): Promise<void> {
    await this.mutate((data) => ({
      ...data,
      daAttestationCandidates: {
        ...data.daAttestationCandidates,
        [`${record.headerHash}:${record.outRef}`]: record,
      },
    }));
  }

  async listDaAttestationCandidates(
    headerHash?: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    const data = await this.read();
    return Object.values(data.daAttestationCandidates)
      .filter(
        (record) =>
          headerHash === undefined || record.headerHash === headerHash,
      )
      .sort(
        (left, right) =>
          left.headerHash.localeCompare(right.headerHash) ||
          left.outRef.localeCompare(right.outRef),
      );
  }

  async saveL1Submission(record: L1SubmissionRecord): Promise<void> {
    await this.mutate((data) => ({
      ...data,
      l1Submissions: {
        ...data.l1Submissions,
        [`${record.headerHash}:${record.txKind}:${record.txHash}`]: record,
      },
    }));
  }

  async listL1Submissions(): Promise<readonly L1SubmissionRecord[]> {
    const data = await this.read();
    return Object.values(data.l1Submissions).sort(
      (left, right) =>
        left.headerHash.localeCompare(right.headerHash) ||
        left.txKind.localeCompare(right.txKind) ||
        left.txHash.localeCompare(right.txHash),
    );
  }

  async savePeerBroadcast(record: DaPeerBroadcastRecord): Promise<void> {
    await this.mutate((data) => ({
      ...data,
      peerBroadcasts: {
        ...data.peerBroadcasts,
        [peerBroadcastKey(
          record.peerId,
          record.headerHash,
          record.availabilityCommitmentDigest,
          record.signerIndex,
        )]: record,
      },
    }));
  }

  async getPeerBroadcast(args: {
    readonly peerId: string;
    readonly headerHash: string;
    readonly availabilityCommitmentDigest: string;
    readonly signerIndex: number;
  }): Promise<DaPeerBroadcastRecord | undefined> {
    const data = await this.read();
    return data.peerBroadcasts[
      peerBroadcastKey(
        args.peerId,
        args.headerHash,
        args.availabilityCommitmentDigest,
        args.signerIndex,
      )
    ];
  }

  async listPeerBroadcasts(
    headerHash?: string,
  ): Promise<readonly DaPeerBroadcastRecord[]> {
    const data = await this.read();
    return Object.values(data.peerBroadcasts)
      .filter(
        (record) =>
          headerHash === undefined || record.headerHash === headerHash,
      )
      .sort(
        (left, right) =>
          left.headerHash.localeCompare(right.headerHash) ||
          left.availabilityCommitmentDigest.localeCompare(
            right.availabilityCommitmentDigest,
          ) ||
          left.signerIndex - right.signerIndex ||
          left.peerId.localeCompare(right.peerId),
      );
  }

  async savePeerHealth(record: DaPeerHealthRecord): Promise<void> {
    await this.mutate((data) => ({
      ...data,
      peerHealth: {
        ...data.peerHealth,
        [record.peerId]: record,
      },
    }));
  }

  async listPeerHealth(): Promise<readonly DaPeerHealthRecord[]> {
    const data = await this.read();
    return Object.values(data.peerHealth).sort((left, right) =>
      left.peerId.localeCompare(right.peerId),
    );
  }

  async recordPeerNonce(record: DaPeerNonceRecord): Promise<boolean> {
    let accepted = false;
    await this.mutate((data) => {
      const key = peerNonceKey(
        record.deploymentFingerprint,
        record.signerIndex,
        record.nonce,
      );
      if (data.peerNonces[key] !== undefined) {
        return data;
      }
      accepted = true;
      return {
        ...data,
        peerNonces: {
          ...data.peerNonces,
          [key]: record,
        },
      };
    });
    return accepted;
  }

  private async mutate(update: (data: StoreData) => StoreData): Promise<void> {
    if (this.closing || this.closed) {
      throw new Error("committee node file store is closed");
    }
    const operation = this.writeQueue.then(async () => {
      const data = await this.read();
      await this.write(update(data));
    });
    this.writeQueue = operation.catch(() => undefined);
    await operation;
  }

  private async read(): Promise<StoreData> {
    try {
      const raw = await readFile(this.filePath, "utf8");
      return normalizeStoreData(JSON.parse(raw, jsonReviver) as unknown);
    } catch (error) {
      if (isNodeError(error) && error.code === "ENOENT") {
        const data = emptyStoreData();
        await this.write(data);
        return data;
      }
      throw error;
    }
  }

  private async write(data: StoreData): Promise<void> {
    await this.lease.assertHeld();
    const tmpPath = `${this.filePath}.${this.lease.owner.replace(":", "-")}.tmp`;
    await writeFile(tmpPath, `${JSON.stringify(data, jsonReplacer, 2)}\n`);
    await rename(tmpPath, this.filePath);
  }
}

const terminalPayloadStatuses = new Set<DaPayloadRecord["validationStatus"]>([
  "verified",
  "malformed_da",
  "root_mismatch",
  "conflicted",
]);

export const resolveDaPayloadSave = (
  existing: DaStoredPayloadRecord | undefined,
  record: DaStoredPayloadRecord,
): DaStoredPayloadRecord => {
  if (existing === undefined) {
    return withDerivedPayloadFetchStatus(record);
  }
  if (hasPayloadBytes(existing) && !hasPayloadBytes(record)) {
    return existing;
  }
  if (
    hasPayloadBytes(existing) &&
    hasPayloadBytes(record) &&
    existing.payloadSha256 !== record.payloadSha256
  ) {
    return {
      ...withDerivedPayloadFetchStatus(record),
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
      validationError: `payload bytes conflict with existing sha256 ${existing.payloadSha256}`,
    };
  }
  if (
    hasPayloadBytes(existing) &&
    hasPayloadBytes(record) &&
    existing.payloadSha256 === record.payloadSha256 &&
    terminalPayloadStatuses.has(existing.validationStatus) &&
    !terminalPayloadStatuses.has(record.validationStatus)
  ) {
    return existing;
  }
  return withDerivedPayloadFetchStatus(record);
};

const normalizeStoreData = (value: unknown): StoreData => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("committee node store data must be an object");
  }
  const record = value as Partial<StoreData>;
  return {
    ...(record.deployment === undefined
      ? {}
      : { deployment: parseCommitteeDeploymentRecord(record.deployment) }),
    chainCursor:
      record.chainCursor === undefined
        ? undefined
        : parseL1SourceState(record.chainCursor),
    stateQueueHeaders: record.stateQueueHeaders ?? {},
    daPayloads: parseStoredRecordMap(
      record.daPayloads,
      parseDaStoredPayloadRecord,
      (entry) => entry.headerHash,
      "DA stored payload records V1",
    ),
    daSignatures: parseStoredRecordMap(
      record.daSignatures,
      parseDaSignatureRecord,
      (entry) =>
        signatureKey(
          entry.headerHash,
          entry.availabilityCommitmentDigest,
          entry.signerIndex,
        ),
      "DA signature records V1",
    ),
    daConflictEvidence: parseStoredRecordMap(
      record.daConflictEvidence,
      parseDaStoredConflictEvidenceRecord,
      conflictEvidenceKey,
      "DA conflict evidence records V1",
    ),
    daAttestationCandidates: record.daAttestationCandidates ?? {},
    l1Submissions: record.l1Submissions ?? {},
    peerBroadcasts: record.peerBroadcasts ?? {},
    peerHealth: record.peerHealth ?? {},
    peerNonces: record.peerNonces ?? {},
    decisionOutbox: parseStoredRecordMap(
      record.decisionOutbox,
      parseDecisionOutboxRecord,
      (entry) => entry.effectId,
      "decision outbox records V1",
    ),
  };
};

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

/** A non-empty list of well-formed steps, or undefined. */
const parseStateQueueOutputSteps = (
  value: unknown,
): readonly StateQueueOutputStep[] | undefined => {
  if (!Array.isArray(value) || value.length === 0) return undefined;
  const steps: StateQueueOutputStep[] = [];
  for (const entry of value as unknown[]) {
    if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
      return undefined;
    }
    const step = entry as Partial<StateQueueOutputStep>;
    if (
      Object.keys(step).some(
        (key) => !["fromOutRef", "toOutRef", "slot", "blockHash"].includes(key),
      ) ||
      typeof step.fromOutRef !== "string" ||
      !OUT_REF.test(step.fromOutRef) ||
      (step.toOutRef !== undefined &&
        (typeof step.toOutRef !== "string" || !OUT_REF.test(step.toOutRef))) ||
      typeof step.slot !== "number" ||
      !Number.isSafeInteger(step.slot) ||
      step.slot < 0 ||
      typeof step.blockHash !== "string" ||
      !/^[0-9a-f]{64}$/u.test(step.blockHash)
    ) {
      return undefined;
    }
    steps.push({
      fromOutRef: step.fromOutRef,
      ...(step.toOutRef === undefined ? {} : { toOutRef: step.toOutRef }),
      slot: step.slot,
      blockHash: step.blockHash,
    });
  }
  return steps;
};

export const parseL1SourceState = (value: unknown): L1SourceState => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("committee node L1 source state must be an object");
  }
  const state = value as Partial<L1SourceState>;
  const stateKeys = new Set([
    "schemaVersion",
    "sourceMode",
    "network",
    "authoritySha256",
    "status",
    "observations",
    "observedAt",
    "stateQueueReplayAnchor",
    "quarantineReason",
    "quarantinedAt",
  ]);
  if (
    Object.keys(state).some((key) => !stateKeys.has(key)) ||
    state.schemaVersion !== 1 ||
    (state.sourceMode !== "local_node" &&
      state.sourceMode !== "external_providers") ||
    typeof state.network !== "string" ||
    state.network.trim() !== state.network ||
    state.network.length === 0 ||
    typeof state.authoritySha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(state.authoritySha256) ||
    (state.status !== "healthy" && state.status !== "quarantined") ||
    typeof state.observedAt !== "string" ||
    !isCanonicalIsoTimestamp(state.observedAt) ||
    !Array.isArray(state.observations)
  ) {
    throw new Error("committee node L1 source state is malformed");
  }
  if (
    state.status === "quarantined" &&
    (typeof state.quarantineReason !== "string" ||
      state.quarantineReason.length === 0 ||
      typeof state.quarantinedAt !== "string" ||
      !isCanonicalIsoTimestamp(state.quarantinedAt))
  ) {
    throw new Error(
      "quarantined committee node L1 source state lacks evidence",
    );
  }
  if (
    state.status === "healthy" &&
    (state.quarantineReason !== undefined || state.quarantinedAt !== undefined)
  ) {
    throw new Error(
      "healthy committee node L1 source state contains quarantine fields",
    );
  }
  const observations = state.observations.map((entry) => {
    if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
      throw new Error("committee node L1 source observation is malformed");
    }
    const record = entry as Partial<L1ObservedDecision>;
    const observationKeys = new Set([
      "headerHash",
      "stateQueueOutRef",
      "stateQueueStatus",
      "lastKnownStatus",
      "slot",
      "blockHash",
      "finalized",
      "hasPersistedDecision",
      "authenticatedSteps",
    ]);
    const authenticatedSteps = parseStateQueueOutputSteps(
      record.authenticatedSteps,
    );
    if (
      Object.keys(record).some((key) => !observationKeys.has(key)) ||
      typeof record.headerHash !== "string" ||
      !/^[0-9a-f]{56}$/u.test(record.headerHash) ||
      typeof record.stateQueueOutRef !== "string" ||
      !/^[0-9a-f]{64}#[0-9]+$/u.test(record.stateQueueOutRef) ||
      (record.stateQueueStatus !== "unattested" &&
        record.stateQueueStatus !== "attesting" &&
        record.stateQueueStatus !== "attested" &&
        record.stateQueueStatus !== "merged" &&
        record.stateQueueStatus !== "removed" &&
        record.stateQueueStatus !== "conflicted" &&
        record.stateQueueStatus !== UNKNOWN_STATE_QUEUE_STATUS) ||
      (record.stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS
        ? record.lastKnownStatus !== "unattested" &&
          record.lastKnownStatus !== "attesting" &&
          record.lastKnownStatus !== "attested" &&
          record.lastKnownStatus !== "merged" &&
          record.lastKnownStatus !== "removed" &&
          record.lastKnownStatus !== "conflicted"
        : record.lastKnownStatus !== undefined) ||
      typeof record.finalized !== "boolean" ||
      typeof record.hasPersistedDecision !== "boolean" ||
      (record.slot !== undefined &&
        (!Number.isSafeInteger(record.slot) || record.slot < 0)) ||
      (record.blockHash !== undefined &&
        (typeof record.blockHash !== "string" ||
          !/^[0-9a-f]{64}$/u.test(record.blockHash))) ||
      (record.authenticatedSteps !== undefined &&
        authenticatedSteps === undefined)
    ) {
      throw new Error("committee node L1 source observation is malformed");
    }
    return {
      headerHash: record.headerHash,
      stateQueueOutRef: record.stateQueueOutRef,
      stateQueueStatus: record.stateQueueStatus,
      ...(record.lastKnownStatus === undefined
        ? {}
        : { lastKnownStatus: record.lastKnownStatus }),
      ...(record.slot === undefined ? {} : { slot: record.slot }),
      ...(record.blockHash === undefined
        ? {}
        : { blockHash: record.blockHash }),
      finalized: record.finalized,
      hasPersistedDecision: record.hasPersistedDecision,
      ...(authenticatedSteps === undefined ? {} : { authenticatedSteps }),
    };
  });
  observations.sort((left, right) =>
    left.headerHash.localeCompare(right.headerHash),
  );
  if (
    new Set(observations.map(({ headerHash }) => headerHash)).size !==
    observations.length
  ) {
    throw new Error(
      "committee node L1 source observations contain duplicate headers",
    );
  }
  const stateQueueReplayAnchor = parseStateQueueReplayAnchor(
    state.stateQueueReplayAnchor,
  );
  if (
    state.stateQueueReplayAnchor !== undefined &&
    stateQueueReplayAnchor === undefined
  ) {
    throw new Error("committee node L1 source replay anchor is malformed");
  }
  return {
    schemaVersion: 1,
    sourceMode: state.sourceMode,
    network: state.network,
    authoritySha256: state.authoritySha256,
    status: state.status,
    observations,
    observedAt: state.observedAt,
    ...(stateQueueReplayAnchor === undefined ? {} : { stateQueueReplayAnchor }),
    ...(state.quarantineReason === undefined
      ? {}
      : { quarantineReason: state.quarantineReason }),
    ...(state.quarantinedAt === undefined
      ? {}
      : { quarantinedAt: state.quarantinedAt }),
  };
};
