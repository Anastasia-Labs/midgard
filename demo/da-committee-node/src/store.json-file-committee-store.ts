import { mkdir, open, readFile, rename } from "node:fs/promises";
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
import {
  type CommitteeDeploymentRecord,
  type CommitteeStore,
  conflictEvidenceKey,
  type DecisionOutboxRecord,
  type DecisionOutboxStatus,
  InFlightDecisionAttempts,
  type L1SourceState,
  peerBroadcastKey,
  peerNonceKey,
  type RetainedPayloadPruneRequest,
  signatureKey,
  type StoreData,
} from "./store.committee-store.js";
import {
  JsonStoreInstanceLock,
  type JsonStoreInstanceLockOptions,
} from "./store.json-file-instance-lock.js";
import {
  assertDecisionRetry,
  emptyStoreData,
  parseDecisionOutboxRecord,
} from "./store.parse-decision-outbox-record.js";
import {
  committeeStoreFilePath,
  isNodeError,
  jsonReplacer,
  parseStoredJson,
} from "./store.parse-stored-record-map.js";
import {
  assertDecisionSignature,
  assertDecisionSourceState,
  mergeL1SourceState,
  mergeQuarantinedL1SourceState,
} from "./store.persisted-decision-transition.js";
import {
  jsonApplyL1Recovery,
  jsonL1RecoverySnapshot,
} from "./store/l1-recovery-json.js";
import {
  jsonCapacityReader,
  jsonCapacityWriter,
} from "./store/promise-capacity-json.js";
import { jsonPromiseResources } from "./store/promise-resource-usage.js";
import { resolveDaPayloadSave } from "./store/resolve-da-payload-save.js";
export { resolveDaPayloadSave } from "./store/resolve-da-payload-save.js";
import { parseL1SourceState } from "./store.parse-l1-source-state.js";
import { dropCrossHeaderConflicts } from "./store/drop-cross-header-conflict-evidence.js";
import {
  retentionBlockEndTimeMs,
  retentionQueueReference,
  terminalRecoveryFinal,
} from "./store/retention.js";
export { parseL1SourceState } from "./store.parse-l1-source-state.js";
import {
  type CommitteeRetirementCertificate,
  consumeRetirementCertificate,
  readRetirementCertificate,
} from "./store/retirement-certificate.js";
import {
  type CommitteeRetirementBreachPoint,
  CommitteeRetirementController,
  type CommitteeRetirementFloor,
  type CommitteeRetirementGuard,
  type CommitteeRetirementSnapshot,
  makeRetirementFloor,
  parseRetirementBreachPoint,
} from "./store/retirement-model.js";
import {
  applyRetirementPlan,
  assertRetirementWrite,
  retirementStoreDigest,
} from "./store/retirement-transition.js";

export class JsonFileCommitteeStore implements CommitteeStore {
  promiseStoreResourceUsage = jsonPromiseResources(() => this.filePath);
  private readonly filePath: string;
  private readonly instanceLock: JsonStoreInstanceLock;
  private readonly retirement = new CommitteeRetirementController();
  private writeQueue: Promise<void> = Promise.resolve();
  private readonly inFlightDecisions = new InFlightDecisionAttempts();
  private closePromise: Promise<void> | undefined;
  private closing = false;
  private closed = false;

  readL1RecoverySnapshot = jsonL1RecoverySnapshot(
    () => this.writeQueue,
    () => this.instanceLock.assertHeld(),
    () => this.read(),
  );
  applyL1RecoveryCertificate = jsonApplyL1Recovery(
    this.retirement,
    () => this.read(),
    (data) => this.write(data),
    () => this.instanceLock.assertHeld(),
    () => this.closing || this.closed,
    (run) => {
      const operation = this.writeQueue.then(run);
      this.writeQueue = operation.catch(() => undefined);
      return operation;
    },
  );

  private constructor(args: {
    readonly filePath: string;
    readonly instanceLock: JsonStoreInstanceLock;
  }) {
    this.filePath = args.filePath;
    this.instanceLock = args.instanceLock;
  }

  static async open(
    path: string,
    options: JsonStoreInstanceLockOptions = {},
  ): Promise<JsonFileCommitteeStore> {
    const filePath = path.endsWith(".json")
      ? path
      : await committeeStoreFilePath(path);
    await mkdir(dirname(filePath), { recursive: true });
    const instanceLock = await JsonStoreInstanceLock.acquire(filePath, options);
    const store = new JsonFileCommitteeStore({ filePath, instanceLock });
    try {
      store.retirement.load((await store.read(true)).retirementFloor);
      return store;
    } catch (error) {
      await instanceLock.release().catch(() => undefined);
      throw error;
    }
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
    return (await this.read()).retirementFloor;
  }
  async readRetirementSnapshot(): Promise<CommitteeRetirementSnapshot> {
    const guard = this.captureRetirementGuard();
    const data = await this.read();
    this.assertRetirementGuard(guard);
    return { data, digest: retirementStoreDigest(data), guard };
  }
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
  async applyRetirementCertificate(
    certificate: CommitteeRetirementCertificate,
  ): Promise<readonly string[]> {
    const verified = readRetirementCertificate(certificate);
    await verified.assertCurrent();
    if (this.closing || this.closed)
      throw new Error("Committee store is closed");
    this.retirement.begin(verified.snapshot.guard);
    const operation = this.writeQueue.then(async () => {
      const data = await this.read();
      if (retirementStoreDigest(data) !== verified.snapshot.digest)
        throw new Error("Retirement store snapshot changed");
      if (
        verified.plan.headerHashes.some((h) =>
          this.retirement.pinned().has(h),
        ) ||
        Object.values(data.decisionOutbox).some(
          (e) =>
            verified.plan.headerHashes.includes(e.headerHash) &&
            this.inFlightDecisions.has(e.effectId),
        )
      )
        throw new Error("Retirement cohort acquired a live callback");
      await verified.assertCurrent();
      verified.assertScopeCurrent();
      const next = applyRetirementPlan(data, verified.plan);
      await this.write(next);
      this.retirement.load(next.retirementFloor);
      return verified.plan.headerHashes;
    });
    this.writeQueue = operation.then(
      () => undefined,
      () => undefined,
    );
    try {
      return await operation;
    } finally {
      consumeRetirementCertificate(certificate);
      this.retirement.end();
    }
  }
  async recordRetirementBreach(
    reason: string,
    observedAt: CommitteeRetirementBreachPoint,
  ): Promise<void> {
    const point = parseRetirementBreachPoint(observedAt);
    this.retirement.holdBreach();
    const operation = this.writeQueue.then(async () => {
      const data = await this.read();
      const floor = data.retirementFloor;
      if (!floor || floor.breach) {
        this.retirement.persistedBreach();
        return;
      }
      const { digest: _digest, ...prior } = floor;
      const next = makeRetirementFloor({
        ...prior,
        generation: floor.generation + 1,
        breach: { reason, observedAt: point },
      });
      await this.write({ ...data, retirementFloor: next });
      this.retirement.load(next);
      this.retirement.persistedBreach();
    });
    this.writeQueue = operation.catch(() => undefined);
    await operation;
  }

  async close(): Promise<void> {
    if (this.closePromise === undefined) {
      this.closing = true;
      this.closePromise = (async () => {
        await this.writeQueue.catch(() => undefined);
        await this.instanceLock.release();
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
        // Retained payloads are left exactly as they were: see
        // `CommitteeStore.quarantineL1Decisions`.
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
      if (data.retirementFloor !== undefined) return data;
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
        terminalRecoveryFinal: terminalRecoveryFinal(
          header,
          request,
          payload.deploymentFingerprint,
        ),
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

  getPromiseCapacityEvidence = jsonCapacityReader(this.read.bind(this));
  savePromiseCapacityEvidence = jsonCapacityWriter(this.mutate.bind(this));
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
    const generation = this.retirement.generation();
    const operation = this.writeQueue.then(async () => {
      const data = await this.read();
      this.retirement.assertGeneration(generation);
      const next = update(data);
      assertRetirementWrite(data, next);
      await this.write(next);
    });
    this.writeQueue = operation.catch(() => undefined);
    await operation;
  }
  private async read(persistCleanup = false): Promise<StoreData> {
    try {
      const raw = parseStoredJson(await readFile(this.filePath, "utf8"));
      return await dropCrossHeaderConflicts(raw, persistCleanup && this.write);
    } catch (error) {
      if (isNodeError(error) && error.code === "ENOENT") {
        const data = emptyStoreData();
        await this.write(data);
        return data;
      }
      throw error;
    }
  }
  private write = async (data: StoreData): Promise<void> => {
    await this.instanceLock.assertHeld();
    const tmpPath = `${this.filePath}.${this.instanceLock.owner.replace(":", "-")}.tmp`;
    const file = await open(tmpPath, "w", 0o600);
    try {
      await file.writeFile(`${JSON.stringify(data, jsonReplacer, 2)}\n`);
      await file.sync();
    } finally {
      await file.close();
    }
    await rename(tmpPath, this.filePath);
    const directory = await open(dirname(this.filePath), "r");
    try {
      await directory.sync();
    } finally {
      await directory.close();
    }
  };
}
