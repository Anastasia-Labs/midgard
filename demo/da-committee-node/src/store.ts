import { randomUUID } from "node:crypto";
import {
  type FileHandle,
  mkdir,
  open as openFile,
  readFile,
  rename,
  unlink,
  writeFile,
} from "node:fs/promises";
import { dirname, join } from "node:path";

import {
  assertDeploymentMarkerV1Matches,
  type DeploymentMarkerV1,
  parseDeploymentMarkerV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";

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
} from "./domain.js";
import {
  parseDaSignatureRecordV1,
  parseDaStoredConflictEvidenceRecordV1,
  parseDaStoredPayloadRecordV1,
} from "./domain.js";

type StoreData = {
  readonly deployment?: WatcherDeploymentRecord;
  readonly chainCursor?: L1SourceState;
  readonly stateQueueHeaders: Record<string, StateQueueHeaderRecord>;
  readonly daPayloads: Record<string, DaStoredPayloadRecordV1>;
  readonly daSignatures: Record<string, DaSignatureRecordV1>;
  readonly daConflictEvidence: Record<string, DaStoredConflictEvidenceRecordV1>;
  readonly daAttestationCandidates: Record<
    string,
    DaAttestationCandidateRecord
  >;
  readonly l1Submissions: Record<string, L1SubmissionRecord>;
  readonly peerBroadcasts: Record<string, DaPeerBroadcastRecord>;
  readonly peerHealth: Record<string, DaPeerHealthRecord>;
  readonly peerNonces: Record<string, DaPeerNonceRecord>;
  readonly decisionOutbox: Record<string, DecisionOutboxRecord>;
};

export type DecisionOutboxStatus =
  | "pending"
  | "published"
  | "failed"
  | "reconciled";

export const DECISION_EFFECT_PENDING_LEASE_MS = 5 * 60 * 1_000;

export type DecisionOutboxRecord = {
  readonly schemaVersion: 1;
  readonly effectId: string;
  readonly deploymentFingerprint: string;
  readonly sourceMode: L1SourceState["sourceMode"];
  readonly network: string;
  readonly effectKind: "signature_publish" | "l1_reconcile";
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly signerIndex?: number;
  readonly slot?: number;
  readonly blockHash?: string;
  readonly finalized: true;
  readonly status: DecisionOutboxStatus;
  readonly attemptCount: number;
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly lastError?: string;
  readonly quarantineReason?: string;
  readonly quarantinedAt?: string;
};

export type L1ObservedDecision = {
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly stateQueueStatus: StateQueueHeaderRecord["status"];
  readonly slot?: number;
  readonly blockHash?: string;
  readonly finalized: boolean;
  readonly hasPersistedDecision: boolean;
};

export type L1SourceState = {
  readonly schemaVersion: 1;
  readonly sourceMode: "local_node" | "external_providers";
  readonly network: string;
  readonly authoritySha256: string;
  readonly status: "healthy" | "quarantined";
  readonly observations: readonly L1ObservedDecision[];
  readonly observedAt: string;
  readonly quarantineReason?: string;
  readonly quarantinedAt?: string;
};

export type WatcherDeploymentRecord = {
  readonly marker: DeploymentMarkerV1;
  readonly manifestSha256: string;
  readonly contractDeploymentInfoSha256: string;
  readonly manifestRaw: string;
};

export interface WatcherStore {
  close?(): Promise<void>;
  initDeployment(args: {
    readonly marker: DeploymentMarkerV1;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void>;
  getDeployment(): Promise<WatcherDeploymentRecord | undefined>;
  getL1SourceState(): Promise<L1SourceState | undefined>;
  saveL1SourceState(state: L1SourceState): Promise<void>;
  getDecisionOutbox(
    effectId: string,
  ): Promise<DecisionOutboxRecord | undefined>;
  listDecisionOutbox(
    headerHash?: string,
  ): Promise<readonly DecisionOutboxRecord[]>;
  beginDecisionEffect(args: {
    readonly effect: DecisionOutboxRecord;
    readonly sourceState: L1SourceState;
    readonly signature?: DaSignatureRecord;
  }): Promise<void>;
  completeDecisionEffect(args: {
    readonly effectId: string;
    readonly expectedAttemptCount: number;
    readonly status: Exclude<DecisionOutboxStatus, "pending">;
    readonly updatedAt: string;
    readonly lastError?: string;
    readonly signature?: DaSignatureRecord;
  }): Promise<void>;
  quarantineL1Decisions(state: L1SourceState): Promise<void>;
  upsertStateQueueHeader(record: StateQueueHeaderRecord): Promise<void>;
  listStateQueueHeaders(): Promise<readonly StateQueueHeaderRecord[]>;
  getStateQueueHeader(
    headerHash: string,
  ): Promise<StateQueueHeaderRecord | undefined>;
  saveDaPayload(record: DaPayloadRecord): Promise<DaPayloadRecord>;
  getDaPayload(headerHash: string): Promise<DaPayloadRecord | undefined>;
  saveDaSignature(record: DaSignatureRecord): Promise<void>;
  getDaSignature(args: {
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecord | undefined>;
  listDaSignatures(headerHash?: string): Promise<readonly DaSignatureRecord[]>;
  saveDaConflictEvidence(
    record: DaStoredConflictEvidenceRecordV1,
  ): Promise<boolean>;
  listDaConflictEvidence(
    headerHash?: string,
  ): Promise<readonly DaStoredConflictEvidenceRecordV1[]>;
  saveDaAttestationCandidate(
    record: DaAttestationCandidateRecord,
  ): Promise<void>;
  listDaAttestationCandidates(
    headerHash?: string,
  ): Promise<readonly DaAttestationCandidateRecord[]>;
  saveL1Submission(record: L1SubmissionRecord): Promise<void>;
  listL1Submissions(): Promise<readonly L1SubmissionRecord[]>;
  savePeerBroadcast(record: DaPeerBroadcastRecord): Promise<void>;
  getPeerBroadcast(args: {
    readonly peerId: string;
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaPeerBroadcastRecord | undefined>;
  listPeerBroadcasts(
    headerHash?: string,
  ): Promise<readonly DaPeerBroadcastRecord[]>;
  savePeerHealth(record: DaPeerHealthRecord): Promise<void>;
  listPeerHealth(): Promise<readonly DaPeerHealthRecord[]>;
  recordPeerNonce(record: DaPeerNonceRecord): Promise<boolean>;
}

export class JsonFileWatcherStore implements WatcherStore {
  private readonly filePath: string;
  private readonly lockPath: string;
  private readonly lockHandle: FileHandle;
  private readonly lockOwner: string;
  private writeQueue: Promise<void> = Promise.resolve();
  private closePromise: Promise<void> | undefined;
  private closing = false;
  private closed = false;

  private constructor(args: {
    readonly filePath: string;
    readonly lockPath: string;
    readonly lockHandle: FileHandle;
    readonly lockOwner: string;
  }) {
    this.filePath = args.filePath;
    this.lockPath = args.lockPath;
    this.lockHandle = args.lockHandle;
    this.lockOwner = args.lockOwner;
  }

  static async open(path: string): Promise<JsonFileWatcherStore> {
    const filePath = path.endsWith(".json") ? path : join(path, "watcher.json");
    await mkdir(dirname(filePath), { recursive: true });
    const lockPath = `${filePath}.lock`;
    const lockOwner = `${process.pid.toString()}:${randomUUID()}`;
    let lockHandle: FileHandle;
    try {
      lockHandle = await openFile(lockPath, "wx", 0o600);
    } catch (error) {
      if (isNodeError(error) && error.code === "EEXIST") {
        throw new Error(
          `watcher file store is already exclusively leased: ${lockPath}; close the active watcher or perform explicit stale-lock recovery`,
        );
      }
      throw error;
    }
    const store = new JsonFileWatcherStore({
      filePath,
      lockPath,
      lockHandle,
      lockOwner,
    });
    try {
      await lockHandle.writeFile(
        `${JSON.stringify({ schemaVersion: 1, owner: lockOwner })}\n`,
        "utf8",
      );
      await lockHandle.sync();
      await store.read();
      return store;
    } catch (error) {
      await lockHandle.close().catch(() => undefined);
      await unlink(lockPath).catch(() => undefined);
      throw error;
    }
  }

  async close(): Promise<void> {
    if (this.closePromise === undefined) {
      this.closing = true;
      this.closePromise = (async () => {
        await this.writeQueue.catch(() => undefined);
        await this.lockHandle.close();
        await unlink(this.lockPath);
        this.closed = true;
      })();
    }
    await this.closePromise;
  }

  async initDeployment(args: {
    readonly marker: DeploymentMarkerV1;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void> {
    await this.mutate((data) => {
      const marker = parseDeploymentMarkerV1(args.marker);
      if (data.deployment !== undefined) {
        try {
          assertDeploymentMarkerV1Matches(
            marker,
            data.deployment.marker,
            "DA file store",
          );
        } catch {
          throw new Error(
            `stale_deployment_state_requires_fresh_redeploy: stored_manifest_id=${data.deployment.marker.manifestId}, canonical_manifest_id=${marker.manifestId}, contract_deployment_info_sha256=${args.contractDeploymentInfoSha256}; refusing to reuse stale watcher state; perform an explicit fresh redeploy/reset before deleting local watcher state.`,
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

  async getDeployment(): Promise<WatcherDeploymentRecord | undefined> {
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
        : parseDaSignatureRecordV1(args.signature);
    assertDecisionSignature(effect, signature);
    await this.mutate((data) => {
      const sourceState = mergeL1SourceState(
        data.chainCursor,
        proposedSourceState,
      );
      assertDecisionSourceState(effect, sourceState);
      assertDecisionRetry(data.decisionOutbox[effect.effectId], effect);
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
                [signatureKey(signature.headerHash, signature.signerIndex)]:
                  signature,
              },
            }),
      };
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
          : parseDaSignatureRecordV1(args.signature);
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
                [signatureKey(signature.headerHash, signature.signerIndex)]:
                  signature,
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

  async saveDaPayload(
    record: DaPayloadRecord,
  ): Promise<DaStoredPayloadRecordV1> {
    const canonicalRecord = parseDaStoredPayloadRecordV1(record);
    let saved: DaStoredPayloadRecordV1 = canonicalRecord;
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
  ): Promise<DaStoredPayloadRecordV1 | undefined> {
    const data = await this.read();
    return data.daPayloads[headerHash];
  }

  async saveDaSignature(record: DaSignatureRecord): Promise<void> {
    const canonicalRecord = parseDaSignatureRecordV1(record);
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
            canonicalRecord.signerIndex,
          )]: canonicalRecord,
        },
      };
    });
  }

  async getDaSignature(args: {
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecordV1 | undefined> {
    const data = await this.read();
    return data.daSignatures[signatureKey(args.headerHash, args.signerIndex)];
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
          left.signerIndex - right.signerIndex,
      );
  }

  async saveDaConflictEvidence(
    record: DaStoredConflictEvidenceRecordV1,
  ): Promise<boolean> {
    const canonicalRecord = parseDaStoredConflictEvidenceRecordV1(record);
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
  ): Promise<readonly DaStoredConflictEvidenceRecordV1[]> {
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
          record.signerIndex,
        )]: record,
      },
    }));
  }

  async getPeerBroadcast(args: {
    readonly peerId: string;
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaPeerBroadcastRecord | undefined> {
    const data = await this.read();
    return data.peerBroadcasts[
      peerBroadcastKey(args.peerId, args.headerHash, args.signerIndex)
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
      throw new Error("watcher file store is closed");
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
    const tmpPath = `${this.filePath}.${this.lockOwner.replace(":", "-")}.tmp`;
    await writeFile(tmpPath, `${JSON.stringify(data, jsonReplacer, 2)}\n`);
    await rename(tmpPath, this.filePath);
  }
}

const signatureKey = (headerHash: string, signerIndex: number): string =>
  `${headerHash}:${signerIndex.toString()}`;

const conflictEvidenceKey = (
  record: Pick<
    DaStoredConflictEvidenceRecordV1,
    "deploymentFingerprint" | "evidenceHash"
  >,
): string => `${record.deploymentFingerprint}:${record.evidenceHash}`;

const peerBroadcastKey = (
  peerId: string,
  headerHash: string,
  signerIndex: number,
): string => `${peerId}:${headerHash}:${signerIndex.toString()}`;

const peerNonceKey = (
  deploymentFingerprint: string,
  signerIndex: number,
  nonce: string,
): string => `${deploymentFingerprint}:${signerIndex.toString()}:${nonce}`;

export const hasPayloadBytes = (record: DaPayloadRecord): boolean =>
  record.payloadSha256.length > 0 && record.payloadCborHex.length > 0;

const terminalPayloadStatuses = new Set<DaPayloadRecord["validationStatus"]>([
  "verified",
  "malformed_da",
  "root_mismatch",
  "conflicted",
]);

export const resolveDaPayloadSave = (
  existing: DaStoredPayloadRecordV1 | undefined,
  record: DaStoredPayloadRecordV1,
): DaStoredPayloadRecordV1 => {
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

export const libp2pSubmittedDaPayloadRecord = (args: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly payloadSchemaVersion: 1;
  readonly payloadCbor: Uint8Array;
  readonly payloadSha256: string;
  readonly receivedAt: Date;
}): DaStoredPayloadRecordV1 => ({
  deploymentFingerprint: args.deploymentFingerprint,
  headerHash: args.headerHash,
  payloadSchemaVersion: args.payloadSchemaVersion,
  payloadCborHex: Buffer.from(args.payloadCbor).toString("hex"),
  payloadSha256: args.payloadSha256,
  sourcePeerId: "libp2p:payload-submit",
  fetchedAt: args.receivedAt.toISOString(),
  payloadFetchStatus: "available",
  validationStatus: "fetched",
});

const withDerivedPayloadFetchStatus = (
  record: DaStoredPayloadRecordV1,
): DaStoredPayloadRecordV1 => {
  if (record.payloadFetchStatus !== undefined) {
    return record;
  }
  if (hasPayloadBytes(record)) {
    return { ...record, payloadFetchStatus: "available" };
  }
  if (record.validationStatus === "missing_da") {
    return { ...record, payloadFetchStatus: "missing_da" };
  }
  return record;
};

const emptyStoreData = (): StoreData => ({
  stateQueueHeaders: {},
  daPayloads: {},
  daSignatures: {},
  daConflictEvidence: {},
  daAttestationCandidates: {},
  l1Submissions: {},
  peerBroadcasts: {},
  peerHealth: {},
  peerNonces: {},
  decisionOutbox: {},
});

const normalizeStoreData = (value: unknown): StoreData => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("watcher store data must be an object");
  }
  const record = value as Partial<StoreData>;
  return {
    ...(record.deployment === undefined
      ? {}
      : { deployment: parseWatcherDeploymentRecord(record.deployment) }),
    chainCursor:
      record.chainCursor === undefined
        ? undefined
        : parseL1SourceState(record.chainCursor),
    stateQueueHeaders: record.stateQueueHeaders ?? {},
    daPayloads: parseStoredRecordMap(
      record.daPayloads,
      parseDaStoredPayloadRecordV1,
      (entry) => entry.headerHash,
      "DA stored payload records V1",
    ),
    daSignatures: parseStoredRecordMap(
      record.daSignatures,
      parseDaSignatureRecordV1,
      (entry) => signatureKey(entry.headerHash, entry.signerIndex),
      "DA signature records V1",
    ),
    daConflictEvidence: parseStoredRecordMap(
      record.daConflictEvidence,
      parseDaStoredConflictEvidenceRecordV1,
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

const parseWatcherDeploymentRecord = (
  value: unknown,
): WatcherDeploymentRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("watcher deployment marker record must be an object");
  }
  const record = value as Record<string, unknown>;
  const expected = [
    "marker",
    "manifestSha256",
    "contractDeploymentInfoSha256",
    "manifestRaw",
  ] as const;
  if (
    Object.keys(record).length !== expected.length ||
    expected.some((key) => !Object.prototype.hasOwnProperty.call(record, key))
  ) {
    throw new Error(
      "watcher deployment marker record must contain exactly marker, manifestSha256, contractDeploymentInfoSha256, and manifestRaw",
    );
  }
  const digest = (field: "manifestSha256" | "contractDeploymentInfoSha256") => {
    const entry = record[field];
    if (typeof entry !== "string" || !/^[0-9a-f]{64}$/u.test(entry)) {
      throw new Error(
        `watcher deployment marker record ${field} must be lowercase SHA-256 hex`,
      );
    }
    return entry;
  };
  if (typeof record.manifestRaw !== "string") {
    throw new Error(
      "watcher deployment marker record manifestRaw must be a string",
    );
  }
  return {
    marker: parseDeploymentMarkerV1(record.marker),
    manifestSha256: digest("manifestSha256"),
    contractDeploymentInfoSha256: digest("contractDeploymentInfoSha256"),
    manifestRaw: record.manifestRaw,
  };
};


export const decisionEffectId = (args: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly effectKind: DecisionOutboxRecord["effectKind"];
  readonly signerIndex?: number;
}): string =>
  [
    args.deploymentFingerprint,
    args.headerHash,
    args.stateQueueOutRef,
    args.effectKind,
    args.signerIndex === undefined ? "-" : args.signerIndex.toString(),
  ].join(":");

export const parseDecisionOutboxRecord = (
  value: unknown,
): DecisionOutboxRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("decision outbox record must be an object");
  }
  const record = value as Partial<DecisionOutboxRecord>;
  const allowedKeys = new Set([
    "schemaVersion",
    "effectId",
    "deploymentFingerprint",
    "sourceMode",
    "network",
    "effectKind",
    "headerHash",
    "stateQueueOutRef",
    "signerIndex",
    "slot",
    "blockHash",
    "finalized",
    "status",
    "attemptCount",
    "createdAt",
    "updatedAt",
    "lastError",
    "quarantineReason",
    "quarantinedAt",
  ]);
  const signerMatchesKind =
    record.effectKind === "signature_publish"
      ? Number.isInteger(record.signerIndex) &&
        record.signerIndex! >= 0 &&
        record.signerIndex! <= 255
      : record.signerIndex === undefined;
  if (
    Object.keys(record).some((key) => !allowedKeys.has(key)) ||
    record.schemaVersion !== 1 ||
    typeof record.effectId !== "string" ||
    typeof record.deploymentFingerprint !== "string" ||
    record.deploymentFingerprint.length === 0 ||
    (record.sourceMode !== "local_node" &&
      record.sourceMode !== "external_providers") ||
    typeof record.network !== "string" ||
    record.network.length === 0 ||
    (record.effectKind !== "signature_publish" &&
      record.effectKind !== "l1_reconcile") ||
    typeof record.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(record.headerHash) ||
    typeof record.stateQueueOutRef !== "string" ||
    !/^[0-9a-f]{64}#[0-9]+$/u.test(record.stateQueueOutRef) ||
    !signerMatchesKind ||
    (record.slot !== undefined &&
      (!Number.isSafeInteger(record.slot) || record.slot < 0)) ||
    (record.blockHash !== undefined &&
      (typeof record.blockHash !== "string" ||
        !/^[0-9a-f]{64}$/u.test(record.blockHash))) ||
    record.finalized !== true ||
    (record.status !== "pending" &&
      record.status !== "published" &&
      record.status !== "failed" &&
      record.status !== "reconciled") ||
    !Number.isSafeInteger(record.attemptCount) ||
    record.attemptCount! < 1 ||
    typeof record.createdAt !== "string" ||
    !isCanonicalIsoTimestamp(record.createdAt) ||
    typeof record.updatedAt !== "string" ||
    !isCanonicalIsoTimestamp(record.updatedAt) ||
    (record.lastError !== undefined &&
      (typeof record.lastError !== "string" ||
        record.lastError.length === 0)) ||
    (record.quarantineReason !== undefined &&
      (typeof record.quarantineReason !== "string" ||
        record.quarantineReason.length === 0)) ||
    (record.quarantinedAt !== undefined &&
      (typeof record.quarantinedAt !== "string" ||
        !isCanonicalIsoTimestamp(record.quarantinedAt)))
  ) {
    throw new Error("decision outbox record is malformed");
  }
  if (
    record.slot === undefined ||
    record.blockHash === undefined ||
    (record.quarantineReason === undefined) !==
      (record.quarantinedAt === undefined) ||
    (record.quarantineReason !== undefined && record.status !== "failed") ||
    (record.status === "failed" && record.lastError === undefined) ||
    (record.status !== "failed" && record.lastError !== undefined)
  ) {
    throw new Error(
      "decision outbox record has inconsistent finality or terminal status",
    );
  }
  const canonical: DecisionOutboxRecord = {
    schemaVersion: 1,
    effectId: record.effectId,
    deploymentFingerprint: record.deploymentFingerprint,
    sourceMode: record.sourceMode,
    network: record.network,
    effectKind: record.effectKind,
    headerHash: record.headerHash,
    stateQueueOutRef: record.stateQueueOutRef,
    ...(record.signerIndex === undefined
      ? {}
      : { signerIndex: record.signerIndex }),
    ...(record.slot === undefined ? {} : { slot: record.slot }),
    ...(record.blockHash === undefined ? {} : { blockHash: record.blockHash }),
    finalized: true,
    status: record.status,
    attemptCount: record.attemptCount!,
    createdAt: record.createdAt,
    updatedAt: record.updatedAt,
    ...(record.lastError === undefined ? {} : { lastError: record.lastError }),
    ...(record.quarantineReason === undefined
      ? {}
      : { quarantineReason: record.quarantineReason }),
    ...(record.quarantinedAt === undefined
      ? {}
      : { quarantinedAt: record.quarantinedAt }),
  };
  if (
    canonical.effectId !==
    decisionEffectId({
      deploymentFingerprint: canonical.deploymentFingerprint,
      headerHash: canonical.headerHash,
      stateQueueOutRef: canonical.stateQueueOutRef,
      effectKind: canonical.effectKind,
      ...(canonical.signerIndex === undefined
        ? {}
        : { signerIndex: canonical.signerIndex }),
    })
  ) {
    throw new Error("decision outbox effectId does not match record identity");
  }
  return canonical;
};

const assertDecisionRetry = (
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
  assertDecisionPendingLeaseExpired(existing, next);
};

export const assertDecisionPendingLeaseExpired = (
  existing: DecisionOutboxRecord,
  next: DecisionOutboxRecord,
): void => {
  if (
    existing.status === "pending" &&
    Date.parse(next.updatedAt) - Date.parse(existing.updatedAt) <
      DECISION_EFFECT_PENDING_LEASE_MS
  ) {
    throw new Error("decision outbox pending attempt lease has not expired");
  }
};

export const mergeL1SourceState = (
  current: L1SourceState | undefined,
  proposed: L1SourceState,
): L1SourceState => {
  if (current === undefined) {
    return proposed;
  }
  if (
    current.sourceMode !== proposed.sourceMode ||
    current.network !== proposed.network
  ) {
    throw new Error("watcher L1 source authority changed without a reset");
  }
  if (current.status === "quarantined") {
    if (proposed.status === "healthy") {
      throw new Error("quarantined watcher L1 source state is terminal");
    }
    return current;
  }
  if (
    proposed.status === "quarantined" &&
    current.observations.some(
      ({ hasPersistedDecision }) => hasPersistedDecision,
    )
  ) {
    throw new Error(
      "persisted L1 decisions must be quarantined atomically with their artifacts",
    );
  }
  if (proposed.status === "quarantined") {
    return proposed;
  }
  return {
    ...proposed,
    observations: mergePersistedDecisionObservations(
      current.observations,
      proposed.observations,
    ),
  };
};

const mergePersistedDecisionObservations = (
  current: readonly L1ObservedDecision[],
  proposed: readonly L1ObservedDecision[],
): readonly L1ObservedDecision[] => {
  const observations = new Map(
    proposed.map((entry) => [entry.headerHash, entry] as const),
  );
  for (const prior of current) {
    if (!prior.hasPersistedDecision) {
      continue;
    }
    const next = observations.get(prior.headerHash);
    if (next === undefined) {
      observations.set(prior.headerHash, prior);
      continue;
    }
    const expectedAttestationAdvance =
      (prior.stateQueueStatus === "unattested" ||
        prior.stateQueueStatus === "attesting") &&
      next.stateQueueStatus === "attested" &&
      prior.slot !== undefined &&
      next.slot !== undefined &&
      next.slot > prior.slot &&
      next.stateQueueOutRef !== prior.stateQueueOutRef &&
      next.finalized;
    if (
      (!expectedAttestationAdvance &&
        next.stateQueueOutRef !== prior.stateQueueOutRef) ||
      (!expectedAttestationAdvance &&
        (next.stateQueueStatus !== prior.stateQueueStatus ||
          next.slot !== prior.slot ||
          next.blockHash !== prior.blockHash))
    ) {
      throw new Error(
        "persisted L1 decision changed canonical output or chain point",
      );
    }
    observations.set(prior.headerHash, {
      ...next,
      hasPersistedDecision: true,
    });
  }
  return [...observations.values()].sort((left, right) =>
    left.headerHash.localeCompare(right.headerHash),
  );
};

export const mergeQuarantinedL1SourceState = (
  current: L1SourceState | undefined,
  proposed: L1SourceState,
): L1SourceState => {
  if (proposed.status !== "quarantined") {
    throw new Error("L1 source quarantine state is required");
  }
  if (current === undefined) {
    return proposed;
  }
  if (
    current.sourceMode !== proposed.sourceMode ||
    current.network !== proposed.network
  ) {
    throw new Error("watcher L1 source authority changed without a reset");
  }
  if (current.status === "quarantined") {
    return current;
  }
  return {
    ...proposed,
    observations: mergePersistedDecisionObservations(
      current.observations,
      proposed.observations,
    ),
  };
};

const assertDecisionSourceState = (
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

const assertDecisionSignature = (
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

export const parseL1SourceState = (value: unknown): L1SourceState => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("watcher L1 source state must be an object");
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
    throw new Error("watcher L1 source state is malformed");
  }
  if (
    state.status === "quarantined" &&
    (typeof state.quarantineReason !== "string" ||
      state.quarantineReason.length === 0 ||
      typeof state.quarantinedAt !== "string" ||
      !isCanonicalIsoTimestamp(state.quarantinedAt))
  ) {
    throw new Error("quarantined watcher L1 source state lacks evidence");
  }
  if (
    state.status === "healthy" &&
    (state.quarantineReason !== undefined || state.quarantinedAt !== undefined)
  ) {
    throw new Error(
      "healthy watcher L1 source state contains quarantine fields",
    );
  }
  const observations = state.observations.map((entry) => {
    if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
      throw new Error("watcher L1 source observation is malformed");
    }
    const record = entry as Partial<L1ObservedDecision>;
    const observationKeys = new Set([
      "headerHash",
      "stateQueueOutRef",
      "stateQueueStatus",
      "slot",
      "blockHash",
      "finalized",
      "hasPersistedDecision",
    ]);
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
        record.stateQueueStatus !== "conflicted") ||
      typeof record.finalized !== "boolean" ||
      typeof record.hasPersistedDecision !== "boolean" ||
      (record.slot !== undefined &&
        (!Number.isSafeInteger(record.slot) || record.slot < 0)) ||
      (record.blockHash !== undefined &&
        (typeof record.blockHash !== "string" ||
          !/^[0-9a-f]{64}$/u.test(record.blockHash)))
    ) {
      throw new Error("watcher L1 source observation is malformed");
    }
    return {
      headerHash: record.headerHash,
      stateQueueOutRef: record.stateQueueOutRef,
      stateQueueStatus: record.stateQueueStatus,
      ...(record.slot === undefined ? {} : { slot: record.slot }),
      ...(record.blockHash === undefined
        ? {}
        : { blockHash: record.blockHash }),
      finalized: record.finalized,
      hasPersistedDecision: record.hasPersistedDecision,
    };
  });
  observations.sort((left, right) =>
    left.headerHash.localeCompare(right.headerHash),
  );
  if (
    new Set(observations.map(({ headerHash }) => headerHash)).size !==
    observations.length
  ) {
    throw new Error("watcher L1 source observations contain duplicate headers");
  }
  return {
    schemaVersion: 1,
    sourceMode: state.sourceMode,
    network: state.network,
    authoritySha256: state.authoritySha256,
    status: state.status,
    observations,
    observedAt: state.observedAt,
    ...(state.quarantineReason === undefined
      ? {}
      : { quarantineReason: state.quarantineReason }),
    ...(state.quarantinedAt === undefined
      ? {}
      : { quarantinedAt: state.quarantinedAt }),
  };
};

const isCanonicalIsoTimestamp = (value: string): boolean => {
  const time = Date.parse(value);
  return Number.isFinite(time) && new Date(time).toISOString() === value;
};

const parseStoredRecordMap = <T>(
  value: unknown,
  parseRecord: (entry: unknown) => T,
  expectedKey: (entry: T) => string,
  label: string,
): Record<string, T> => {
  if (value === undefined) {
    return {};
  }
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return Object.fromEntries(
    Object.entries(value).map(([key, rawEntry]) => {
      const entry = parseRecord(rawEntry);
      if (key !== expectedKey(entry)) {
        throw new Error(`${label} key ${key} does not match record identity`);
      }
      return [key, entry];
    }),
  );
};

const isNodeError = (error: unknown): error is NodeJS.ErrnoException =>
  error instanceof Error && "code" in error;

export const jsonReplacer = (_key: string, value: unknown): unknown =>
  typeof value === "bigint"
    ? { __midgardWatcherType: "bigint", value: value.toString() }
    : value;

export const jsonReviver = (_key: string, value: unknown): unknown => {
  if (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value) &&
    (value as { __midgardWatcherType?: unknown }).__midgardWatcherType ===
      "bigint"
  ) {
    const record = value as Record<string, unknown>;
    if (
      Object.keys(record).length !== 2 ||
      !Object.hasOwn(record, "__midgardWatcherType") ||
      !Object.hasOwn(record, "value") ||
      typeof record.value !== "string" ||
      !/^(?:0|-?[1-9][0-9]*)$/.test(record.value)
    ) {
      throw new Error("invalid canonical watcher bigint encoding");
    }
    return BigInt(record.value);
  }
  return value;
};
