import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import type {
  DaAttestationCandidateRecord,
  DaPayloadRecord,
  DaPeerBroadcastRecord,
  DaPeerHealthRecord,
  DaPeerNonceRecord,
  DaSignatureRecord,
  L1SubmissionRecord,
  StateQueueHeaderRecord,
} from "./domain.js";

type StoreData = {
  readonly deployment?: {
    readonly fingerprint: string;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256?: string;
    readonly manifestRaw: string;
  };
  readonly chainCursor?: Record<string, unknown>;
  readonly stateQueueHeaders: Record<string, StateQueueHeaderRecord>;
  readonly daPayloads: Record<string, DaPayloadRecord>;
  readonly daSignatures: Record<string, DaSignatureRecord>;
  readonly daAttestationCandidates: Record<
    string,
    DaAttestationCandidateRecord
  >;
  readonly l1Submissions: Record<string, L1SubmissionRecord>;
  readonly peerBroadcasts: Record<string, DaPeerBroadcastRecord>;
  readonly peerHealth: Record<string, DaPeerHealthRecord>;
  readonly peerNonces: Record<string, DaPeerNonceRecord>;
};

export type WatcherDeploymentRecord = {
  readonly fingerprint: string;
  readonly manifestSha256: string;
  readonly contractDeploymentInfoSha256?: string;
  readonly manifestRaw: string;
};

export interface WatcherStore {
  close?(): Promise<void>;
  initDeployment(args: {
    readonly fingerprint: string;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void>;
  getDeployment(): Promise<WatcherDeploymentRecord | undefined>;
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
  private writeQueue: Promise<void> = Promise.resolve();

  private constructor(filePath: string) {
    this.filePath = filePath;
  }

  static async open(path: string): Promise<JsonFileWatcherStore> {
    const filePath = path.endsWith(".json") ? path : join(path, "watcher.json");
    await mkdir(dirname(filePath), { recursive: true });
    const store = new JsonFileWatcherStore(filePath);
    await store.read();
    return store;
  }

  async initDeployment(args: {
    readonly fingerprint: string;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void> {
    await this.mutate((data) => {
      if (
        data.deployment !== undefined &&
        data.deployment.fingerprint !== args.fingerprint
      ) {
        throw new Error(
          `stale_deployment_state_requires_fresh_redeploy: stored_fingerprint=${data.deployment.fingerprint}, canonical_manifest_id=${args.fingerprint}, contract_deployment_info_sha256=${args.contractDeploymentInfoSha256}; refusing to reuse stale watcher state; perform an explicit fresh redeploy/reset before deleting local watcher state.`,
        );
      }
      return {
        ...data,
        deployment: {
          fingerprint: args.fingerprint,
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

  async saveDaPayload(record: DaPayloadRecord): Promise<DaPayloadRecord> {
    let saved = record;
    await this.mutate((data) => {
      const existing = data.daPayloads[record.headerHash];
      saved = resolveDaPayloadSave(existing, record);
      return {
        ...data,
        daPayloads: {
          ...data.daPayloads,
          [record.headerHash]: saved,
        },
      };
    });
    return saved;
  }

  async getDaPayload(headerHash: string): Promise<DaPayloadRecord | undefined> {
    const data = await this.read();
    return data.daPayloads[headerHash];
  }

  async saveDaSignature(record: DaSignatureRecord): Promise<void> {
    await this.mutate((data) => ({
      ...data,
      daSignatures: {
        ...data.daSignatures,
        [signatureKey(record.headerHash, record.signerIndex)]: record,
      },
    }));
  }

  async getDaSignature(args: {
    readonly headerHash: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecord | undefined> {
    const data = await this.read();
    return data.daSignatures[signatureKey(args.headerHash, args.signerIndex)];
  }

  async listDaSignatures(
    headerHash?: string,
  ): Promise<readonly DaSignatureRecord[]> {
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
    this.writeQueue = this.writeQueue.then(async () => {
      const data = await this.read();
      await this.write(update(data));
    });
    await this.writeQueue;
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
    const tmpPath = `${this.filePath}.${process.pid.toString()}.tmp`;
    await writeFile(tmpPath, `${JSON.stringify(data, jsonReplacer, 2)}\n`);
    await rename(tmpPath, this.filePath);
  }
}

const signatureKey = (headerHash: string, signerIndex: number): string =>
  `${headerHash}:${signerIndex.toString()}`;

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
  existing: DaPayloadRecord | undefined,
  record: DaPayloadRecord,
): DaPayloadRecord => {
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
}): DaPayloadRecord => ({
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
  record: DaPayloadRecord,
): DaPayloadRecord => {
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
  daAttestationCandidates: {},
  l1Submissions: {},
  peerBroadcasts: {},
  peerHealth: {},
  peerNonces: {},
});

const normalizeStoreData = (value: unknown): StoreData => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return emptyStoreData();
  }
  const record = value as Partial<StoreData>;
  return {
    deployment: record.deployment,
    chainCursor: record.chainCursor,
    stateQueueHeaders: record.stateQueueHeaders ?? {},
    daPayloads: record.daPayloads ?? {},
    daSignatures: record.daSignatures ?? {},
    daAttestationCandidates: record.daAttestationCandidates ?? {},
    l1Submissions: record.l1Submissions ?? {},
    peerBroadcasts: record.peerBroadcasts ?? {},
    peerHealth: record.peerHealth ?? {},
    peerNonces: record.peerNonces ?? {},
  };
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
      "bigint" &&
    typeof (value as { value?: unknown }).value === "string"
  ) {
    return BigInt((value as { value: string }).value);
  }
  return value;
};
