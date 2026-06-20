import type { AttestationCoordinator } from "../coordinator/coordinator.js";
import type { DaPeerConfig, DaSignatureRecord } from "../domain.js";
import { jsonBigIntStringReplacer } from "../json.js";
import type { DaSigner, DaSignerValidation } from "../signer.js";
import type { WatcherStore } from "../store.js";
import { signPeerRequest } from "./auth.js";
import {
  PeerSignaturePoller,
  recordPeerFailure,
  recordPeerSuccess,
} from "./poller.js";

export type PeerSignatureCoordinatorDeps = {
  readonly deploymentFingerprint: string;
  readonly peers: readonly DaPeerConfig[];
  readonly signer: DaSigner;
  readonly signerIndex: number;
  readonly signerValidation: DaSignerValidation;
  readonly store: WatcherStore;
  readonly requestTimeoutMs: number;
  readonly retryInitialDelayMs: number;
  readonly retryMaxDelayMs: number;
  readonly retryMaxAttempts: number;
  readonly onChainCoordinator?: AttestationCoordinator;
};

export class PeerSignatureCoordinator implements AttestationCoordinator {
  readonly retryPublishedSignatures = true;
  readonly retryPublishedSignaturesForAttestedHeaders = true;

  private readonly deps: PeerSignatureCoordinatorDeps;
  private readonly poller: PeerSignaturePoller;
  private readonly lastErrors = new Map<string, string>();

  constructor(deps: PeerSignatureCoordinatorDeps) {
    this.deps = deps;
    this.poller = new PeerSignaturePoller({
      deploymentFingerprint: deps.deploymentFingerprint,
      peers: deps.peers,
      signerValidation: deps.signerValidation,
      store: deps.store,
      requestTimeoutMs: deps.requestTimeoutMs,
    });
  }

  async publishSignature(
    record: DaSignatureRecord,
  ): Promise<"posted" | "post_failed"> {
    await this.pollPeerSignatures(record.headerHash);
    const peerResults = await Promise.all(
      this.deps.peers.map((peer) => this.broadcastSignature(peer, record)),
    );
    await this.pollPeerSignatures(record.headerHash);
    const shouldRunOnChain = await this.shouldRunOnChainLifecycle(record);
    const l1Result =
      this.deps.onChainCoordinator === undefined || !shouldRunOnChain
        ? "posted"
        : await this.deps.onChainCoordinator.publishSignature(record);
    if (l1Result === "posted" && this.deps.onChainCoordinator !== undefined) {
      this.lastErrors.delete(record.headerHash);
      return "posted";
    }
    if (peerResults.every(Boolean) && l1Result === "posted") {
      this.lastErrors.delete(record.headerHash);
      return "posted";
    }
    const l1Error = this.deps.onChainCoordinator?.lastPublishError?.(record);
    this.lastErrors.set(
      record.headerHash,
      l1Error ?? "one or more peer broadcasts failed",
    );
    return "post_failed";
  }

  private async shouldRunOnChainLifecycle(
    record: DaSignatureRecord,
  ): Promise<boolean> {
    const stateQueueHeader = await this.deps.store.getStateQueueHeader(
      record.headerHash,
    );
    return (
      stateQueueHeader === undefined ||
      stateQueueHeader.status === "unattested" ||
      stateQueueHeader.status === "attesting"
    );
  }

  lastPublishError(
    record: Pick<DaSignatureRecord, "headerHash">,
  ): string | undefined {
    return this.lastErrors.get(record.headerHash);
  }

  async pollPeerSignatures(headerHash: string): Promise<void> {
    await this.poller.pollPeerSignatures(headerHash);
  }

  private async broadcastSignature(
    peer: DaPeerConfig,
    record: DaSignatureRecord,
  ): Promise<boolean> {
    const existing = await this.deps.store.getPeerBroadcast({
      peerBaseUrl: peer.baseUrl,
      headerHash: record.headerHash,
      signerIndex: record.signerIndex,
    });
    if (existing?.status === "posted") {
      return true;
    }
    const now = Date.now();
    if (
      existing?.nextAttemptAt !== undefined &&
      Date.parse(existing.nextAttemptAt) > now
    ) {
      return false;
    }
    const attempts = (existing?.attempts ?? 0) + 1;
    if (attempts > this.deps.retryMaxAttempts) {
      await this.deps.store.savePeerBroadcast({
        deploymentFingerprint: record.deploymentFingerprint,
        peerBaseUrl: peer.baseUrl,
        headerHash: record.headerHash,
        signerIndex: record.signerIndex,
        status: "failed",
        attempts: existing?.attempts ?? this.deps.retryMaxAttempts,
        lastError: "peer retry budget exhausted",
        updatedAt: new Date().toISOString(),
      });
      return false;
    }
    await this.deps.store.savePeerBroadcast({
      deploymentFingerprint: record.deploymentFingerprint,
      peerBaseUrl: peer.baseUrl,
      headerHash: record.headerHash,
      signerIndex: record.signerIndex,
      status: "pending",
      attempts,
      lastAttemptAt: new Date(now).toISOString(),
      updatedAt: new Date(now).toISOString(),
    });
    const pathAndSearch = `/v1/deployments/${encodeURIComponent(
      this.deps.deploymentFingerprint,
    )}/headers/${record.headerHash}/signatures`;
    const url = `${peer.baseUrl}${pathAndSearch}`;
    const body = Buffer.from(
      JSON.stringify(record, jsonBigIntStringReplacer),
      "utf8",
    );
    const headers = {
      "content-type": "application/json",
      ...signPeerRequest({
        signer: this.deps.signer,
        signerIndex: this.deps.signerIndex,
        deploymentFingerprint: this.deps.deploymentFingerprint,
        method: "POST",
        pathAndSearch,
        body,
      }),
    };
    try {
      const response = await fetch(url, {
        method: "POST",
        headers,
        body,
        signal: AbortSignal.timeout(this.deps.requestTimeoutMs),
      });
      if (!response.ok) {
        throw new Error(
          `HTTP ${response.status.toString()}: ${await response.text()}`,
        );
      }
      await this.deps.store.savePeerBroadcast({
        deploymentFingerprint: record.deploymentFingerprint,
        peerBaseUrl: peer.baseUrl,
        headerHash: record.headerHash,
        signerIndex: record.signerIndex,
        status: "posted",
        attempts,
        lastAttemptAt: new Date(now).toISOString(),
        lastSuccessAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      });
      await this.recordPeerSuccess(peer);
      return true;
    } catch (error) {
      const lastError = error instanceof Error ? error.message : String(error);
      await this.deps.store.savePeerBroadcast({
        deploymentFingerprint: record.deploymentFingerprint,
        peerBaseUrl: peer.baseUrl,
        headerHash: record.headerHash,
        signerIndex: record.signerIndex,
        status: "failed",
        attempts,
        nextAttemptAt: new Date(
          now + this.retryDelayMs(attempts),
        ).toISOString(),
        lastAttemptAt: new Date(now).toISOString(),
        lastError,
        updatedAt: new Date().toISOString(),
      });
      await this.recordPeerFailure(peer, lastError);
      return false;
    }
  }

  private retryDelayMs(attempts: number): number {
    const exponential = Math.min(
      this.deps.retryInitialDelayMs * 2 ** Math.max(0, attempts - 1),
      this.deps.retryMaxDelayMs,
    );
    const jitter = Math.floor(Math.random() * Math.min(1_000, exponential));
    return exponential + jitter;
  }

  private async recordPeerSuccess(peer: DaPeerConfig): Promise<void> {
    await recordPeerSuccess(this.deps.store, peer);
  }

  private async recordPeerFailure(
    peer: DaPeerConfig,
    lastError: string,
  ): Promise<void> {
    await recordPeerFailure(this.deps.store, peer, lastError);
  }
}
