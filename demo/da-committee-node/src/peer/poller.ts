import type {
  DaPeerConfig,
  DaSignatureRecord,
} from "../domain.js";
import type { DaCommitteeValidation } from "../signer.js";
import type { WatcherStore } from "../store.js";
import { validateDaSignatureRecord } from "./signatures.js";

export type PeerSignaturePollerDeps = {
  readonly deploymentFingerprint: string;
  readonly peers: readonly DaPeerConfig[];
  readonly signerValidation: DaCommitteeValidation;
  readonly store: Pick<
    WatcherStore,
    | "getDaPayload"
    | "saveDaSignature"
    | "savePeerHealth"
    | "listPeerHealth"
  >;
  readonly requestTimeoutMs: number;
  readonly fetchFn?: typeof fetch;
};

export class PeerSignaturePoller {
  private readonly deps: PeerSignaturePollerDeps & {
    readonly fetchFn: typeof fetch;
  };

  constructor(deps: PeerSignaturePollerDeps) {
    this.deps = { ...deps, fetchFn: deps.fetchFn ?? fetch };
  }

  async pollPeerSignatures(headerHash: string): Promise<void> {
    await Promise.all(
      this.deps.peers.map((peer) => this.pollPeerSignaturesFrom(peer, headerHash)),
    );
  }

  private async pollPeerSignaturesFrom(
    peer: DaPeerConfig,
    headerHash: string,
  ): Promise<void> {
    const pathAndSearch = `/v1/deployments/${encodeURIComponent(
      this.deps.deploymentFingerprint,
    )}/headers/${headerHash}/signatures`;
    try {
      const response = await this.deps.fetchFn(
        `${peer.baseUrl}${pathAndSearch}`,
        {
          method: "GET",
          signal: AbortSignal.timeout(this.deps.requestTimeoutMs),
        },
      );
      if (!response.ok) {
        throw new Error(`HTTP ${response.status.toString()}`);
      }
      const body = (await response.json()) as unknown;
      const signatures = Array.isArray(body)
        ? body
        : typeof body === "object" &&
            body !== null &&
            Array.isArray((body as { signatures?: unknown }).signatures)
          ? (body as { signatures: unknown[] }).signatures
          : [];
      const verifiedPayload = await this.deps.store.getDaPayload(headerHash);
      if (verifiedPayload === undefined) {
        return;
      }
      for (const signature of signatures) {
        if (typeof signature !== "object" || signature === null) {
          continue;
        }
        const candidate = signature as Partial<DaSignatureRecord>;
        const validationError = validateDaSignatureRecord({
          body: candidate,
          headerHash,
          deploymentFingerprint: this.deps.deploymentFingerprint,
          signerValidation: this.deps.signerValidation,
          verifiedPayload,
        });
        if (validationError !== undefined) {
          continue;
        }
        const now = new Date().toISOString();
        await this.deps.store.saveDaSignature({
          ...(candidate as DaSignatureRecord),
          broadcastStatus: "posted",
          source: "peer",
          sourcePeer: peer.baseUrl,
          receivedAt: now,
          verifiedAt: now,
        });
      }
      await recordPeerSuccess(this.deps.store, peer);
    } catch (error) {
      await recordPeerFailure(
        this.deps.store,
        peer,
        error instanceof Error ? error.message : String(error),
      );
    }
  }
}

export const recordPeerSuccess = async (
  store: Pick<WatcherStore, "savePeerHealth">,
  peer: DaPeerConfig,
): Promise<void> => {
  await store.savePeerHealth({
    peerBaseUrl: peer.baseUrl,
    signerIndex: peer.signerIndex,
    lastSuccessAt: new Date().toISOString(),
    consecutiveFailures: 0,
    updatedAt: new Date().toISOString(),
  });
};

export const recordPeerFailure = async (
  store: Pick<WatcherStore, "listPeerHealth" | "savePeerHealth">,
  peer: DaPeerConfig,
  lastError: string,
): Promise<void> => {
  const existing = (await store.listPeerHealth()).find(
    (entry) => entry.peerBaseUrl === peer.baseUrl,
  );
  await store.savePeerHealth({
    peerBaseUrl: peer.baseUrl,
    signerIndex: peer.signerIndex,
    lastFailureAt: new Date().toISOString(),
    lastError,
    consecutiveFailures: (existing?.consecutiveFailures ?? 0) + 1,
    updatedAt: new Date().toISOString(),
  });
};
