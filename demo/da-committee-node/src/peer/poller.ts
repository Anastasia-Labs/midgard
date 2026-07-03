import type {
  DaAttestationExchange,
  DaAttestationPeer,
} from "../da/libp2p/attestations.js";
import type { DaSignatureRecord } from "../domain.js";
import type { DaCommitteeValidation } from "../signer.js";
import type { WatcherStore } from "../store.js";
import { validateDaSignatureRecord } from "./signatures.js";
import { attestationPeersExcludingLocal } from "./targets.js";

export type PeerSignaturePollerDeps = {
  readonly deploymentFingerprint: string;
  readonly peers: readonly DaAttestationPeer[];
  readonly localPeerId?: string;
  readonly attestationExchange?: DaAttestationExchange;
  readonly signerValidation: DaCommitteeValidation;
  readonly store: Pick<
    WatcherStore,
    "getDaPayload" | "saveDaSignature" | "savePeerHealth" | "listPeerHealth"
  >;
  readonly requestTimeoutMs?: number;
};

export class PeerSignaturePoller {
  private readonly deps: PeerSignaturePollerDeps;

  constructor(deps: PeerSignaturePollerDeps) {
    this.deps = {
      ...deps,
      peers:
        deps.localPeerId === undefined
          ? deps.peers
          : attestationPeersExcludingLocal(deps.peers, deps.localPeerId),
    };
  }

  async pollPeerSignatures(headerHash: string): Promise<void> {
    await Promise.all(
      this.deps.peers.map((peer) =>
        this.pollPeerSignaturesFrom(peer, headerHash),
      ),
    );
  }

  private async pollPeerSignaturesFrom(
    peer: DaAttestationPeer,
    headerHash: string,
  ): Promise<void> {
    try {
      if (this.deps.attestationExchange === undefined) {
        throw new Error("libp2p attestation exchange is not configured");
      }
      const signatures =
        await this.deps.attestationExchange.attestationsByHeader({
          peer,
          deploymentFingerprint: this.deps.deploymentFingerprint,
          headerHash,
        });
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
          sourcePeer: peer.peerId,
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
  peer: DaAttestationPeer,
): Promise<void> => {
  await store.savePeerHealth({
    peerId: peer.peerId,
    signerIndex: peer.signerIndex,
    lastSuccessAt: new Date().toISOString(),
    consecutiveFailures: 0,
    updatedAt: new Date().toISOString(),
  });
};

export const recordPeerFailure = async (
  store: Pick<WatcherStore, "listPeerHealth" | "savePeerHealth">,
  peer: DaAttestationPeer,
  lastError: string,
): Promise<void> => {
  const existing = (await store.listPeerHealth()).find(
    (entry) => entry.peerId === peer.peerId,
  );
  await store.savePeerHealth({
    peerId: peer.peerId,
    signerIndex: peer.signerIndex,
    lastFailureAt: new Date().toISOString(),
    lastError,
    consecutiveFailures: (existing?.consecutiveFailures ?? 0) + 1,
    updatedAt: new Date().toISOString(),
  });
};
