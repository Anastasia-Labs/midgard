import type {
  DaAttestationExchange,
  DaAttestationPeer,
} from "../da/libp2p/attestations.js";
import type { DaSignatureRecord } from "../domain.js";
import type { DaCommitteeValidation } from "../signer.js";
import type { WatcherStore } from "../store.js";
import {
  buildDaSignatureConflictEvidenceV1,
  type DaAvailabilityCommitmentAuthorityV1,
  deriveExpectedDaAvailabilityCommitmentV1,
  validateDaSignatureRecord,
} from "./signatures.js";
import { attestationPeersExcludingLocal } from "./targets.js";

export type PeerSignaturePollerDeps = {
  readonly deploymentFingerprint: string;
  readonly peers: readonly DaAttestationPeer[];
  readonly localPeerId?: string;
  readonly attestationExchange?: DaAttestationExchange;
  readonly signerValidation: DaCommitteeValidation;
  readonly availabilityCommitmentAuthority: DaAvailabilityCommitmentAuthorityV1;
  readonly store: Pick<
    WatcherStore,
    | "getDaPayload"
    | "saveDaSignature"
    | "listDaSignatures"
    | "saveDaConflictEvidence"
    | "savePeerHealth"
    | "listPeerHealth"
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
      const expectedCommitment = deriveExpectedDaAvailabilityCommitmentV1({
        authority: this.deps.availabilityCommitmentAuthority,
        headerHash,
        payloadCborHex: verifiedPayload.payloadCborHex,
      });
      for (const signature of signatures) {
        if (typeof signature !== "object" || signature === null) {
          continue;
        }
        const candidate = signature as Partial<DaSignatureRecord>;
        const cryptographicValidationError = validateDaSignatureRecord({
          body: candidate,
          headerHash,
          deploymentFingerprint: this.deps.deploymentFingerprint,
          signerValidation: this.deps.signerValidation,
        });
        if (cryptographicValidationError !== undefined) {
          continue;
        }
        const now = new Date().toISOString();
        const canonicalCandidate: DaSignatureRecord = {
          ...(candidate as DaSignatureRecord),
          broadcastStatus: "posted",
          source: "peer",
          sourcePeer: peer.peerId,
          receivedAt: now,
          verifiedAt: now,
        };
        const priorSameHeaderSigner = (
          await this.deps.store.listDaSignatures(headerHash)
        ).find(
          (entry) =>
            entry.signerIndex === canonicalCandidate.signerIndex &&
            entry.availabilityCommitmentDigest !==
              canonicalCandidate.availabilityCommitmentDigest,
        );
        await this.deps.store.saveDaSignature(canonicalCandidate);
        if (priorSameHeaderSigner !== undefined) {
          const conflict = buildDaSignatureConflictEvidenceV1({
            first: priorSameHeaderSigner,
            second: canonicalCandidate,
            daVkey:
              this.deps.signerValidation.committeeKeys[
                canonicalCandidate.signerIndex
              ]!,
            reporterPeerId: this.deps.localPeerId ?? "local-da-committee",
            receivedAt: now,
          });
          if (
            conflict !== undefined &&
            (await this.deps.store.saveDaConflictEvidence(conflict.record))
          ) {
            await this.deps.attestationExchange.publishConflictEvidence(
              conflict.gossipCbor,
            );
          }
        }
        const authorityValidationError = validateDaSignatureRecord({
          body: canonicalCandidate,
          headerHash,
          deploymentFingerprint: this.deps.deploymentFingerprint,
          signerValidation: this.deps.signerValidation,
          verifiedPayload,
          expectedAvailabilityCommitmentCbor: expectedCommitment.commitmentCbor,
          expectedAvailabilityCommitmentDigest:
            expectedCommitment.commitmentDigest,
        });
        if (authorityValidationError !== undefined) {
          continue;
        }
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
