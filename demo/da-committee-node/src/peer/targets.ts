import type { Libp2pDaPeerConfig } from "../config.js";
import type { DaAttestationPeer } from "../da/libp2p/attestations.js";

export type DaAttestationTargetPeer = Pick<
  Libp2pDaPeerConfig,
  "peerId" | "signerIndex" | "roles"
>;

export type RemoteDaAttestationTargets = {
  readonly localPeer: DaAttestationTargetPeer;
  readonly remotePeers: readonly DaAttestationPeer[];
};

export const resolveRemoteDaAttestationTargets = ({
  peers,
  localPeerId,
  signerIndex,
}: {
  readonly peers: readonly DaAttestationTargetPeer[];
  readonly localPeerId: string;
  readonly signerIndex?: number;
}): RemoteDaAttestationTargets => {
  const localPeer = peers.find((peer) => peer.peerId === localPeerId);
  if (localPeer === undefined) {
    throw new Error(
      `local libp2p peer ${localPeerId} is not present in the DA transport manifest`,
    );
  }
  if (signerIndex !== undefined) {
    if (!localPeer.roles.includes("committee")) {
      throw new Error(
        `DA_SIGNER_INDEX is configured but local libp2p peer ${localPeerId} is not a committee peer`,
      );
    }
    if (localPeer.signerIndex !== signerIndex) {
      throw new Error(
        `DA_SIGNER_INDEX ${signerIndex.toString()} does not match manifest signerIndex ${localPeer.signerIndex.toString()} for local libp2p peer ${localPeerId}`,
      );
    }
  }
  const committeePeers = peers.filter((peer) =>
    peer.roles.includes("committee"),
  );
  return {
    localPeer,
    remotePeers: attestationPeersExcludingLocal(committeePeers, localPeerId),
  };
};

export const attestationPeersExcludingLocal = (
  peers: readonly DaAttestationPeer[],
  localPeerId: string,
): readonly DaAttestationPeer[] =>
  peers
    .filter((peer) => peer.peerId !== localPeerId)
    .map((peer) => ({
      peerId: peer.peerId,
      ...(peer.signerIndex === undefined
        ? {}
        : { signerIndex: peer.signerIndex }),
    }));
