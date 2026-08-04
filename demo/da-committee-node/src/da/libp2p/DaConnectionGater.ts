import type { Libp2pOptions } from "libp2p";

import { type DaPeerRegistry, peerIdFromMultiaddr } from "./DaPeerRegistry.js";

export type DaConnectionGater = NonNullable<Libp2pOptions["connectionGater"]>;

export const createDaConnectionGater = (
  registry: DaPeerRegistry,
): DaConnectionGater => ({
  denyDialPeer(peerId): boolean {
    return !registry.isKnownPeerId(peerId);
  },

  denyDialMultiaddr(addr): boolean {
    return !registry.isKnownMultiaddr(addr);
  },

  denyOutboundConnection(peerId): boolean {
    return !registry.isKnownPeerId(peerId);
  },

  denyInboundEncryptedConnection(peerId): boolean {
    return !registry.isKnownPeerId(peerId);
  },

  denyOutboundEncryptedConnection(peerId): boolean {
    return !registry.isKnownPeerId(peerId);
  },

  denyInboundUpgradedConnection(peerId): boolean {
    return !registry.isKnownPeerId(peerId);
  },

  denyOutboundUpgradedConnection(peerId): boolean {
    return !registry.isKnownPeerId(peerId);
  },

  denyInboundRelayReservation(peerId): boolean {
    return !registry.isKnownPeerId(peerId);
  },

  denyOutboundRelayedConnection(source, destination): boolean {
    return (
      !registry.isKnownPeerId(source) || !registry.isKnownPeerId(destination)
    );
  },

  denyInboundRelayedConnection(relay, remotePeer): boolean {
    return (
      !registry.isKnownPeerId(relay) || !registry.isKnownPeerId(remotePeer)
    );
  },

  filterMultiaddrForPeer(peerId, addr): boolean {
    const addrPeerId = peerIdFromMultiaddr(addr);
    return (
      addrPeerId !== undefined &&
      addrPeerId === peerId.toString() &&
      registry.isKnownPeerMultiaddr(peerId, addr)
    );
  },
});
