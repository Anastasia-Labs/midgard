import type { Multiaddr } from "@multiformats/multiaddr";

import type {
  Libp2pDaPeerConfig,
  Libp2pDaRole,
  Libp2pDaTransportConfig,
} from "../../config.js";

export type DaPeerRegistryEntry = {
  readonly peerId: string;
  readonly signerIndex?: number;
  readonly daVkey?: string;
  readonly roles: readonly Libp2pDaRole[];
  readonly multiaddrs: readonly string[];
  readonly bootstrap: boolean;
};

export class DaPeerRegistry {
  private readonly byPeerId = new Map<string, DaPeerRegistryEntry>();
  private readonly bySignerIndex = new Map<number, DaPeerRegistryEntry>();
  private readonly knownMultiaddrs = new Set<string>();

  constructor(entries: readonly DaPeerRegistryEntry[]) {
    for (const entry of entries) {
      if (this.byPeerId.has(entry.peerId)) {
        throw new Error(`duplicate DA libp2p peer id ${entry.peerId}`);
      }
      this.byPeerId.set(entry.peerId, entry);
      if (entry.signerIndex !== undefined) {
        if (this.bySignerIndex.has(entry.signerIndex)) {
          throw new Error(
            `duplicate DA libp2p signer index ${entry.signerIndex.toString()}`,
          );
        }
        this.bySignerIndex.set(entry.signerIndex, entry);
      }
      for (const addr of entry.multiaddrs) {
        this.knownMultiaddrs.add(addr);
      }
    }
  }

  static fromConfig(config: Libp2pDaTransportConfig): DaPeerRegistry {
    const committeeEntries = config.peers.map(peerConfigToEntry);
    const entriesByPeerId = new Map<string, DaPeerRegistryEntry>(
      committeeEntries.map((entry) => [entry.peerId, entry]),
    );
    for (const addr of config.bootstrapMultiaddrs) {
      const peerId = peerIdFromMultiaddrString(addr);
      if (peerId === undefined) {
        throw new Error(`bootstrap multiaddr must include a peer id: ${addr}`);
      }
      const existing = entriesByPeerId.get(peerId);
      entriesByPeerId.set(
        peerId,
        existing === undefined
          ? {
              peerId,
              roles: [],
              multiaddrs: [addr],
              bootstrap: true,
            }
          : {
              ...existing,
              multiaddrs: Object.freeze([...existing.multiaddrs, addr]),
              bootstrap: true,
            },
      );
    }
    return new DaPeerRegistry([...entriesByPeerId.values()]);
  }

  get size(): number {
    return this.byPeerId.size;
  }

  entries(): readonly DaPeerRegistryEntry[] {
    return [...this.byPeerId.values()];
  }

  getByPeerId(
    peerId: string | { toString(): string },
  ): DaPeerRegistryEntry | undefined {
    return this.byPeerId.get(peerId.toString());
  }

  getBySignerIndex(signerIndex: number): DaPeerRegistryEntry | undefined {
    return this.bySignerIndex.get(signerIndex);
  }

  peersForRole(role: Libp2pDaRole): readonly DaPeerRegistryEntry[] {
    return this.entries().filter((entry) => entry.roles.includes(role));
  }

  isKnownPeerId(peerId: string | { toString(): string }): boolean {
    return this.getByPeerId(peerId) !== undefined;
  }

  isKnownMultiaddr(addr: string | Multiaddr): boolean {
    return this.knownMultiaddrs.has(addr.toString());
  }

  isKnownPeerMultiaddr(
    peerId: string | { toString(): string },
    addr: string | Multiaddr,
  ): boolean {
    const expectedPeerId = peerId.toString();
    if (!this.isKnownPeerId(expectedPeerId)) {
      return false;
    }
    const addrPeerId =
      typeof addr === "string"
        ? peerIdFromMultiaddrString(addr)
        : peerIdFromMultiaddr(addr);
    return (
      addrPeerId === expectedPeerId ||
      (addrPeerId === undefined && this.isKnownMultiaddr(addr))
    );
  }

  requireKnownPeer(
    peerId: string | { toString(): string },
  ): DaPeerRegistryEntry {
    const entry = this.getByPeerId(peerId);
    if (entry === undefined) {
      throw new Error(`unknown DA libp2p peer ${peerId.toString()}`);
    }
    return entry;
  }
}

export const peerIdFromMultiaddr = (addr: Multiaddr): string | undefined =>
  lastDefined(
    addr
      .getComponents()
      .filter((component) => component.name === "p2p")
      .map((component) => component.value),
  );

export const peerIdFromMultiaddrString = (addr: string): string | undefined => {
  const parts = addr.trim().split("/");
  const p2pIndex = parts.lastIndexOf("p2p");
  const peerId = p2pIndex === -1 ? undefined : parts[p2pIndex + 1];
  return peerId === undefined || peerId.length === 0 ? undefined : peerId;
};

const peerConfigToEntry = (peer: Libp2pDaPeerConfig): DaPeerRegistryEntry => ({
  peerId: peer.peerId,
  signerIndex: peer.signerIndex,
  daVkey: peer.daVkey,
  roles: peer.roles,
  multiaddrs: peer.multiaddrs,
  bootstrap: false,
});

const lastDefined = <T>(values: readonly (T | undefined)[]): T | undefined => {
  for (let index = values.length - 1; index >= 0; index -= 1) {
    const value = values[index];
    if (value !== undefined) {
      return value;
    }
  }
  return undefined;
};
