import type { Libp2pDaTransportLimits } from "../../config.js";
import { DaLibp2pNode } from "./DaLibp2pNode.js";
import type { DaPeerRegistry, DaPeerRegistryEntry } from "./DaPeerRegistry.js";

export type DaLibp2pPayloadSourceOptions = {
  readonly deploymentFingerprint: string;
  readonly node: DaLibp2pNode;
  readonly registry: DaPeerRegistry;
  readonly limits: Libp2pDaTransportLimits;
  readonly peers?: readonly DaPeerRegistryEntry[];
};

export const dedupePeers = (
  peers: readonly DaPeerRegistryEntry[],
): readonly DaPeerRegistryEntry[] => [
  ...new Map(peers.map((peer) => [peer.peerId, peer])).values(),
];
