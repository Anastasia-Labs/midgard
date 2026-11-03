import { type DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";

import { type WatcherConfig } from "../runtime/config.js";
import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";

/** What one public DA libp2p request-response exchange carries. */
export type WatcherPublicDaRequest = Readonly<{
  peerIdentity: string;
  peerId: string;
  multiaddr: string;
  protocol: DaRequestResponseProtocol;
  protocolId: string;
  requestCbor: Buffer;
  timeoutMs: number;
  signal: AbortSignal;
  /**
   * The admitted watcher config and its verified identity: the only
   * authority for dialing an ip4 peer, never a boolean bypass.
   */
  admission?: Readonly<{
    watcherConfig: WatcherConfig;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  }>;
}>;

/**
 * One public DA request-response exchange. Implementations dial the supplied
 * public libp2p multiaddress and deployment-scoped protocol ID.
 */
export interface WatcherPublicDaLibp2pTransportV1 {
  request(request: WatcherPublicDaRequest): Promise<Uint8Array>;
}
