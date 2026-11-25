/**
 * The configured snapshot peers (`LEDGER_SNAPSHOT_PEERS`), dialed from the
 * node's libp2p host for DA requests (`getPublicationTransport`) under the
 * deployment's DA manifest: its fingerprint names the `ledger-snapshot`
 * protocol and its request timeout bounds each page.
 */
import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { Effect } from "effect";

import { loadDaProducerPublicationManifestFromEnv } from "../da/libp2p-producer.parse-da-producer-publication-manifest.js";
import { getPublicationTransport } from "../da/libp2p-producer.publication-transport.js";
import type { NodeConfigDep } from "./config.js";
import type { LedgerSnapshotPeerSource } from "./ledger-snapshot-start.js";

/** The peers that served a wrong ledger, for the process's lifetime. */
const excluded = new Set<string>();

export const configuredLedgerSnapshotPeers = (
  nodeConfig: Pick<NodeConfigDep, "LEDGER_SNAPSHOT_PEERS">,
): Effect.Effect<LedgerSnapshotPeerSource, unknown> =>
  Effect.tryPromise({
    try: async () => {
      const configured = nodeConfig.LEDGER_SNAPSHOT_PEERS;
      if (configured.length === 0)
        return { deploymentFingerprint: "", peers: [], excluded };
      const manifest = await loadDaProducerPublicationManifestFromEnv();
      const transport = await getPublicationTransport(manifest);
      const protocolId = daRequestResponseProtocolId(
        manifest.deploymentFingerprint,
        DaRequestResponseProtocol.ledgerSnapshot,
      );
      return {
        deploymentFingerprint: manifest.deploymentFingerprint,
        peers: configured.map((peer) => ({
          identity: peer.peerId,
          request: (requestCbor: Buffer) =>
            transport.request(
              { peerId: peer.peerId, multiaddrs: [peer.multiaddr] },
              protocolId,
              requestCbor,
              manifest.requestTimeoutMs,
            ),
        })),
        excluded,
      };
    },
    catch: (cause) => cause,
  });
