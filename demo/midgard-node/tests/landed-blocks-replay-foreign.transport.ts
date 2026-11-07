/**
 * The public DA transport the foreign replayer fetches a payload it does not
 * retain from, stubbed at the node's publication manifest and transport
 * seams (as `foreign-retained-da.test.ts` does): committee peers (one by
 * default), each serving the envelope of whatever `serve` returns for its
 * index, or nothing.
 */
import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderRequestCbor,
  encodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { vi } from "vitest";

import type { DaProducerPublicationManifest } from "../src/da/libp2p-producer.parse-committee-peers.js";
import * as Manifest from "../src/da/libp2p-producer.parse-da-producer-publication-manifest.js";
import * as Publication from "../src/da/libp2p-producer.publish-da-payload-insert-from-env.js";
import { sha256 } from "../src/sha256.js";
import { type DaCopy } from "./landed-blocks-replay-foreign.availability.js";
import {
  envelope,
  MANIFEST_ID,
} from "./landed-blocks-replay-foreign.fixture.js";

const manifest: DaProducerPublicationManifest = {
  deploymentFingerprint: "de".repeat(32),
  contractDeploymentManifestId: MANIFEST_ID,
  localPrivateKeySource: "seed:" + "ef".repeat(32),
  threshold: 1,
  requestTimeoutMs: 100,
  retentionDays: 15,
  maxPayloadBytes: 16 * 1024 * 1024,
  maxInlineResponseBytes: 1024 * 1024,
  maxChunkBytes: 64 * 1024,
  maxStreamsPerPeer: 1,
  maxGossipMessageBytes: 1024,
  listenMultiaddrs: [],
  announceMultiaddrs: [],
  bootstrapMultiaddrs: [],
  committeePeers: [],
};

/**
 * Serves `serve(peerIndex)`'s copy (or nothing) from each of `peers`
 * committee peers to the replayer's DA fetch; returns the indices of the
 * peers asked, in order.
 */
export const serveDa = (
  serve: (peerIndex: number) => DaCopy | undefined,
  peers = 1,
) => {
  const requests: number[] = [];
  vi.spyOn(
    Manifest,
    "loadDaProducerPublicationManifestFromEnv",
  ).mockResolvedValue({
    ...manifest,
    committeePeers: Array.from({ length: peers }, (_, index) => ({
      signerIndex: index,
      daVkey: (0xab + index).toString(16).repeat(32),
      peerId: `fabricated-committee-${index.toString()}`,
      multiaddrs: [],
      roles: ["committee"],
    })),
  });
  vi.spyOn(Publication, "getPublicationTransport").mockResolvedValue({
    localPeerId: () => "fabricated-outbound-client",
    sign: () => Buffer.alloc(64),
    publish: async () => ({ recipients: [] }),
    request: async (peer, protocol, bytes) => {
      const request = decodeDaPayloadByHeaderRequestCbor(bytes);
      if (
        protocol ===
        daRequestResponseProtocolId(
          manifest.deploymentFingerprint,
          DaRequestResponseProtocol.payloadByHeader,
        )
      ) {
        requests.push(peer.signerIndex);
        const payload = serve(peer.signerIndex);
        const payloadBytes =
          payload === undefined ? undefined : await envelope(payload);
        return encodeDaPayloadByHeaderResponseCbor({
          status: payloadBytes === undefined ? "not_found" : "found_inline",
          headerHash: request.headerHash,
          payloadHash: payloadBytes === undefined ? null : sha256(payloadBytes),
          payloadBytes: payloadBytes ?? null,
          chunkManifest: null,
          reasonCode: null,
        });
      }
      return encodeDaMetadataByHeaderResponseCbor({
        ...Manifest.emptyRetainedPayloadMetadataResponse(request.headerHash),
        status: "not_found",
      });
    },
  });
  return requests;
};
