/**
 * The public DA transport the foreign replayer fetches a payload it does not
 * retain from, stubbed at the node's publication manifest and transport
 * seams (as `foreign-retained-da.test.ts` does): one committee peer serving
 * the envelope of whatever `serve` returns, or nothing.
 */
import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderRequestCbor,
  encodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import type * as SDK from "@al-ft/midgard-sdk";
import { vi } from "vitest";

import type { DaProducerPublicationManifest } from "../src/da/libp2p-producer.parse-committee-peers.js";
import * as Manifest from "../src/da/libp2p-producer.parse-da-producer-publication-manifest.js";
import * as Publication from "../src/da/libp2p-producer.publish-da-payload-insert-from-env.js";
import { sha256 } from "../src/sha256.js";
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
  maxPayloadBytes: 16 * 1024 * 1024,
  maxInlineResponseBytes: 1024 * 1024,
  maxChunkBytes: 64 * 1024,
  maxStreamsPerPeer: 1,
  maxGossipMessageBytes: 1024,
  listenMultiaddrs: [],
  announceMultiaddrs: [],
  bootstrapMultiaddrs: [],
  committeePeers: [
    {
      signerIndex: 0,
      daVkey: "ab".repeat(32),
      peerId: "fabricated-committee-0",
      multiaddrs: [],
      roles: ["committee"],
    },
  ],
};

/** Serves `serve()`'s payload (or nothing) to the replayer's DA fetch. */
export const serveDa = (serve: () => SDK.DaPayload | undefined) => {
  vi.spyOn(
    Manifest,
    "loadDaProducerPublicationManifestFromEnv",
  ).mockResolvedValue(manifest);
  vi.spyOn(Publication, "getPublicationTransport").mockResolvedValue({
    localPeerId: () => "fabricated-outbound-client",
    sign: () => Buffer.alloc(64),
    publish: async () => ({ recipients: [] }),
    request: async (_peer, protocol, bytes) => {
      const request = decodeDaPayloadByHeaderRequestCbor(bytes);
      if (
        protocol ===
        daRequestResponseProtocolId(
          manifest.deploymentFingerprint,
          DaRequestResponseProtocol.payloadByHeader,
        )
      ) {
        const payload = serve();
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
};
