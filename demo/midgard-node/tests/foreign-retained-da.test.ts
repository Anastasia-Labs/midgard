import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderRequestCbor,
  encodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import { fetchForeignRetainedDa } from "../src/da/foreign-retained-da.js";
import type { DaProducerPublicationManifest } from "../src/da/libp2p-producer.parse-committee-peers.js";
import * as Manifest from "../src/da/libp2p-producer.parse-da-producer-publication-manifest.js";
import * as Publication from "../src/da/libp2p-producer.publish-da-payload-insert-from-env.js";
import { ContractDeploymentIdentity } from "../src/services/index.js";
import { sha256 } from "../src/sha256.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";

const manifest: DaProducerPublicationManifest = {
  deploymentFingerprint: "de".repeat(32),
  contractDeploymentManifestId: "de".repeat(32),
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
  committeePeers: [0, 1].map((index) => ({
    signerIndex: index,
    daVkey: (index === 0 ? "ab" : "ac").repeat(32),
    peerId: `fabricated-committee-${index}`,
    multiaddrs: [],
    roles: ["committee"],
  })),
};
const identity = ContractDeploymentIdentity.make({
  kind: "manifest",
  manifestId: manifest.contractDeploymentManifestId,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  deploymentMarker: makeDeploymentMarker(manifest.contractDeploymentManifestId),
});
const acquire = (payload: SDK.DaPayload) =>
  Effect.runPromise(
    Effect.either(
      fetchForeignRetainedDa(
        payload.block_body.header_hash,
        payload.block_body.header,
      ),
    ).pipe(Effect.provideService(ContractDeploymentIdentity, identity)),
  );
const envelope = (payload: SDK.DaPayload) =>
  wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" });

const transport = (material: (peerIndex: number) => Buffer | undefined) => {
  vi.spyOn(
    Manifest,
    "loadDaProducerPublicationManifestFromEnv",
  ).mockResolvedValue(manifest);
  const requests: number[] = [];
  const get = vi
    .spyOn(Publication, "getPublicationTransport")
    .mockResolvedValue({
      localPeerId: () => "fabricated-outbound-client",
      sign: () => Buffer.alloc(64),
      publish: async () => ({ recipients: [] }),
      request: async (peer, protocol, bytes) => {
        const request = decodeDaPayloadByHeaderRequestCbor(bytes);
        expect(request.deploymentFingerprint.toString("hex")).toBe(
          manifest.deploymentFingerprint,
        );
        if (
          protocol ===
          daRequestResponseProtocolId(
            manifest.deploymentFingerprint,
            DaRequestResponseProtocol.payloadByHeader,
          )
        ) {
          requests.push(peer.signerIndex);
          const payloadBytes = material(peer.signerIndex);
          return encodeDaPayloadByHeaderResponseCbor({
            status: payloadBytes === undefined ? "not_found" : "found_inline",
            headerHash: request.headerHash,
            payloadHash:
              payloadBytes === undefined ? null : sha256(payloadBytes),
            payloadBytes: payloadBytes ?? null,
            chunkManifest: null,
            reasonCode: null,
          });
        }
        expect(protocol).toBe(
          daRequestResponseProtocolId(
            manifest.deploymentFingerprint,
            DaRequestResponseProtocol.metadataByHeader,
          ),
        );
        return encodeDaMetadataByHeaderResponseCbor({
          ...Manifest.emptyRetainedPayloadMetadataResponse(request.headerHash),
          status: "not_found",
        });
      },
    });
  return { requests, get };
};

afterEach(() => vi.restoreAllMocks());

it("acquires canonical-header-bound bytes from a configured committee without an optional retrieval role", async () => {
  const { payload } = await normalFixture();
  const bytes = await envelope(payload);
  const t = transport(() => bytes);
  const result = await acquire(payload);
  expect(result._tag).toBe("Right");
  if (result._tag === "Right") expect(result.right.payloadBytes).toEqual(bytes);
  expect(t.requests).toEqual([0]);
});

it("refuses a self-consistent peer envelope whose body differs from canonical commitments", async () => {
  const { payload } = await normalFixture();
  const substituted = await envelope({
    ...payload,
    block_body: { ...payload.block_body, transactions: [] },
  });
  const canonical = await envelope(payload);
  const t = transport((index) => (index === 0 ? substituted : canonical));
  const result = await acquire(payload);
  expect(result._tag).toBe("Right");
  if (result._tag === "Right")
    expect(result.right.payloadBytes).toEqual(canonical);
  expect(t.requests).toEqual([0, 1]);
});

it("returns typed missing when every configured public source is unavailable", async () => {
  const { payload } = await normalFixture();
  const t = transport(() => undefined);
  const result = await acquire(payload);
  expect(result._tag).toBe("Left");
  if (result._tag === "Left")
    expect(result.left).toMatchObject({
      reason: "missing",
      foreignHeaderHash: payload.block_body.header_hash,
    });
  expect(t.requests).toEqual([0, 1]);
});

it("refuses a publication manifest from another deployment before opening transport", async () => {
  const { payload } = await normalFixture();
  const bytes = await envelope(payload);
  const t = transport(() => bytes);
  vi.mocked(
    Manifest.loadDaProducerPublicationManifestFromEnv,
  ).mockResolvedValue({
    ...manifest,
    contractDeploymentManifestId: "ff".repeat(32),
  });
  const result = await acquire(payload);
  expect(result._tag).toBe("Left");
  if (result._tag === "Left")
    expect(result.left).toMatchObject({ reason: "missing" });
  expect(t.get).not.toHaveBeenCalled();
});
