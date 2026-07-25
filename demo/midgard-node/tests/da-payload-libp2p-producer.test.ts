import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createServer, type Server } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE_V1_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  DaGossipTopic,
  daGossipTopic,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaCapabilitiesRequestV1Cbor,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadAnnouncementV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadSubmitRequestV1Cbor,
  encodeDaCapabilitiesResponseV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadSubmitResponseV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { loadDaLibp2pIdentity } from "@/da/libp2p-identity.js";
import {
  assertDaEnvelopeCapabilityQuorum,
  closeDaLibp2pPublicationTransport,
  createDaLibp2pProducerProbeTransport,
  createDaLibp2pRetainedPayloadRequestHandlers,
  type DaProducerProbeTransport,
  type DaProducerStream,
  type DaProducerTransport,
  decodeLengthPrefixedDaFrameForTest,
  encodeLengthPrefixedDaFrameForTest,
  getDaPublicationTransportForTest,
  parseDaProducerPublicationManifest,
  publishDaPayloadInsert,
  runDaLibp2pPreflight,
  runDaLibp2pPreflightFromEnv,
  writeSharedDaFrameChunksForTest,
} from "@/da/libp2p-producer.js";
import { DaPayloadsDB } from "@/database/index.js";

const PEER_A = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
const PEER_B = "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47";
const PEER_C = "12D3KooWKf1kXPQFRZ6SR6WQF1Z7gqDRUjUe7S4hSm8LRmSk5kvA";
const DEPLOYMENT = "ab".repeat(32);
const HEADER_HASH = Buffer.alloc(28, 0x02);
const PAYLOAD_CBOR = Buffer.from("d87980", "hex");
const PAYLOAD_HASH = computeDaSha256Hash(PAYLOAD_CBOR);
const PRODUCER_PRIVATE_KEY_SOURCE = `seed:${"00".repeat(31)}01`;

describe("DA payload libp2p producer publication", () => {
  it("requires a threshold of exact V1 envelope capabilities", async () => {
    const manifest = parseThreeCommitteePeerManifest();
    let incapablePeers = new Set([PEER_C]);
    const transport: DaProducerProbeTransport = {
      localPeerId: () => PEER_C,
      request: async (peer, protocolId, payload) => {
        expect(protocolId).toBe(
          daRequestResponseProtocolId(
            DEPLOYMENT,
            DaRequestResponseProtocol.capabilities,
          ),
        );
        expect(
          decodeDaCapabilitiesRequestV1Cbor(payload).deploymentFingerprint,
        ).toEqual(Buffer.from(DEPLOYMENT, "hex"));
        return encodeDaCapabilitiesResponseV1Cbor({
          deploymentFingerprint: Buffer.from(DEPLOYMENT, "hex"),
          transportProtocolVersion: 1,
          payloadSchemaVersions: [1],
          envelopeContentEncodings: incapablePeers.has(peer.peerId)
            ? [0]
            : [0, 1],
          maxPayloadBytes: manifest.maxPayloadBytes,
          maxInlineResponseBytes: manifest.maxInlineResponseBytes,
          maxChunkBytes: manifest.maxChunkBytes,
          maxStreamsPerPeer: manifest.maxStreamsPerPeer,
          requestTimeoutMs: manifest.requestTimeoutMs,
        });
      },
    };

    await expect(
      assertDaEnvelopeCapabilityQuorum({
        manifest,
        mode: "zstd",
        transport,
      }),
    ).resolves.toHaveLength(3);
    incapablePeers = new Set([PEER_B, PEER_C]);
    await expect(
      assertDaEnvelopeCapabilityQuorum({
        manifest,
        mode: "zstd",
        transport,
      }),
    ).rejects.toThrow(/capability quorum failed/);
  });

  it("sends payload-submit/1 to manifest committee peers and gossips an announcement", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const requests: {
      readonly peerId: string;
      readonly protocolId: string;
      readonly payload: Uint8Array;
    }[] = [];
    let published:
      | {
          readonly topic: string;
          readonly payload: Uint8Array;
        }
      | undefined;
    const transport: DaProducerTransport = {
      localPeerId: () => "producer-peer",
      sign: () => Buffer.alloc(64, 0x0b),
      request: async (peer, protocolId, payload) => {
        requests.push({ peerId: peer.peerId, protocolId, payload });
        return encodeDaPayloadSubmitResponseV1Cbor({
          status: peer.peerId === PEER_A ? "accepted" : "duplicate",
          headerHash: HEADER_HASH,
          payloadHash: PAYLOAD_HASH,
          reasonCode: null,
          retryAfterMs: null,
        });
      },
      publish: async (topic, payload) => {
        published = { topic, payload };
        return { recipients: [PEER_A, PEER_B] };
      },
    };

    const report = await publishDaPayloadInsert({
      insert: insertFixture(),
      manifest,
      transport,
      announcedAtSlot: 42,
    });

    expect(report.acceptedPeers).toBe(2);
    expect(requests.map((request) => request.peerId)).toEqual([PEER_A, PEER_B]);
    expect(requests[0]?.protocolId).toBe(
      daRequestResponseProtocolId(
        DEPLOYMENT,
        DaRequestResponseProtocol.payloadSubmit,
      ),
    );
    const decodedRequest = decodeDaPayloadSubmitRequestV1Cbor(
      requests[0]!.payload,
    );
    expect(decodedRequest.mode).toBe("inline");
    expect(decodedRequest.payloadBytes).toEqual(PAYLOAD_CBOR);
    expect(decodedRequest.headerHash).toEqual(HEADER_HASH);
    expect(decodedRequest.payloadHash).toEqual(PAYLOAD_HASH);
    expect(published?.topic).toBe(
      daGossipTopic(DEPLOYMENT, DaGossipTopic.payloadAnnouncements),
    );
    const announcement = decodeDaPayloadAnnouncementV1Cbor(published!.payload);
    expect(announcement.announcedByPeerId).toBe("producer-peer");
    expect(announcement.announcedAtSlot).toBe(42);
    expect(announcement.signature).toEqual(Buffer.alloc(64, 0x0b));
    expect(announcement.payloadHash).toEqual(PAYLOAD_HASH);
  });

  it("serves retained DA payloads by header hash for watcher backfill", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const handlers = createDaLibp2pRetainedPayloadRequestHandlers({
      manifest,
      retrieveByHeaderHash: async (headerHash) =>
        headerHash.equals(HEADER_HASH) ? rowFixture() : undefined,
    });

    const byHeaderResponse = decodeDaPayloadByHeaderResponseV1Cbor(
      await callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.payloadByHeader,
        request: encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: Buffer.from(DEPLOYMENT, "hex"),
          headerHash: HEADER_HASH,
          acceptedPayloadHashes: null,
          maxInlineBytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
        }),
      }),
    );
    expect(byHeaderResponse).toMatchObject({
      status: "found_inline",
      headerHash: HEADER_HASH,
      payloadHash: PAYLOAD_HASH,
      payloadBytes: PAYLOAD_CBOR,
      reasonCode: null,
    });

    const metadataResponse = decodeDaMetadataByHeaderResponseV1Cbor(
      await callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.metadataByHeader,
        request: encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: Buffer.from(DEPLOYMENT, "hex"),
          headerHash: HEADER_HASH,
          acceptedPayloadHashes: [PAYLOAD_HASH],
          maxInlineBytes: 0,
        }),
      }),
    );
    expect(metadataResponse).toMatchObject({
      status: "found",
      headerHash: HEADER_HASH,
      payloadHash: PAYLOAD_HASH,
      payloadSchemaVersion: 1,
      payloadBytes: PAYLOAD_CBOR.length,
      localStatus: "verified",
    });
    expect(metadataResponse.transitionTraceRoot).toEqual(
      Buffer.from("15".repeat(32), "hex"),
    );

    const missingResponse = decodeDaPayloadByHeaderResponseV1Cbor(
      await callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.payloadByHeader,
        request: encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: Buffer.from(DEPLOYMENT, "hex"),
          headerHash: Buffer.alloc(28, 0xff),
          acceptedPayloadHashes: null,
          maxInlineBytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
        }),
      }),
    );
    expect(missingResponse).toMatchObject({
      status: "not_found",
      payloadHash: null,
      payloadBytes: null,
      chunkManifest: null,
    });
  });

  it("surfaces per-peer failures without HTTP fallback and enforces threshold", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    let publishCalled = false;
    const transport: DaProducerTransport = {
      localPeerId: () => "producer-peer",
      sign: () => Buffer.alloc(64, 0x0b),
      request: async (peer) => {
        if (peer.peerId === PEER_B) {
          throw new Error("dial failed");
        }
        return encodeDaPayloadSubmitResponseV1Cbor({
          status: "accepted",
          headerHash: HEADER_HASH,
          payloadHash: PAYLOAD_HASH,
          reasonCode: null,
          retryAfterMs: null,
        });
      },
      publish: async () => {
        publishCalled = true;
        return { recipients: [PEER_A] };
      },
    };

    await expect(
      publishDaPayloadInsert({
        insert: insertFixture(),
        manifest,
        transport,
      }),
    ).rejects.toMatchObject({
      name: "DaPayloadPublicationError",
      report: {
        acceptedPeers: 1,
        peerResults: expect.arrayContaining([
          expect.objectContaining({
            peerId: PEER_B,
            status: "transport_error",
          }),
        ]),
      },
    });
    expect(publishCalled).toBe(false);
  });

  it("returns at threshold without waiting for a slow peer and safely records the straggler", async () => {
    const manifest = parseThreeCommitteePeerManifest();
    let releaseSlowPeer!: () => void;
    const slowPeer = new Promise<void>((resolve) => {
      releaseSlowPeer = resolve;
    });
    const transport: DaProducerTransport = {
      localPeerId: () => "producer-peer",
      sign: () => Buffer.alloc(64, 0x0b),
      request: async () => {
        throw new Error("framed request path expected");
      },
      requestFramed: async (peer) => {
        if (peer.peerId === PEER_C) {
          await slowPeer;
        }
        return encodeDaPayloadSubmitResponseV1Cbor({
          status: "accepted",
          headerHash: HEADER_HASH,
          payloadHash: PAYLOAD_HASH,
          reasonCode: null,
          retryAfterMs: null,
        });
      },
      publish: async () => ({ recipients: [PEER_A, PEER_B] }),
    };

    const publication = publishDaPayloadInsert({
      insert: insertFixture(),
      manifest,
      transport,
      onPeerResult: async () => {
        throw new Error("durable result callback failed");
      },
    });
    const report = await Promise.race([
      publication,
      new Promise<never>((_, reject) =>
        setTimeout(
          () => reject(new Error("publication waited for straggler")),
          100,
        ),
      ),
    ]);
    expect(report.acceptedPeers).toBe(2);
    expect(report.peerResults).toHaveLength(2);
    releaseSlowPeer();
    const allPeerResults = await report.allPeerResults;
    expect(report.peerResults).toHaveLength(2);
    expect(allPeerResults).toHaveLength(3);
    expect(allPeerResults).toEqual(
      expect.arrayContaining([expect.objectContaining({ peerId: PEER_C })]),
    );
  });

  it("succeeds when one committee peer rejects the payload", async () => {
    const manifest = parseThreeCommitteePeerManifest();
    const transport: DaProducerTransport = {
      localPeerId: () => "producer-peer",
      sign: () => Buffer.alloc(64, 0x0b),
      request: async (peer) =>
        encodeDaPayloadSubmitResponseV1Cbor({
          status: peer.peerId === PEER_C ? "rejected" : "accepted",
          headerHash: HEADER_HASH,
          payloadHash: PAYLOAD_HASH,
          reasonCode: peer.peerId === PEER_C ? "payload_decode_failed" : null,
          retryAfterMs: null,
        }),
      publish: async () => ({ recipients: [PEER_A, PEER_B] }),
    };
    const report = await publishDaPayloadInsert({
      insert: { ...insertFixture(), [DaPayloadsDB.Columns.VERSION]: 1 },
      manifest,
      transport,
    });
    expect(report.acceptedPeers).toBeGreaterThanOrEqual(2);
    expect(await report.allPeerResults).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          peerId: PEER_C,
          status: "rejected",
          error: "payload_decode_failed",
        }),
      ]),
    );
  });

  it("writes one shared frame in zero-copy chunks and honors backpressure", async () => {
    const chunks: Uint8Array[] = [];
    let drains = 0;
    const frame = Buffer.from("0123456789abcdef", "utf8");
    const stream: DaProducerStream = {
      async *[Symbol.asyncIterator]() {},
      send: (chunk) => {
        chunks.push(chunk);
        return chunks.length !== 2;
      },
      onDrain: async () => {
        drains += 1;
      },
    };
    await writeSharedDaFrameChunksForTest(stream, frame, 5);
    expect(Buffer.concat(chunks)).toEqual(frame);
    expect(chunks.map((chunk) => chunk.length)).toEqual([5, 5, 5, 1]);
    expect(drains).toBe(1);
    expect(chunks.every((chunk) => chunk.buffer === frame.buffer)).toBe(true);
  });

  it("preflights committee reachability with metadata-by-header probes", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const requests: {
      readonly peerId: string;
      readonly protocolId: string;
    }[] = [];
    const transport: DaProducerProbeTransport = {
      localPeerId: () => PEER_C,
      request: async (peer, protocolId) => {
        requests.push({ peerId: peer.peerId, protocolId });
        return encodeDaMetadataByHeaderResponseV1Cbor({
          status: "not_found",
          headerHash: Buffer.alloc(28),
          payloadHash: null,
          payloadSchemaVersion: null,
          payloadBytes: null,
          rootSummaryHash: null,
          proofBundleHash: null,
          transitionTraceRoot: null,
          eventToStepRoot: null,
          retainedUntilSlot: null,
          localStatus: null,
        });
      },
    };

    const report = await runDaLibp2pPreflight({ manifest, transport });

    expect(report).toMatchObject({
      configured: true,
      mode: "bind-listen",
      passed: true,
      reachableCommitteePeers: 2,
      reachableCommitteeSignerIndexes: [0, 1],
      threshold: 2,
      listenCheck: {
        checked: true,
        status: "bound",
        listenMultiaddrs: ["/ip4/0.0.0.0/tcp/0"],
        announceMultiaddrs: [`/dns4/producer.example/tcp/4001/p2p/${PEER_C}`],
      },
      failures: [],
      warnings: [],
    });
    expect(requests.map((request) => request.peerId)).toEqual([PEER_A, PEER_B]);
    expect(requests[0]?.protocolId).toBe(
      daRequestResponseProtocolId(
        DEPLOYMENT,
        DaRequestResponseProtocol.metadataByHeader,
      ),
    );
    expect(report.peerResults).toEqual([
      expect.objectContaining({ peerId: PEER_A, status: "not_found" }),
      expect.objectContaining({ peerId: PEER_B, status: "not_found" }),
    ]);
  });

  it("reports dial-only preflight as a probe without listener validation", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const transport: DaProducerProbeTransport = {
      localPeerId: () => PEER_C,
      request: async (peer) => {
        if (peer.peerId === PEER_B) {
          throw new Error("dial failed");
        }
        return encodeDaMetadataByHeaderResponseV1Cbor({
          status: "not_found",
          headerHash: Buffer.alloc(28),
          payloadHash: null,
          payloadSchemaVersion: null,
          payloadBytes: null,
          rootSummaryHash: null,
          proofBundleHash: null,
          transitionTraceRoot: null,
          eventToStepRoot: null,
          retainedUntilSlot: null,
          localStatus: null,
        });
      },
    };

    const report = await runDaLibp2pPreflight({
      manifest,
      transport,
      mode: "dial-only",
    });

    expect(report).toMatchObject({
      configured: true,
      mode: "dial-only",
      passed: false,
      reachableCommitteePeers: 1,
      reachableCommitteeSignerIndexes: [0],
      listenCheck: {
        checked: false,
        status: "skipped",
        listenMultiaddrs: ["/ip4/0.0.0.0/tcp/0"],
        announceMultiaddrs: [`/dns4/producer.example/tcp/4001/p2p/${PEER_C}`],
      },
      failures: expect.arrayContaining([
        expect.objectContaining({
          phase: "dial",
          kind: "peer_unreachable",
          peerId: PEER_B,
          signerIndex: 1,
        }),
        expect.objectContaining({
          phase: "dial",
          kind: "peer_unreachable",
          error: expect.stringContaining("below threshold"),
        }),
      ]),
    });
    expect(report.warnings).toEqual([
      expect.stringContaining("does not bind, announce, or validate"),
    ]);
  });

  it("fails closed when an injected preflight transport has the wrong peer id", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const transport: DaProducerProbeTransport = {
      localPeerId: () => "producer-peer",
      request: async () => {
        throw new Error("identity failure should happen before peer probes");
      },
    };

    const report = await runDaLibp2pPreflight({
      manifest,
      transport,
      mode: "dial-only",
    });

    expect(report).toMatchObject({
      configured: true,
      mode: "dial-only",
      passed: false,
      reachableCommitteeSignerIndexes: [],
      peerResults: [],
      listenCheck: {
        checked: false,
        status: "skipped",
      },
      failures: [
        expect.objectContaining({
          phase: "identity",
          kind: "identity_mismatch",
        }),
      ],
    });
  });

  it("classifies bind-listen startup port conflicts as structured preflight JSON", async () => {
    const server = await listenOnLoopback();
    const tmp = await mkdtemp(join(tmpdir(), "midgard-da-libp2p-"));
    try {
      const port = serverPort(server);
      const identity = await loadDaLibp2pIdentity(PRODUCER_PRIVATE_KEY_SOURCE);
      const manifestPath = join(tmp, "manifest.json");
      await writeFile(
        manifestPath,
        JSON.stringify(runtimeManifestFixture(identity.peerId, port)),
      );

      const report = await runDaLibp2pPreflightFromEnv({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE,
      });

      expect(report).toMatchObject({
        configured: true,
        mode: "bind-listen",
        passed: false,
        reachableCommitteeSignerIndexes: [],
        listenCheck: {
          checked: true,
          status: "failed",
        },
        failures: [
          expect.objectContaining({
            phase: "listen",
            kind: "producer_port_already_bound",
          }),
        ],
      });
      expect(report.listenCheck.error).toContain("EADDRINUSE");
    } finally {
      await closeServer(server);
      await rm(tmp, { recursive: true, force: true });
    }
  });

  it("starts dial-only probe transport without binding the producer listen address", async () => {
    const server = await listenOnLoopback();
    try {
      const port = serverPort(server);
      const identity = await loadDaLibp2pIdentity(PRODUCER_PRIVATE_KEY_SOURCE);
      const manifest = parseDaProducerPublicationManifest(
        runtimeManifestFixture(identity.peerId, port),
        { DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE },
      );
      if (manifest === null) {
        throw new Error("expected libp2p publication manifest");
      }

      const transport = await createDaLibp2pProducerProbeTransport(manifest, {
        mode: "dial-only",
      });
      try {
        expect("publish" in transport).toBe(false);
        expect("sign" in transport).toBe(false);
      } finally {
        await transport.close?.();
      }
    } finally {
      await closeServer(server);
    }
  });

  it("clears a rejected cached transport creation so a corrected retry can recover", async () => {
    const identity = await loadDaLibp2pIdentity(PRODUCER_PRIVATE_KEY_SOURCE);
    const valid = parseDaProducerPublicationManifest(
      runtimeManifestFixture(identity.peerId, 0),
      { DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE },
    );
    if (valid === null) {
      throw new Error("expected valid retry manifest");
    }
    await expect(
      getDaPublicationTransportForTest({
        ...valid,
        localPrivateKeySource: "seed:not-hex",
      }),
    ).rejects.toThrow();
    const recovered = await getDaPublicationTransportForTest(valid);
    expect(await recovered.localPeerId()).toBe(identity.peerId);
    await closeDaLibp2pPublicationTransport();
  });

  it("does not count accepted responses for the wrong payload hash", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const transport: DaProducerTransport = {
      localPeerId: () => "producer-peer",
      sign: () => Buffer.alloc(64, 0x0b),
      request: async (peer) =>
        encodeDaPayloadSubmitResponseV1Cbor({
          status: "accepted",
          headerHash: HEADER_HASH,
          payloadHash:
            peer.peerId === PEER_B ? Buffer.alloc(32, 0xff) : PAYLOAD_HASH,
          reasonCode: null,
          retryAfterMs: null,
        }),
      publish: async () => {
        throw new Error("announcement should wait for threshold acceptance");
      },
    };

    await expect(
      publishDaPayloadInsert({
        insert: insertFixture(),
        manifest,
        transport,
      }),
    ).rejects.toMatchObject({
      report: {
        acceptedPeers: 1,
        peerResults: expect.arrayContaining([
          expect.objectContaining({
            peerId: PEER_B,
            status: "transport_error",
            error: expect.stringContaining("payload_hash mismatch"),
          }),
        ]),
      },
    });
  });

  it("rejects HTTP-shaped DA manifest fields in libp2p mode", () => {
    const manifest = manifestFixture();
    (
      (manifest.da_committee as Record<string, unknown>).members as Record<
        string,
        unknown
      >[]
    )[0]!.baseUrls = ["http://127.0.0.1:8787"];

    expect(() => parseDaProducerPublicationManifest(manifest)).toThrow(
      /baseUrls/,
    );
  });

  it("rejects an unsupported runtime-manifest schema", () => {
    const manifest = manifestFixture();
    manifest.schemaVersion = "midgard-da-libp2p-runtime-manifest-v999";

    expect(() => parseDaProducerPublicationManifest(manifest)).toThrow(
      /schemaVersion/,
    );
  });

  it("rejects runtime manifests with split deployment identity", () => {
    const manifest = manifestFixture();
    (manifest.deployment as Record<string, unknown>).fingerprint = "cd".repeat(
      32,
    );

    expect(() => parseDaProducerPublicationManifest(manifest)).toThrow(
      /contract_deployment_manifest_id/,
    );
  });

  it("keeps DA payload bytes out of the production HTTP router", async () => {
    const routerPath = fileURLToPath(
      new URL("../src/commands/listen-router.ts", import.meta.url),
    );
    const routerSource = await readFile(routerPath, "utf8");

    expect(routerSource).not.toContain("/da/payload");
    expect(routerSource).toContain("healthz");
    expect(routerSource).toContain("readyz");
    expect(routerSource).toContain("submit");
  });
});

const manifestFixture = (): Record<string, unknown> => ({
  schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
  deployment: {
    fingerprint: DEPLOYMENT.toUpperCase(),
    contract_deployment_manifest_id: DEPLOYMENT,
    contract_deployment_info_sha256: "cd".repeat(32),
    identity_source: "contract_deployment_manifest_id",
  },
  runtime_topology: {
    target: "producer",
    profile: "public",
    producer_peer_id: PEER_C,
  },
  da_transport: {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
    announce_multiaddrs: [`/dns4/producer.example/tcp/4001/p2p/${PEER_C}`],
    bootstrap_multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${PEER_A}`],
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
    },
    limits: {
      max_payload_bytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      max_inline_response_bytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
      max_chunk_bytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
      max_streams_per_peer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
      request_timeout_ms: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
    },
    retention_days: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
  },
  da_committee: {
    threshold: 2,
    members: [
      {
        signer_index: 0,
        da_vkey: "01".repeat(32),
        peer_id: PEER_A,
        multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${PEER_A}`],
        roles: ["committee", "retrieval"],
      },
      {
        signer_index: 1,
        da_vkey: "02".repeat(32),
        peer_id: PEER_B,
        multiaddrs: [`/dns4/da-b.example/tcp/4001/p2p/${PEER_B}`],
        roles: ["committee"],
      },
      {
        signer_index: 2,
        da_vkey: "03".repeat(32),
        peer_id: PEER_C,
        multiaddrs: [`/dns4/watcher.example/tcp/4001/p2p/${PEER_C}`],
        roles: ["watcher"],
      },
    ],
  },
});

const runtimeManifestFixture = (
  producerPeerId: string,
  listenPort: number,
): Record<string, unknown> => ({
  ...manifestFixture(),
  da_transport: {
    ...(manifestFixture().da_transport as Record<string, unknown>),
    listen_multiaddrs: [`/ip4/127.0.0.1/tcp/${listenPort.toString()}`],
    announce_multiaddrs: [
      `/ip4/127.0.0.1/tcp/${listenPort.toString()}/p2p/${producerPeerId}`,
    ],
    bootstrap_multiaddrs: [],
  },
});

const parseManifestFixture = () =>
  parseDaProducerPublicationManifest(manifestFixture(), {
    DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE,
  });

const parseThreeCommitteePeerManifest = () => {
  const fixture = manifestFixture();
  const committee = fixture.da_committee as Record<string, unknown>;
  const members = committee.members as Record<string, unknown>[];
  members[2] = { ...members[2], roles: ["committee", "watcher"] };
  const parsed = parseDaProducerPublicationManifest(fixture, {
    DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE,
  });
  return parsed;
};

const listenOnLoopback = (): Promise<Server> =>
  new Promise((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      server.off("error", reject);
      resolve(server);
    });
  });

const closeServer = (server: Server): Promise<void> =>
  new Promise((resolve, reject) => {
    server.close((error) => (error === undefined ? resolve() : reject(error)));
  });

const serverPort = (server: Server): number => {
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("expected TCP server address");
  }
  return address.port;
};

const insertFixture = (): DaPayloadsDB.InsertInput => ({
  [DaPayloadsDB.Columns.HEADER_HASH]: HEADER_HASH,
  [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]:
    MIDGARD_CONSENSUS_PROFILE_V1_ID,
  [DaPayloadsDB.Columns.VERSION]: 1,
  [DaPayloadsDB.Columns.PAYLOAD_CBOR]: PAYLOAD_CBOR,
  [DaPayloadsDB.Columns.PAYLOAD_SHA256]: PAYLOAD_HASH,
  [DaPayloadsDB.Columns.UTXOS_ROOT]: "10".repeat(32),
  [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]: "11".repeat(32),
  [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: "12".repeat(32),
  [DaPayloadsDB.Columns.DEPOSITS_ROOT]: "13".repeat(32),
  [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: "14".repeat(32),
  [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: "15".repeat(32),
  [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: "16".repeat(32),
  [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: EMPTY_MERKLE_TREE_ROOT,
  [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
  [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
  [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
  [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
  [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
  [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
  [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
  [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date("2026-06-21T00:00:00Z"),
  [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date("2026-06-21T00:00:01Z"),
});

const rowFixture = (): DaPayloadsDB.Row => ({
  ...insertFixture(),
  [DaPayloadsDB.Columns.CREATED_AT]: new Date("2026-06-21T00:00:02Z"),
  [DaPayloadsDB.Columns.UPDATED_AT]: new Date("2026-06-21T00:00:03Z"),
});

const callRetainedPayloadHandler = async ({
  handlers,
  protocol,
  request,
}: {
  readonly handlers: ReturnType<
    typeof createDaLibp2pRetainedPayloadRequestHandlers
  >;
  readonly protocol: DaRequestResponseProtocol;
  readonly request: Uint8Array;
}): Promise<Buffer> => {
  const protocolId = daRequestResponseProtocolId(DEPLOYMENT, protocol);
  const handler = handlers.get(protocolId);
  if (handler === undefined) {
    throw new Error(`missing handler for ${protocol}`);
  }
  const requestFrame = encodeLengthPrefixedDaFrameForTest(
    request,
    DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
  );
  let responseFrame: Buffer | undefined;
  const stream: DaProducerStream = {
    async *[Symbol.asyncIterator]() {
      yield requestFrame;
    },
    send: (data) => {
      responseFrame = Buffer.from(data);
      return true;
    },
    close: async () => {},
  };

  await handler(stream);
  if (responseFrame === undefined) {
    throw new Error("retained payload handler did not send a response");
  }
  return decodeLengthPrefixedDaFrameForTest(responseFrame);
};
