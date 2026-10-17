import "node:fs/promises";
import "node:net";
import "node:os";
import "node:path";
import "node:url";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-libp2p-identity";
import "@al-ft/midgard-core/da-stream-codec";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-sdk";
import "vitest";
import "../src/da/libp2p-producer.js";
import "../src/database/index.js";
import "./da-payload-libp2p-producer.manifest-fixture.js";

import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  DA_TRANSPORT_LIMITS,
  DaGossipTopic,
  daGossipTopic,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaCapabilitiesRequestCbor,
  decodeDaMetadataByHeaderResponseCbor,
  decodeDaPayloadAnnouncementCbor,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadSubmitRequestCbor,
  encodeDaCapabilitiesResponseCbor,
  encodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadSubmitResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { readSourceFacets } from "../../../scripts/lib/source-facets.mjs";
import {
  assertDaEnvelopeCapabilityQuorum,
  closeDaLibp2pPublicationTransport,
  createDaLibp2pProducerProbeTransport,
  createDaLibp2pRetainedPayloadRequestHandlers,
  type DaProducerProbeTransport,
  type DaProducerStream,
  type DaProducerTransport,
  getDaPublicationTransportForTest,
  parseDaProducerPublicationManifest,
  publishDaPayloadInsert,
  runDaLibp2pPreflight,
  runDaLibp2pPreflightFromEnv,
  writeSharedDaFrameChunksForTest,
} from "../src/da/libp2p-producer.js";
import { DaPayloadsDB } from "../src/database/index.js";
import {
  callRetainedPayloadHandler,
  closeServer,
  DEPLOYMENT,
  HEADER_HASH,
  insertFixture,
  listenOnLoopback,
  manifestFixture,
  parseManifestFixture,
  parseThreeCommitteePeerManifest,
  PAYLOAD_CBOR,
  PAYLOAD_HASH,
  PEER_A,
  PEER_B,
  PEER_C,
  PRODUCER_PRIVATE_KEY_SOURCE,
  rowFixture,
  runtimeManifestFixture,
  serverPort,
} from "./da-payload-libp2p-producer.manifest-fixture.js";

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
          decodeDaCapabilitiesRequestCbor(payload).deploymentFingerprint,
        ).toEqual(Buffer.from(DEPLOYMENT, "hex"));
        return encodeDaCapabilitiesResponseCbor({
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
        return encodeDaPayloadSubmitResponseCbor({
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
    const decodedRequest = decodeDaPayloadSubmitRequestCbor(
      requests[0]!.payload,
    );
    expect(decodedRequest.mode).toBe("inline");
    expect(decodedRequest.payloadBytes).toEqual(PAYLOAD_CBOR);
    expect(decodedRequest.headerHash).toEqual(HEADER_HASH);
    expect(decodedRequest.payloadHash).toEqual(PAYLOAD_HASH);
    expect(published?.topic).toBe(
      daGossipTopic(DEPLOYMENT, DaGossipTopic.payloadAnnouncements),
    );
    const announcement = decodeDaPayloadAnnouncementCbor(published!.payload);
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

    const byHeaderResponse = decodeDaPayloadByHeaderResponseCbor(
      await callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.payloadByHeader,
        request: encodeDaPayloadByHeaderRequestCbor({
          deploymentFingerprint: Buffer.from(DEPLOYMENT, "hex"),
          headerHash: HEADER_HASH,
          acceptedPayloadHashes: null,
          maxInlineBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
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

    const metadataResponse = decodeDaMetadataByHeaderResponseCbor(
      await callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.metadataByHeader,
        request: encodeDaPayloadByHeaderRequestCbor({
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

    const missingResponse = decodeDaPayloadByHeaderResponseCbor(
      await callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.payloadByHeader,
        request: encodeDaPayloadByHeaderRequestCbor({
          deploymentFingerprint: Buffer.from(DEPLOYMENT, "hex"),
          headerHash: Buffer.alloc(28, 0xff),
          acceptedPayloadHashes: null,
          maxInlineBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
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

  it("rejects an oversized retained-payload request at its length prefix", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const handlers = createDaLibp2pRetainedPayloadRequestHandlers({
      manifest,
      retrieveByHeaderHash: async () => undefined,
    });
    const handler = handlers.get(
      daRequestResponseProtocolId(
        DEPLOYMENT,
        DaRequestResponseProtocol.payloadByHeader,
      ),
    );
    if (handler === undefined) {
      throw new Error("missing payload-by-header handler");
    }
    const oversizedPrefix = Buffer.alloc(4);
    oversizedPrefix.writeUInt32BE(manifest.maxPayloadBytes + 1, 0);
    let bodyChunksPulled = 0;
    let abortedWith: Error | undefined;
    const stream: DaProducerStream = {
      async *[Symbol.asyncIterator]() {
        yield oversizedPrefix;
        for (;;) {
          bodyChunksPulled += 1;
          yield Buffer.alloc(1024);
        }
      },
      send: () => {
        throw new Error("an oversized request must not get a response");
      },
      abort: (error) => {
        abortedWith = error;
      },
    };

    await expect(handler(stream)).rejects.toThrow(/exceeds/);
    expect(bodyChunksPulled).toBe(0);
    expect(abortedWith?.message).toMatch(/exceeds/);
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
        return encodeDaPayloadSubmitResponseCbor({
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
        return encodeDaPayloadSubmitResponseCbor({
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
        encodeDaPayloadSubmitResponseCbor({
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
        return encodeDaMetadataByHeaderResponseCbor({
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
        return encodeDaMetadataByHeaderResponseCbor({
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
        encodeDaPayloadSubmitResponseCbor({
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

  it("rejects missing or unknown runtime-manifest root and nested keys", () => {
    const cases: readonly {
      readonly mutate: (manifest: Record<string, unknown>) => void;
      readonly error: RegExp;
    }[] = [
      {
        mutate: (manifest) => {
          delete manifest.network;
        },
        error: /network is required/u,
      },
      {
        mutate: (manifest) => {
          manifest.unknown_root = true;
        },
        error: /unknown_root is unexpected/u,
      },
      {
        mutate: (manifest) => {
          const limits = (
            manifest.da_transport as Record<string, Record<string, unknown>>
          ).limits;
          limits.unknown_limit = 1;
        },
        error: /limits\.unknown_limit is unexpected/u,
      },
      {
        mutate: (manifest) => {
          const committee = manifest.da_committee as {
            members: Record<string, unknown>[];
          };
          delete committee.members[0]!.roles;
        },
        error: /members\[0\]\.roles is required/u,
      },
    ];

    for (const testCase of cases) {
      const manifest = manifestFixture();
      testCase.mutate(manifest);
      expect(() => parseDaProducerPublicationManifest(manifest)).toThrow(
        testCase.error,
      );
    }
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
    const routerSource = readSourceFacets(routerPath);

    expect(routerSource).not.toContain("/da/payload");
    expect(routerSource).toContain("healthz");
    expect(routerSource).toContain("readyz");
    expect(routerSource).toContain("submit");
  });
});
