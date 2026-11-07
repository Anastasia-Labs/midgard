import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  encodeDaStreamFrame,
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  computeDaSha256Hash,
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaCapabilitiesResponseCbor,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadChunkResponseCbor,
  encodeDaCapabilitiesRequestCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadChunkRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { noise } from "@chainsafe/libp2p-noise";
import { yamux } from "@chainsafe/libp2p-yamux";
import type { Stream } from "@libp2p/interface";
import { ping, PING_PROTOCOL } from "@libp2p/ping";
import { tcp } from "@libp2p/tcp";
import { multiaddr } from "@multiformats/multiaddr";
import { createLibp2p, type Libp2p } from "libp2p";
import { WatcherPublicDaLibp2pTransport } from "midgard-watcher";
import { describe, expect, it, vi } from "vitest";

import { PublicRetainedDaListener } from "../src/da/libp2p/PublicRetainedDaListener.js";
import { stopPublicRetainedDaRuntime } from "../src/public-retained-da-runtime.js";
import {
  publicRetainedDaConfig,
  realisticIdentityEnvelope,
} from "./libp2p-runtime.da-libp2p-protocol-and-topic-allowlists.js";
import { DEPLOYMENT_FINGERPRINT } from "./libp2p-runtime.da-libp2p-stream-framing.js";

describe("public retained-DA listener", () => {
  it("keeps bidirectional heartbeats alive beside eight held DA reads without admitting a ninth read", async () => {
    const identity = await loadDaLibp2pIdentity(`seed:${"5e".repeat(32)}`);
    let server: Libp2p | undefined;
    const listener = new PublicRetainedDaListener({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      config: {
        ...publicRetainedDaConfig(identity.peerId),
        limits: {
          maxStreamsPerPeer: 8,
          maxInflightRequests: 16,
          maxInflightRequestsPerPeer: 16,
          maxInflightProofRequests: 8,
          requestTimeoutMs: 20_000,
        },
      },
      privateKey: identity.privateKey,
      store: {
        getDaPayload: async () => undefined,
        getStateQueueHeader: async () => undefined,
      },
      dataLimits: {
        ...DA_TRANSPORT_LIMITS,
        requestTimeoutMs: 20_000,
      },
      libp2pFactory: async (options) => {
        server = await createLibp2p(options);
        return server;
      },
    });
    const client = await createLibp2p({
      start: false,
      transports: [tcp()],
      connectionEncrypters: [noise()],
      streamMuxers: [yamux({ maxInboundStreams: 2, maxOutboundStreams: 10 })],
      services: { ping: ping() },
    });
    const held: Stream[] = [];
    try {
      await listener.start();
      await client.start();
      const address = listener.getMultiaddrs()[0];
      if (address === undefined || server === undefined)
        throw new Error("missing public DA listener");
      const capabilities = daRequestResponseProtocolId(
        DEPLOYMENT_FINGERPRINT,
        DaRequestResponseProtocol.capabilities,
      );
      // Negotiated reads wait for their request frame while occupying all
      // eight DA permits. The heartbeat must still negotiate in both directions.
      for (let index = 0; index < 8; index += 1) {
        held.push(
          await client.dialProtocol(multiaddr(address), capabilities, {
            negotiateFully: true,
            signal: AbortSignal.timeout(2_000),
          }),
        );
      }
      const clientConnection = client.getConnections()[0];
      const serverConnection = server.getConnections()[0];
      expect(clientConnection).toBeDefined();
      expect(serverConnection).toBeDefined();
      expect(clientConnection.rtt).toBeUndefined();
      expect(serverConnection.rtt).toBeUndefined();

      // Different protocol: rejection must come from aggregate DA admission,
      // not the capabilities handler's separate per-protocol stream limit.
      await expect(
        (async () => {
          const excess = await client.dialProtocol(
            multiaddr(address),
            daRequestResponseProtocolId(
              DEPLOYMENT_FINGERPRINT,
              DaRequestResponseProtocol.payloadByHeader,
            ),
            { negotiateFully: true, signal: AbortSignal.timeout(2_000) },
          );
          try {
            await readSingleDaStreamFrame(excess);
          } finally {
            excess.abort(new Error("overload test finished"));
          }
        })(),
      ).rejects.toThrow();

      // Leave the production monitor defaults enabled: its first heartbeat
      // arrives after ten seconds. Both RTTs require a successful monitor read.
      await vi.waitFor(
        () => {
          expect(clientConnection.rtt).toEqual(expect.any(Number));
          expect(serverConnection.rtt).toEqual(expect.any(Number));
        },
        { timeout: 12_000, interval: 25 },
      );
      expect(client.getConnections()).toEqual([clientConnection]);
      expect(server.getConnections()).toEqual([serverConnection]);
      expect(clientConnection.status).toBe("open");
      expect(serverConnection.status).toBe("open");
      expect([...server.getProtocols()].sort()).toEqual(
        [...listener.protocols, PING_PROTOCOL].sort(),
      );
      expect(client.getProtocols()).toEqual([PING_PROTOCOL]);

      const request = encodeDaCapabilitiesRequestCbor({
        deploymentFingerprint: Buffer.from(DEPLOYMENT_FINGERPRINT, "hex"),
      });
      await Promise.all(
        held.map(async (stream) => {
          await writeDaStreamFrame(stream, request, { close: true });
          const response = await readSingleDaStreamFrame(stream);
          expect(decodeDaCapabilitiesResponseCbor(response)).toMatchObject({
            transportProtocolVersion: 1,
          });
        }),
      );
      expect(listener.getActivePeerPermitCountForTest()).toBe(0);
    } finally {
      for (const stream of held)
        stream.abort(new Error("heartbeat test finished"));
      await client.stop();
      await listener.stop();
    }
  }, 20_000);

  it("serves a public Noise-authenticated read over TCP and refuses payload submission", async () => {
    const identity = await loadDaLibp2pIdentity(`seed:${"5a".repeat(32)}`);
    const listener = new PublicRetainedDaListener({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      config: publicRetainedDaConfig(identity.peerId),
      store: {
        getDaPayload: async () => undefined,
        getStateQueueHeader: async () => undefined,
      },
      privateKey: identity.privateKey,
      dataLimits: {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
        requestTimeoutMs: 2_000,
      },
    });
    const transport = new WatcherPublicDaLibp2pTransport();
    try {
      await listener.start();
      await transport.start();
      const address = listener
        .getMultiaddrs()
        .find((candidate) => candidate.startsWith("/ip4/127.0.0.1/tcp/"));
      if (address === undefined) {
        throw new Error(
          "public retained-DA listener did not bind localhost TCP",
        );
      }
      const watcherMultiaddr = address.replace(
        "/ip4/127.0.0.1/",
        "/dns4/localhost/",
      );
      const capabilitiesProtocolId = daRequestResponseProtocolId(
        DEPLOYMENT_FINGERPRINT,
        DaRequestResponseProtocol.capabilities,
      );
      const response = await transport.request({
        peerIdentity: "public-retained-da",
        peerId: identity.peerId,
        multiaddr: watcherMultiaddr,
        protocol: DaRequestResponseProtocol.capabilities,
        protocolId: capabilitiesProtocolId,
        requestCbor: encodeDaCapabilitiesRequestCbor({
          deploymentFingerprint: Buffer.from(DEPLOYMENT_FINGERPRINT, "hex"),
        }),
        timeoutMs: 2_000,
        signal: AbortSignal.timeout(2_000),
      });
      expect(decodeDaCapabilitiesResponseCbor(response)).toMatchObject({
        transportProtocolVersion: 1,
      });

      await expect(
        transport.request({
          peerIdentity: "public-retained-da",
          peerId: identity.peerId,
          multiaddr: watcherMultiaddr,
          protocol: DaRequestResponseProtocol.payloadSubmit,
          protocolId: daRequestResponseProtocolId(
            DEPLOYMENT_FINGERPRINT,
            DaRequestResponseProtocol.payloadSubmit,
          ),
          requestCbor: Buffer.from([0xa0]),
          timeoutMs: 2_000,
          signal: AbortSignal.timeout(2_000),
        }),
      ).rejects.toThrow();
    } finally {
      await transport.stop();
      await listener.stop();
    }
  });

  it("installs only public read handlers and ping with no gossip or outbound dialing", async () => {
    const identity = await loadDaLibp2pIdentity(`seed:${"5b".repeat(32)}`);
    const handled: string[] = [];
    const handlers = new Map<
      string,
      (stream: unknown, connection: unknown) => Promise<void> | void
    >();
    let options: Record<string, unknown> | undefined;
    const listener = new PublicRetainedDaListener({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      config: publicRetainedDaConfig(identity.peerId),
      store: {
        getDaPayload: async () => undefined,
        getStateQueueHeader: async () => undefined,
      },
      privateKey: identity.privateKey,
      dataLimits: {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
        requestTimeoutMs: 100,
      },
      libp2pFactory: async (capturedOptions) => {
        options = capturedOptions as Record<string, unknown>;
        return {
          start: async (): Promise<void> => undefined,
          stop: async (): Promise<void> => undefined,
          handle: async (protocol, handler): Promise<void> => {
            handled.push(protocol);
            handlers.set(protocol, handler);
          },
          unhandle: async (): Promise<void> => undefined,
        };
      },
    });

    await listener.start();
    expect(handled).toEqual(
      DA_PUBLIC_RETAINED_DA_PROTOCOLS.map((protocol) =>
        daRequestResponseProtocolId(DEPLOYMENT_FINGERPRINT, protocol),
      ),
    );
    expect(handled).not.toContain(
      daRequestResponseProtocolId(
        DEPLOYMENT_FINGERPRINT,
        DaRequestResponseProtocol.payloadSubmit,
      ),
    );
    expect(options).toMatchObject({
      start: false,
      addresses: {
        listen: ["/ip4/127.0.0.1/tcp/0"],
      },
    });
    expect(Object.keys(options?.services ?? {})).toEqual(["ping"]);
    expect(options).not.toHaveProperty("peerDiscovery");
    const gater = options?.connectionGater as {
      readonly denyDialPeer?: () => boolean;
      readonly denyOutboundConnection?: () => boolean;
      readonly denyInboundEncryptedConnection?: () => boolean;
    };
    expect(gater.denyDialPeer?.()).toBe(true);
    expect(gater.denyOutboundConnection?.()).toBe(true);
    expect(gater.denyInboundEncryptedConnection).toBeUndefined();
    const capabilitiesHandler = handlers.get(
      daRequestResponseProtocolId(
        DEPLOYMENT_FINGERPRINT,
        DaRequestResponseProtocol.capabilities,
      ),
    );
    if (capabilitiesHandler === undefined)
      throw new Error("missing capabilities handler");
    for (let index = 0; index < 32; index += 1) {
      const sent: Buffer[] = [];
      await capabilitiesHandler(
        {
          async *[Symbol.asyncIterator](): AsyncGenerator<Uint8Array> {
            yield encodeDaStreamFrame(
              encodeDaCapabilitiesRequestCbor({
                deploymentFingerprint: Buffer.from(
                  DEPLOYMENT_FINGERPRINT,
                  "hex",
                ),
              }),
            );
          },
          send: (data: Uint8Array): boolean => {
            sent.push(Buffer.from(data));
            return true;
          },
          close: async (): Promise<void> => undefined,
          abort: (): void => undefined,
        },
        { remotePeer: { toString: () => `sybil-${index.toString()}` } },
      );
      expect(sent).toHaveLength(1);
      expect(listener.getActivePeerPermitCountForTest()).toBe(0);
    }
    await listener.stop();
  });

  it("aborts a stalled public request at its deadline and rejects overload without queueing", async () => {
    const identity = await loadDaLibp2pIdentity(`seed:${"5c".repeat(32)}`);
    const handlers = new Map<
      string,
      (stream: unknown, connection: unknown) => Promise<void> | void
    >();
    const listener = new PublicRetainedDaListener({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      config: {
        ...publicRetainedDaConfig(identity.peerId),
        limits: {
          maxStreamsPerPeer: 4,
          maxInflightRequests: 1,
          maxInflightRequestsPerPeer: 1,
          maxInflightProofRequests: 1,
          requestTimeoutMs: 25,
        },
      },
      store: {
        getDaPayload: async () => undefined,
        getStateQueueHeader: async () => undefined,
      },
      privateKey: identity.privateKey,
      dataLimits: {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
        requestTimeoutMs: 25,
      },
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop: async (): Promise<void> => undefined,
        handle: async (protocol, handler): Promise<void> => {
          handlers.set(protocol, handler);
        },
        unhandle: async (): Promise<void> => undefined,
      }),
    });
    await listener.start();
    const capabilitiesHandler = handlers.get(
      daRequestResponseProtocolId(
        DEPLOYMENT_FINGERPRINT,
        DaRequestResponseProtocol.capabilities,
      ),
    );
    if (capabilitiesHandler === undefined)
      throw new Error("missing capabilities handler");

    let rejectRead!: (error: Error) => void;
    const stalledRead = new Promise<never>((_resolve, reject) => {
      rejectRead = reject;
    });
    const abort = vi.fn((error: Error) => rejectRead(error));
    const stalledStream = {
      abort,
      async *[Symbol.asyncIterator](): AsyncGenerator<Uint8Array> {
        await stalledRead;
      },
    };
    const first = capabilitiesHandler(stalledStream, {
      remotePeer: { toString: () => "unlisted-noise-peer" },
    });
    await new Promise((resolve) => setTimeout(resolve, 1));
    const rejectedOverloadStream = {
      abort: vi.fn(),
      async *[Symbol.asyncIterator](): AsyncGenerator<Uint8Array> {
        await new Promise<void>(() => undefined);
      },
    };
    await expect(
      capabilitiesHandler(rejectedOverloadStream, {
        remotePeer: { toString: () => "different-noise-peer" },
      }),
    ).rejects.toThrow(/overloaded/u);
    expect(rejectedOverloadStream.abort).toHaveBeenCalledOnce();
    await expect(first).rejects.toThrow(/exceeded the 25ms deadline/u);
    expect(abort).toHaveBeenCalledOnce();
    await listener.stop();
  });

  it.each([
    // The first live block with two L2 transfers and their validation traces
    // stored a 323,146-byte identity envelope: served inline.
    { label: "inline", envelopeBytes: 323_146, status: "found_inline" },
    // Above the 1 MiB inline bound the same read must switch to chunks.
    { label: "chunked", envelopeBytes: 2_621_440, status: "found_chunked" },
  ] as const)(
    "serves a $label retained payload of $envelopeBytes bytes byte-exact over TCP",
    async ({ envelopeBytes, status }) => {
      const identity = await loadDaLibp2pIdentity(`seed:${"5f".repeat(32)}`);
      const headerHash = "9a".repeat(28);
      const envelope = await realisticIdentityEnvelope(envelopeBytes);
      expect(envelope.length).toBe(envelopeBytes);
      const payloadSha256 = computeDaSha256Hash(envelope).toString("hex");
      const listener = new PublicRetainedDaListener({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        config: {
          ...publicRetainedDaConfig(identity.peerId),
          // The deployed public profile's request deadline.
          limits: {
            ...publicRetainedDaConfig(identity.peerId).limits,
            requestTimeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
          },
        },
        store: {
          getDaPayload: async (requested) =>
            requested === headerHash
              ? {
                  deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
                  headerHash,
                  payloadSchemaVersion: 1,
                  payloadCborHex: envelope.toString("hex"),
                  payloadSha256,
                  sourcePeerId: identity.peerId,
                  fetchedAt: new Date(0).toISOString(),
                  // A member that has not verified (or rejected) the payload
                  // still retains its bytes; the reader serves them.
                  validationStatus: "fetched",
                }
              : undefined,
          getStateQueueHeader: async () => undefined,
        },
        privateKey: identity.privateKey,
        dataLimits: {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
          maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
          maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
          maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
          requestTimeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
        },
      });
      const transport = new WatcherPublicDaLibp2pTransport();
      try {
        await listener.start();
        await transport.start();
        const address = listener
          .getMultiaddrs()
          .find((candidate) => candidate.startsWith("/ip4/127.0.0.1/tcp/"))!
          .replace("/ip4/127.0.0.1/", "/dns4/localhost/");
        const request = async (
          protocol: DaRequestResponseProtocol,
          requestCbor: Buffer,
        ): Promise<Uint8Array> =>
          transport.request({
            peerIdentity: "public-retained-da",
            peerId: identity.peerId,
            multiaddr: address,
            protocol,
            protocolId: daRequestResponseProtocolId(
              DEPLOYMENT_FINGERPRINT,
              protocol,
            ),
            requestCbor,
            timeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
            signal: AbortSignal.timeout(DA_TRANSPORT_LIMITS.requestTimeoutMs),
          });
        const fingerprint = Buffer.from(DEPLOYMENT_FINGERPRINT, "hex");
        const headerHashBytes = Buffer.from(headerHash, "hex");
        // The request the watcher's retained-DA source sends.
        const response = decodeDaPayloadByHeaderResponseCbor(
          await request(
            DaRequestResponseProtocol.payloadByHeader,
            encodeDaPayloadByHeaderRequestCbor({
              deploymentFingerprint: fingerprint,
              headerHash: headerHashBytes,
              acceptedPayloadHashes: null,
              maxInlineBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
            }),
          ),
        );
        expect(response.status).toBe(status);
        expect(Buffer.from(response.payloadHash!).toString("hex")).toBe(
          payloadSha256,
        );
        let retrieved: Buffer;
        if (response.status === "found_inline") {
          retrieved = Buffer.from(response.payloadBytes!);
        } else {
          const chunks: Buffer[] = [];
          for (
            let chunkIndex = 0;
            chunkIndex < response.chunkManifest!.chunkHashes.length;
            chunkIndex += 1
          ) {
            const chunk = decodeDaPayloadChunkResponseCbor(
              await request(
                DaRequestResponseProtocol.payloadChunk,
                encodeDaPayloadChunkRequestCbor({
                  deploymentFingerprint: fingerprint,
                  headerHash: headerHashBytes,
                  payloadHash: Buffer.from(response.payloadHash!),
                  chunkIndex,
                }),
              ),
            );
            expect(chunk.status).toBe("found");
            chunks.push(Buffer.from(chunk.chunkBytes!));
          }
          retrieved = Buffer.concat(chunks);
        }
        expect(retrieved.equals(envelope)).toBe(true);
        const unwrapped = await unwrapDaPayload(retrieved, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        });
        expect(unwrapped.innerBytes.length).toBe(envelopeBytes - 47);
      } finally {
        await transport.stop();
        await listener.stop();
      }
    },
    30_000,
  );

  it("tears down every protocol and the runtime after an unhandle failure", async () => {
    const identity = await loadDaLibp2pIdentity(`seed:${"5d".repeat(32)}`);
    const unhandled: string[] = [];
    const stop = vi.fn(async (): Promise<void> => undefined);
    const listener = new PublicRetainedDaListener({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      config: publicRetainedDaConfig(identity.peerId),
      store: {
        getDaPayload: async () => undefined,
        getStateQueueHeader: async () => undefined,
      },
      privateKey: identity.privateKey,
      dataLimits: {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
        requestTimeoutMs: 100,
      },
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop,
        handle: async (): Promise<void> => undefined,
        unhandle: async (protocol): Promise<void> => {
          unhandled.push(protocol);
          if (unhandled.length === 1) throw new Error("first unhandle failed");
        },
      }),
    });
    await listener.start();
    await expect(listener.stop()).rejects.toBeInstanceOf(AggregateError);
    expect(unhandled).toEqual(listener.protocols);
    expect(stop).toHaveBeenCalledOnce();
    expect(listener.isStarted()).toBe(false);
    await expect(listener.stop()).resolves.toBeUndefined();
  });

  it("attempts listener and store shutdown even when both fail", async () => {
    const listenerStop = vi.fn(async (): Promise<void> => {
      throw new Error("listener failure");
    });
    const storeClose = vi.fn(async (): Promise<void> => {
      throw new Error("store failure");
    });
    await expect(
      stopPublicRetainedDaRuntime({
        listener: { stop: listenerStop },
        store: { close: storeClose },
      }),
    ).rejects.toBeInstanceOf(AggregateError);
    expect(listenerStop).toHaveBeenCalledOnce();
    expect(storeClose).toHaveBeenCalledOnce();
  });
});
