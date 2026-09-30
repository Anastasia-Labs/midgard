import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { noise } from "@chainsafe/libp2p-noise";
import { yamux } from "@chainsafe/libp2p-yamux";
import { ping, PING_PROTOCOL } from "@libp2p/ping";
import { tcp } from "@libp2p/tcp";
import { createLibp2p } from "libp2p";
import { describe, expect, it, vi } from "vitest";

import type { WatcherPublicDaRequest } from "../../src/storage/public-da-client.js";
import {
  encodeWatcherPublicDaFrame,
  readWatcherPublicDaFrames,
  type WatcherPublicDaLibp2pFactory,
  WatcherPublicDaLibp2pTransport,
} from "../../src/storage/public-da-libp2p-transport.js";

// Exercise the pinned TCP/Noise/Yamux implementation. Only the timing of the
// first negotiated stream's closure is controlled; no response is fabricated
// by the transport factory and no external node or committee is used.
describe("public DA negotiated stream lifecycle", () => {
  it("keeps a held DA response on the same connection through bidirectional heartbeats", async () => {
    const protocol = daRequestResponseProtocolId(
      "11".repeat(32),
      DaRequestResponseProtocol.capabilities,
    );
    const heartbeats = { client: 0, server: 0 };
    let heartbeatFinished = () => {};
    const monitor = (
      node: Awaited<ReturnType<typeof createLibp2p>>,
      side: keyof typeof heartbeats,
    ) => {
      node.addEventListener("connection:open", ({ detail: connection }) => {
        const newStream = connection.newStream.bind(connection);
        vi.spyOn(connection, "newStream").mockImplementation(
          async (protocols, options) => {
            const stream = await newStream(protocols, options);
            if (
              (Array.isArray(protocols) ? protocols : [protocols]).includes(
                PING_PROTOCOL,
              )
            ) {
              const close = stream.close.bind(stream);
              vi.spyOn(stream, "close").mockImplementation(async (options) => {
                await close(options);
                heartbeats[side] += 1;
                heartbeatFinished();
              });
            }
            return stream;
          },
        );
      });
    };
    const server = await createLibp2p({
      start: false,
      addresses: { listen: ["/ip4/127.0.0.1/tcp/0"] },
      transports: [tcp()],
      connectionEncrypters: [noise()],
      streamMuxers: [yamux()],
      services: { ping: ping() },
      connectionMonitor: { pingInterval: 30 },
    });
    monitor(server, "server");
    let release!: () => void;
    const heldResponse = new Promise<void>((resolve) => {
      release = resolve;
    });
    let received!: () => void;
    const firstRequest = new Promise<void>((resolve) => {
      received = resolve;
    });
    const requests: Buffer[] = [];
    await server.handle(protocol, async (stream) => {
      try {
        for await (const request of readWatcherPublicDaFrames(stream)) {
          requests.push(request);
          received();
          await heldResponse;
          stream.send(encodeWatcherPublicDaFrame(Buffer.from("response")));
        }
        await stream.close();
      } catch (error) {
        stream.abort(error instanceof Error ? error : new Error(String(error)));
      }
    });
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async (options) => {
        const node = await createLibp2p({
          ...options,
          connectionMonitor: { pingInterval: 30 },
        });
        monitor(node, "client");
        return node as Awaited<ReturnType<WatcherPublicDaLibp2pFactory>>;
      },
    });
    // Drive real heartbeat exchanges one round at a time. A 30 ms wall-clock
    // interval can start another ping before the previous one closes on a
    // loaded runner, exhausting the ping protocol's stream limit.
    vi.useFakeTimers({ toFake: ["setInterval", "clearInterval"] });
    try {
      await server.start();
      await transport.start();
      const port = server
        .getMultiaddrs()[0]!
        .getComponents()
        .find(({ name }) => name === "tcp")!.value;
      const request: WatcherPublicDaRequest = {
        peerIdentity: "local-test",
        peerId: server.peerId.toString(),
        multiaddr: `/dns4/localhost/tcp/${port}/p2p/${server.peerId.toString()}`,
        protocol: DaRequestResponseProtocol.capabilities,
        protocolId: protocol,
        requestCbor: Buffer.from([0xa0]),
        timeoutMs: 5_000,
        signal: AbortSignal.timeout(5_000),
      };
      const result = transport
        .request(request)
        .catch((cause: unknown) => cause);
      await firstRequest;
      const connectionId = server.getConnections()[0]!.id;
      for (let round = 1; round <= 2; round += 1) {
        const finished = new Promise<void>((resolve) => {
          heartbeatFinished = () => {
            if (heartbeats.client >= round && heartbeats.server >= round)
              resolve();
          };
        });
        await vi.advanceTimersByTimeAsync(30);
        await finished;
      }
      release();
      expect(await result).toEqual(Buffer.from("response"));
      expect(requests).toEqual([Buffer.from([0xa0])]);
      expect(heartbeats.client).toBeGreaterThanOrEqual(2);
      expect(heartbeats.server).toBeGreaterThanOrEqual(2);
      expect(server.getConnections().map(({ id }) => id)).toEqual([
        connectionId,
      ]);
      await expect(
        server.getConnections()[0]!.newStream(protocol, {
          signal: AbortSignal.timeout(1_000),
        }),
      ).rejects.toMatchObject({ name: "UnsupportedProtocolError" });
    } finally {
      release();
      await transport.stop();
      await server.stop();
      vi.useRealTimers();
    }
  }, 15_000);

  it.each([false, true])(
    "handles remote Yamux response resets with permanent=%s",
    async (permanent) => {
      const protocol = daRequestResponseProtocolId(
        "11".repeat(32),
        DaRequestResponseProtocol.capabilities,
      );
      const server = await createLibp2p({
        start: false,
        addresses: { listen: ["/ip4/127.0.0.1/tcp/0"] },
        transports: [tcp()],
        connectionEncrypters: [noise()],
        streamMuxers: [yamux()],
      });
      const received: Buffer[] = [];
      let acknowledgeResponse: (() => void) | undefined;
      let partialResponsesRead = 0;
      await server.handle(protocol, async (stream) => {
        try {
          for await (const request of readWatcherPublicDaFrames(stream)) {
            received.push(request);
            if (permanent || received.length === 1) {
              const responseRead = new Promise<void>((resolve) => {
                acknowledgeResponse = resolve;
              });
              stream.send(
                encodeWatcherPublicDaFrame(
                  Buffer.from("discard this partial response"),
                ).subarray(0, 6),
              );
              // Send a real remote RST only after the client consumed the prefix.
              await responseRead;
              stream.abort(new Error("controlled server response reset"));
              return;
            }
            stream.send(encodeWatcherPublicDaFrame(Buffer.from("response")));
          }
          await stream.close();
        } catch (error) {
          stream.abort(
            error instanceof Error ? error : new Error(String(error)),
          );
        }
      });
      const signals: (AbortSignal | undefined)[] = [];
      const factory: WatcherPublicDaLibp2pFactory = async (options) => {
        const node = await createLibp2p(options);
        const dial = node.dialProtocol.bind(node);
        vi.spyOn(node, "dialProtocol").mockImplementation(
          async (peer, protocol, options) => {
            signals.push(options?.signal);
            const stream = await dial(peer, protocol, options);
            const iterator = stream[Symbol.asyncIterator].bind(stream);
            vi.spyOn(stream, Symbol.asyncIterator).mockImplementation(
              async function* () {
                for await (const chunk of {
                  [Symbol.asyncIterator]: iterator,
                }) {
                  if (acknowledgeResponse !== undefined) {
                    partialResponsesRead += 1;
                    const acknowledge = acknowledgeResponse;
                    acknowledgeResponse = undefined;
                    acknowledge();
                  }
                  yield chunk;
                }
              },
            );
            return stream;
          },
        );
        return node as Awaited<ReturnType<WatcherPublicDaLibp2pFactory>>;
      };
      const transport = new WatcherPublicDaLibp2pTransport({
        libp2pFactory: factory,
      });
      await server.start();
      try {
        await transport.start();
        const port = server
          .getMultiaddrs()[0]!
          .getComponents()
          .find(({ name }) => name === "tcp")!.value;
        const signal = AbortSignal.timeout(5_000);
        const request: WatcherPublicDaRequest = {
          peerIdentity: "local-test",
          peerId: server.peerId.toString(),
          multiaddr: `/dns4/localhost/tcp/${port}/p2p/${server.peerId.toString()}`,
          protocol: DaRequestResponseProtocol.capabilities,
          protocolId: protocol,
          requestCbor: Buffer.from([0xa0]),
          timeoutMs: 5_000,
          signal,
        };
        if (permanent) {
          await expect(transport.request(request)).rejects.toMatchObject({
            message: expect.stringContaining(
              "read failed (attempt 2, StreamResetError, writeStatus=closed)",
            ),
            cause: {
              name: "StreamResetError",
              message: "The stream has been reset",
            },
          });
        } else {
          await expect(transport.request(request)).resolves.toEqual(
            Buffer.from("response"),
          );
        }
        expect(received).toEqual([Buffer.from([0xa0]), Buffer.from([0xa0])]);
        expect(partialResponsesRead).toBe(permanent ? 2 : 1);
        expect(signals).toEqual([signal, signal]);
      } finally {
        acknowledgeResponse?.();
        await transport.stop();
        await server.stop();
      }
    },
    15_000,
  );

  it("recovers one closed negotiated stream with an identical authenticated read", async () => {
    const protocol = daRequestResponseProtocolId(
      "11".repeat(32),
      DaRequestResponseProtocol.capabilities,
    );
    const server = await createLibp2p({
      start: false,
      addresses: { listen: ["/ip4/127.0.0.1/tcp/0"] },
      transports: [tcp()],
      connectionEncrypters: [noise()],
      streamMuxers: [yamux()],
    });
    const received: Buffer[] = [];
    await server.handle(protocol, async (stream) => {
      try {
        for await (const request of readWatcherPublicDaFrames(stream)) {
          received.push(request);
          stream.send(encodeWatcherPublicDaFrame(Buffer.from("response")));
        }
        await stream.close();
      } catch (error) {
        stream.abort(error instanceof Error ? error : new Error(String(error)));
      }
    });
    let attempts = 0;
    let closedState: string | undefined;
    const dialOptions: unknown[] = [];
    const factory: WatcherPublicDaLibp2pFactory = async (options) => {
      const node = await createLibp2p(options);
      const dial = node.dialProtocol.bind(node);
      vi.spyOn(node, "dialProtocol").mockImplementation(
        async (peer, protocol, options) => {
          attempts += 1;
          dialOptions.push(options);
          const stream = await dial(peer, protocol, options);
          if (attempts === 1) {
            await stream.close();
            closedState = stream.writeStatus;
          }
          return stream;
        },
      );
      return node as Awaited<ReturnType<WatcherPublicDaLibp2pFactory>>;
    };
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: factory,
    });
    await server.start();
    try {
      await transport.start();
      const port = server
        .getMultiaddrs()[0]!
        .getComponents()
        .find(({ name }) => name === "tcp")!.value;
      const signal = AbortSignal.timeout(5_000);
      const request: WatcherPublicDaRequest = {
        peerIdentity: "local-test",
        peerId: server.peerId.toString(),
        multiaddr: `/dns4/localhost/tcp/${port}/p2p/${server.peerId.toString()}`,
        protocol: DaRequestResponseProtocol.capabilities,
        protocolId: protocol,
        requestCbor: Buffer.from([0xa0]),
        timeoutMs: 5_000,
        signal,
      };
      await expect(transport.request(request)).resolves.toEqual(
        Buffer.from("response"),
      );
      expect(closedState).toBe("closed");
      expect(attempts).toBe(2);
      expect(received).toEqual([Buffer.from([0xa0])]);
      expect(dialOptions).toEqual([
        { signal, negotiateFully: false },
        { signal, negotiateFully: false },
      ]);
    } finally {
      await transport.stop();
      await server.stop();
    }
  }, 15_000);
});
