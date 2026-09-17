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
    const monitor = (
      node: Awaited<ReturnType<typeof createLibp2p>>,
      side: keyof typeof heartbeats,
    ) => {
      node.addEventListener("connection:open", ({ detail: connection }) => {
        const newStream = connection.newStream.bind(connection);
        vi.spyOn(connection, "newStream").mockImplementation(
          async (protocols, options) => {
            if (
              (Array.isArray(protocols) ? protocols : [protocols]).includes(
                PING_PROTOCOL,
              )
            )
              heartbeats[side] += 1;
            return newStream(protocols, options);
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
    await server.start();
    try {
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
      await new Promise((resolve) => setTimeout(resolve, 180));
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

const peerId = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
const requestFixture = (
  signal = new AbortController().signal,
): WatcherPublicDaRequest => ({
  peerIdentity: "bounded-test",
  peerId,
  multiaddr: `/dns4/public-da.example/tcp/39003/p2p/${peerId}`,
  protocol: DaRequestResponseProtocol.capabilities,
  protocolId: daRequestResponseProtocolId(
    "22".repeat(32),
    DaRequestResponseProtocol.capabilities,
  ),
  requestCbor: Buffer.from([0xa0]),
  timeoutMs: 100,
  signal,
});
const closure = (name = "StreamStateError") =>
  Object.assign(
    new Error(
      name === "StreamStateError"
        ? "Cannot write to a stream that is closed"
        : "stream closed",
    ),
    { name },
  );
type FailurePhase = "dial" | "send" | "drain" | "close" | "read";
const scriptedTransport = (
  input: {
    phase?: FailurePhase;
    error?: Error;
    permanent?: boolean;
    malformed?: boolean;
    foreignPeer?: boolean;
    onFailure?: () => void | Promise<void>;
  } = {},
) => {
  const sent: Buffer[] = [];
  const abort = vi.fn();
  const dials: { peer: unknown; protocol: string; signal: AbortSignal }[] = [];
  const transport = new WatcherPublicDaLibp2pTransport({
    libp2pFactory: async () => ({
      start: async () => undefined,
      stop: async () => undefined,
      getConnections: () => [
        {
          remotePeer: {
            toString: () => (input.foreignPeer ? "foreign" : peerId),
          },
        },
      ],
      dialProtocol: async (peer, protocol, options) => {
        const fails = input.permanent === true || dials.length === 0;
        dials.push({ peer, protocol, signal: options.signal });
        const fail = async () => {
          await input.onFailure?.();
          throw input.error ?? closure();
        };
        if (fails && input.phase === "dial") return fail();
        return {
          // The logical stream can remain writable while its underlying mux
          // is closed. The retry decision must inspect the typed failure.
          writeStatus: "writable",
          send: (data) => {
            sent.push(Buffer.from(data));
            if (fails && input.phase === "send") throw input.error ?? closure();
            return !(fails && input.phase === "drain");
          },
          onDrain: fail,
          close: async () => {
            if (fails && input.phase === "close") await fail();
          },
          abort,
          async *[Symbol.asyncIterator]() {
            if (input.malformed) {
              yield Buffer.from([0, 0, 0, 0]);
              return;
            }
            if (fails && input.phase === "read") {
              yield encodeWatcherPublicDaFrame(
                Buffer.from("discard me"),
              ).subarray(0, 6);
              await fail();
            }
            yield encodeWatcherPublicDaFrame(Buffer.from("response"));
          },
        };
      },
    }),
  });
  return { transport, sent, abort, dials };
};

describe("bounded public DA closed-stream recovery", () => {
  it.each(
    ["submission", "mismatched protocol", "foreign namespace"].flatMap(
      (scenario) =>
        ["StreamStateError", "StreamResetError"].map((errorName) => ({
          scenario,
          errorName,
        })),
    ),
  )(
    "does not replay $scenario after $errorName",
    async ({ scenario, errorName }) => {
      const request = requestFixture();
      if (scenario === "submission") {
        Object.assign(request, {
          protocol: DaRequestResponseProtocol.payloadSubmit,
          protocolId: daRequestResponseProtocolId(
            "22".repeat(32),
            DaRequestResponseProtocol.payloadSubmit,
          ),
        });
      } else if (scenario === "mismatched protocol") {
        Object.assign(request, {
          protocolId: daRequestResponseProtocolId(
            "22".repeat(32),
            DaRequestResponseProtocol.payloadSubmit,
          ),
        });
      } else {
        Object.assign(request, {
          protocolId: request.protocolId.replace("/midgard/", "/foreign/"),
        });
      }
      const { transport, dials } = scriptedTransport({
        phase: "read",
        error: closure(errorName),
      });
      await transport.start();
      try {
        await expect(transport.request(request)).rejects.toThrow(
          "read failed (attempt 1",
        );
        expect(dials).toHaveLength(1);
      } finally {
        await transport.stop();
      }
    },
  );
  it.each<FailurePhase>(["dial", "send", "drain", "close", "read"])(
    "retries closure during %s once, discarding any partial response",
    async (phase) => {
      const { transport, dials, sent, abort } = scriptedTransport({ phase });
      await transport.start();
      try {
        const request = requestFixture();
        await expect(transport.request(request)).resolves.toEqual(
          Buffer.from("response"),
        );
        expect(dials).toHaveLength(2);
        expect(dials[0]).toEqual(dials[1]);
        expect(dials[0]?.signal).toBe(request.signal);
        expect(
          sent.every((frame) =>
            frame.equals(encodeWatcherPublicDaFrame(request.requestCbor)),
          ),
        ).toBe(true);
        expect(abort).toHaveBeenCalledTimes(phase === "dial" ? 0 : 1);
      } finally {
        await transport.stop();
      }
    },
  );

  it.each([
    "StreamClosedError",
    "StreamResetError",
    "ConnectionClosedError",
    "ConnectionClosingError",
    "MuxerClosedError",
  ])(
    "recognizes pinned %s closure without retrying generic errors",
    async (name) => {
      const { transport, dials } = scriptedTransport({
        phase: "read",
        error: closure(name),
      });
      await transport.start();
      try {
        await expect(transport.request(requestFixture())).resolves.toEqual(
          Buffer.from("response"),
        );
        expect(dials).toHaveLength(2);
      } finally {
        await transport.stop();
      }
    },
  );

  it("bounds permanent closure at two attempts and retains stage, type, state and cause", async () => {
    const error = closure();
    const { transport, dials, abort } = scriptedTransport({
      phase: "read",
      permanent: true,
      error,
    });
    await transport.start();
    try {
      await expect(transport.request(requestFixture())).rejects.toMatchObject({
        cause: error,
        message: expect.stringContaining(
          "read failed (attempt 2, StreamStateError, writeStatus=writable)",
        ),
      });
      expect(dials).toHaveLength(2);
      expect(abort).toHaveBeenCalledTimes(2);
    } finally {
      await transport.stop();
    }
  });

  it.each([
    {
      phase: "read" as const,
      error: new Error("Cannot write to a stream that is closed"),
    },
    {
      phase: "send" as const,
      error: Object.assign(new Error("invalid stream operation"), {
        name: "StreamStateError",
      }),
    },
    {
      phase: "read" as const,
      error: Object.assign(new Error("aborted"), {
        name: "StreamAbortedError",
      }),
    },
    { malformed: true },
    { foreignPeer: true },
  ])(
    "does not retry non-closure, identity or malformed-frame failure %j",
    async (input) => {
      const { transport, dials, abort } = scriptedTransport(input);
      await transport.start();
      try {
        await expect(
          transport.request(requestFixture()),
        ).rejects.toBeInstanceOf(Error);
        expect(dials).toHaveLength(1);
        expect(abort).toHaveBeenCalledOnce();
      } finally {
        await transport.stop();
      }
    },
  );

  it("snapshots exact bytes, peer, protocol and signal across caller mutation", async () => {
    const request = requestFixture();
    const originalSignal = request.signal;
    const originalProtocol = request.protocolId;
    const { transport, sent, dials } = scriptedTransport({
      phase: "read",
      onFailure: () => {
        request.requestCbor.fill(0xff);
        Object.assign(request, {
          protocolId: "changed",
          peerId: "foreign",
          signal: new AbortController().signal,
        });
      },
    });
    await transport.start();
    try {
      await expect(transport.request(request)).resolves.toEqual(
        Buffer.from("response"),
      );
      expect(sent).toEqual([
        encodeWatcherPublicDaFrame(Buffer.from([0xa0])),
        encodeWatcherPublicDaFrame(Buffer.from([0xa0])),
      ]);
      expect(
        dials.map(({ signal, protocol }) => ({ signal, protocol })),
      ).toEqual([
        { signal: originalSignal, protocol: originalProtocol },
        { signal: originalSignal, protocol: originalProtocol },
      ]);
    } finally {
      await transport.stop();
    }
  });

  it.each(["StreamStateError", "StreamResetError"])(
    "does not redial after cancellation during a partial response with %s",
    async (errorName) => {
      const controller = new AbortController();
      const reason = new Error("lease revoked");
      const { transport, dials } = scriptedTransport({
        phase: "read",
        error: closure(errorName),
        onFailure: () => controller.abort(reason),
      });
      await transport.start();
      try {
        await expect(
          transport.request(requestFixture(controller.signal)),
        ).rejects.toBe(reason);
        expect(dials).toHaveLength(1);
      } finally {
        await transport.stop();
      }
    },
  );

  it("does not extend the original deadline to redial", async () => {
    const signal = AbortSignal.timeout(20);
    const { transport, dials } = scriptedTransport({
      phase: "dial",
      onFailure: () => new Promise((resolve) => setTimeout(resolve, 30)),
    });
    await transport.start();
    try {
      const failure: unknown = await transport
        .request(requestFixture(signal))
        .catch((cause: unknown) => cause);
      expect(signal.aborted).toBe(true);
      expect(failure).toBe(signal.reason);
      expect(dials).toHaveLength(1);
    } finally {
      await transport.stop();
    }
  });

  it("does not redial after owner shutdown during a partial response", async () => {
    const { transport, dials } = scriptedTransport({
      phase: "read",
      onFailure: () => transport.stop(),
    });
    await transport.start();
    await expect(transport.request(requestFixture())).rejects.toThrow(
      "read failed (attempt 1",
    );
    expect(dials).toHaveLength(1);
  });
});
