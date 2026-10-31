import "./public-da-transport-lifecycle.public-da-negotiated-stream-lifecycle.js";

import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { vi } from "vitest";

import type { WatcherPublicDaRequest } from "../../src/storage/public-da-client.js";
import {
  encodeWatcherPublicDaFrame,
  WatcherPublicDaLibp2pTransport,
} from "../../src/storage/public-da-libp2p-transport.js";

export const peerId = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";

export const requestFixture = (
  signal: WatcherPublicDaRequest["signal"] = new AbortController().signal,
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

export const closure = (name = "StreamStateError") =>
  Object.assign(
    new Error(
      name === "StreamStateError"
        ? "Cannot write to a stream that is closed"
        : "stream closed",
    ),
    { name },
  );

export type FailurePhase = "dial" | "send" | "drain" | "close" | "read";

export const scriptedTransport = (
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
  const abort = vi.fn<() => void>();
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
