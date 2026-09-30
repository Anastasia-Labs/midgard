import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it, vi } from "vitest";

import { type WatcherPublicDaRequest } from "../../src/storage/public-da-client.js";
import {
  encodeWatcherPublicDaFrame,
  readWatcherPublicDaFrames,
  WatcherPublicDaLibp2pTransport,
} from "../../src/storage/public-da-libp2p-transport.js";
import { FINGERPRINT } from "./public-da-client.raw-config.js";
import {
  asAsyncIterable,
  collectBuffers,
} from "./public-da-client.watcher-public-da-client-v1-auxiliary-da-surfaces.js";

// ---------------------------------------------------------------------------
// 10. Concrete TCP + Noise + Yamux transport framing and peer binding
// ---------------------------------------------------------------------------

describe("WatcherPublicDaLibp2pTransport", () => {
  const authenticatedPeerId =
    "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
  const requestFor = (
    signal = new AbortController().signal,
  ): WatcherPublicDaRequest => ({
    peerIdentity: "public-da-a",
    peerId: authenticatedPeerId,
    multiaddr: `/dns4/public-da.example/tcp/39003/p2p/${authenticatedPeerId}`,
    protocol: DaRequestResponseProtocol.capabilities,
    protocolId: daRequestResponseProtocolId(
      FINGERPRINT,
      DaRequestResponseProtocol.capabilities,
    ),
    requestCbor: Buffer.from([0xa0]),
    timeoutMs: 100,
    signal,
  });

  it("uses exact bounded framing across fragments and rejects adjacent responses", async () => {
    const frame = encodeWatcherPublicDaFrame(Buffer.from("response"), 32);
    const decoded = await collectBuffers(
      readWatcherPublicDaFrames(
        asAsyncIterable([frame.subarray(0, 3), frame.subarray(3)]),
        32,
      ),
    );
    expect(decoded).toEqual([Buffer.from("response")]);

    expect(() => encodeWatcherPublicDaFrame(Buffer.alloc(0), 32)).toThrow(
      /must not be empty/u,
    );
    expect(() => encodeWatcherPublicDaFrame(Buffer.alloc(33), 32)).toThrow(
      /exceeds configured bound/u,
    );
    await expect(
      collectBuffers(
        readWatcherPublicDaFrames(asAsyncIterable([Buffer.from([0, 0])]), 32),
      ),
    ).rejects.toThrow(/incomplete/u);
  });

  it("requires the Noise-authenticated connection peer to equal the configured peer", async () => {
    const sent: Uint8Array[] = [];
    const stream = {
      send: (frame: Uint8Array): boolean => {
        sent.push(frame);
        return true;
      },
      close: async (): Promise<void> => undefined,
      abort: (): void => undefined,
      async *[Symbol.asyncIterator](): AsyncGenerator<Uint8Array> {
        yield encodeWatcherPublicDaFrame(Buffer.from("response"));
      },
    };
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop: async (): Promise<void> => undefined,
        dialProtocol: async () => stream,
        getConnections: () => [
          { remotePeer: { toString: () => authenticatedPeerId } },
        ],
      }),
    });
    await transport.start();
    await expect(transport.request(requestFor())).resolves.toEqual(
      Buffer.from("response"),
    );
    expect(sent).toHaveLength(1);
    await transport.stop();

    const abortWrongPeerStream = vi.fn();
    const wrongPeerStream = {
      ...stream,
      abort: abortWrongPeerStream,
    };
    const wrongPeerTransport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop: async (): Promise<void> => undefined,
        dialProtocol: async () => wrongPeerStream,
        getConnections: () => [
          {
            remotePeer: {
              toString: () =>
                "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47",
            },
          },
        ],
      }),
    });
    await wrongPeerTransport.start();
    await expect(wrongPeerTransport.request(requestFor())).rejects.toThrow(
      /Noise-authenticated remote peer/u,
    );
    expect(abortWrongPeerStream).toHaveBeenCalledOnce();
    await wrongPeerTransport.stop();
  });

  it("aborts a stream returned after cancellation without sending a request", async () => {
    const controller = new AbortController();
    const send = vi.fn(() => true);
    const abort = vi.fn();
    const stream = {
      send,
      abort,
      close: async () => undefined,
      async *[Symbol.asyncIterator]() {
        yield encodeWatcherPublicDaFrame(Buffer.from("response"));
      },
    };
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async () => undefined,
        stop: async () => undefined,
        dialProtocol: async () => {
          controller.abort(new Error("lease closed during dial"));
          return stream;
        },
        getConnections: () => [
          { remotePeer: { toString: () => authenticatedPeerId } },
        ],
      }),
    });
    await transport.start();
    try {
      await expect(
        transport.request(requestFor(controller.signal)),
      ).rejects.toThrow("lease closed during dial");
      expect(send).not.toHaveBeenCalled();
      expect(abort).toHaveBeenCalledOnce();
    } finally {
      await transport.stop();
    }
  });

  it("honors an already-aborted request before it can dial", async () => {
    const controller = new AbortController();
    controller.abort(new Error("test cancellation"));
    const dialProtocol = vi.fn();
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async (): Promise<void> => undefined,
        stop: async (): Promise<void> => undefined,
        dialProtocol,
        getConnections: () => [],
      }),
    });
    await transport.start();
    await expect(
      transport.request(requestFor(controller.signal)),
    ).rejects.toThrow(/test cancellation/u);
    expect(dialProtocol).not.toHaveBeenCalled();
    await transport.stop();
  });
});
