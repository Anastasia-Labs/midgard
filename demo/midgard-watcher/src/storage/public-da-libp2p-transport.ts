import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import { noise } from "@chainsafe/libp2p-noise";
import { yamux } from "@chainsafe/libp2p-yamux";
import { peerIdFromString } from "@libp2p/peer-id";
import { tcp } from "@libp2p/tcp";
import { multiaddr } from "@multiformats/multiaddr";
import { createLibp2p, type Libp2pOptions } from "libp2p";

import type {
  WatcherPublicDaLibp2pTransportV1,
  WatcherPublicDaRequestV1,
} from "./public-da-client.js";

type PublicDaStreamChunk =
  | Uint8Array
  | {
      readonly byteLength: number;
      subarray(start?: number, end?: number): Uint8Array;
    };

type PublicDaStream = AsyncIterable<PublicDaStreamChunk> & {
  send(data: Uint8Array): boolean;
  onDrain?(): Promise<void>;
  close(): Promise<void>;
  abort(error: Error): void;
};

type PublicDaRuntimeNode = {
  start(): Promise<void>;
  stop(): Promise<void>;
  dialProtocol(
    peer: unknown,
    protocol: string,
    options: { readonly signal: AbortSignal; readonly negotiateFully: false },
  ): Promise<PublicDaStream>;
  getConnections(peerId: unknown): readonly {
    readonly remotePeer: { toString(): string };
  }[];
};

export type WatcherPublicDaLibp2pFactoryV1 = (
  options: Libp2pOptions,
) => Promise<PublicDaRuntimeNode>;

export type WatcherPublicDaLibp2pTransportOptionsV1 = Readonly<{
  libp2pFactory?: WatcherPublicDaLibp2pFactoryV1;
  maxFrameBytes?: number;
}>;

/** Real watcher-owned TCP + Noise + Yamux transport. No discovery or inbound APIs. */
export class WatcherPublicDaLibp2pTransport
  implements WatcherPublicDaLibp2pTransportV1
{
  private readonly maxFrameBytes: number;
  private readonly factory: WatcherPublicDaLibp2pFactoryV1;
  private node?: PublicDaRuntimeNode;

  constructor(options: WatcherPublicDaLibp2pTransportOptionsV1 = {}) {
    this.maxFrameBytes =
      options.maxFrameBytes ?? DA_TRANSPORT_LIMITS_V1.maxPayloadBytes;
    if (!Number.isSafeInteger(this.maxFrameBytes) || this.maxFrameBytes <= 0) {
      throw new RangeError(
        "public DA maxFrameBytes must be a positive safe integer",
      );
    }
    this.factory = options.libp2pFactory ?? defaultLibp2pFactory;
  }

  async start(): Promise<void> {
    if (this.node !== undefined) return;
    const node = await this.factory({
      start: false,
      transports: [tcp()],
      connectionEncrypters: [noise()],
      streamMuxers: [
        yamux({
          maxInboundStreams: 0,
          maxMessageSize: this.maxFrameBytes,
        }),
      ],
      connectionGater: {
        denyInboundConnection: () => true,
        denyInboundEncryptedConnection: () => true,
        denyInboundUpgradedConnection: () => true,
        denyInboundRelayReservation: () => true,
        denyInboundRelayedConnection: () => true,
        denyOutboundRelayedConnection: () => true,
      },
    });
    await node.start();
    this.node = node;
  }

  async stop(): Promise<void> {
    const node = this.node;
    this.node = undefined;
    await node?.stop();
  }

  async request(request: WatcherPublicDaRequestV1): Promise<Uint8Array> {
    const node = this.node;
    if (node === undefined) {
      throw new Error("watcher public DA libp2p transport is not started");
    }
    request.signal.throwIfAborted();
    const { address, peerId } = parseExpectedPeer(request);
    const stream = await node.dialProtocol(address, request.protocolId, {
      signal: request.signal,
      negotiateFully: false,
    });
    let onAbort: (() => void) | undefined;
    try {
      // This scope starts as soon as a stream exists. A post-dial identity
      // mismatch is a security failure and must not leave the stream open.
      assertAuthenticatedPeer(node, peerId, request.peerId);
      onAbort = (): void =>
        stream.abort(
          request.signal.reason instanceof Error
            ? request.signal.reason
            : new Error("public DA request aborted"),
        );
      request.signal.addEventListener("abort", onAbort, { once: true });
      await writeSingleFrame(stream, request.requestCbor, this.maxFrameBytes);
      await stream.close();
      return await readSingleFrame(stream, this.maxFrameBytes);
    } catch (cause) {
      stream.abort(
        cause instanceof Error
          ? cause
          : new Error(`public DA request failed: ${String(cause)}`),
      );
      throw cause;
    } finally {
      if (onAbort !== undefined) {
        request.signal.removeEventListener("abort", onAbort);
      }
    }
  }
}

export const createWatcherPublicDaLibp2pTransportV1 = async (
  options: WatcherPublicDaLibp2pTransportOptionsV1 = {},
): Promise<WatcherPublicDaLibp2pTransport> => {
  const transport = new WatcherPublicDaLibp2pTransport(options);
  await transport.start();
  return transport;
};

/** Exact unsigned 32-bit big-endian, one nonempty bounded frame. */
export const encodeWatcherPublicDaFrameV1 = (
  payload: Uint8Array,
  maxFrameBytes = DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
): Buffer => {
  if (!Number.isSafeInteger(payload.length) || payload.length <= 0) {
    throw new Error("public DA frame must not be empty");
  }
  if (payload.length > maxFrameBytes || payload.length > 0xffff_ffff) {
    throw new Error("public DA frame exceeds configured bound");
  }
  const frame = Buffer.allocUnsafe(4 + payload.length);
  frame.writeUInt32BE(payload.length, 0);
  Buffer.from(payload).copy(frame, 4);
  return frame;
};

const writeSingleFrame = async (
  stream: Pick<PublicDaStream, "send" | "onDrain">,
  payload: Uint8Array,
  maxFrameBytes: number,
): Promise<void> => {
  if (!stream.send(encodeWatcherPublicDaFrameV1(payload, maxFrameBytes))) {
    await stream.onDrain?.();
  }
};

export const readWatcherPublicDaFramesV1 = async function* (
  chunks: AsyncIterable<PublicDaStreamChunk>,
  maxFrameBytes = DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
): AsyncGenerator<Buffer> {
  let buffer = Buffer.alloc(0);
  for await (const chunk of chunks) {
    buffer = Buffer.concat([
      buffer,
      Buffer.from(chunk.subarray(0, chunk.byteLength)),
    ]);
    while (buffer.length >= 4) {
      const length = buffer.readUInt32BE(0);
      if (length === 0 || length > maxFrameBytes) {
        throw new Error("public DA frame exceeds configured bound or is empty");
      }
      if (buffer.length < 4 + length) break;
      yield buffer.subarray(4, 4 + length);
      buffer = buffer.subarray(4 + length);
    }
  }
  if (buffer.length !== 0) {
    throw new Error("incomplete public DA frame");
  }
};

const readSingleFrame = async (
  stream: AsyncIterable<PublicDaStreamChunk>,
  maxFrameBytes: number,
): Promise<Buffer> => {
  let frame: Buffer | undefined;
  for await (const candidate of readWatcherPublicDaFramesV1(
    stream,
    maxFrameBytes,
  )) {
    if (frame !== undefined)
      throw new Error("expected exactly one public DA frame");
    frame = candidate;
  }
  if (frame === undefined) throw new Error("missing public DA response frame");
  return frame;
};

const parseExpectedPeer = (
  request: WatcherPublicDaRequestV1,
): {
  readonly address: ReturnType<typeof multiaddr>;
  readonly peerId: ReturnType<typeof peerIdFromString>;
} => {
  const address = multiaddr(request.multiaddr);
  const components = address.getComponents();
  const names = components.map((component) => component.name);
  if (
    names.length !== 3 ||
    (names[0] !== "dns4" && names[0] !== "dns6") ||
    names[1] !== "tcp" ||
    names[2] !== "p2p"
  ) {
    throw new Error("watcher public DA requires a direct TCP DNS multiaddr");
  }
  const embeddedPeerId = components[2]?.value;
  if (embeddedPeerId !== request.peerId) {
    throw new Error("watcher public DA peer id does not match multiaddr");
  }
  return { address, peerId: peerIdFromString(request.peerId) };
};

const assertAuthenticatedPeer = (
  node: PublicDaRuntimeNode,
  peerId: ReturnType<typeof peerIdFromString>,
  expectedPeerId: string,
): void => {
  if (
    !node
      .getConnections(peerId)
      .some((connection) => connection.remotePeer.toString() === expectedPeerId)
  ) {
    throw new Error(
      "Noise-authenticated remote peer does not match configured peer id",
    );
  }
};

const defaultLibp2pFactory: WatcherPublicDaLibp2pFactoryV1 = async (
  options,
): Promise<PublicDaRuntimeNode> =>
  createLibp2p(options) as Promise<PublicDaRuntimeNode>;
