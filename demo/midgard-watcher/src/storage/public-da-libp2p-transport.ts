import {
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  DA_TRANSPORT_LIMITS,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { noise } from "@chainsafe/libp2p-noise";
import { yamux } from "@chainsafe/libp2p-yamux";
import { peerIdFromString } from "@libp2p/peer-id";
import { ping } from "@libp2p/ping";
import { tcp } from "@libp2p/tcp";
import { multiaddr } from "@multiformats/multiaddr";
import { createLibp2p, type Libp2pOptions } from "libp2p";

import { parseWatcherConfig } from "../runtime/config.js";
import { assertVerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import type {
  WatcherPublicDaLibp2pTransportV1,
  WatcherPublicDaRequest,
} from "./public-da-client.js";

type PublicDaStreamChunk =
  | Uint8Array
  | {
      readonly byteLength: number;
      subarray(start?: number, end?: number): Uint8Array;
    };

type PublicDaStream = AsyncIterable<PublicDaStreamChunk> & {
  readonly writeStatus?: string;
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

export type WatcherPublicDaLibp2pFactory = (
  options: Libp2pOptions,
) => Promise<PublicDaRuntimeNode>;

export type WatcherPublicDaLibp2pTransportOptions = Readonly<{
  libp2pFactory?: WatcherPublicDaLibp2pFactory;
  maxFrameBytes?: number;
}>;

/** Real watcher-owned TCP + Noise + Yamux transport. No discovery or inbound DA APIs. */
export class WatcherPublicDaLibp2pTransport
  implements WatcherPublicDaLibp2pTransportV1
{
  private readonly maxFrameBytes: number;
  private readonly factory: WatcherPublicDaLibp2pFactory;
  private node?: PublicDaRuntimeNode;

  constructor(options: WatcherPublicDaLibp2pTransportOptions = {}) {
    this.maxFrameBytes =
      options.maxFrameBytes ?? DA_TRANSPORT_LIMITS.maxPayloadBytes;
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
          // The standard ping protocol permits two overlapping inbound streams
          // while the remote closes one and opens its successor. No inbound
          // DA handlers or new inbound TCP connections are admitted.
          maxInboundStreams: 2,
          maxMessageSize: this.maxFrameBytes,
        }),
      ],
      services: { ping: ping() },
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

  async request(request: WatcherPublicDaRequest): Promise<Uint8Array> {
    const node = this.node;
    if (node === undefined) {
      throw new Error("watcher public DA libp2p transport is not started");
    }
    const { signal, protocolId, peerId: expectedPeerId } = request;
    signal.throwIfAborted();
    const requestFrame = encodeWatcherPublicDaFrame(
      request.requestCbor,
      this.maxFrameBytes,
    );
    const { address, peerId } = parseExpectedPeer(request);
    const readProtocol = DA_PUBLIC_RETAINED_DA_PROTOCOLS.find(
      (protocol) => protocol === request.protocol,
    );
    const fingerprint = protocolId.split("/")[2] ?? "";
    const retryRead =
      readProtocol !== undefined &&
      /^[0-9a-f]{64}$/.test(fingerprint) &&
      protocolId === daRequestResponseProtocolId(fingerprint, readProtocol);
    for (let attempt = 1; ; attempt += 1) {
      signal.throwIfAborted();
      if (this.node !== node)
        throw new Error(
          "watcher public DA libp2p transport stopped during request",
        );
      let stream: PublicDaStream | undefined;
      let onAbort: (() => void) | undefined;
      let phase = "dial";
      try {
        stream = await node.dialProtocol(address, protocolId, {
          signal,
          negotiateFully: false,
        });
        phase = "authenticate";
        assertAuthenticatedPeer(node, peerId, expectedPeerId);
        signal.throwIfAborted();
        if (this.node !== node)
          throw new Error(
            "watcher public DA libp2p transport stopped during dial",
          );
        const activeStream = stream;
        onAbort = (): void =>
          activeStream.abort(
            signal.reason instanceof Error
              ? signal.reason
              : new Error("public DA request aborted"),
          );
        signal.addEventListener("abort", onAbort, { once: true });
        phase = "write";
        if (!stream.send(requestFrame)) await stream.onDrain?.();
        phase = "close";
        await stream.close();
        phase = "read";
        return await readSingleFrame(stream, this.maxFrameBytes);
      } catch (cause) {
        const writeStatus = stream?.writeStatus;
        stream?.abort(
          cause instanceof Error
            ? cause
            : new Error("public DA request failed"),
        );
        signal.throwIfAborted();
        // Public read protocols only retrieve data. A closed stream may interrupt
        // negotiation, an underlying mux write, or a partial response. Discard
        // that attempt and repeat the identical read once, with its original
        // deadline. Authentication and framing failures are not closure errors.
        if (
          attempt === 1 &&
          retryRead &&
          this.node === node &&
          isClosedPublicDaStreamError(cause)
        )
          continue;
        const detail = (cause instanceof Error ? cause.message : String(cause))
          .replace(/[a-fA-F0-9]{128,}/g, "[hex omitted]")
          .slice(0, 1024);
        throw new Error(
          `public DA stream ${phase} failed (attempt ${attempt}, ${cause instanceof Error ? cause.name : "non-Error"}, writeStatus=${writeStatus ?? "unknown"}): ${detail}`,
          { cause },
        );
      } finally {
        if (onAbort !== undefined) {
          signal.removeEventListener("abort", onAbort);
        }
      }
    }
  }
}

const isClosedPublicDaStreamError = (cause: unknown): boolean => {
  if (!(cause instanceof Error)) return false;
  if (cause.name === "StreamStateError") {
    return (
      cause.message === "Cannot write to a stream that is closed" ||
      cause.message === "Cannot write to a stream that is closing"
    );
  }
  return [
    "StreamClosedError",
    "StreamResetError",
    "ConnectionClosedError",
    "ConnectionClosingError",
    "MuxerClosedError",
  ].includes(cause.name);
};

export const createWatcherPublicDaLibp2pTransport = async (
  options: WatcherPublicDaLibp2pTransportOptions = {},
): Promise<WatcherPublicDaLibp2pTransport> => {
  const transport = new WatcherPublicDaLibp2pTransport(options);
  await transport.start();
  return transport;
};

/** Exact unsigned 32-bit big-endian, one nonempty bounded frame. */
export const encodeWatcherPublicDaFrame = (
  payload: Uint8Array,
  maxFrameBytes = DA_TRANSPORT_LIMITS.maxPayloadBytes,
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

export const readWatcherPublicDaFrames = async function* (
  chunks: AsyncIterable<PublicDaStreamChunk>,
  maxFrameBytes = DA_TRANSPORT_LIMITS.maxPayloadBytes,
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
  for await (const candidate of readWatcherPublicDaFrames(
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
  request: WatcherPublicDaRequest,
): {
  readonly address: ReturnType<typeof multiaddr>;
  readonly peerId: ReturnType<typeof peerIdFromString>;
} => {
  const address = multiaddr(request.multiaddr);
  const components = address.getComponents();
  const names = components.map((component) => component.name);
  let customIp4 = false;
  if (names[0] === "ip4" && request.customNetwork !== undefined) {
    const { deploymentIdentity } = request.customNetwork;
    assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
    const config = parseWatcherConfig(request.customNetwork.watcherConfig);
    customIp4 =
      config.targetNetwork === "Custom" &&
      deploymentIdentity.network === "Custom" &&
      request.protocolId ===
        daRequestResponseProtocolId(
          deploymentIdentity.manifestId,
          request.protocol,
        ) &&
      config.da.peers.some(
        (peer) =>
          peer.identity === request.peerIdentity &&
          peer.peerId === request.peerId &&
          peer.multiaddr === request.multiaddr,
      );
  }
  if (
    names.length !== 3 ||
    (names[0] !== "dns4" && names[0] !== "dns6" && !customIp4) ||
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

const defaultLibp2pFactory: WatcherPublicDaLibp2pFactory = async (
  options,
): Promise<PublicDaRuntimeNode> =>
  createLibp2p(options) as Promise<PublicDaRuntimeNode>;
